#!/usr/bin/env python3
"""FB bot replay review engine — full game state tracking per decision.

Implements BOT_REVIEW_GAME_STATE_DEFINITION.md. Tracks every state fact the
definition doc calls out, snapshots at every FB decision, compares bot trace
vs baseline replay via similarity matching, classifies discrepancies.

Usage:
    python3 analyze-replay.py <file>
    python3 analyze-replay.py <bot> --compare <baseline>
    python3 analyze-replay.py <file> --per-decision       # dump every decision
    python3 analyze-replay.py <file> --per-ap             # per-AP milestones only
"""

import sys, re, os, copy
from collections import defaultdict

# ── CLI ─────────────────────────────────────────────────────────────────
if len(sys.argv) < 2:
    print("Usage: analyze-replay.py <file> [--compare baseline] [--per-decision] [--per-ap]")
    sys.exit(1)

path = sys.argv[1]
compare_path = None
per_decision = False
per_ap = False
baseline_folder = None
discrepancies_out = None
for i, arg in enumerate(sys.argv[2:], start=2):
    if arg == "--compare" and i + 1 < len(sys.argv):
        compare_path = sys.argv[i + 1]
    elif arg == "--baseline-folder" and i + 1 < len(sys.argv):
        baseline_folder = sys.argv[i + 1]
    elif arg == "--discrepancies-out" and i + 1 < len(sys.argv):
        discrepancies_out = sys.argv[i + 1]
    elif arg == "--per-decision":
        per_decision = True
    elif arg == "--per-ap":
        per_ap = True

# ── Constants ───────────────────────────────────────────────────────────
FACTION_NAMES = {
    "Firstborn": "FB", "Black Goat": "BG", "Yellow Sign": "YS", "Sleeper": "SL",
    "Windwalker": "WW", "Opener of the Way": "OW", "The Ancients": "AN",
    "Crawling Chaos": "CC", "Great Cthulhu": "GC", "Tombstalker": "TS",
}
FACTION_TIER_ORDER = ["SL", "CC", "BG", "WW", "GC", "TS", "AN", "OW"]  # for FB pain targeting

GOO_TYPES = {
    "Shub-Niggurath", "King in Yellow", "Hastur", "Nyarlathotep", "Cthulhu",
    "Tsathoggua", "Rhan-Tegoth", "Ithaqua", "Yog-Sothoth", "Ghatanothoa",
    "Glaaki",
}
IGOO_TYPES = {"Byatis", "Abhoth", "Daoloth", "Nyogtha", "Tulzscha", "Ygolonac"}
CULTIST_TYPES = {"Acolyte", "High Priest"}
# Monsters per faction (approximate; for threat classification)
OW_DREAD_UNITS = {"Abomination", "Spawn"}  # OW DreadCurse relevant

def strip_html(s):
    s = re.sub(r"<[^>]+>", "", s)
    s = s.replace("&gt;", ">").replace("&lt;", "<").replace("&nbsp;", " ").replace("&amp;", "&")
    return s.strip()

def load_file(p):
    """Return (html_game_log_lines, action_strings)."""
    with open(p) as f:
        raw = f.read()
    is_html = raw.lstrip().startswith("<") or "<!DOCTYPE" in raw[:500]
    html_log_lines = []
    action_strings = []
    if is_html:
        idx = raw.find('<div id="replay"')
        if idx < 0:
            idx = raw.find("<div id='replay'")
        if idx >= 0:
            start = raw.find(">", idx) + 1
            end = raw.find("</div>", start)
            replay_body = raw[start:end]
            action_strings = [l.strip() for l in replay_body.split("\n") if l.strip() and "(" in l and ")" in l]
        html_log_lines = [strip_html(l) for l in raw.splitlines() if l.strip()]
    else:
        parts = raw.split("\n\n", 1)
        if len(parts) > 0:
            action_strings = [l.strip() for l in parts[0].strip().split("\n") if l.strip()]
        if len(parts) > 1:
            html_log_lines = [strip_html(l) for l in parts[1].splitlines() if l.strip()]
    return html_log_lines, action_strings

def faction_prefix(line):
    """If line starts with a faction name, return (short, full, rest_of_line)."""
    for full, short in FACTION_NAMES.items():
        if line.startswith(full):
            return short, full, line[len(full):].strip()
    return None, None, line

# ── State classes ───────────────────────────────────────────────────────
class FactionState:
    def __init__(self):
        self.power = 0
        self.doom = 0
        self.es = 0
        self.gates = set()                        # regions
        self.awakens = 0
        self.rituals = 0
        self.rituals_detail = []                  # [(cost, doom, es), ...]
        self.craters = set()                      # FB only — regions
        self.sb_earned = set()
        self.sb_facedown = set()
        self.units_by_region = defaultdict(lambda: defaultdict(int))
        self.pool = defaultdict(int)
        self.captured_prisoners = 0

    # Max units FB can have on map at any time
    MAX_UNITS = {"Acolyte": 6, "Desiccated": 6, "Revenant of K'Naa": 2, "Ghatanothoa": 1, "High Priest": 1}

    def validate_units(self, event_info=""):
        """Error if any unit type exceeds its pool maximum."""
        for utype, max_count in self.MAX_UNITS.items():
            total = sum(d.get(utype, 0) for d in self.units_by_region.values())
            if total > max_count:
                regions = {r: d[utype] for r, d in self.units_by_region.items() if d.get(utype, 0) > 0}
                raise ValueError(f"BUG: {utype} count {total} exceeds max {max_count}. By region: {regions}. After event: {event_info}")

    def unit_count(self, utype):
        return sum(d.get(utype, 0) for d in self.units_by_region.values())

    def units_in(self, region):
        return dict(self.units_by_region.get(region, {}))

    def total_units(self):
        return sum(sum(d.values()) for d in self.units_by_region.values())

    def has_goo_at(self, region):
        return any(u in GOO_TYPES for u in self.units_by_region.get(region, {}))

    def has_monsterly_at(self, region):
        """Monsters + terrors (non-cultist, non-GOO)."""
        for u in self.units_by_region.get(region, {}):
            if u not in CULTIST_TYPES and u not in GOO_TYPES:
                return True
        return False

    def cultists_at(self, region):
        return sum(self.units_by_region.get(region, {}).get(u, 0) for u in CULTIST_TYPES)

    def clone(self):
        c = FactionState()
        c.power = self.power; c.doom = self.doom; c.es = self.es
        c.gates = set(self.gates); c.awakens = self.awakens; c.rituals = self.rituals
        c.rituals_detail = list(self.rituals_detail)
        c.craters = set(self.craters)
        c.sb_earned = set(self.sb_earned); c.sb_facedown = set(self.sb_facedown)
        c.units_by_region = defaultdict(lambda: defaultdict(int))
        for r, d in self.units_by_region.items():
            c.units_by_region[r] = defaultdict(int)
            for u, n in d.items():
                if n > 0:
                    c.units_by_region[r][u] = n
        c.pool = defaultdict(int, self.pool)
        c.captured_prisoners = self.captured_prisoners
        return c

class GameState:
    def __init__(self):
        self.setup = []                           # all factions (short)
        self.play_order = []                      # current round's order
        self.starting = {}                        # faction → region
        self.factions = defaultdict(FactionState)
        self.region_gate_owner = {}               # region → faction
        self.cathedrals = set()                   # regions
        self.desecrated_regions = set()
        self.iceAge_regions = set()               # set of regions WW marked
        self.round_num = 0
        self.doom_phase_active = False
        self.ritual_track_pos = 0                 # advanced each ritual (approx)
        self.ritual_cost = 5                      # starts at 5 (standard)
        self.ritual_history = []                  # [(faction, cost, doom)] chronological

    def region_state(self, region):
        """Full per-region snapshot."""
        units_by_faction = {}
        for f in self.setup:
            u = self.factions[f].units_in(region)
            if u:
                units_by_faction[f] = u
        return {
            "region": region,
            "gate_owner": self.region_gate_owner.get(region),
            "crater": any(region in self.factions[f].craters for f in self.setup),
            "cathedral": region in self.cathedrals,
            "desecrated": region in self.desecrated_regions,
            "ice_age": region in self.iceAge_regions,
            "units": units_by_faction,
        }

    def all_touched_regions(self):
        regs = set(self.region_gate_owner.keys())
        for f in self.setup:
            fs = self.factions[f]
            regs.update(fs.gates)
            regs.update(fs.craters)
            for r, d in fs.units_by_region.items():
                if any(n > 0 for n in d.values()):
                    regs.add(r)
        regs.update(self.cathedrals)
        regs.update(self.desecrated_regions)
        regs.update(self.iceAge_regions)
        return regs

    def clone(self):
        c = GameState()
        c.setup = list(self.setup); c.play_order = list(self.play_order)
        c.starting = dict(self.starting)
        c.factions = defaultdict(FactionState)
        for f in self.setup:
            c.factions[f] = self.factions[f].clone()
        c.region_gate_owner = dict(self.region_gate_owner)
        c.cathedrals = set(self.cathedrals)
        c.desecrated_regions = set(self.desecrated_regions)
        c.iceAge_regions = set(self.iceAge_regions)
        c.round_num = self.round_num
        c.doom_phase_active = self.doom_phase_active
        c.ritual_track_pos = self.ritual_track_pos
        c.ritual_cost = self.ritual_cost
        c.ritual_history = list(self.ritual_history)
        return c

# ── Event parser ────────────────────────────────────────────────────────
def parse_events(html_lines):
    """Return list of (line_idx, faction, etype, details) events extracted from the game log."""
    events = []
    for i, l in enumerate(html_lines):
        # Phase headers (standalone lines)
        if l == "DOOM PHASE":
            events.append((i, None, "doom_phase_start", None))
            continue
        if l == "ACTIONS":
            events.append((i, None, "action_phase_start", None))
            continue
        if l == "POWER GATHER":
            continue
        # Play order (round boundary)
        if l.startswith("Play order "):
            order_str = l[len("Play order "):]
            order_names = re.findall(r"([A-Z][a-z]+(?: [A-Z][a-z]+)*)", order_str)
            order_shorts = [FACTION_NAMES.get(n, n) for n in order_names]
            events.append((i, None, "round_start", order_shorts))
            continue

        short, full, rest = faction_prefix(l)
        if short is None:
            continue

        # Match each event pattern
        patterns = [
            (r"^started in (\w[\w ]*)$", "start"),
            (r"^built a gate in (\w[\w ]*)$", "build_gate"),
            (r"^lost control of the gate in (\w[\w ]*)$", "lost_gate"),
            (r"^gained control of the gate in (\w[\w ]*)$", "gained_gate"),
            (r"^got (\d+) Power", "power_gather"),
            (r"^power increased to (\d+) Power", "power_set"),
            (r"^got (\d+) Doom", "doom_gather"),
            (r"^gained (\d+) Power", "power_gain"),
            (r"^gained (\d+) Doom", "doom_gain"),
            (r"^ran out of power$", "out_of_power"),
            (r"^had no power$", "no_power"),
            (r"^hibernated for extra (\d+) Power", "hibernate"),
        ]
        matched = False
        for pat, etype in patterns:
            m = re.match(pat, rest)
            if m:
                if etype in ("build_gate", "lost_gate", "gained_gate", "start"):
                    events.append((i, short, etype, m.group(1).strip()))
                elif etype in ("power_gather", "power_set", "doom_gather", "power_gain", "doom_gain", "hibernate"):
                    events.append((i, short, etype, int(m.group(1))))
                else:
                    events.append((i, short, etype, None))
                matched = True
                break
        if matched:
            continue

        # Rituals
        m = re.match(r"^performed the ritual for (\d+) Power and gained (\d+) Doom", rest)
        if m:
            cost = int(m.group(1))
            doom = int(m.group(2))
            es = "Elder Sign" in rest
            events.append((i, short, "ritual", (cost, doom, es)))
            continue

        # Awakened GOO
        m = re.match(r"^awakened (\w[\w' ]*) in (\w[\w ]*) for (\d+)", rest)
        if m:
            unit = m.group(1).strip(); region = m.group(2).strip(); cost = int(m.group(3))
            events.append((i, short, "awaken", (unit, region, cost)))
            continue
        m = re.match(r"^awakened (\w[\w' ]*)$", rest)
        if m:
            events.append((i, short, "awaken_bare", m.group(1).strip()))
            continue

        # Cathedral (AN)
        m = re.match(r"^built a Cathedral in (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "cathedral", m.group(1).strip()))
            continue

        # Crater (FB)
        m = re.match(r"^placed Crater in (\w[\w ]*) with Devil", rest)
        if m:
            events.append((i, short, "crater", m.group(1).strip()))
            continue
        m = re.match(r"^Gate in (\w[\w ]*) destroyed by Crater", rest)
        if m:
            events.append((i, short, "gate_destroyed", m.group(1).strip()))
            continue

        # Spellbook earned
        m = re.match(r"^received (.+)$", rest)
        if m:
            events.append((i, short, "sb_earned", m.group(1).strip()))
            continue

        # Ice Age
        if "Ice Age" in rest:
            m2 = re.search(r"Ice Age (\w[\w ]*)", rest)
            if m2:
                events.append((i, short, "ice_age", m2.group(1).strip()))
                continue

        # Desecrated
        m = re.match(r"^desecrated (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "desecrate", m.group(1).strip()))
            continue

        # Movement
        m = re.match(r"^moved (\w[\w' ]*) from (\w[\w ]*) to (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "move", (m.group(1).strip(), m.group(2).strip(), m.group(3).strip())))
            continue

        # Recruit
        m = re.match(r"^recruited (\w[\w' ]*) in (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "recruit", (m.group(1).strip(), m.group(2).strip())))
            continue

        # Summon
        m = re.match(r"^summoned (\w[\w' ]*) in (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "summon", (m.group(1).strip(), m.group(2).strip())))
            continue

        # Capture — syntax: "X captured Y in Z" where Y is the captured unit.
        # The captured unit's faction (prefix) tells us the VICTIM.
        m = re.match(r"^captured ([\w' ]+?)\s*in (\w[\w ]*)", rest)
        if m:
            victim_unit = m.group(1).strip()
            region = m.group(2).strip()
            # If victim_unit is just "Acolyte" we don't know which faction from this line;
            # but HTML has faction spans earlier. Track the capture as (capturer, victim_faction=??, region).
            events.append((i, short, "capture", (victim_unit, region)))
            continue

        # Supply doom (e.g., YS Provide3Doom)
        m = re.match(r"^supplied (\w[\w ]*) with (\d+) Doom", rest)
        if m:
            target_full = m.group(1).strip()
            target_short = FACTION_NAMES.get(target_full, target_full)
            events.append((i, short, "supply_doom", (target_short, int(m.group(2)))))
            continue

        # Eliminated / killed
        m = re.match(r"^eliminated (\w[\w' ]*) in (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "eliminate", (m.group(1).strip(), m.group(2).strip())))
            continue

        # FB cultist captured by enemy — look in ALL lines for "captured FB/X"
        # The game log shows "<faction> captured FB <unit> in <region>"
        # but since we already parse faction_prefix, the `short` here is the capturer

        # FB Writhe events
        m = re.match(r"^used Writhe rolling (\d+) dice", rest)
        if m:
            events.append((i, short, "writhe_roll", int(m.group(1))))
            continue
        m = re.match(r"^rolled (\d+) Kill ?(\d+) Pain ?(\d+) Miss", rest)
        if m:
            events.append((i, short, "dice", (int(m.group(1)), int(m.group(2)), int(m.group(3)))))
            continue
        if "rerolled ALL dice with Writhe" in rest:
            events.append((i, short, "reroll", None))
            continue
        m = re.match(r"^Writhe: Acolyte replaced with Desiccated in (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "writhe_kill", m.group(1).strip()))
            continue
        m = re.match(r"^Writhe: eliminated (\w[\w' ]*) in (\w[\w ]*)", rest)
        if m:
            unit_name = m.group(1).strip()
            region = m.group(2).strip()
            events.append((i, short, "eliminate", (unit_name, region)))
            continue
        m = re.match(r"^Writhe: relocated (\w[\w' ]*) from (\w[\w ]*) to (\w[\w ]*)", rest)
        if m:
            events.append((i, short, "writhe_pain", (m.group(1).strip(), m.group(2).strip(), m.group(3).strip())))
            continue

        # Call of the Faithful
        m = re.match(r"^Call of the Faithful: placed Acolyte (?:on the gate )?in (\w[\w ]*)", rest)
        if m:
            on_gate = "on the gate" in rest
            events.append((i, short, "cof", (m.group(1).strip(), on_gate)))
            continue

        # Achievement (SBR satisfied)
        m = re.match(r"^achieved (.+)$", rest)
        if m:
            events.append((i, short, "achievement", m.group(1).strip()))
            continue

    return events

# ── Action-string parser (for full HTML replays without game log) ─────
def parse_action_strings(action_log):
    """Parse serialized action strings (e.g. StartingRegionAction(FB, Australia))
    into the same (line_idx, faction, etype, details) event tuples."""
    events = []
    for idx, s in enumerate(action_log):
        s = s.strip()
        # Phase marker actions (no parens arguments needed)
        if s.startswith("DoomPhaseAction"):
            events.append((idx, None, "doom_phase_start", None))
            continue
        if s.startswith("ActionPhaseAction"):
            events.append((idx, None, "action_phase_start", None))
            continue
        m = re.match(r"(\w+)\((.*)\)$", s)
        if not m:
            continue
        atype = m.group(1)
        argstr = m.group(2)
        # Split args by top-level comma (ignoring nested parens/brackets)
        args = []
        depth = 0
        current = ""
        for ch in argstr:
            if ch in "([":
                depth += 1; current += ch
            elif ch in ")]":
                depth -= 1; current += ch
            elif ch == "," and depth == 0:
                args.append(current.strip())
                current = ""
            else:
                current += ch
        if current.strip():
            args.append(current.strip())

        fname = args[0] if args else None

        if atype == "StartingRegionAction" and len(args) >= 2:
            events.append((idx, fname, "start", args[1]))
        elif atype == "PlayDirectionAction":
            # Round starts implicitly — treat as play_order
            order = re.findall(r"\b(FB|BG|YS|SL|WW|OW|AN|CC|GC|TS)\b", argstr)
            events.append((idx, None, "round_start", order))
        elif atype == "BuildGateAction" and len(args) >= 2:
            events.append((idx, fname, "build_gate", args[1]))
        elif atype == "FBAwakenGhatanothoaAction" and len(args) >= 2:
            cost_str = args[1]
            try: cost = int(cost_str)
            except: cost = 0
            start_region = "?"  # Ghato placed at starting region
            events.append((idx, fname, "awaken", ("Ghatanothoa", start_region, cost)))
        elif atype == "AwakenAction" and len(args) >= 4:
            unit = args[1].split("/")[-1] if "/" in args[1] else args[1]
            region = args[2]
            try: cost = int(args[3])
            except: cost = 0
            events.append((idx, fname, "awaken", (unit, region, cost)))
        elif atype == "RitualAction" and len(args) >= 2:
            try: cost = int(args[1])
            except: cost = 0
            try: es = int(args[2]) if len(args) >= 3 else 0
            except: es = 0
            # In standard CW, ritual doom gained equals the cost.
            events.append((idx, fname, "ritual", (cost, cost, es > 0)))
        elif atype == "ElderSignAction" and len(args) >= 3:
            # ElderSignAction(faction, count, value, revealed?, context)
            # Each reveal contributes `value` doom.
            try: value = int(args[2])
            except: value = 0
            events.append((idx, fname, "doom_gain", value))
        elif atype == "FBDevilsMarkPlaceCraterAction" and len(args) >= 2:
            events.append((idx, fname, "crater", args[1]))
        elif atype == "MoveAction" and len(args) >= 4:
            # MoveAction(FB, FB/Acolyte/3, SrcRegion, DstRegion, cost)
            unit_ref = args[1]
            src = args[2]
            dst = args[3]
            unit_type = unit_ref.split("/")[-2] if unit_ref.count("/") >= 2 else unit_ref
            events.append((idx, fname, "move", (unit_type, src, dst)))
        elif atype == "RecruitAction" and len(args) >= 3:
            unit = args[1]
            region = args[2]
            events.append((idx, fname, "recruit", (unit, region)))
        elif atype == "SummonAction" and len(args) >= 3:
            unit = args[1]
            region = args[2]
            events.append((idx, fname, "summon", (unit, region)))
        elif atype == "CaptureTargetAction" and len(args) >= 4:
            # CaptureTargetAction(victim, region, captor, unitRef, effect)
            # Emit with captor as the faction for prisoner counting, and carry
            # the victim faction in details so FB-capture tracking can still key
            # off it.
            victim = fname
            region = args[1]
            captor = args[2]
            unit_ref = args[3]
            events.append((idx, captor, "capture", (unit_ref, region, victim)))
        elif atype == "SpellbookAction" and len(args) >= 2:
            events.append((idx, fname, "sb_earned", args[1]))
        elif atype == "FBWritheMainAction":
            events.append((idx, fname, "writhe_roll", 0))
        elif atype == "FBWritheRerollAction":
            events.append((idx, fname, "reroll", None))
        elif atype == "FBWritheKillUnitAction" and len(args) >= 2:
            unit_ref = args[1]
            # Extract region from context — not directly in action
            events.append((idx, fname, "writhe_kill_ref", unit_ref))
        elif atype == "FBWritheMoveOneToRegionAction" and len(args) >= 3:
            unit_ref = args[1]
            dst = args[2]
            unit_type = unit_ref.split("/")[-2] if unit_ref.count("/") >= 2 else unit_ref
            events.append((idx, fname, "writhe_pain", (unit_type, "?", dst)))
        elif atype == "FBWritheMoveOneJoinAction" and len(args) >= 3:
            unit_ref = args[1]
            dst = args[2]
            unit_type = unit_ref.split("/")[-2] if unit_ref.count("/") >= 2 else unit_ref
            events.append((idx, fname, "writhe_pain", (unit_type, "?", dst)))
        elif atype == "FBCallOfTheFaithfulAction" and len(args) >= 2:
            events.append((idx, fname, "cof", (args[1], False)))
        elif atype == "BuildCathedralAction" and len(args) >= 2:
            events.append((idx, fname, "cathedral", args[1]))
        elif atype == "IceAgeAction" and len(args) >= 2:
            events.append((idx, fname, "ice_age", args[1]))
        elif atype == "DesecrateAction" and len(args) >= 2:
            events.append((idx, fname, "desecrate", args[1]))
    return events

# ── State updater ───────────────────────────────────────────────────────
def apply_event(state, faction, etype, details):
    if faction is None:
        if etype == "round_start":
            # PlayDirectionAction = authoritative round marker (present in both
            # sim traces and HRF saved replays). Each occurrence begins a new round.
            state.round_num += 1
            state.play_order = details[:] if details else state.play_order
            state.doom_phase_active = False
            # Compute power from state using CW gather-power formula:
            #   power = 2*gates + abandoned + cultists + captured
            # This always fires at round_start. For bot traces with HTML log,
            # the subsequent HTML power_gather event (inserted right after
            # round_start by _merge_state_events) overwrites this with the
            # exact game value. For baselines without a game log, this
            # heuristic is the authoritative value.
            abandoned_count = sum(1 for r, owner in state.region_gate_owner.items() if owner is None)
            for f in state.setup:
                fs = state.factions[f]
                cultist_count = sum(
                    sum(c for u, c in d.items() if u in CULTIST_TYPES)
                    for d in fs.units_by_region.values()
                )
                fs.power = (len(fs.gates) * 2 + abandoned_count +
                            cultist_count + fs.captured_prisoners)
        elif etype == "doom_phase_start":
            state.doom_phase_active = True
        elif etype == "action_phase_start":
            state.doom_phase_active = False
        return

    fs = state.factions[faction]

    if etype == "start":
        region = details
        state.starting[faction] = region
        if faction not in state.setup:
            state.setup.append(faction)
        fs.gates.add(region)
        state.region_gate_owner[region] = faction
        # Each faction starts with 6 acolytes in start region (standard CW)
        fs.units_by_region[region]["Acolyte"] = 6
    elif etype == "build_gate":
        fs.gates.add(details)
        state.region_gate_owner[details] = faction
    elif etype == "lost_gate":
        fs.gates.discard(details)
        if state.region_gate_owner.get(details) == faction:
            # Gate is now abandoned (still on board, no owner).
            state.region_gate_owner[details] = None
    elif etype == "gained_gate":
        fs.gates.add(details)
        state.region_gate_owner[details] = faction
    elif etype == "power_gather":
        fs.power = details
    elif etype == "power_set":
        fs.power = details
    elif etype == "power_gain":
        fs.power += details
    elif etype == "doom_gather":
        fs.doom += details
    elif etype == "doom_gain":
        fs.doom += details
    elif etype == "hibernate":
        fs.power += details
    elif etype in ("out_of_power", "no_power"):
        fs.power = 0
    elif etype == "ritual":
        cost, doom, es = details
        fs.power = max(0, fs.power - cost)
        fs.doom += doom
        if es:
            fs.es += 1
        fs.rituals += 1
        fs.rituals_detail.append(details)
        state.ritual_track_pos += 1
        state.ritual_history.append((faction, cost, doom))
    elif etype == "awaken":
        unit, region, cost = details
        fs.power = max(0, fs.power - cost)
        fs.awakens += 1
        # Remove existing copy first (re-awaken: old one was killed but
        # the kill event may not have been processed yet due to event ordering)
        for r in list(fs.units_by_region.keys()):
            if fs.units_by_region[r].get(unit, 0) > 0:
                fs.units_by_region[r][unit] -= 1
                if fs.units_by_region[r][unit] <= 0:
                    del fs.units_by_region[r][unit]
                break
        fs.units_by_region[region][unit] += 1
    elif etype == "awaken_bare":
        fs.awakens += 1
    elif etype == "cathedral":
        state.cathedrals.add(details)
    elif etype == "crater":
        fs.craters.add(details)
    elif etype == "gate_destroyed":
        fs.gates.discard(details)
        if state.region_gate_owner.get(details) == faction:
            del state.region_gate_owner[details]
    elif etype == "sb_earned":
        fs.sb_earned.add(details)
    elif etype == "ice_age":
        state.iceAge_regions.add(details)
    elif etype == "desecrate":
        state.desecrated_regions.add(details)
    elif etype == "move":
        unit, src, dst = details
        if fs.units_by_region[src].get(unit, 0) > 0:
            fs.units_by_region[src][unit] -= 1
            if fs.units_by_region[src][unit] <= 0:
                del fs.units_by_region[src][unit]
            fs.units_by_region[dst][unit] += 1
    elif etype == "recruit":
        unit, region = details
        total_before = sum(d.get(unit, 0) for d in fs.units_by_region.values())
        max_allowed = FactionState.MAX_UNITS.get(unit, 99)
        if total_before < max_allowed:
            fs.units_by_region[region][unit] += 1
    elif etype == "summon":
        unit, region = details
        total_before = sum(d.get(unit, 0) for d in fs.units_by_region.values())
        max_allowed = FactionState.MAX_UNITS.get(unit, 99)
        if total_before < max_allowed:
            fs.units_by_region[region][unit] += 1
    elif etype == "capture":
        # Two formats: (unit_ref, region, victim_faction) or (unit_name, region)
        if isinstance(details, tuple):
            if len(details) == 3:
                _, region, victim_faction = details
                victim_fs = state.factions.get(victim_faction)
                if victim_fs:
                    for ut in CULTIST_TYPES:
                        if victim_fs.units_by_region[region].get(ut, 0) > 0:
                            victim_fs.units_by_region[region][ut] -= 1
                            if victim_fs.units_by_region[region][ut] <= 0:
                                del victim_fs.units_by_region[region][ut]
                            break
            elif len(details) == 2:
                unit_name, region = details
                # Captor is `faction`. Victim is whoever has a cultist at that region.
                # Try all other factions.
                for vf_name, vfs in state.factions.items():
                    if vf_name != faction:
                        for ut in CULTIST_TYPES:
                            if vfs.units_by_region[region].get(ut, 0) > 0:
                                vfs.units_by_region[region][ut] -= 1
                                if vfs.units_by_region[region][ut] <= 0:
                                    del vfs.units_by_region[region][ut]
                                break
        fs.captured_prisoners += 1
    elif etype == "supply_doom":
        target, amt = details
        state.factions[target].doom += amt
    elif etype == "eliminate":
        unit, region = details
        if fs.units_by_region[region].get(unit, 0) > 0:
            fs.units_by_region[region][unit] -= 1
            if fs.units_by_region[region][unit] <= 0:
                del fs.units_by_region[region][unit]
    elif etype == "writhe_kill":
        region = details
        if fs.units_by_region[region].get("Acolyte", 0) > 0:
            fs.units_by_region[region]["Acolyte"] -= 1
            if fs.units_by_region[region]["Acolyte"] <= 0:
                del fs.units_by_region[region]["Acolyte"]
        desc_total = sum(d.get("Desiccated", 0) for d in fs.units_by_region.values())
        if desc_total < FactionState.MAX_UNITS.get("Desiccated", 6):
            fs.units_by_region[region]["Desiccated"] += 1
    elif etype == "writhe_kill_ref":
        unit_ref = details  # e.g. "FB/Acolyte/3"
        parts = unit_ref.replace(" ", "").split("/")
        unit_type = parts[1] if len(parts) >= 2 else "Acolyte"
        # Map unit class names
        type_map = {"Acolyte": "Acolyte", "Desiccated": "Desiccated",
                    "RevenantOfKnaa": "Revenant of K'Naa", "Ghatanothoa": "Ghatanothoa",
                    "HighPriest": "High Priest"}
        unit_name = type_map.get(unit_type, unit_type)
        # Find which region has this unit type and remove it
        killed = False
        for r in list(fs.units_by_region.keys()):
            if fs.units_by_region[r].get(unit_name, 0) > 0:
                fs.units_by_region[r][unit_name] -= 1
                if fs.units_by_region[r][unit_name] <= 0:
                    del fs.units_by_region[r][unit_name]
                # If it was an acolyte, add desiccated in same region (if pool allows)
                if unit_name == "Acolyte":
                    desc_total = sum(d.get("Desiccated", 0) for d in fs.units_by_region.values())
                    max_desc = FactionState.MAX_UNITS.get("Desiccated", 6)
                    if desc_total < max_desc:
                        fs.units_by_region[r]["Desiccated"] += 1
                    # else: pool empty, desiccated not created
                elif unit_name == "Ghatanothoa":
                    pass  # Ghato killed, goes to pool (no replacement created)
                killed = True
                break
    elif etype == "writhe_kill_desc":
        region = details
        if fs.units_by_region[region].get("Desiccated", 0) > 0:
            fs.units_by_region[region]["Desiccated"] -= 1
    elif etype == "writhe_pain":
        unit, src, dst = details
        removed = False
        if src == "?":
            for r in list(fs.units_by_region.keys()):
                if r != dst and fs.units_by_region[r].get(unit, 0) > 0:
                    fs.units_by_region[r][unit] -= 1
                    if fs.units_by_region[r][unit] <= 0:
                        del fs.units_by_region[r][unit]
                    removed = True
                    break
        elif fs.units_by_region[src].get(unit, 0) > 0:
            fs.units_by_region[src][unit] -= 1
            if fs.units_by_region[src][unit] <= 0:
                del fs.units_by_region[src][unit]
            removed = True
        if removed:
            fs.units_by_region[dst][unit] += 1
    elif etype == "cof":
        region, on_gate = details
        aco_total = sum(d.get("Acolyte", 0) for d in fs.units_by_region.values())
        if aco_total < FactionState.MAX_UNITS.get("Acolyte", 6):
            fs.units_by_region[region]["Acolyte"] += 1

# ── Derived: threats and opportunities ─────────────────────────────────
def derive_threats_and_opps(state, target="FB"):
    """From full state, derive threat and opportunity lists for a faction."""
    fs = state.factions[target]
    threats = []
    opps = []

    # Per-region analysis
    for r in state.all_touched_regions():
        rs = state.region_state(r)
        fb_units = rs["units"].get(target, {})
        fb_cultists = sum(fb_units.get(u, 0) for u in CULTIST_TYPES)
        fb_goo = any(u in GOO_TYPES for u in fb_units)
        fb_monster = any(u not in CULTIST_TYPES and u not in GOO_TYPES for u in fb_units)

        # Enemy presence in this region
        for ef in state.setup:
            if ef == target:
                continue
            eu = rs["units"].get(ef, {})
            if not eu:
                continue
            e_cult = sum(eu.get(u, 0) for u in CULTIST_TYPES)
            e_monster = any(u not in CULTIST_TYPES and u not in GOO_TYPES for u in eu)
            e_goo = any(u in GOO_TYPES for u in eu)

            # Opportunity: lone unprotected enemy cultist
            if e_cult > 0 and not e_monster and not e_goo:
                if e_cult == 1:
                    opps.append(f"lone_cultist:{ef}:{r}")
                else:
                    opps.append(f"multi_cultist:{ef}:{r}")

            # Opportunity: vulnerable enemy gate (no GOO defender)
            if rs["gate_owner"] == ef and not e_goo:
                opps.append(f"vulnerable_enemy_gate:{ef}:{r}")

            # Threat: enemy monster/goo adjacent to FB cultists (capture risk)
            if fb_cultists > 0 and not fb_monster and not fb_goo and (e_monster or e_goo):
                threats.append(f"capture_risk:{ef}:{r}")

            # Threat: enemy threatens FB gate
            if rs["gate_owner"] == target and (e_monster or e_goo):
                threats.append(f"gate_threat:{ef}:{r}")

        # Opportunity: empty land region (no units, no gate, land)
        if not rs["units"] and not rs["gate_owner"] and not rs["crater"]:
            opps.append(f"empty_region:{r}")

    # Threat: WW Ice Age
    for ir in state.iceAge_regions:
        if ir in [r for r in fs.units_by_region if fs.units_by_region[r]]:
            threats.append(f"ice_age:{ir}")

    # Threat: OW DreadCurse + 3+ dread units
    if "OW" in state.setup:
        ow = state.factions["OW"]
        dread_count = 0
        for r, d in ow.units_by_region.items():
            for u, c in d.items():
                if any(k in u for k in OW_DREAD_UNITS):
                    dread_count += c
        if dread_count >= 3 and "Dread Curse" in ow.sb_earned:
            # Check if FB Ghato is alone
            for r, d in fs.units_by_region.items():
                if "Ghatanothoa" in d:
                    others = sum(c for u, c in d.items() if u != "Ghatanothoa")
                    if others == 0:
                        threats.append(f"dread_curse_ghato_alone:{r}")

    # Threat: AN Cathedrals with combat
    if "AN" in state.setup:
        an = state.factions["AN"]
        for cr in state.cathedrals:
            combat = sum(c for u, c in an.units_by_region.get(cr, {}).items()
                        if u not in CULTIST_TYPES)
            if combat > 0:
                threats.append(f"an_cathedral_combat:{cr}")

    return threats, opps

# ── Snapshot at a moment ───────────────────────────────────────────────
def snapshot(state, target="FB"):
    fs = state.factions[target]
    threats, opps = derive_threats_and_opps(state, target)
    snap = {
        "round": state.round_num,
        "doom_phase": state.doom_phase_active,
        "ritual_cost": state.ritual_cost,
        "ritual_track": state.ritual_track_pos,
        "fb": {
            "power": fs.power,
            "doom": fs.doom,
            "es": fs.es,
            "real_doom": fs.doom + fs.es,
            "gates": sorted(fs.gates),
            "gate_count": len(fs.gates),
            "awakens": fs.awakens,
            "rituals": fs.rituals,
            "craters": sorted(fs.craters),
            "crater_count": len(fs.craters),
            "sb_earned": sorted(fs.sb_earned),
            "sb_facedown": sorted(fs.sb_facedown),
            "total_units_on_map": fs.total_units(),
            "units_by_region": {r: dict(d) for r, d in fs.units_by_region.items() if any(n > 0 for n in d.values())},
            "acolyte_count": fs.unit_count("Acolyte"),
            "hp_count": fs.unit_count("High Priest"),
            "desiccated_count": fs.unit_count("Desiccated"),
            "revenant_count": fs.unit_count("Revenant of K'Naa"),
            "ghato_on_map": fs.unit_count("Ghatanothoa") > 0,
        },
        "enemies": {},
        "regions": {},
        "threats": threats,
        "opportunities": opps,
        "cathedrals": sorted(state.cathedrals),
        "desecrated": sorted(state.desecrated_regions),
        "ice_age": sorted(state.iceAge_regions),
    }
    # Enemy states
    for f in state.setup:
        if f == target:
            continue
        efs = state.factions[f]
        snap["enemies"][f] = {
            "power": efs.power,
            "doom": efs.doom,
            "gates": sorted(efs.gates),
            "gate_count": len(efs.gates),
            "awakens": efs.awakens,
            "rituals": efs.rituals,
            "sb_earned": sorted(efs.sb_earned),
            "total_units": efs.total_units(),
        }
    # Per-region full state
    for r in state.all_touched_regions():
        snap["regions"][r] = state.region_state(r)
    return snap

# ── State-event merge (action-string stream + HTML state events) ──────
# The HTML game log is the source of truth for ALL state mutations (unit
# movements, kills, power, gates, doom, captures, eliminations, writhe
# results, etc.).  Action strings are used ONLY for action identification
# (round markers, phase markers, spellbook earned, and FB decision events
# used by the comparison engine).
#
# Previous design merged a small whitelist of HTML events into the action-
# string stream, which caused double-counting (e.g. writhe kills counted
# from both writhe_kill_ref action strings AND writhe_kill HTML events).

# Action-string event types to KEEP in the merged stream.  Everything else
# from action strings is dropped — the HTML log already covers it.
_ACTION_KEEP_ETYPES = {
    "round_start",          # round boundary / play order
    "doom_phase_start",     # phase markers
    "action_phase_start",
    "writhe_kill_ref",      # Ghato kills via Writhe have no HTML equivalent
    # NOTE: sb_earned comes from HTML too ("received X") — no need to
    # duplicate from action strings.  HTML version has human-readable names.
}

def _merge_state_events(action_events, html_events):
    """Return a merged event list.  HTML events supply ALL state changes.
    Action-string events supply only phase/round markers and spellbook
    events (for the comparison engine).

    Round boundaries are detected in the action stream via `round_start`.
    HTML state events are bucketed by their HTML-round position and inserted
    at the START of the matching action-stream round (so power-gather
    applies before any action that spends power)."""
    if not html_events:
        return list(action_events)

    # Build html_events_by_round using the html's own round_start markers.
    # Include ALL html events (they are the authoritative state source).
    # Round 0 = setup events (before first Play order line).
    html_by_round = defaultdict(list)
    html_round = 0
    for ev in html_events:
        (i, f, et, d) = ev
        if f is None and et == "round_start":
            html_round += 1
            continue  # action stream's round_start is the canonical marker
        # Skip phase markers from HTML — action stream's are canonical
        if f is None and et in ("doom_phase_start", "action_phase_start"):
            continue
        html_by_round[html_round].append(ev)

    merged = []
    action_round = 0
    setup_injected = False
    state_inserted_for_round = set()

    def inject_setup():
        """Inject round-0 (setup) HTML events at the very start."""
        nonlocal setup_injected
        if setup_injected:
            return
        setup_injected = True
        for ev in html_by_round.get(0, []):
            merged.append(ev)

    def inject_round(r):
        if r in state_inserted_for_round:
            return
        state_inserted_for_round.add(r)
        for ev in html_by_round.get(r, []):
            merged.append(ev)

    for ev in action_events:
        (i, f, et, d) = ev
        if f is None and et == "round_start":
            # Before first round_start, inject setup events
            inject_setup()
            action_round += 1
            merged.append(ev)
            inject_round(action_round)
            continue
        # Only keep action-string events in the allow-list; drop all others
        # (their state effects are already covered by the HTML events).
        if et not in _ACTION_KEEP_ETYPES:
            continue
        if action_round == 0:
            merged.append(ev)
            continue
        inject_round(action_round)
        merged.append(ev)

    # Inject any remaining HTML rounds that had no matching action-stream round
    # (e.g. if HTML has more rounds than action strings, or setup wasn't injected).
    inject_setup()
    for r in sorted(html_by_round.keys()):
        if r == 0:
            continue  # already handled by inject_setup
        if r not in state_inserted_for_round:
            for ev in html_by_round[r]:
                merged.append(ev)

    return merged

# ── Reconstruction ──────────────────────────────────────────────────────
class Reconstruction:
    def __init__(self, html_lines, action_strings=None):
        self.html_lines = html_lines
        self.action_strings = action_strings or []
        # HTML game log = source of truth for all state mutations.
        # Action strings = source of truth for action identification only.
        # When both are available, merge: HTML provides state, action strings
        # provide round/phase markers and spellbook events.
        events_from_actions = parse_action_strings(action_strings) if action_strings else []
        events_html = parse_events(html_lines) if html_lines else []
        if events_from_actions and any(f == "FB" for (_, f, _, _) in events_from_actions):
            primary = _merge_state_events(events_from_actions, events_html)
            self.events = primary
        elif events_html and any(f == "FB" for (_, f, _, _) in events_html):
            self.events = events_html
        else:
            self.events = events_from_actions or events_html
        self.final_state = None
        self.fb_decision_snapshots = []  # [(line_idx, event_type, details, snap_before)]
        self.fb_captures_per_ap = defaultdict(int)  # AP -> count of FB units captured by enemies
        self._walk()
        self._count_fb_captures()

    def _count_fb_captures(self):
        """Scan action_strings (reliable victim info) for FB captures per AP."""
        ap = 0
        for s in self.action_strings:
            s = s.strip()
            if s.startswith("PlayDirectionAction"):
                ap += 1
                continue
            # CaptureTargetAction(victim, region, capturer, unit_ref, effect)
            m = re.match(r"CaptureTargetAction\(FB,", s)
            if m:
                self.fb_captures_per_ap[max(1, ap)] += 1

    def _walk(self):
        state = GameState()
        # Event types that fire during the doom phase — independent of whether
        # the trace has explicit phase markers.
        DOOM_ETYPES = {"ritual", "crater"}
        for (i, f, et, d) in self.events:
            if f == "FB" and et in (
                "build_gate", "lost_gate", "gained_gate", "ritual", "awaken", "awaken_bare",
                "cathedral", "crater", "sb_earned", "move", "recruit", "summon", "capture",
                "eliminate", "writhe_roll", "writhe_kill", "writhe_pain", "cof",
                "ice_age", "desecrate", "reroll", "writhe_kill_desc",
            ):
                snap = snapshot(state, "FB")
                is_doom = (et in DOOM_ETYPES) or state.doom_phase_active
                # Off-by-one fix: game increments `turn` at PowerGather BEFORE
                # the doom phase for the prior AP runs. So doom events fire
                # at round_num = N+1 but conceptually belong to AP N.
                # Action events fire at round_num = N and belong to AP N.
                snap["ap"] = max(1, state.round_num - 1) if is_doom else max(1, state.round_num)
                snap["phase"] = "doom" if is_doom else "action"
                action_str = self.action_strings[i] if (self.action_strings and i < len(self.action_strings)) else f"{et} {d}"
                self.fb_decision_snapshots.append((i, et, d, snap, action_str))
            apply_event(state, f, et, d)
            if "FB" in state.factions:
                state.factions["FB"].validate_units(event_info=f"f={f} {et} {d}")
        self.final_state = state

    def final_doom(self):
        """Return dict faction → final revealed doom (from ritual events + doom_gain + supply_doom)."""
        doom = defaultdict(int)
        for (i, f, et, d) in self.events:
            if f is None:
                continue
            if et == "ritual":
                _, dm, _ = d
                doom[f] += dm
            elif et in ("doom_gain", "doom_gather"):
                doom[f] += d
            elif et == "supply_doom":
                target, amt = d
                doom[target] += amt
        return dict(doom)

    def fb_won(self):
        doom = self.final_doom()
        if "FB" not in doom:
            return False
        fb_d = doom["FB"]
        return all(fb_d >= v for v in doom.values()) and fb_d > 0 and \
               fb_d == max(doom.values())

    def per_ap_snapshots(self):
        """Return per-AP snapshots of FB state at round boundaries."""
        state = GameState()
        results = []
        current_ap_snapshot = None
        last_round = 0
        for (i, f, et, d) in self.events:
            if f is None and et == "round_start":
                new_round = state.round_num + 1  # about to be
                # Before advancing, capture the state at start of the new round
                # We need to apply round_start first
                apply_event(state, f, et, d)
                results.append((state.round_num, snapshot(state, "FB")))
                continue
            apply_event(state, f, et, d)
        return results

# ── Similarity distance ─────────────────────────────────────────────────
def distance(s1, s2):
    """Lower = more similar. Weighted multi-dimensional."""
    if s1["ap"] != s2["ap"]:
        return 10000
    fb1, fb2 = s1["fb"], s2["fb"]
    d = 0
    d += abs(fb1["power"] - fb2["power"]) * 30
    d += abs(fb1["gate_count"] - fb2["gate_count"]) * 200
    d += abs(fb1["awakens"] - fb2["awakens"]) * 150
    d += abs(fb1["rituals"] - fb2["rituals"]) * 100
    d += abs(fb1["crater_count"] - fb2["crater_count"]) * 80
    d += abs(len(fb1["sb_earned"]) - len(fb2["sb_earned"])) * 40
    d += abs(fb1["desiccated_count"] - fb2["desiccated_count"]) * 30
    d += abs(fb1["revenant_count"] - fb2["revenant_count"]) * 40
    d += (0 if fb1["ghato_on_map"] == fb2["ghato_on_map"] else 200)
    # Threat / opportunity class overlap
    o1 = set(x.split(":")[0] for x in s1.get("opportunities", []))
    o2 = set(x.split(":")[0] for x in s2.get("opportunities", []))
    d += (len(o1 ^ o2)) * 40
    t1 = set(x.split(":")[0] for x in s1.get("threats", []))
    t2 = set(x.split(":")[0] for x in s2.get("threats", []))
    d += (len(t1 ^ t2)) * 40
    return d

# ── Comparison ──────────────────────────────────────────────────────────
SIMILARITY_MATCH = 150
SIMILARITY_SITUATIONAL = 600

def compare_multi(bot_recon, baselines):
    """Compare every bot FB decision to the corresponding baseline decision.

    Within each (AP, phase) bucket:
      1. Pick the SINGLE closest baseline (by state distance of the bucket's
         first bot decision) — so the comparison reads a coherent baseline game.
      2. Pair positionally within the bucket: bot decision #k matches baseline
         decision #k from the selected baseline.

    Returns a list of comparison entries."""
    bot_decisions = bot_recon.fb_decision_snapshots

    # Group bot decisions by (ap, phase), preserving order
    bot_by_key = defaultdict(list)
    for entry in bot_decisions:
        s = entry[3]
        bot_by_key[(s["ap"], s["phase"])].append(entry)

    # For each baseline, group its decisions similarly
    base_buckets = []  # [(label, {(ap,phase): [entries]}), ...]
    for label, recon in baselines:
        b = defaultdict(list)
        for entry in recon.fb_decision_snapshots:
            s = entry[3]
            b[(s["ap"], s["phase"])].append(entry)
        base_buckets.append((label, b))

    entries = []
    for key in sorted(bot_by_key.keys()):
        bot_bucket = bot_by_key[key]
        if not bot_bucket:
            continue

        # Choose the closest baseline for this bucket based on the first
        # bot decision's state vs the baseline bucket's first decision.
        first_snap = bot_bucket[0][3]
        best_base_label = None
        best_base_bucket = None
        best_initial_dist = 99999
        for lbl, b in base_buckets:
            base_bucket = b.get(key, [])
            if not base_bucket:
                continue
            d = distance(first_snap, base_bucket[0][3])
            if d < best_initial_dist:
                best_initial_dist = d
                best_base_label = lbl
                best_base_bucket = base_bucket

        for idx_in_bucket, bot_ent in enumerate(bot_bucket):
            (i, etype, details, snap, action_str) = bot_ent
            if best_base_bucket is None:
                entries.append({
                    "ap": key[0], "phase": key[1],
                    "class": "NO_BASELINE", "distance": 99999,
                    "bot_line": i, "bot_etype": etype,
                    "bot_action": action_str, "bot_snap": snap,
                    "base_label": None, "base_action": None, "base_snap": None,
                })
                continue
            if idx_in_bucket < len(best_base_bucket):
                base_ent = best_base_bucket[idx_in_bucket]
            else:
                # Bot has more actions in this bucket than baseline
                base_ent = best_base_bucket[-1]
            base_snap = base_ent[3]
            base_action_str = base_ent[4]
            base_etype = base_ent[1]
            d = distance(snap, base_snap)
            if d <= SIMILARITY_MATCH:
                cls = "MATCH" if etype == base_etype else "TRULY DIFFERENT"
            elif d <= SIMILARITY_SITUATIONAL:
                cls = "NEAR_MATCH" if etype == base_etype else "LIKELY DIFFERENT"
            else:
                cls = "SITUATIONAL"
            entries.append({
                "ap": key[0], "phase": key[1],
                "class": cls, "distance": d,
                "bot_line": i, "bot_etype": etype,
                "bot_action": action_str, "bot_snap": snap,
                "base_label": best_base_label,
                "base_action": base_action_str,
                "base_snap": base_snap,
            })
    return entries

# ── Report helpers ──────────────────────────────────────────────────────
def print_summary(recon, label):
    state = recon.final_state
    print("=" * 76)
    print(label)
    print("=" * 76)
    print(f"Factions: {', '.join(state.setup)}")
    for f in state.setup:
        fs = state.factions[f]
        print(f"  {f:3s} start={state.starting.get(f, '?'):15s} gates={len(fs.gates)} "
              f"awakens={fs.awakens} rituals={fs.rituals} "
              f"SBs={len(fs.sb_earned)} craters={len(fs.craters)}")
    # Per-AP FB milestones
    print(f"\nFB decisions captured: {len(recon.fb_decision_snapshots)}")
    # Group by AP
    by_ap = defaultdict(list)
    for entry in recon.fb_decision_snapshots:
        s = entry[3]
        by_ap[s["ap"]].append(entry)
    for ap in sorted(by_ap):
        first_snap = by_ap[ap][0][3]
        fb = first_snap["fb"]
        fb_cap = recon.fb_captures_per_ap.get(ap, 0)
        print(f"  AP{ap} start: power={fb['power']} gates={fb['gate_count']}{fb['gates']} "
              f"awakens={fb['awakens']} rituals={fb['rituals']} "
              f"desc={fb['desiccated_count']} rev={fb['revenant_count']} "
              f"ghato={fb['ghato_on_map']} SBs={fb['sb_earned']} craters={fb['crater_count']} "
              f"FBcaptured={fb_cap}")
    total_fb_cap = sum(recon.fb_captures_per_ap.values())
    print(f"  TOTAL FB cultists captured by enemies: {total_fb_cap} "
          f"(per-AP: {dict(recon.fb_captures_per_ap)})")

def print_per_decision(recon):
    print("\n" + "=" * 76)
    print("FB PER-DECISION STATE DUMP")
    print("=" * 76)
    for (i, et, d, snap, action_str) in recon.fb_decision_snapshots:
        fb = snap["fb"]
        print(f"\n@{i} AP{snap['ap']} {snap['phase']} [{et}] {action_str}")
        print(f"  FB: pow={fb['power']} doom={fb['doom']} es={fb['es']} "
              f"gates={fb['gate_count']} awakens={fb['awakens']} rituals={fb['rituals']} "
              f"desc={fb['desiccated_count']} rev={fb['revenant_count']} ghato={fb['ghato_on_map']}")
        print(f"  FB units: {fb['units_by_region']}")
        print(f"  FB SBs: {fb['sb_earned']}")
        # Enemies summary
        for ef, es in snap["enemies"].items():
            print(f"  {ef}: pow={es['power']} doom={es['doom']} gates={es['gate_count']} "
                  f"awakens={es['awakens']} rituals={es['rituals']} units={es['total_units']}")
        # Threats
        if snap["threats"]:
            print(f"  THREATS: {snap['threats'][:10]}")
        if snap["opportunities"]:
            print(f"  OPPS:    {snap['opportunities'][:10]}")

def print_per_ap(recon):
    print("\n" + "=" * 76)
    print("PER-AP FB MILESTONES")
    print("=" * 76)
    by_ap = defaultdict(list)
    for entry in recon.fb_decision_snapshots:
        by_ap[entry[3]["ap"]].append(entry)
    for ap in sorted(by_ap):
        first = by_ap[ap][0][3]["fb"]
        last = by_ap[ap][-1][3]["fb"]
        events = [e[1] for e in by_ap[ap]]
        event_counts = defaultdict(int)
        for et in events:
            event_counts[et] += 1
        print(f"\nAP{ap}:")
        print(f"  START: power={first['power']} gates={first['gate_count']}{first['gates']} "
              f"awakens={first['awakens']} rituals={first['rituals']} "
              f"desc={first['desiccated_count']} ghato={first['ghato_on_map']}")
        print(f"  END  : power={last['power']} gates={last['gate_count']}{last['gates']} "
              f"awakens={last['awakens']} rituals={last['rituals']}")
        print(f"  EVENTS ({len(events)}): {dict(event_counts)}")

def load_fb_win_baselines(folder):
    """Scan folder for cthulhu-wars-*.html replays; return [(path, recon)]
    for the subset where FB won."""
    import glob as _glob
    results = []
    rejected = []
    for p in sorted(_glob.glob(os.path.join(folder, "cthulhu-wars-*.html"))):
        try:
            html, actions = load_file(p)
            recon = Reconstruction(html, actions)
            if recon.fb_won():
                results.append((p, recon))
            else:
                final = recon.final_doom()
                rejected.append((p, final))
        except Exception as e:
            rejected.append((p, f"parse_error: {e}"))
    return results, rejected

def describe_action(action_str, snap_fb):
    """Generate a brief prose description of a single action + state context."""
    if not action_str:
        return "no action"
    state = f"power={snap_fb.get('power','?')}, gates={snap_fb.get('gate_count','?')}{snap_fb.get('gates','')}, awakens={snap_fb.get('awakens','?')}, rituals={snap_fb.get('rituals','?')}, desc={snap_fb.get('desiccated_count','?')}, rev={snap_fb.get('revenant_count','?')}, ghato={snap_fb.get('ghato_on_map','?')}, SBs={snap_fb.get('sb_earned',[])}"
    a = action_str
    # Extract action type and key params
    m = re.match(r"(\w+)\((.*)\)", a)
    if not m:
        return f"{a} | state: {state}"
    atype = m.group(1)
    params = m.group(2)
    desc = ""
    if atype == "FBWritheMainAction":
        desc = "activates Writhe (rolls dice for kill/pain/move chain, costs 2 power)"
    elif atype == "FBWritheRerollAction":
        desc = f"rerolls all Writhe dice (rolls were {params.split(', ', 1)[1] if ', ' in params else '?'})"
    elif atype == "FBWritheKeepAction":
        desc = "keeps current Writhe dice (no reroll)"
    elif atype == "FBWritheKillUnitAction":
        desc = f"Writhe-kills a unit (likely an own Acolyte for Desiccated production)"
    elif atype == "FBWritheChoosePainUnitAction":
        desc = "selects a unit for Writhe-pain (will be relocated)"
    elif atype == "FBWritheMoveOneToRegionAction":
        m2 = re.search(r"(\w+)/Acolyte/\d+, (\w+),", params) or re.search(r"(\w+)/Desiccated/\d+, (\w+),", params)
        unit_to = m2.group(2) if m2 else "?"
        desc = f"Writhe-moves a unit to {unit_to}"
    elif atype == "FBWritheMoveAllToRegionAction":
        desc = "Writhe-moves all selected painted units to one region"
    elif atype == "FBWritheMoveSeparatelyAction":
        desc = "Writhe-moves painted units separately (each to own destination)"
    elif atype == "FBWritheMoveOneJoinAction":
        desc = "Writhe-moves a unit to join an existing group"
    elif atype == "BuildGateAction":
        m2 = re.search(r"FB, (\w+)", params)
        region = m2.group(1) if m2 else "?"
        desc = f"builds a new gate in {region} (costs 1 power)"
    elif atype == "BuildGateMainAction":
        desc = "selects build-gate as main action (will pick region next)"
    elif atype == "RecruitAction":
        m2 = re.search(r"FB, (\w+), (\w+)", params)
        unit, region = (m2.group(1), m2.group(2)) if m2 else ("?", "?")
        desc = f"recruits {unit} at {region} (costs 1 power)"
    elif atype == "MoveAction":
        m2 = re.search(r"FB/(\w+)/\d+, (\w+), (\w+),", params)
        unit, frm, to = (m2.group(1), m2.group(2), m2.group(3)) if m2 else ("?", "?", "?")
        desc = f"moves {unit} from {frm} to {to} (costs 1 power per region traveled)"
    elif atype == "SummonAction":
        m2 = re.search(r"FB, (\w+), (\w+)", params)
        unit, region = (m2.group(1), m2.group(2)) if m2 else ("?", "?")
        desc = f"summons {unit} at {region} (cost varies by unit)"
    elif atype == "FBAwakenGhatanothoaAction":
        m2 = re.search(r"FB, (\d+)", params)
        cost = m2.group(1) if m2 else "?"
        desc = f"awakens Ghatanothoa for {cost} power (gains 5 doom + ES bonus per future ritual)"
    elif atype == "RitualAction":
        m2 = re.search(r"FB, (\d+), (\d+)", params)
        cost, k = (m2.group(1), m2.group(2)) if m2 else ("?", "?")
        desc = f"performs Ritual of Annihilation for {cost} power, k={k} (gain doom = gates×k + 1 ES)"
    elif atype == "SpellbookAction":
        m2 = re.search(r"FB, (\w+),", params)
        sb = m2.group(1) if m2 else "?"
        if "EndAction" in params or "PreMainAction" in params or "MainAction" in params or "DoomAction" in params:
            desc = f"USES spellbook {sb} (active trigger)"
        else:
            desc = f"earns/picks spellbook {sb}"
    elif atype == "CaptureTargetAction":
        m2 = re.search(r"FB, (\w+), (\w+), (\w+)/(\w+)", params)
        if m2:
            region = m2.group(2)
            target_f = m2.group(3)
            target_u = m2.group(4)
            desc = f"captures enemy {target_f} {target_u} in {region}"
        else:
            desc = "captures an enemy unit"
    elif atype == "FBInfernalPactMainAction":
        desc = "enters Infernal Pact (will flip SBs for power discount)"
    elif atype == "FBInfernalPactChooseAction":
        m2 = re.search(r"FB, (\w+)", params)
        sb = m2.group(1) if m2 else "?"
        desc = f"flips spellbook {sb} facedown for +1 IP discount"
    elif atype == "FBInfernalPactDoneAction":
        desc = "exits Infernal Pact (commits accumulated discount)"
    elif atype == "FBDevilsMarkPlaceCraterAction":
        m2 = re.search(r"FB, (\w+)", params)
        region = m2.group(1) if m2 else "?"
        desc = f"places a Devil's Mark crater in {region} (destroys gate there)"
    elif atype == "FBDevilsMarkDoomAction":
        desc = "uses Devil's Mark in doom phase (will pick crater target)"
    elif atype == "FBCallOfTheFaithfulMainAction":
        desc = "uses Call of the Faithful (place free cultist; auto-claim gate if unclaimed)"
    elif atype == "FBCallOfTheFaithfulAction":
        m2 = re.search(r"FB, (\w+)", params)
        region = m2.group(1) if m2 else "?"
        desc = f"places Call-of-Faithful cultist at {region}"
    elif atype == "DoomDoneAction":
        desc = "ends doom phase (no more rituals/DM/IP this AP)"
    elif atype == "PassAction":
        desc = "passes (skip this turn)"
    elif atype == "EndTurnAction":
        desc = "ends turn"
    elif atype == "FirstPlayerAction":
        m2 = re.search(r"FB, (\w+)", params)
        f = m2.group(1) if m2 else "?"
        desc = f"picks {f} as first player"
    elif atype == "FBCyclopeanGazePainUnitAction":
        desc = "Cyclopean Gaze: pains an enemy unit"
    elif atype == "FBCyclopeanGazeDestinationAction":
        desc = "Cyclopean Gaze: chooses where to relocate the pained unit"
    elif atype == "FBCyclopeanGazeKillChoiceAction":
        desc = "Cyclopean Gaze: chooses unit to eliminate (no retreat possible)"
    elif atype == "FBTheEyeOpensMainAction":
        desc = "uses The Eye Opens (kill enemy cultist adjacent to Desiccated)"
    elif atype == "FBTheEyeOpensTargetAction":
        desc = "Eye Opens: selects region to target"
    elif atype == "FBCarnagePayPowerAction":
        desc = "Carnage: pays 1 power for 1 ES"
    elif atype == "FBCarnageFlipSpellbookAction":
        desc = "Carnage: flips a spellbook for 1 ES"
    else:
        desc = f"{atype}({params[:40]}{'...' if len(params)>40 else ''})"
    return f"{desc} | state: {state}"

def describe_discrepancy(entry, fb, base_fb):
    """Generate a one-paragraph discrepancy description."""
    bot_d = describe_action(entry["bot_action"], fb)
    base_d = describe_action(entry["base_action"], base_fb) if base_fb else "no baseline pairing"
    cls = entry["class"]
    classification_note = {
        "MATCH": "Bot and baseline took same action type with similar state — bot is on track.",
        "NEAR_MATCH": "Same action type but state differs moderately — bot followed strategy, state-context drift is variance.",
        "TRULY DIFFERENT": "Same state, different action — likely bot bug or rule mis-firing.",
        "LIKELY DIFFERENT": "Different action with state divergence — possible bug or strategy gap; review.",
        "SITUATIONAL": "Very different state — bot in unusual situation, comparison less reliable.",
        "NO_BASELINE": "No baseline decision found at this AP/phase — bot in unprecedented state."
    }.get(cls, "")
    return f"BOT: {bot_d}. BASELINE: {base_d}. {classification_note}"

# ── Power cost mapping ─────────────────────────────────────────────────
ACTION_POWER_COSTS = {
    "FBWritheMainAction": 2,
    "BuildGateAction": 3,
    "RecruitAction": 1,
    "MoveAction": 1,  # per unit moved (each MoveAction = 1 unit)
    "BattleAction": 1,
    "FBTheEyeOpensMainAction": 1,
    "FBCarnagePayPowerAction": 1,
    "PassAction": 0,
    "EndTurnAction": 0,
    "DoomDoneAction": 0,
    "FBCallOfTheFaithfulMainAction": 0,
    "FBCallOfTheFaithfulAction": 0,
    "FBDevilsMarkDoomAction": 0,
    "FBDevilsMarkPlaceCraterAction": 0,
    "FBInfernalPactMainAction": 0,
    "FBInfernalPactChooseAction": -1,  # gains 1 discount
    "FBInfernalPactDoneAction": 0,
    "FBWritheRerollAction": 0,
    "FBWritheKeepAction": 0,
    "FBWritheKillUnitAction": 0,
    "FBWritheChoosePainUnitAction": 0,
    "FBWritheMoveOneToRegionAction": 0,
    "FBWritheMoveAllToRegionAction": 0,
    "FBWritheMoveSeparatelyAction": 0,
    "FBWritheMoveOneJoinAction": 0,
    "SpellbookAction": 0,
    "FirstPlayerAction": 0,
    "FBCyclopeanGazePainUnitAction": 0,
    "FBCyclopeanGazeDestinationAction": 0,
    "FBCyclopeanGazeKillChoiceAction": 0,
    "FBTheEyeOpensTargetAction": 0,
    "FBCarnageFlipSpellbookAction": 0,
    "CaptureTargetAction": 0,
    "BuildGateMainAction": 0,
}

# Main actions that consume a turn (vs sub-actions within a Writhe/etc)
MAIN_ACTION_TYPES = {
    "FBWritheMainAction", "BuildGateAction", "RecruitAction", "MoveAction",
    "BattleAction", "FBTheEyeOpensMainAction", "FBCarnagePayPowerAction",
    "PassAction", "EndTurnAction", "FBCallOfTheFaithfulMainAction",
    "FBAwakenGhatanothoaAction", "AwakenAction", "SummonAction",
    "RitualAction", "FBDevilsMarkDoomAction", "FBInfernalPactMainAction",
}

def _action_type(action_str):
    """Extract action type name from action string."""
    m = re.match(r"(\w+)\(", action_str)
    return m.group(1) if m else action_str

def _action_cost(action_str, ritual_cost=5):
    """Compute power cost for an action string."""
    atype = _action_type(action_str)
    if atype == "SummonAction":
        # SummonAction(FB, Desiccated, region) = 2, Revenant = 3
        if "Desiccated" in action_str:
            return 2
        elif "Revenant" in action_str:
            return 3
        return 2  # default
    if atype == "FBAwakenGhatanothoaAction":
        m = re.search(r"FB, (\d+)", action_str)
        return int(m.group(1)) if m else (11 - ritual_cost)
    if atype == "AwakenAction":
        m = re.search(r", (\d+)\)", action_str)
        return int(m.group(1)) if m else 0
    if atype == "RitualAction":
        m = re.search(r"FB, (\d+)", action_str)
        return int(m.group(1)) if m else ritual_cost
    return ACTION_POWER_COSTS.get(atype, 0)

def _is_main_action(action_str):
    """Is this a main-action turn (not a sub-action)?"""
    return _action_type(action_str) in MAIN_ACTION_TYPES

def _natural_action_name(action_str):
    """Convert action string to natural language."""
    atype = _action_type(action_str)
    m_full = re.match(r"(\w+)\((.*)\)$", action_str)
    params = m_full.group(2) if m_full else ""

    if atype == "FBWritheMainAction":
        return "Writhe"
    elif atype == "BuildGateAction":
        m = re.search(r"FB, (\w[\w ]*)", params)
        return f"Build Gate in {m.group(1)}" if m else "Build Gate"
    elif atype == "RecruitAction":
        m = re.search(r"FB, (\w+), (\w[\w ]*)", params)
        if m:
            return f"Recruit {m.group(1)} in {m.group(2)}"
        return "Recruit"
    elif atype == "MoveAction":
        m = re.search(r"FB/(\w+)/\d+, (\w[\w ]*), (\w[\w ]*)", params)
        if m:
            return f"Move {m.group(1)} from {m.group(2)} to {m.group(3)}"
        return "Move"
    elif atype == "SummonAction":
        m = re.search(r"FB, (\w[\w ]*), (\w[\w ]*)", params)
        if m:
            return f"Summon {m.group(1)} in {m.group(2)}"
        return "Summon"
    elif atype == "FBAwakenGhatanothoaAction":
        m = re.search(r"FB, (\d+)", params)
        cost = m.group(1) if m else "?"
        return f"Awaken Ghatanothoa for {cost} power"
    elif atype == "AwakenAction":
        return f"Awaken ({params})"
    elif atype == "RitualAction":
        m = re.search(r"FB, (\d+)", params)
        cost = m.group(1) if m else "?"
        return f"Ritual of Annihilation for {cost} power"
    elif atype == "FBCallOfTheFaithfulMainAction":
        return "Call of the Faithful"
    elif atype == "FBCallOfTheFaithfulAction":
        m = re.search(r"FB, (\w[\w ]*)", params)
        return f"Call of the Faithful: place Acolyte in {m.group(1)}" if m else "Call of the Faithful"
    elif atype == "FBDevilsMarkDoomAction":
        return "Devil's Mark"
    elif atype == "FBDevilsMarkPlaceCraterAction":
        m = re.search(r"FB, (\w[\w ]*)", params)
        return f"Devil's Mark crater in {m.group(1)}" if m else "Devil's Mark crater"
    elif atype == "FBInfernalPactMainAction":
        return "Infernal Pact"
    elif atype == "FBInfernalPactChooseAction":
        m = re.search(r"FB, (\w[\w ]*)", params)
        return f"Infernal Pact: flip {m.group(1)}" if m else "Infernal Pact: flip SB"
    elif atype == "FBInfernalPactDoneAction":
        return "Infernal Pact done"
    elif atype == "FBTheEyeOpensMainAction":
        return "The Eye Opens"
    elif atype == "FBCarnagePayPowerAction":
        return "Carnage (pay 1 power for 1 ES)"
    elif atype == "FBCarnageFlipSpellbookAction":
        return "Carnage (flip SB for 1 ES)"
    elif atype == "PassAction":
        return "Pass"
    elif atype == "EndTurnAction":
        return "End Turn"
    elif atype == "DoomDoneAction":
        return "End Doom Phase"
    elif atype == "SpellbookAction":
        m = re.search(r"FB, (\w[\w ]*)", params)
        return f"Chose SB: {m.group(1)}" if m else "Chose SB"
    elif atype == "FBWritheRerollAction":
        return "Writhe: reroll all dice"
    elif atype == "FBWritheKeepAction":
        return "Writhe: keep dice"
    elif atype == "FBWritheKillUnitAction":
        return "Writhe: kill unit (Acolyte -> Desiccated)"
    elif atype == "FBWritheChoosePainUnitAction":
        return "Writhe: choose pain target"
    elif atype == "FBWritheMoveOneToRegionAction":
        m = re.search(r"FB/(\w+)/\d+, (\w[\w ]*)", params)
        if m:
            return f"Writhe: move {m.group(1)} to {m.group(2)}"
        return "Writhe: move unit"
    elif atype == "FBWritheMoveOneJoinAction":
        m = re.search(r"FB/(\w+)/\d+, (\w[\w ]*)", params)
        if m:
            return f"Writhe: move {m.group(1)} to join at {m.group(2)}"
        return "Writhe: move unit to join"
    elif atype == "CaptureTargetAction":
        return f"Capture target ({params})"
    elif atype == "FirstPlayerAction":
        m = re.search(r"FB, (\w+)", params)
        return f"Pick {m.group(1)} as first player" if m else "Pick first player"
    elif atype == "BattleAction":
        return f"Battle ({params})"
    else:
        return f"{atype}({params[:50]})"

# ── Game analysis for template output ──────────────────────────────────

class GameAnalysis:
    """Analyze a Reconstruction to extract all data needed for the template."""

    def __init__(self, recon, label="bot", html_lines=None):
        self.recon = recon
        self.label = label
        self.html_lines = html_lines or []
        self.action_strings = recon.action_strings

        # Per-AP data
        self.ap_start_power = {}  # ap -> power at start of AP
        self.ap_actions = defaultdict(list)  # ap -> [(action_str, cost, power_before, power_after, turn_num)]
        self.ap_writhes = defaultdict(list)  # ap -> [power_at_writhe, ...]
        self.ap_moves = defaultdict(int)     # ap -> count of MoveAction
        self.ap_move_turns = defaultdict(int) # ap -> turns spent on Move
        self.ap_recruits = defaultdict(int)
        self.ap_summons = defaultdict(int)
        self.ap_builds = defaultdict(int)
        self.ap_captures_by_enemy = defaultdict(int)  # ap -> FB cultists captured
        self.ap_gates_gained = defaultdict(list)  # ap -> [region, ...]
        self.ap_gates_lost = defaultdict(list)    # ap -> [region, ...]
        self.ap_sb_earned = defaultdict(list)      # ap -> [sb_name, ...]
        self.ap_sb_chosen = defaultdict(list)
        self.ap_sb_flipped = defaultdict(list)     # ap -> [sb flipped via IP]
        self.ap_achievements = defaultdict(list)   # ap -> [achievement, ...]

        # Doom phase data
        self.doom_rituals = defaultdict(lambda: None)   # ap -> (cost, doom, es_bool)
        self.doom_dm = defaultdict(lambda: None)         # ap -> region
        self.doom_ip = defaultdict(list)                 # ap -> [sb_names flipped]
        self.doom_gate_doom = defaultdict(int)           # ap -> doom from gates

        # Gather power lines
        self.gather_power_lines = defaultdict(str)  # ap -> "Firstborn got X Power (...)"

        # Overall
        self.placement_region = None
        self.max_ap = 0
        self.total_writhes = 0
        self.total_moves = 0
        self.total_recruits = 0
        self.total_summons = 0
        self.total_builds = 0
        self.total_enemy_captures = 0
        self.end_gates = 0
        self.ap_end_gates = {}  # ap -> gate count at end of AP
        self.end_doom = 0
        self.end_es = 0
        self.end_sbs = []
        self.peak_gates = 0
        self.total_rituals = 0
        self.total_dm = 0
        self.sb_earn_order = []
        self.awakened = False

        self._analyze()

    def _analyze(self):
        """Walk through events and action strings to extract all metrics."""
        state = GameState()
        ap = 0
        power = 8  # FB always starts with 8
        turn_num = 0
        ritual_cost = 5
        in_doom = False
        last_move_turn = -1

        # Extract gather power lines from HTML
        for line in self.html_lines:
            if "Firstborn" in line and "got" in line and "Power" in line:
                # Try to figure out which AP this belongs to
                # We'll collect them all and assign by order
                pass

        # Walk action strings for the bot trace
        gather_power_idx = 0
        gather_lines_all = []
        for line in self.html_lines:
            if "Firstborn" in line and "got" in line and "Power" in line:
                clean = re.sub(r'<[^>]+>', '', line).strip()
                gather_lines_all.append(clean)

        # Track placement
        for s in self.action_strings:
            m = re.match(r"StartingRegionAction\(FB, (\w[\w ]*)\)", s)
            if m:
                self.placement_region = m.group(1)
                break

        # Main walk
        current_ap = 0
        in_doom_phase = False
        fb_power = 8
        fb_gates = set()
        fb_sbs = set()
        fb_sbs_facedown = set()
        fb_doom = 0
        fb_es = 0
        fb_awakened = False
        fb_rituals_total = 0
        fb_dm_total = 0
        sb_order = []
        ip_discount = 0
        peak_gates = 0
        all_gates_gained = []
        all_gates_lost = []

        # Use the reconstruction's events for accurate state tracking
        for (i, f, et, d) in self.recon.events:
            if f is None and et == "round_start":
                if current_ap > 0:
                    self.ap_end_gates[current_ap] = len(fb_gates)
                current_ap += 1
                in_doom_phase = False
                turn_num = 0
                # Assign gather power line
                if gather_power_idx < len(gather_lines_all):
                    self.gather_power_lines[current_ap] = gather_lines_all[gather_power_idx]
                    gather_power_idx += 1
                continue
            if f is None and et == "doom_phase_start":
                in_doom_phase = True
                continue
            if f is None and et == "action_phase_start":
                in_doom_phase = False
                continue

            if f != "FB":
                # Track enemy captures of FB
                if f is not None and et == "capture":
                    if isinstance(d, tuple) and len(d) == 3:
                        _, region, victim = d
                        if victim == "FB":
                            effective_ap = max(1, current_ap)
                            self.ap_captures_by_enemy[effective_ap] += 1
                continue

            effective_ap = max(1, current_ap)
            self.max_ap = max(self.max_ap, effective_ap)

            if et == "start":
                self.placement_region = d
                fb_gates.add(d)
                peak_gates = max(peak_gates, len(fb_gates))

            elif et == "power_gather":
                fb_power = d

            elif et == "power_set":
                fb_power = d

            elif et == "power_gain":
                fb_power += d

            elif et in ("out_of_power", "no_power"):
                fb_power = 0

            elif et == "build_gate":
                fb_gates.add(d)
                peak_gates = max(peak_gates, len(fb_gates))
                self.ap_gates_gained[effective_ap].append(d)
                self.ap_builds[effective_ap] += 1
                all_gates_gained.append(d)

            elif et == "lost_gate":
                fb_gates.discard(d)
                # Only count as "lost" if not immediately regained in same AP
                # (Writhe temporarily loses control when keeper moves out, but
                # another unit in the region takes over immediately)
                if not in_doom_phase:
                    self.ap_gates_lost[effective_ap].append(d)
                    all_gates_lost.append(d)

            elif et == "gained_gate":
                fb_gates.add(d)
                peak_gates = max(peak_gates, len(fb_gates))
                self.ap_gates_gained[effective_ap].append(d)
                all_gates_gained.append(d)

            elif et == "sb_earned":
                fb_sbs.add(d)
                sb_order.append(d)
                self.ap_sb_chosen[effective_ap].append(d)

            elif et == "achievement":
                self.ap_achievements[effective_ap].append(d)

            elif et == "ritual":
                cost, doom, es = d
                fb_power = max(0, fb_power - cost)
                fb_doom += doom
                if es:
                    fb_es += 1
                fb_rituals_total += 1
                doom_ap = max(1, effective_ap - 1)
                self.doom_rituals[doom_ap] = (cost, doom, es)

            elif et == "crater":
                doom_ap = max(1, effective_ap - 1)
                self.doom_dm[doom_ap] = d
                fb_dm_total += 1

            elif et == "gate_destroyed":
                fb_gates.discard(d)

            elif et == "doom_gain":
                fb_doom += d

            elif et == "doom_gather":
                fb_doom += d
                # Doom gather always refers to the doom phase AFTER the previous AP.
                # round_start for the next AP fires before doom events are injected,
                # so effective_ap is already incremented. Always subtract 1.
                doom_ap = max(1, effective_ap - 1)
                self.doom_gate_doom[doom_ap] = d

            elif et == "awaken" or et == "awaken_bare":
                fb_awakened = True

            # Track writhes, moves, recruits, summons from action strings
            # (done below via action_strings walk)

        # Now walk action strings for per-turn detail
        current_ap = 0
        in_doom_phase = False
        turn_num = 0
        fb_power_track = 8
        ritual_cost_track = 5
        self.ap_start_power[1] = 8  # AP1 always starts at 8
        last_doom_ritual_cost = 0  # ritual cost spent in most recent doom phase
        last_doom_ip_discount = 0  # IP discount earned in most recent doom phase

        # Gather power values from game log, in order.
        # First value = gather before AP2's doom phase, second = before AP3's, etc.
        # AP start power = gather value - ritual cost (from that doom phase) + IP discount
        gather_power_values = []
        for gline in gather_lines_all:
            gp_match = re.search(r'(\d+)\s*Power', gline)
            if gp_match:
                gather_power_values.append(int(gp_match.group(1)))

        # Pre-compute AP start powers from gather values + doom phase costs
        # Walk action strings just for doom phase ritual/IP tracking
        temp_ap = 0
        temp_gather_idx = 0
        temp_doom_ritual_cost = 0
        temp_doom_ip_flips = 0
        for s2 in self.action_strings:
            s2 = s2.strip()
            if s2.startswith("PlayDirectionAction"):
                temp_ap += 1
                if temp_ap == 1:
                    self.ap_start_power[1] = 8
                elif temp_gather_idx - 1 >= 0 and temp_gather_idx - 1 < len(gather_power_values):
                    raw_gather = gather_power_values[temp_gather_idx - 1]
                    self.ap_start_power[temp_ap] = max(0, raw_gather - temp_doom_ritual_cost + temp_doom_ip_flips)
                temp_doom_ritual_cost = 0
                temp_doom_ip_flips = 0
            elif s2.startswith("PowerGatherAction"):
                temp_gather_idx += 1
            elif s2.startswith("RitualAction") and "FB" in s2:
                m_rit = re.search(r'FB,\s*(\d+)', s2)
                if m_rit:
                    temp_doom_ritual_cost = int(m_rit.group(1))
            elif s2.startswith("FBInfernalPactDoomChooseAction") or (s2.startswith("FBInfernalPactChooseAction") and temp_doom_ritual_cost >= 0):
                if "FB" in s2:
                    temp_doom_ip_flips += 1

        # Now set fb_power_track for the main walk
        for idx, s in enumerate(self.action_strings):
            s = s.strip()
            if s.startswith("PowerGatherAction"):
                continue
            if s.startswith("PlayDirectionAction"):
                current_ap += 1
                in_doom_phase = False
                turn_num = 0
                fb_power_track = self.ap_start_power.get(current_ap, fb_power_track)
                continue
            if s.startswith("DoomPhaseAction"):
                in_doom_phase = True
                continue
            if s.startswith("ActionPhaseAction"):
                in_doom_phase = False
                continue
            if s.startswith("DoomPhaseAction"):
                in_doom_phase = True
                continue
            if s.startswith("ActionPhaseAction"):
                in_doom_phase = False
                continue

            m = re.match(r"(\w+)\((.*)\)$", s)
            if not m:
                continue
            atype = m.group(1)
            params = m.group(2)

            # Only track FB actions
            if not params.startswith("FB"):
                continue

            effective_ap = max(1, current_ap)

            if atype in MAIN_ACTION_TYPES:
                turn_num += 1

            cost = _action_cost(s, ritual_cost_track)
            power_before = fb_power_track
            fb_power_track = max(0, fb_power_track - cost)

            if _is_main_action(s) or atype in ("FBWritheRerollAction", "FBWritheKeepAction",
                                                  "FBWritheKillUnitAction", "FBWritheMoveOneToRegionAction",
                                                  "FBWritheMoveOneJoinAction", "FBWritheChoosePainUnitAction",
                                                  "SpellbookAction", "FBCallOfTheFaithfulAction",
                                                  "FBDevilsMarkPlaceCraterAction", "FBInfernalPactChooseAction",
                                                  "FBInfernalPactDoneAction", "CaptureTargetAction",
                                                  "FBCarnageFlipSpellbookAction"):
                self.ap_actions[effective_ap].append((
                    s, cost, power_before, fb_power_track, turn_num,
                    _natural_action_name(s), _is_main_action(s), in_doom_phase
                ))

            # Count specific action types
            if atype == "FBWritheMainAction":
                self.ap_writhes[effective_ap].append(power_before)
                self.total_writhes += 1
            elif atype == "MoveAction":
                self.ap_moves[effective_ap] += 1
                self.total_moves += 1
                if turn_num != last_move_turn:
                    self.ap_move_turns[effective_ap] += 1
                    last_move_turn = turn_num
            elif atype == "RecruitAction":
                self.ap_recruits[effective_ap] += 1
                self.total_recruits += 1
            elif atype == "SummonAction":
                self.ap_summons[effective_ap] += 1
                self.total_summons += 1
            elif atype == "BuildGateAction":
                # already counted in events walk
                pass
            elif atype == "RitualAction":
                m2 = re.search(r"FB, (\d+)", params)
                if m2:
                    ritual_cost_track = int(m2.group(1))
            elif atype == "FBInfernalPactChooseAction":
                m2 = re.search(r"FB, (\w[\w ]*)", params)
                if m2:
                    self.ap_sb_flipped[effective_ap].append(m2.group(1))
                    self.doom_ip[effective_ap].append(m2.group(1))

            # Reset power at gather from reconstruction
            # (handled by power_gather events above)

        # Record final AP gate count
        if current_ap > 0:
            self.ap_end_gates[current_ap] = len(fb_gates)

        # Compute totals
        self.total_builds = sum(self.ap_builds.values())
        self.total_enemy_captures = sum(self.ap_captures_by_enemy.values())
        self.end_gates = len(fb_gates)
        self.end_doom = fb_doom
        self.end_es = fb_es
        self.end_sbs = list(fb_sbs)
        self.peak_gates = peak_gates
        self.total_rituals = fb_rituals_total
        self.total_dm = fb_dm_total
        self.sb_earn_order = sb_order
        self.awakened = fb_awakened

    def avg_power_at_writhe(self):
        all_powers = []
        for ap_writhes in self.ap_writhes.values():
            all_powers.extend(ap_writhes)
        if not all_powers:
            return 0
        return sum(all_powers) / len(all_powers)

    def avg_gates_across_aps(self):
        if not self.ap_end_gates:
            return self.end_gates
        vals = list(self.ap_end_gates.values())
        return sum(vals) / len(vals)


def _compute_baseline_averages(baselines):
    """Compute average metrics across all baseline GameAnalysis objects."""
    analyses = []
    for label, recon in baselines:
        html_lines, _ = load_file(label)
        ga = GameAnalysis(recon, label="baseline", html_lines=html_lines)
        analyses.append(ga)

    if not analyses:
        return None, []

    n = len(analyses)
    avg = {
        "writhes": sum(a.total_writhes for a in analyses) / n,
        "moves": sum(a.total_moves for a in analyses) / n,
        "recruits": sum(a.total_recruits for a in analyses) / n,
        "summons": sum(a.total_summons for a in analyses) / n,
        "builds": sum(a.total_builds for a in analyses) / n,
        "enemy_captures": sum(a.total_enemy_captures for a in analyses) / n,
        "gates_lost": sum(sum(len(v) for v in a.ap_gates_lost.values()) for a in analyses) / n,
        "avg_gates_end": sum(a.avg_gates_across_aps() for a in analyses) / n,
    }
    return avg, analyses


def write_discrepancies_md(entries, bot_path, baselines, rejected, out_path):
    """Write full template-matching discrepancy document as markdown."""
    import datetime as _dt

    # Load bot analysis
    bot_html, bot_actions = load_file(bot_path)
    bot_recon_fresh = Reconstruction(bot_html, bot_actions)
    bot_ga = GameAnalysis(bot_recon_fresh, label="bot", html_lines=bot_html)

    # Load baseline analyses
    base_avg, base_analyses = _compute_baseline_averages(baselines)

    # Pick the closest baseline for per-AP comparison (first one, or best match)
    base_ga = base_analyses[0] if base_analyses else None

    lines = []

    # ── Header ─────────────────────────────────────────────────────────
    replay_name = os.path.basename(bot_path).replace(".txt", ".html").replace("-trace", "")
    lines.append(f"# FB Discrepancies — {replay_name}")
    lines.append("")
    lines.append(f"**Generated**: {_dt.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    lines.append(f"**Bot trace**: `{os.path.basename(bot_path)}`")
    lines.append(f"**Baselines used**: {len(baselines)} FB-victory replay(s)")
    for lbl, _ in baselines:
        lines.append(f"  - `{os.path.basename(lbl)}`")
    lines.append("")

    def pad(s, w):
        s = str(s)
        return s + '_' * max(0, w - len(s))

    # ── Action Comparison Table ────────────────────────────────────────
    lines.append("## Action Comparison Table")
    lines.append("")
    lines.append(f"| {pad('Action', 33)} | {pad('Bot', 5)} | {pad('Baseline_avg', 12)} |")
    lines.append(f"|{'-'*35}|{'-'*7}|{'-'*14}|")
    gates_lost = sum(len(v) for v in bot_ga.ap_gates_lost.values())
    rows = [
        ("Writhes", bot_ga.total_writhes, f"{base_avg['writhes']:.1f}" if base_avg else "?"),
        ("Moves", bot_ga.total_moves, f"{base_avg['moves']:.1f}" if base_avg else "?"),
        ("Recruits", bot_ga.total_recruits, f"{base_avg['recruits']:.1f}" if base_avg else "?"),
        ("Summons", bot_ga.total_summons, f"{base_avg['summons']:.1f}" if base_avg else "?"),
        ("Builds", bot_ga.total_builds, f"{base_avg['builds']:.1f}" if base_avg else "?"),
        ("Enemy_captures_of_FB_cultists", bot_ga.total_enemy_captures, f"~{base_avg['enemy_captures']:.0f}" if base_avg else "~1"),
        ("Gates_lost_(not_DM)", gates_lost, f"{base_avg['gates_lost']:.0f}" if base_avg else "0"),
        ("Avg_gates_(across_APs)", f"{bot_ga.avg_gates_across_aps():.1f}", f"{base_avg['avg_gates_end']:.1f}" if base_avg else "3"),
    ]
    for label, bot_val, base_val in rows:
        lines.append(f"| {pad(label, 33)} | {pad(str(bot_val), 5)} | {pad(str(base_val), 12)} |")
    lines.append("")

    # ── Writhe / Move Chart ────────────────────────────────────────────
    lines.append("## Writhe / Move Comparison Chart")
    lines.append("")
    lines.append(f"| {pad('AP', 5)} | {pad('Bot_Writhes', 11)} | {pad('Bot_Power@Writhe', 17)} | {pad('Bot_Moves', 9)} | {pad('Base_Writhes', 12)} | {pad('Base_Power@Writhe', 18)} | {pad('Base_Moves', 10)} |")
    lines.append(f"|{'-'*7}|{'-'*13}|{'-'*19}|{'-'*11}|{'-'*14}|{'-'*20}|{'-'*12}|")

    # Baseline reference averages from template (from 7 FB-win replays).
    # Baseline HTML replays don't contain game log lines, so power tracking
    # can't be computed — use these known reference values instead.
    base_ref = {
        1: ("1.3", "8", "0.1"),
        2: ("0.6", "6-12", "0"),
        3: ("1.0", "12", "0"),
        4: ("1.7", "12/7", "0.4"),
        5: ("2.0", "12/4", "0.1"),
        6: ("1.0", "12", "0"),
    }

    max_ap = max(bot_ga.max_ap, 6)
    bot_all_writhe_powers = []
    for ap in range(1, max_ap + 1):
        bot_w = len(bot_ga.ap_writhes.get(ap, []))
        bot_wp = bot_ga.ap_writhes.get(ap, [])
        bot_wp_str = "/".join(str(p) for p in bot_wp) if bot_wp else "-"
        bot_m = bot_ga.ap_moves.get(ap, 0)
        bot_all_writhe_powers.extend(bot_wp)

        ref = base_ref.get(ap, ("-", "-", "-"))
        lines.append(f"| {pad(f'AP{ap}', 5)} | {pad(str(bot_w), 11)} | {pad(bot_wp_str, 17)} | {pad(str(bot_m), 9)} | {pad(ref[0], 12)} | {pad(ref[1], 18)} | {pad(ref[2], 10)} |")

    bot_avg_wp = f"{sum(bot_all_writhe_powers)/len(bot_all_writhe_powers):.1f}" if bot_all_writhe_powers else "-"
    lines.append(f"| {pad('Total', 5)} | {pad(str(bot_ga.total_writhes), 11)} | {pad(f'avg {bot_avg_wp}', 17)} | {pad(str(bot_ga.total_moves), 9)} | {pad('7.6', 12)} | {pad('avg ~10', 18)} | {pad('0.7', 10)} |")
    lines.append("")

    # ── Compute per-AP snapshots for tables ─────────────────────────────
    def _compute_ap_snaps(ga):
        state = GameState()
        start_snaps = {}
        end_snaps = {}
        cur_ap = 0
        for (i, f, et, d) in ga.recon.events:
            if f is None and et == "round_start":
                if cur_ap > 0:
                    end_snaps[cur_ap] = snapshot(state, "FB")
                apply_event(state, f, et, d)
                cur_ap = state.round_num
                start_snaps[cur_ap] = snapshot(state, "FB")
                continue
            apply_event(state, f, et, d)
        if cur_ap > 0:
            end_snaps[cur_ap] = snapshot(state, "FB")
        return start_snaps, end_snaps

    bot_ap_start_snaps, bot_ap_end_snaps = _compute_ap_snaps(bot_ga)
    base_ap_start_snaps, base_ap_end_snaps = _compute_ap_snaps(base_ga) if base_ga else ({}, {})

    # ── AP State Table (Bot) ──────────────────────────────────────────
    # Per-AP start/end state: SP, SG, SGh, Sr, Sd, Sc, EP, EG, EGh, Er, Ed, Ec
    lines.append("## Bot AP State")
    lines.append("Key: SP=Starting Power, SG=Starting Gates, SGh=Started w/Ghato, Sr=Starting Revs, Sd=Starting Desiccated, Sc=Starting Cultists. E prefix=Ending.")
    lines.append("")
    lines.append(f"| {pad('AP', 4)} | {pad('SP', 3)} | {pad('SG', 3)} | {pad('SGh', 3)} | {pad('Sr', 3)} | {pad('Sd', 3)} | {pad('Sc', 3)} | {pad('EP', 3)} | {pad('EG', 3)} | {pad('EGh', 3)} | {pad('Er', 3)} | {pad('Ed', 3)} | {pad('Ec', 3)} |")
    lines.append(f"|{'-'*6}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|")

    for ap in range(1, bot_ga.max_ap + 1):
        ss = bot_ap_start_snaps.get(ap, {}).get("fb", {})
        es = bot_ap_end_snaps.get(ap, {}).get("fb", {})
        sp = bot_ga.ap_start_power.get(ap, ss.get("power", "?"))
        sg = ss.get("gate_count", "?")
        sgh = "Y" if ss.get("ghato_on_map", False) else "N"
        sr = ss.get("revenant_count", "?")
        sd = ss.get("desiccated_count", "?")
        sc = ss.get("acolyte_count", "?")
        ep = es.get("power", "?")
        eg = es.get("gate_count", "?")
        egh = "Y" if es.get("ghato_on_map", False) else "N"
        er = es.get("revenant_count", "?")
        ed = es.get("desiccated_count", "?")
        ec = es.get("acolyte_count", "?")
        lines.append(f"| {pad(f'AP{ap}', 4)} | {pad(str(sp), 3)} | {pad(str(sg), 3)} | {pad(sgh, 3)} | {pad(str(sr), 3)} | {pad(str(sd), 3)} | {pad(str(sc), 3)} | {pad(str(ep), 3)} | {pad(str(eg), 3)} | {pad(egh, 3)} | {pad(str(er), 3)} | {pad(str(ed), 3)} | {pad(str(ec), 3)} |")
    lines.append("")

    # ── Doom Phase Table (Bot) ────────────────────────────────────────
    # Per doom phase: Ds, ESs, G, R, Psr, IPd, Dg, DM, Pdm, ESg, De
    lines.append("## Bot Doom Phase")
    lines.append("Key: Ds=Doom at start, ESs=ES at start, G=Gates, R=Ritual y/n, Psr=Power spent on ritual, IPd=IP discount, Dg=Doom gained, DM=Devil's Mark y/n, ESg=ES gained, De=Doom at end.")
    lines.append("")
    lines.append(f"| {pad('DP', 4)} | {pad('Ds', 3)} | {pad('ESs', 3)} | {pad('G', 2)} | {pad('R', 2)} | {pad('Psr', 3)} | {pad('IPd', 3)} | {pad('Dg', 3)} | {pad('DM', 2)} | {pad('ESg', 3)} | {pad('De', 3)} |")
    lines.append(f"|{'-'*6}|{'-'*5}|{'-'*5}|{'-'*4}|{'-'*4}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*4}|{'-'*5}|{'-'*5}|")

    running_doom = 0
    running_es = 0
    for ap in range(1, bot_ga.max_ap + 1):
        ritual = bot_ga.doom_rituals.get(ap)
        dm = bot_ga.doom_dm.get(ap)
        ip_sbs = bot_ga.doom_ip.get(ap, [])
        gate_doom = bot_ga.doom_gate_doom.get(ap, 0)
        ds = running_doom
        ess = running_es
        g = gate_doom
        r = "Y" if ritual else "N"
        psr = ritual[0] if ritual else 0
        ipd = len(ip_sbs)
        dg = gate_doom + (ritual[1] if ritual else 0)
        dm_yn = "Y" if dm else "N"
        esg = (1 if ritual and ritual[2] else 0) + (1 if dm else 0)
        running_doom += dg
        running_es += esg
        de = running_doom
        lines.append(f"| {pad(f'DP{ap}', 4)} | {pad(str(ds), 3)} | {pad(str(ess), 3)} | {pad(str(g), 2)} | {pad(r, 2)} | {pad(str(psr), 3)} | {pad(str(ipd), 3)} | {pad(str(dg), 3)} | {pad(dm_yn, 2)} | {pad(str(esg), 3)} | {pad(str(de), 3)} |")
    lines.append("")

    # ── Baseline versions (reference data) ────────────────────────────
    if base_ga:
        lines.append("## Baseline AP State")
        lines.append("Key: same as Bot AP State above.")
        lines.append("")
        lines.append(f"| {pad('AP', 4)} | {pad('SP', 3)} | {pad('SG', 3)} | {pad('SGh', 3)} | {pad('Sr', 3)} | {pad('Sd', 3)} | {pad('Sc', 3)} | {pad('EP', 3)} | {pad('EG', 3)} | {pad('EGh', 3)} | {pad('Er', 3)} | {pad('Ed', 3)} | {pad('Ec', 3)} |")
        lines.append(f"|{'-'*6}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*5}|")
        for ap in range(1, base_ga.max_ap + 1):
            ss = base_ap_start_snaps.get(ap, {}).get("fb", {})
            es_snap = base_ap_end_snaps.get(ap, {}).get("fb", {})
            sp = base_ga.ap_start_power.get(ap, ss.get("power", "?"))
            sg = ss.get("gate_count", "?")
            sgh = "Y" if ss.get("ghato_on_map", False) else "N"
            sr = ss.get("revenant_count", "?")
            sd = ss.get("desiccated_count", "?")
            sc = ss.get("acolyte_count", "?")
            ep = es_snap.get("power", "?")
            eg = es_snap.get("gate_count", "?")
            egh = "Y" if es_snap.get("ghato_on_map", False) else "N"
            er = es_snap.get("revenant_count", "?")
            ed = es_snap.get("desiccated_count", "?")
            ec = es_snap.get("acolyte_count", "?")
            lines.append(f"| {pad(f'AP{ap}', 4)} | {pad(str(sp), 3)} | {pad(str(sg), 3)} | {pad(sgh, 3)} | {pad(str(sr), 3)} | {pad(str(sd), 3)} | {pad(str(sc), 3)} | {pad(str(ep), 3)} | {pad(str(eg), 3)} | {pad(egh, 3)} | {pad(str(er), 3)} | {pad(str(ed), 3)} | {pad(str(ec), 3)} |")
        lines.append("")

        lines.append("## Baseline Doom Phase")
        lines.append("Key: same as Bot Doom Phase above.")
        lines.append("")
        lines.append(f"| {pad('DP', 4)} | {pad('Ds', 3)} | {pad('ESs', 3)} | {pad('G', 2)} | {pad('R', 2)} | {pad('Psr', 3)} | {pad('IPd', 3)} | {pad('Dg', 3)} | {pad('DM', 2)} | {pad('ESg', 3)} | {pad('De', 3)} |")
        lines.append(f"|{'-'*6}|{'-'*5}|{'-'*5}|{'-'*4}|{'-'*4}|{'-'*5}|{'-'*5}|{'-'*5}|{'-'*4}|{'-'*5}|{'-'*5}|")
        b_running_doom = 0
        b_running_es = 0
        for ap in range(1, base_ga.max_ap + 1):
            ritual = base_ga.doom_rituals.get(ap)
            dm = base_ga.doom_dm.get(ap)
            ip_sbs = base_ga.doom_ip.get(ap, [])
            gate_doom = base_ga.doom_gate_doom.get(ap, 0)
            # Show every doom phase (gate doom happens every AP)
            ds = b_running_doom
            ess = b_running_es
            g = gate_doom
            r = "Y" if ritual else "N"
            psr = ritual[0] if ritual else 0
            ipd = len(ip_sbs)
            dg = gate_doom + (ritual[1] if ritual else 0)
            dm_yn = "Y" if dm else "N"
            esg = (1 if ritual and ritual[2] else 0) + (1 if dm else 0)
            b_running_doom += dg
            b_running_es += esg
            de = b_running_doom
            lines.append(f"| {pad(f'DP{ap}', 4)} | {pad(str(ds), 3)} | {pad(str(ess), 3)} | {pad(str(g), 2)} | {pad(r, 2)} | {pad(str(psr), 3)} | {pad(str(ipd), 3)} | {pad(str(dg), 3)} | {pad(dm_yn, 2)} | {pad(str(esg), 3)} | {pad(str(de), 3)} |")
        lines.append("")

    # ── Top of Document Summary ────────────────────────────────────────
    lines.append("## Summary")
    lines.append("")

    def _write_game_summary(ga, label_prefix, lines):
        """Write the full game summary for bot or baseline."""
        lines.append(f"### {label_prefix}")
        lines.append(f"- Placed in {ga.placement_region or '?'}.")
        lines.append("")

        # Reconstruct state to get per-AP snapshots
        state = GameState()
        ap_start_snaps = {}
        ap_end_snaps = {}
        current_ap_for_snap = 0

        for (i, f, et, d) in ga.recon.events:
            if f is None and et == "round_start":
                # Save end state of previous AP
                if current_ap_for_snap > 0:
                    ap_end_snaps[current_ap_for_snap] = snapshot(state, "FB")
                apply_event(state, f, et, d)
                current_ap_for_snap = state.round_num
                ap_start_snaps[current_ap_for_snap] = snapshot(state, "FB")
                continue
            apply_event(state, f, et, d)

        if current_ap_for_snap > 0:
            ap_end_snaps[current_ap_for_snap] = snapshot(state, "FB")

        # Per-AP summary lines
        doom_from_gates = 0
        doom_from_rituals = 0
        doom_from_es = 0
        for ap in range(1, ga.max_ap + 1):
            start_snap = ap_start_snaps.get(ap, {})
            end_snap = ap_end_snaps.get(ap, {})
            fb_start = start_snap.get("fb", {}) if start_snap else {}
            fb_end = end_snap.get("fb", {}) if end_snap else {}

            start_power = ga.ap_start_power.get(ap, fb_start.get("power", "?"))
            start_sbs = fb_start.get("sb_earned", [])
            start_gates_list = fb_start.get("gates", [])
            end_gates_list = fb_end.get("gates", [])

            # Writhes info
            writhes = ga.ap_writhes.get(ap, [])
            writhe_str = f"{len(writhes)} Writhes"
            if writhes:
                writhe_str += f" (at power {'/'.join(str(p) for p in writhes)})"

            # Moves info
            moves = ga.ap_moves.get(ap, 0)
            move_turns = ga.ap_move_turns.get(ap, 0)
            move_str = f"{moves} movement actions in {move_turns} turns" if moves > 0 else "0 movement actions"

            # Gates: start vs end, net gained/lost
            start_gate_set = set(start_gates_list)
            end_gate_set = set(end_gates_list)
            net_gained = sorted(end_gate_set - start_gate_set)
            net_lost = sorted(start_gate_set - end_gate_set)
            gates_start_str = f"Started on {len(start_gates_list)} gates ({', '.join(sorted(start_gates_list))})" if start_gates_list else "Started on 0 gates"
            gates_end_str = f"Ended on {len(end_gates_list)} gates ({', '.join(sorted(end_gates_list))})" if end_gates_list else "Ended on 0 gates"
            gained_str = f"Gained {', '.join(net_gained)}." if net_gained else ""
            lost_str = f"Lost {', '.join(net_lost)}." if net_lost else ""

            # SB achievements
            achievements = ga.ap_achievements.get(ap, [])
            sb_chosen = ga.ap_sb_chosen.get(ap, [])
            sb_flipped = ga.ap_sb_flipped.get(ap, [])
            sb_str = ""
            if achievements:
                sb_str += f" Achieved SBR: {', '.join(achievements)}."
            if sb_chosen:
                sb_str += f" Chose SB: {', '.join(sb_chosen)}."
            if sb_flipped:
                sb_str += f" Flipped SBs: {', '.join(sb_flipped)} with IP."

            # Captures
            captures = ga.ap_captures_by_enemy.get(ap, 0)
            cap_str = f" {captures} cultists captured by enemies." if captures > 0 else ""

            # Unit breakdown at end
            units_str = ""
            if fb_end:
                ubr = fb_end.get("units_by_region", {})
                region_parts = []
                for region in end_gates_list:
                    units = ubr.get(region, {})
                    if units:
                        unit_descs = []
                        for utype, count in sorted(units.items()):
                            unit_descs.append(f"{count} {utype}")
                        region_parts.append(f"{region} (gate): {', '.join(unit_descs)}")
                    else:
                        region_parts.append(f"{region} (gate): empty")
                # Other regions with FB units
                for region, units in sorted(ubr.items()):
                    if region not in end_gates_list and units:
                        unit_descs = []
                        for utype, count in sorted(units.items()):
                            if count > 0:
                                unit_descs.append(f"{count} {utype}")
                        if unit_descs:
                            region_parts.append(f"{region}: {', '.join(unit_descs)}")
                units_str = " | ".join(region_parts)

            ap_line = (f"- AP{ap} — {start_power}P. "
                      f"{gates_start_str}. "
                      f"SBs: {', '.join(start_sbs) if start_sbs else 'none'}. "
                      f"{writhe_str}. {move_str}. "
                      f"{gates_end_str}. "
                      f"{gained_str} {lost_str}{sb_str}{cap_str}")
            lines.append(ap_line)

            if units_str:
                lines.append(f"  Units: {units_str}")

            # Doom phase line
            ritual = ga.doom_rituals.get(ap)
            dm = ga.doom_dm.get(ap)
            ip_sbs = ga.doom_ip.get(ap, [])
            gate_doom = ga.doom_gate_doom.get(ap, 0)

            if ritual or dm or gate_doom:
                doom_parts = [f"- Doom {ap} —"]
                if gate_doom:
                    doom_parts.append(f"gained {gate_doom} doom from {gate_doom} gates.")
                    doom_from_gates += gate_doom
                if ritual:
                    r_cost, r_doom, r_es = ritual
                    doom_parts.append(f"Ritualed for {r_cost} power, gained {r_doom} doom" +
                                     (f" + 1 ES" if r_es else "") + ".")
                    doom_from_rituals += r_doom
                    if r_es:
                        doom_from_es += 1
                else:
                    doom_parts.append("Did not ritual.")
                if dm:
                    doom_parts.append(f"Devil's Mark in {dm}.")
                if ip_sbs:
                    doom_parts.append(f"Infernal Pact: flipped {', '.join(ip_sbs)}.")
                lines.append(" ".join(doom_parts))

        # End summary
        lines.append("")
        total_doom = ga.end_doom + ga.end_es  # approximate
        lines.append(f"Ended game with {ga.end_doom} doom. "
                     f"{doom_from_gates} from gates, {doom_from_rituals} from rituals, "
                     f"{ga.end_es} from ES. "
                     f"Total rituals {ga.total_rituals}. "
                     f"Total DMs {ga.total_dm}. "
                     f"Ended with {len(ga.end_sbs)} SBs, earned in order: {', '.join(ga.sb_earn_order)}. "
                     f"{ga.total_enemy_captures} total cultists captured by enemies. "
                     f"Total {bot_ga.total_writhes} Writhes, avg power {bot_ga.avg_power_at_writhe():.1f} when Writhing. "
                     f"{ga.total_moves} total Moves.")
        lines.append("")

    _write_game_summary(bot_ga, "Bot", lines)
    if base_ga:
        _write_game_summary(base_ga, "Baseline", lines)

    # ── Per-AP Sections ────────────────────────────────────────────────
    for ap in range(1, bot_ga.max_ap + 1):
        lines.append(f"---")
        lines.append("")

        # ── Placement (AP0 special case) ───────────────────────────────
        if ap == 1:
            lines.append("### Placement")
            lines.append(f"- Bot placed in {bot_ga.placement_region or '?'}.")
            if base_ga:
                lines.append(f"- Baseline placed in {base_ga.placement_region or '?'}.")
            lines.append("")

        # ── AP Action Phase ────────────────────────────────────────────
        lines.append(f"### AP{ap} -- Action Phase")
        lines.append("")

        # Bot Summary (turn by turn) — format: N. P[power] Action -[cost]P. Details.
        lines.append(f"#### Bot Summary")
        bot_ap_actions = bot_ga.ap_actions.get(ap, [])
        action_phase_actions = [a for a in bot_ap_actions if not a[7]]  # not in doom phase
        if action_phase_actions:
            start_power = bot_ga.ap_start_power.get(ap, action_phase_actions[0][2])
            lines.append(f"Bot starts with {start_power} power.")
            lines.append("")
            step_num = 0
            for (act_str, cost, power_before, power_after, turn, nat_name, is_main, is_doom) in action_phase_actions:
                skip = "End Turn" in nat_name or "Pass" in nat_name
                if is_main and not skip:
                    step_num += 1
                    if cost > 0:
                        lines.append(f"{step_num}. P{power_before} {nat_name} -{cost}P.")
                    elif cost < 0:
                        lines.append(f"{step_num}. P{power_before} {nat_name} +{-cost}P.")
                    else:
                        lines.append(f"{step_num}. P{power_before} {nat_name}.")
                elif not is_main and not skip:
                    lines.append(f"   - {nat_name}.")
        else:
            lines.append("No action phase actions recorded for bot.")
        lines.append("")

        # AP-End unit breakdown — all data from per-AP snapshots, no manual math
        ap_entries = [e for e in entries if e["ap"] == ap and e["phase"] == "action"]
        if ap_entries:
            last_snap = ap_entries[-1]["bot_snap"]["fb"]
            ubr = last_snap.get("units_by_region", {})
            gate_list = last_snap.get("gates", [])
            lines.append(f"Ended AP with {last_snap.get('gate_count', '?')} controlled gates:")
            for region in gate_list:
                units = ubr.get(region, {})
                if units:
                    unit_descs = [f"{count} {utype}" for utype, count in sorted(units.items()) if count > 0]
                    lines.append(f"- {region}: {', '.join(unit_descs)}")
                else:
                    lines.append(f"- {region}: (empty)")
            other_regions = [r for r in sorted(ubr.keys()) if r not in gate_list and any(v > 0 for v in ubr[r].values())]
            if other_regions:
                lines.append("Other regions with FB units:")
                for region in other_regions:
                    units = ubr[region]
                    unit_descs = [f"{count} {utype}" for utype, count in sorted(units.items()) if count > 0]
                    if unit_descs:
                        lines.append(f"- {region}: {', '.join(unit_descs)}")
        lines.append("")

        # Baseline Summary
        if base_ga:
            lines.append(f"#### Baseline Summary")
            base_ap_actions = base_ga.ap_actions.get(ap, [])
            base_action_phase = [a for a in base_ap_actions if not a[7]]
            if base_action_phase:
                base_start_power = base_ga.ap_start_power.get(ap, base_action_phase[0][2])
                lines.append(f"Baseline starts with {base_start_power} power.")
                lines.append("")
                step_num = 0
                for (act_str, cost, power_before, power_after, turn, nat_name, is_main, is_doom) in base_action_phase:
                    skip = "End Turn" in nat_name or "Pass" in nat_name
                    if is_main and not skip:
                        step_num += 1
                        if cost > 0:
                            lines.append(f"{step_num}. P{power_before} {nat_name} -{cost}P.")
                        elif cost < 0:
                            lines.append(f"{step_num}. P{power_before} {nat_name} +{-cost}P.")
                        else:
                            lines.append(f"{step_num}. P{power_before} {nat_name}.")
                    elif not is_main and not skip:
                        lines.append(f"   - {nat_name}.")
            else:
                lines.append("No baseline action phase actions recorded.")
            lines.append("")

        # Key Differences
        lines.append("#### Key Differences")
        ap_entries_all = [e for e in entries if e["ap"] == ap]
        truly_diff = [e for e in ap_entries_all if e["class"] in ("TRULY DIFFERENT", "LIKELY DIFFERENT")]
        if truly_diff:
            for e in truly_diff:
                bot_desc = _natural_action_name(e["bot_action"]) if e["bot_action"] else "?"
                base_desc = _natural_action_name(e["base_action"]) if e["base_action"] else "?"
                lines.append(f"- Bot {bot_desc}, baseline {base_desc}.")
        else:
            matches = [e for e in ap_entries_all if e["class"] in ("MATCH", "NEAR_MATCH")]
            if matches:
                lines.append("- No significant action-level differences this AP.")
            else:
                situ = [e for e in ap_entries_all if e["class"] == "SITUATIONAL"]
                if situ:
                    lines.append("- Game states too different for meaningful comparison (SITUATIONAL).")
                else:
                    lines.append("- No comparison data available.")
        lines.append("")

        # Assessment
        lines.append("#### Assessment")
        if truly_diff:
            lines.append(f"AP{ap} has {len(truly_diff)} divergent action(s) from baseline. "
                         f"Bot may have a strategy gap or scoring bug in this phase.")
        else:
            lines.append(f"AP{ap} actions are broadly consistent with baseline play.")
        lines.append("")

        # ── Gather Power / Player Order ────────────────────────────────
        gp = bot_ga.gather_power_lines.get(ap + 1, "")  # gather happens at start of NEXT round
        if gp or ap < bot_ga.max_ap:
            lines.append(f"### AP{ap} -- Gather Power")
            if gp:
                lines.append(f"Bot: {gp}")
            else:
                lines.append(f"Bot: (gather power line not found in game log)")
            if base_ga:
                base_gp = base_ga.gather_power_lines.get(ap + 1, "")
                if base_gp:
                    lines.append(f"Baseline: {base_gp}")
                else:
                    lines.append(f"Baseline: (gather power line not found)")
            lines.append("")

        # ── Doom Phase ─────────────────────────────────────────────────
        lines.append(f"### AP{ap} -- Doom Phase")
        lines.append("")

        # Bot doom phase
        lines.append("**Bot:**")
        ritual = bot_ga.doom_rituals.get(ap)
        dm = bot_ga.doom_dm.get(ap)
        ip_sbs = bot_ga.doom_ip.get(ap, [])
        gate_doom = bot_ga.doom_gate_doom.get(ap, 0)

        if ritual:
            r_cost, r_doom, r_es = ritual
            lines.append(f"- Ritual: yes. Cost {r_cost}, gained {r_doom} doom" +
                         (f" + 1 ES." if r_es else "."))
        else:
            lines.append("- Ritual: no.")
        if dm:
            lines.append(f"- Devil's Mark: yes. Placed crater at {dm}, destroyed gate.")
        else:
            lines.append("- Devil's Mark: no.")
        if ip_sbs:
            lines.append(f"- Infernal Pact: yes. Flipped {', '.join(ip_sbs)}.")
        else:
            lines.append("- Infernal Pact: no.")
        lines.append("")

        # Baseline doom phase
        if base_ga:
            lines.append("**Baseline:**")
            b_ritual = base_ga.doom_rituals.get(ap)
            b_dm = base_ga.doom_dm.get(ap)
            b_ip = base_ga.doom_ip.get(ap, [])
            if b_ritual:
                r_cost, r_doom, r_es = b_ritual
                lines.append(f"- Ritual: yes. Cost {r_cost}, gained {r_doom} doom" +
                             (f" + 1 ES." if r_es else "."))
            else:
                lines.append("- Ritual: no.")
            if b_dm:
                lines.append(f"- Devil's Mark: yes. Placed crater at {b_dm}.")
            else:
                lines.append("- Devil's Mark: no.")
            if b_ip:
                lines.append(f"- Infernal Pact: yes. Flipped {', '.join(b_ip)}.")
            else:
                lines.append("- Infernal Pact: no.")
            lines.append("")

        # Doom phase difference
        doom_entries = [e for e in entries if e["ap"] == ap and e["phase"] == "doom"]
        if doom_entries:
            diff_doom = [e for e in doom_entries if e["class"] in ("TRULY DIFFERENT", "LIKELY DIFFERENT")]
            if diff_doom:
                lines.append(f"Difference: {len(diff_doom)} divergent doom-phase decision(s).")
            else:
                lines.append("Difference: doom phase decisions consistent with baseline.")
        else:
            lines.append("Difference: no doom phase comparison data.")
        lines.append("")

    # ── Overall Assessment ─────────────────────────────────────────────
    lines.append("---")
    lines.append("")
    lines.append("## Overall Assessment")
    lines.append("")
    base_writhes_str = f"{base_ga.total_writhes} (avg P{base_ga.avg_power_at_writhe():.0f})" if base_ga else "?"
    bot_writhes_str = f"{bot_ga.total_writhes} (avg P{bot_ga.avg_power_at_writhe():.0f})"
    oa_rows = [
        ("Gates_peak", str(bot_ga.peak_gates), str(base_ga.peak_gates) if base_ga else "?"),
        ("Gates_end", str(bot_ga.end_gates), str(base_ga.end_gates) if base_ga else "?"),
        ("SBs_earned", str(len(bot_ga.end_sbs)), str(len(base_ga.end_sbs)) if base_ga else "?"),
        ("Awakens", "yes" if bot_ga.awakened else "no", "yes" if base_ga and base_ga.awakened else "?"),
        ("Rituals_total", str(bot_ga.total_rituals), str(base_ga.total_rituals) if base_ga else "?"),
        ("Total_doom", str(bot_ga.end_doom), str(base_ga.end_doom) if base_ga else "?"),
        ("Total_Writhes", bot_writhes_str, base_writhes_str),
        ("Total_Moves", str(bot_ga.total_moves), str(base_ga.total_moves) if base_ga else "?"),
    ]
    lines.append(f"| {pad('Metric', 20)} | {pad('Bot', 20)} | {pad('Baseline', 20)} |")
    lines.append(f"|{'-'*22}|{'-'*22}|{'-'*22}|")
    for label, bot_val, base_val in oa_rows:
        lines.append(f"| {pad(label, 20)} | {pad(bot_val, 20)} | {pad(base_val, 20)} |")
    lines.append("")

    # All factions end-of-game
    bot_final = bot_recon_fresh.final_doom()
    bot_fs = bot_recon_fresh.final_state
    faction_lines = []
    for f in bot_fs.setup:
        fs = bot_fs.factions[f]
        doom = bot_final.get(f, 0)
        sb_count = len(fs.sb_earned)
        faction_lines.append(f"{f} {doom} doom {sb_count} SBs")
    lines.append(f"All factions end-of-game: {', '.join(faction_lines)}.")
    lines.append("")

    # Root cause
    counts = defaultdict(int)
    for e in entries:
        counts[e["class"]] += 1
    truly_total = counts.get("TRULY DIFFERENT", 0) + counts.get("LIKELY DIFFERENT", 0)
    lines.append(f"Root cause of divergence: {truly_total} divergent decisions detected across {bot_ga.max_ap} APs. "
                 f"{'High Move count suggests bot is spending power on positioning instead of Writhing. ' if bot_ga.total_moves > 2 else ''}"
                 f"{'Low Writhe count compared to baseline. ' if bot_ga.total_writhes < 5 else ''}"
                 f"Review per-AP Key Differences sections above for specific action-level bugs.")
    lines.append("")

    # Key bugs
    lines.append("Key bugs:")
    bug_num = 1
    if bot_ga.total_moves > 2:
        base_moves_str = f"{base_avg['moves']:.1f}" if base_avg else "?"
        lines.append(f"{bug_num}. Excessive Move actions ({bot_ga.total_moves}) vs baseline avg ({base_moves_str}). Power wasted on positioning.")
        bug_num += 1
    if bot_ga.total_writhes < 5 and base_avg and base_avg["writhes"] > 5:
        lines.append(f"{bug_num}. Too few Writhes ({bot_ga.total_writhes}) vs baseline avg ({base_avg['writhes']:.1f}). Writhe is FB's primary action.")
        bug_num += 1
    if bot_ga.total_enemy_captures > 1:
        base_cap_str = f"{base_avg['enemy_captures']:.0f}" if base_avg else "1"
        lines.append(f"{bug_num}. {bot_ga.total_enemy_captures} FB cultists captured by enemies (baseline avg ~{base_cap_str}).")
        bug_num += 1
    if truly_total > 0:
        for e in entries:
            if e["class"] in ("TRULY DIFFERENT", "LIKELY DIFFERENT"):
                bot_desc = _natural_action_name(e["bot_action"]) if e["bot_action"] else "?"
                base_desc = _natural_action_name(e["base_action"]) if e["base_action"] else "?"
                lines.append(f"{bug_num}. AP{e['ap']} {e['phase']}: bot {bot_desc} vs baseline {base_desc}.")
                bug_num += 1
    if bug_num == 1:
        lines.append("(No specific bugs detected -- bot play is broadly consistent with baseline.)")
    lines.append("")

    with open(out_path, "w") as f:
        f.write("\n".join(lines))
    return out_path

def print_compare_summary(entries, baselines):
    counts = defaultdict(int)
    for e in entries:
        counts[e["class"]] += 1
    print("\n" + "=" * 76)
    print(f"MULTI-BASELINE COMPARISON — {len(entries)} bot decisions, {len(baselines)} FB-win baselines")
    print("=" * 76)
    for k in ("MATCH", "NEAR_MATCH", "TRULY DIFFERENT", "LIKELY DIFFERENT", "SITUATIONAL", "NO_BASELINE"):
        print(f"  {k}: {counts.get(k, 0)}")
    by_ap_phase = defaultdict(lambda: defaultdict(int))
    for e in entries:
        by_ap_phase[(e["ap"], e["phase"])][e["class"]] += 1
    print("\nPer-AP/phase counts:")
    for k in sorted(by_ap_phase.keys()):
        c = by_ap_phase[k]
        total = sum(c.values())
        bugs = c.get("TRULY DIFFERENT", 0) + c.get("LIKELY DIFFERENT", 0)
        print(f"  AP{k[0]} {k[1]:6s}: {total} decisions, {bugs} likely-bugs, {c.get('MATCH',0) + c.get('NEAR_MATCH',0)} matches")

# ── Main ────────────────────────────────────────────────────────────────
bot_lines, bot_actions = load_file(path)
bot_recon = Reconstruction(bot_lines, bot_actions)
print_summary(bot_recon, f"BOT: {os.path.basename(path)}")

if per_ap:
    print_per_ap(bot_recon)

if per_decision:
    print_per_decision(bot_recon)

if compare_path:
    base_lines, base_actions = load_file(compare_path)
    base_recon = Reconstruction(base_lines, base_actions)
    print_summary(base_recon, f"BASELINE: {os.path.basename(compare_path)}")
    baselines_single = [(compare_path, base_recon)]
    entries = compare_multi(bot_recon, baselines_single)
    print_compare_summary(entries, baselines_single)
    if discrepancies_out:
        out = write_discrepancies_md(entries, path, baselines_single, [], discrepancies_out)
        print(f"\nDiscrepancies written: {out}")

if baseline_folder:
    print(f"\nLoading FB-victory baselines from: {baseline_folder}")
    baselines, rejected = load_fb_win_baselines(baseline_folder)
    print(f"  accepted (FB won): {len(baselines)}")
    for p, r in baselines:
        doom = r.final_doom()
        print(f"    ✓ {os.path.basename(p)}  final_doom={doom}")
    print(f"  rejected: {len(rejected)}")
    for p, reason in rejected[:10]:
        print(f"    ✗ {os.path.basename(p)}  {reason}")
    if not baselines:
        print("  WARNING: no FB-victory baselines found — cannot compare.")
    else:
        entries = compare_multi(bot_recon, baselines)
        print_compare_summary(entries, baselines)
        if discrepancies_out:
            out = write_discrepancies_md(entries, path, baselines, rejected, discrepancies_out)
            print(f"\nDiscrepancies written: {out}")
