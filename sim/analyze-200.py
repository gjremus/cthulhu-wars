#!/usr/bin/env python3
"""Analyze 200 FB game logs and produce comprehensive stats + pick representative replays.

Outputs:
  - FB_DISCREPANCIES.md with full stats
  - Prints 5 recommended replay files
"""

import os, re, sys, glob, json
from collections import defaultdict, Counter
from statistics import mean, median, stdev

LOG_DIR = os.path.expanduser("~/cthulhu-wars FB/sim/win-logs")
OUTPUT_MD = os.path.expanduser("~/My Drive/Personal/Games/Cthulhu Wars/Firstborn/Bot/FB_DISCREPANCIES.md")

# Baseline values
BASELINE = {
    "writhes": 7.2, "moves": 0.8, "recruits": 3.2, "summons": 2.3, "builds": 1.9,
    "doom": 35, "es": 7, "sbs": 6, "awakens": 3, "rituals": 4.5, "dms": 3,
}

def strip_html(s):
    return re.sub(r'<[^>]+>', '', s).strip()

def parse_game(filepath):
    """Parse a single game log file and extract all metrics."""
    with open(filepath) as f:
        content = f.read()

    all_lines = content.strip().split('\n')

    # Split into action section (plain text) and HTML game log section
    html_start = None
    for i, line in enumerate(all_lines):
        if '<div' in line or '<span' in line:
            html_start = i
            break

    action_lines = all_lines[:html_start] if html_start else all_lines
    html_lines = all_lines[html_start:] if html_start else []
    lines = all_lines  # keep for backward compat
    # Use action_content for action-string patterns, html_content for game log parsing
    action_content = '\n'.join(action_lines)
    html_content = '\n'.join(html_lines)

    result = {"file": os.path.basename(filepath)}

    # Winner
    winner = None
    for line in reversed(lines):
        plain = strip_html(line)
        m = re.search(r'(\w[\w\s]+?)\s+won$', plain)
        if m:
            winner = m.group(1).strip()
            break
    result["winner"] = winner
    result["fb_won"] = winner == "Firstborn" if winner else False

    # Action counts (from action strings section only)
    result["writhes"] = len(re.findall(r'FBWritheMainAction\(FB\)', action_content))
    result["builds"] = len(re.findall(r'BuildGateAction\(FB,', action_content))
    result["recruits"] = len(re.findall(r'RecruitAction\(FB,', action_content))
    result["summons"] = len(re.findall(r'SummonAction\(FB,', action_content))
    result["moves"] = len(re.findall(r'^MoveAction\(FB,', action_content, re.MULTILINE))
    result["rituals"] = len(re.findall(r'RitualAction\(FB,', action_content))
    result["awakens_count"] = len(re.findall(r'FBAwakenGhatanothoaAction\(FB', action_content))

    # Awaken AP and game length: use standalone DoomPhaseAction in action section
    # AP1 actions happen before first DoomPhaseAction, AP2 before second, etc.
    awaken_aps = []
    current_ap = 1  # starts at AP1
    for line in action_lines:
        plain = line.strip()
        if plain == 'DoomPhaseAction':
            current_ap += 1
        if 'FBAwakenGhatanothoaAction(FB' in plain:
            awaken_aps.append(current_ap)
    result["awaken_aps"] = awaken_aps

    # Game length = number of completed APs = number of standalone DoomPhaseAction
    # (since doom phase fires at end of each AP)
    standalone_dp = sum(1 for l in action_lines if l.strip() == 'DoomPhaseAction')
    # But also check Turn markers in HTML section as cross-check
    turn_numbers = []
    for line in html_lines:
        plain = strip_html(line)
        m = re.match(r'^Turn (\d+)$', plain)
        if m:
            turn_numbers.append(int(m.group(1)))
    html_aps = max(turn_numbers) if turn_numbers else 0
    # Use Turn markers if available (more reliable), else standalone DP count
    result["aps"] = html_aps if html_aps > 0 else standalone_dp

    # SpellbookAction for FB
    sbs = re.findall(r'SpellbookAction\(FB,\s*(\w+)', action_content)
    result["sbs_count"] = len(sbs)
    result["sbs_list"] = sbs

    # Devil's Mark count - from HTML game log section
    dms = len(re.findall(r'placed.*Crater.*Devil.s Mark|Devil.s Mark.*Crater', strip_html(html_content)))
    if dms == 0:
        # Fallback: count from action section BOT lines
        dms = len(re.findall(r"FBDevils.*Mark.*Place.*Crater", action_content))
    result["dms"] = dms

    # Doom: parse from HTML game log lines
    # "Firstborn got X Doom" - gate doom
    gate_doom_lines = re.findall(r"Firstborn got (\d+) Doom", strip_html(html_content))
    gate_doom = sum(int(x) for x in gate_doom_lines)

    # "Firstborn performed the ritual for X Power and gained Y Doom"
    ritual_doom_matches = re.findall(r"Firstborn performed the ritual for (\d+) Power and gained (\d+) Doom", strip_html(html_content))
    ritual_doom = sum(int(m[1]) for m in ritual_doom_matches)

    # "Firstborn revealed [X] [Y] ... for Z Doom" - ES reveal doom
    es_reveal_matches = re.findall(r"Firstborn revealed.*?for (\d+) Doom", strip_html(html_content))
    es_doom = sum(int(x) for x in es_reveal_matches)

    result["gate_doom"] = gate_doom
    result["ritual_doom"] = ritual_doom
    result["es_doom"] = es_doom
    result["total_doom"] = gate_doom + ritual_doom + es_doom

    # Elder Signs
    es_from_ritual = len(re.findall(r"Firstborn.*ritual.*Elder Sign", strip_html(html_content)))
    es_from_dm = len(re.findall(r"Firstborn.*Elder Sign.*Devil.s Mark", strip_html(html_content)))
    es_other = len(re.findall(r"Firstborn gained an Elder Sign", strip_html(html_content))) - es_from_dm
    # es_other might include ritual ES too, so let's be more careful
    total_es_gained = es_from_ritual + es_from_dm
    result["es_total"] = total_es_gained
    result["es_from_ritual"] = es_from_ritual
    result["es_from_dm"] = es_from_dm

    # ES values revealed
    es_values = []
    for m in re.finditer(r"Firstborn revealed ((?:\[\d+\]\s*)+)for (\d+) Doom", strip_html(html_content)):
        vals = re.findall(r'\[(\d+)\]', m.group(1))
        es_values.extend(int(v) for v in vals)
    result["es_values"] = es_values
    result["es_reveal_total"] = sum(es_values)

    # Gates per AP (track FB gates at end of each AP)
    # Use action section: BuildGateAction adds gates, DoomPhaseAction marks AP end
    # Note: gate loss from DM crater happens in doom phase (after action phase)
    # We need to track crater placements from action strings too
    gates_by_ap = []
    fb_gates = set()

    # Starting region
    start_m = re.search(r'StartingRegionAction\(FB,\s*(\w+)\)', action_content)
    start_region = start_m.group(1) if start_m else None
    result["start_region"] = start_region
    if start_region:
        fb_gates.add(start_region)

    for line in action_lines:
        plain = line.strip()
        if plain == 'DoomPhaseAction':
            gates_by_ap.append(len(fb_gates))

        # Build gate
        m = re.match(r'BuildGateAction\(FB,\s*(\w+)\)', plain)
        if m:
            fb_gates.add(m.group(1))

        # DM crater destroys a gate - look for FBDevilsMarkPlaceCrater or similar action
        # Pattern: FBDevils Mark Place Crater REGION or FBDevilsMarkPlaceCraterAction
        m2 = re.search(r'FBDevils.*Mark.*Place.*Crater.*?(\w+)\s', plain)
        if not m2:
            m2 = re.search(r'FBDevilsMarkPlaceCrater\w*Action\(FB,\s*(\w+)', plain)
        if m2:
            fb_gates.discard(m2.group(1))

    # Also check HTML lines for gate destruction
    for line in html_lines:
        html_plain = strip_html(line)
        if 'Firstborn' in html_plain and 'Gate' in html_plain and 'destroyed' in html_plain:
            m3 = re.search(r'Gate in (\w[\w\s]*?) destroyed', html_plain)
            if m3:
                region = m3.group(1).strip().replace(' ', '')
                fb_gates.discard(region)

    result["gates_by_ap"] = gates_by_ap
    result["gates_peak"] = max(gates_by_ap) if gates_by_ap else len(fb_gates)
    result["gates_end"] = gates_by_ap[-1] if gates_by_ap else len(fb_gates)

    # Writhe kills (Acolyte -> Desiccated conversions)
    writhe_kills = len(re.findall(r'FBWritheKillUnitAction\(FB', action_content))
    result["writhe_kills"] = writhe_kills

    # Killed units by type from Writhe
    killed_units = re.findall(r'FBWritheKillUnitAction\(FB,\s*([\w/]+)', action_content)
    result["writhe_killed_units"] = killed_units

    # Track per-AP actions for standard path analysis (action section, DoomPhaseAction = AP boundary)
    # RitualAction happens AFTER DoomPhaseAction but belongs to that AP's doom phase
    # So: DoomPhaseAction ends AP N, and RitualAction right after belongs to AP N
    ap_actions = defaultdict(lambda: {"writhes": 0, "builds": 0, "recruits": 0, "summons": 0,
                                       "moves": 0, "awakens": 0, "rituals": 0})
    current_ap = 1
    in_doom_phase = False  # True after DoomPhaseAction, until next non-doom action
    doom_phase_ap = 0      # Which AP the current doom phase belongs to
    for line in action_lines:
        plain = line.strip()
        if plain == 'DoomPhaseAction':
            in_doom_phase = True
            doom_phase_ap = current_ap
            current_ap += 1
            continue
        # RitualAction during doom phase -> attribute to doom_phase_ap
        if re.match(r'RitualAction\(FB,', plain):
            if in_doom_phase:
                ap_actions[doom_phase_ap]["rituals"] += 1
            else:
                ap_actions[current_ap]["rituals"] += 1
            continue
        # Any non-ritual, non-doom action = we're in action phase
        if in_doom_phase and (re.match(r'(PreMainAction|MainAction|FBWrithe|BuildGate|Recruit|Summon|Move)', plain)):
            in_doom_phase = False
        if 'FBWritheMainAction(FB)' in plain:
            ap_actions[current_ap]["writhes"] += 1
        if re.match(r'BuildGateAction\(FB,', plain):
            ap_actions[current_ap]["builds"] += 1
        if re.match(r'RecruitAction\(FB,', plain):
            ap_actions[current_ap]["recruits"] += 1
        if re.match(r'SummonAction\(FB,', plain):
            ap_actions[current_ap]["summons"] += 1
        if re.match(r'MoveAction\(FB,', plain):
            ap_actions[current_ap]["moves"] += 1
        if 'FBAwakenGhatanothoaAction(FB' in plain:
            ap_actions[current_ap]["awakens"] += 1

    result["ap_actions"] = dict(ap_actions)

    # Standard path milestones
    # AP1: 2+ gates
    ap1_gates = gates_by_ap[0] if len(gates_by_ap) > 0 else 0
    result["milestone_ap1_2g"] = ap1_gates >= 2

    # AP2: ghato awakened + 2+ gates
    ghato_by_ap2 = any(ap <= 2 for ap in awaken_aps)
    ap2_gates = gates_by_ap[1] if len(gates_by_ap) > 1 else 0
    result["milestone_ap2_ghato_2g"] = ghato_by_ap2 and ap2_gates >= 2

    # AP3: 3+ gates + ritual
    ap3_gates = gates_by_ap[2] if len(gates_by_ap) > 2 else 0
    ap3_ritual = ap_actions.get(3, {}).get("rituals", 0) > 0
    result["milestone_ap3_3g_rit"] = ap3_gates >= 3 and ap3_ritual

    # AP4: 3+ gates + ritual
    ap4_gates = gates_by_ap[3] if len(gates_by_ap) > 3 else 0
    ap4_ritual = ap_actions.get(4, {}).get("rituals", 0) > 0
    result["milestone_ap4_3g_rit"] = ap4_gates >= 3 and ap4_ritual

    result["full_path"] = (result["milestone_ap1_2g"] and result["milestone_ap2_ghato_2g"]
                           and result["milestone_ap3_3g_rit"] and result["milestone_ap4_3g_rit"])

    # ghato awakened but never 3 gates
    result["ghato_no_3g"] = ghato_by_ap2 and all(g < 3 for g in gates_by_ap)

    # Other faction doom (for context)
    faction_doom = {}
    plain_content = strip_html(html_content)
    for faction in ["Crawling Chaos", "Opener of the Way", "The Ancients", "Black Goat",
                    "Yellow Sign", "Great Cthulhu", "Sleeper", "Windwalker"]:
        # Sum "X got Y Doom" + ritual doom + ES doom for each faction
        got_doom = re.findall(rf"{faction} got (\d+) Doom", plain_content)
        rit_doom = re.findall(rf"{faction} performed the ritual.*gained (\d+) Doom", plain_content)
        es_doom_f = re.findall(rf"{faction} revealed.*?for (\d+) Doom", plain_content)
        total = sum(int(x) for x in got_doom) + sum(int(x) for x in rit_doom) + sum(int(x) for x in es_doom_f)
        if total > 0:
            code = {"Crawling Chaos": "CC", "Opener of the Way": "OW", "The Ancients": "AN",
                    "Black Goat": "BG", "Yellow Sign": "YS", "Great Cthulhu": "GC",
                    "Sleeper": "SL", "Windwalker": "WW"}.get(faction, faction)
            faction_doom[code] = total
    result["faction_doom"] = faction_doom

    return result


def format_stats(games):
    """Generate the full markdown stats document."""
    n = len(games)
    wins = [g for g in games if g["fb_won"]]
    losses = [g for g in games if not g["fb_won"]]
    win_rate = len(wins) / n * 100 if n > 0 else 0

    # Helper
    def avg(vals):
        return mean(vals) if vals else 0
    def med(vals):
        return median(vals) if vals else 0
    def sd(vals):
        return stdev(vals) if len(vals) > 1 else 0

    lines = []
    lines.append(f"# FB Bot {n}-Game Analysis")
    lines.append(f"Generated: 2026-04-23")
    lines.append(f"Games analyzed: {n}")
    lines.append(f"Win rate: {len(wins)}/{n} ({win_rate:.1f}%)")
    lines.append("")

    # ── Section 1: Action Comparison Table ──
    lines.append("## 1. Action Comparison Table")
    lines.append("")
    actions = ["writhes", "moves", "recruits", "summons", "builds"]
    action_labels = ["Writhes", "Moves", "Recruits", "Summons", "Builds"]
    lines.append("| Action | Bot Avg | Bot Med | Bot StdDev | Baseline |")
    lines.append("|--------|---------|---------|------------|----------|")
    for key, label in zip(actions, action_labels):
        vals = [g[key] for g in games]
        lines.append(f"| {label:30s} | {avg(vals):5.1f}   | {med(vals):5.1f}   | {sd(vals):5.1f}      | {BASELINE[key]:.1f}      |")

    # Additional rows
    lines.append(f"| {'Rituals':30s} | {avg([g['rituals'] for g in games]):5.1f}   | {med([g['rituals'] for g in games]):5.1f}   | {sd([g['rituals'] for g in games]):5.1f}      | {BASELINE['rituals']:.1f}      |")
    lines.append(f"| {'Devil Marks':30s} | {avg([g['dms'] for g in games]):5.1f}   | {med([g['dms'] for g in games]):5.1f}   | {sd([g['dms'] for g in games]):5.1f}      | {BASELINE['dms']:.1f}      |")
    lines.append(f"| {'Awakens':30s} | {avg([g['awakens_count'] for g in games]):5.1f}   | {med([g['awakens_count'] for g in games]):5.1f}   | {sd([g['awakens_count'] for g in games]):5.1f}      | {BASELINE['awakens']:.1f}      |")
    lines.append(f"| {'Spellbooks':30s} | {avg([g['sbs_count'] for g in games]):5.1f}   | {med([g['sbs_count'] for g in games]):5.1f}   | {sd([g['sbs_count'] for g in games]):5.1f}      | {BASELINE['sbs']:.1f}      |")
    lines.append("")

    # ── Section 2: Gates at End of Each AP ──
    lines.append("## 2. Gates at End of Each AP")
    lines.append("")
    max_aps = max(g["aps"] for g in games) if games else 0
    lines.append("| AP | Avg Gates | Med Gates | Min | Max | Games with 3+ |")
    lines.append("|----|-----------|-----------|-----|-----|---------------|")
    for ap_idx in range(min(max_aps, 10)):
        vals = [g["gates_by_ap"][ap_idx] for g in games if len(g["gates_by_ap"]) > ap_idx]
        if not vals:
            continue
        pct_3plus = sum(1 for v in vals if v >= 3) / len(vals) * 100
        lines.append(f"| AP{ap_idx+1} | {avg(vals):5.2f}     | {med(vals):5.1f}     | {min(vals):3d} | {max(vals):3d} | {pct_3plus:5.1f}%        |")
    lines.append("")

    # ── Section 3: Awaken Distribution ──
    lines.append("## 3. Awaken Distribution")
    lines.append("")
    awaken_counts = Counter(g["awakens_count"] for g in games)
    lines.append("| Awakens | Games | Pct |")
    lines.append("|---------|-------|-----|")
    for k in sorted(awaken_counts.keys()):
        lines.append(f"| {k}       | {awaken_counts[k]:5d} | {awaken_counts[k]/n*100:5.1f}% |")
    lines.append("")

    # Awaken AP distribution
    all_awaken_aps = []
    for g in games:
        all_awaken_aps.extend(g["awaken_aps"])
    if all_awaken_aps:
        ap_dist = Counter(all_awaken_aps)
        lines.append("### Awaken AP Distribution (which AP ghato is awakened)")
        lines.append("")
        lines.append("| AP | Count | Pct of all awakens |")
        lines.append("|----|-------|--------------------|")
        for ap in sorted(ap_dist.keys()):
            lines.append(f"| AP{ap} | {ap_dist[ap]:5d} | {ap_dist[ap]/len(all_awaken_aps)*100:5.1f}%              |")
        lines.append("")

    # ── Section 4: Doom Distribution ──
    lines.append("## 4. Doom Distribution")
    lines.append("")
    doom_vals = [g["total_doom"] for g in games]
    lines.append(f"- Average doom: {avg(doom_vals):.1f}")
    lines.append(f"- Median doom: {med(doom_vals):.1f}")
    lines.append(f"- StdDev: {sd(doom_vals):.1f}")
    lines.append(f"- Min: {min(doom_vals)}, Max: {max(doom_vals)}")
    lines.append(f"- Baseline: {BASELINE['doom']}")
    lines.append("")

    # Doom buckets
    doom_buckets = Counter()
    for d in doom_vals:
        bucket = (d // 5) * 5
        doom_buckets[bucket] += 1
    lines.append("| Doom Range | Games | Pct |")
    lines.append("|------------|-------|-----|")
    for b in sorted(doom_buckets.keys()):
        lines.append(f"| {b:2d}-{b+4:2d}      | {doom_buckets[b]:5d} | {doom_buckets[b]/n*100:5.1f}% |")
    lines.append("")

    # Doom breakdown
    lines.append("### Doom Sources")
    lines.append("")
    lines.append(f"- Gate doom avg: {avg([g['gate_doom'] for g in games]):.1f}")
    lines.append(f"- Ritual doom avg: {avg([g['ritual_doom'] for g in games]):.1f}")
    lines.append(f"- ES doom avg: {avg([g['es_doom'] for g in games]):.1f}")
    lines.append("")

    # ── Section 5: Ritual Detail ──
    lines.append("## 5. Ritual Detail")
    lines.append("")
    rit_vals = [g["rituals"] for g in games]
    lines.append(f"- Average rituals: {avg(rit_vals):.1f}")
    lines.append(f"- Median: {med(rit_vals):.1f}")
    lines.append(f"- Min: {min(rit_vals)}, Max: {max(rit_vals)}")
    lines.append(f"- Baseline: {BASELINE['rituals']}")
    lines.append("")
    rit_dist = Counter(rit_vals)
    lines.append("| Rituals | Games | Pct |")
    lines.append("|---------|-------|-----|")
    for k in sorted(rit_dist.keys()):
        lines.append(f"| {k}       | {rit_dist[k]:5d} | {rit_dist[k]/n*100:5.1f}% |")
    lines.append("")

    # ── Section 6: DM Detail ──
    lines.append("## 6. Devil's Mark Detail")
    lines.append("")
    dm_vals = [g["dms"] for g in games]
    lines.append(f"- Average DMs: {avg(dm_vals):.1f}")
    lines.append(f"- Median: {med(dm_vals):.1f}")
    lines.append(f"- Min: {min(dm_vals)}, Max: {max(dm_vals)}")
    lines.append(f"- Baseline: {BASELINE['dms']}")
    lines.append("")
    dm_dist = Counter(dm_vals)
    lines.append("| DMs | Games | Pct |")
    lines.append("|-----|-------|-----|")
    for k in sorted(dm_dist.keys()):
        lines.append(f"| {k}   | {dm_dist[k]:5d} | {dm_dist[k]/n*100:5.1f}% |")
    lines.append("")

    # ── Section 7: Win Rate ──
    lines.append("## 7. Win Rate")
    lines.append("")
    lines.append(f"- FB wins: {len(wins)}/{n} ({win_rate:.1f}%)")
    lines.append("")
    winner_dist = Counter(g["winner"] for g in games if g["winner"])
    lines.append("| Winner | Games | Pct |")
    lines.append("|--------|-------|-----|")
    for w, cnt in winner_dist.most_common():
        lines.append(f"| {w:20s} | {cnt:5d} | {cnt/n*100:5.1f}% |")
    lines.append("")

    # ── Section 8: Tactic Adherence Summary ──
    lines.append("## 8. Tactic Adherence Summary")
    lines.append("")
    # Percentage that build 2nd gate in AP1
    ap1_2g = sum(1 for g in games if g["milestone_ap1_2g"])
    ap2_ghato = sum(1 for g in games if g["milestone_ap2_ghato_2g"])
    ap3_3g_rit = sum(1 for g in games if g["milestone_ap3_3g_rit"])
    ap4_3g_rit = sum(1 for g in games if g["milestone_ap4_3g_rit"])
    full_path = sum(1 for g in games if g["full_path"])

    lines.append("| Milestone | Games | Pct |")
    lines.append("|-----------|-------|-----|")
    lines.append(f"| AP1: 2+ gates          | {ap1_2g:5d} | {ap1_2g/n*100:5.1f}% |")
    lines.append(f"| AP2: Ghato + 2+ gates  | {ap2_ghato:5d} | {ap2_ghato/n*100:5.1f}% |")
    lines.append(f"| AP3: 3+ gates + ritual | {ap3_3g_rit:5d} | {ap3_3g_rit/n*100:5.1f}% |")
    lines.append(f"| AP4: 3+ gates + ritual | {ap4_3g_rit:5d} | {ap4_3g_rit/n*100:5.1f}% |")
    lines.append(f"| Full standard path     | {full_path:5d} | {full_path/n*100:5.1f}% |")
    lines.append("")

    # ── Section 9: Standard Path Analysis ──
    lines.append("## 9. Standard Path Analysis")
    lines.append("")
    lines.append("Standard path: AP1:2g -> AP2:ghato+2g -> AP3:3g+rit -> AP4:3g+rit")
    lines.append("")

    # First divergence point
    diverge = Counter()
    for g in games:
        if g["full_path"]:
            diverge["Full path"] += 1
        elif not g["milestone_ap1_2g"]:
            diverge["Failed at AP1 (< 2 gates)"] += 1
        elif not g["milestone_ap2_ghato_2g"]:
            diverge["Failed at AP2 (no ghato or < 2 gates)"] += 1
        elif not g["milestone_ap3_3g_rit"]:
            diverge["Failed at AP3 (< 3 gates or no ritual)"] += 1
        else:
            diverge["Failed at AP4 (< 3 gates or no ritual)"] += 1

    lines.append("### First Divergence Point")
    lines.append("")
    lines.append("| Divergence Point | Games | Pct |")
    lines.append("|------------------|-------|-----|")
    for label, cnt in diverge.most_common():
        lines.append(f"| {label:40s} | {cnt:5d} | {cnt/n*100:5.1f}% |")
    lines.append("")

    # ── Section 10: Game Length Distribution ──
    lines.append("## 10. Game Length Distribution")
    lines.append("")
    ap_vals = [g["aps"] for g in games]
    lines.append(f"- Average APs/game: {avg(ap_vals):.1f}")
    lines.append(f"- Median: {med(ap_vals):.1f}")
    lines.append("")
    ap_dist = Counter(ap_vals)
    lines.append("| APs | Games | Pct |")
    lines.append("|----|-------|-----|")
    for k in sorted(ap_dist.keys()):
        lines.append(f"| {k:3d} | {ap_dist[k]:5d} | {ap_dist[k]/n*100:5.1f}% |")
    lines.append("")

    # ── Section 11: Writhe-Kill Count and Awaken Distribution ──
    lines.append("## 11. Writhe-Kill Count and Awaken Distribution")
    lines.append("")
    wk_vals = [g["writhe_kills"] for g in games]
    lines.append(f"- Avg writhe kills: {avg(wk_vals):.1f}")
    lines.append(f"- Median: {med(wk_vals):.1f}")
    lines.append(f"- Min: {min(wk_vals)}, Max: {max(wk_vals)}")
    if avg([g['writhes'] for g in games]) > 0:
        lines.append(f"- Kills per Writhe: {avg(wk_vals)/avg([g['writhes'] for g in games]):.2f}")
    lines.append("")

    # Writhe kills distribution
    wk_buckets = Counter()
    for v in wk_vals:
        bucket = (v // 2) * 2
        wk_buckets[bucket] += 1
    lines.append("| Kills Range | Games | Pct |")
    lines.append("|-------------|-------|-----|")
    for b in sorted(wk_buckets.keys()):
        lines.append(f"| {b:2d}-{b+1:2d}        | {wk_buckets[b]:5d} | {wk_buckets[b]/n*100:5.1f}% |")
    lines.append("")

    # ── Section 12: Win vs Loss Comparison ──
    lines.append("## 12. Win vs Loss Comparison")
    lines.append("")
    if wins and losses:
        metrics = [
            ("Total Doom", "total_doom"),
            ("ES Doom", "es_doom"),
            ("SBs", "sbs_count"),
            ("Awakens", "awakens_count"),
            ("Writhe Kills", "writhe_kills"),
            ("Rituals", "rituals"),
            ("Gates Peak", "gates_peak"),
            ("Gates End", "gates_end"),
            ("Writhes", "writhes"),
            ("Builds", "builds"),
            ("Moves", "moves"),
            ("DMs", "dms"),
            ("APs", "aps"),
        ]
        lines.append("| Metric | Wins Avg | Losses Avg | Delta |")
        lines.append("|--------|----------|------------|-------|")
        for label, key in metrics:
            w_avg = avg([g[key] for g in wins])
            l_avg = avg([g[key] for g in losses])
            lines.append(f"| {label:15s} | {w_avg:8.1f} | {l_avg:10.1f} | {w_avg-l_avg:+6.1f} |")
        lines.append("")

        # Gates by AP for wins vs losses
        lines.append("### Gates by AP: Wins vs Losses")
        lines.append("")
        lines.append("| AP | Wins Avg Gates | Losses Avg Gates |")
        lines.append("|----|---------------|-----------------|")
        for ap_idx in range(min(max_aps, 8)):
            w_vals = [g["gates_by_ap"][ap_idx] for g in wins if len(g["gates_by_ap"]) > ap_idx]
            l_vals = [g["gates_by_ap"][ap_idx] for g in losses if len(g["gates_by_ap"]) > ap_idx]
            if w_vals or l_vals:
                lines.append(f"| AP{ap_idx+1} | {avg(w_vals):13.2f} | {avg(l_vals):15.2f} |")
        lines.append("")
    else:
        lines.append("Not enough data (need both wins and losses).")
        lines.append("")

    # ── Section 13: Win Rate by Game Length ──
    lines.append("## 13. Win Rate by Game Length")
    lines.append("")
    # Group: 4-5, 5-6, 7, 8+
    length_groups = [
        ("APs 4-5", lambda g: g["aps"] in (4, 5)),
        ("APs 5-6", lambda g: g["aps"] in (5, 6)),
        ("APs 7", lambda g: g["aps"] == 7),
        ("APs 8+", lambda g: g["aps"] >= 8),
    ]
    lines.append("| Length | Games | Wins | Win Rate |")
    lines.append("|--------|-------|------|----------|")
    for label, filt in length_groups:
        group = [g for g in games if filt(g)]
        if group:
            group_wins = sum(1 for g in group if g["fb_won"])
            lines.append(f"| {label:10s} | {len(group):5d} | {group_wins:4d} | {group_wins/len(group)*100:6.1f}%  |")
    lines.append("")

    # ── Section 14: Win Rate by Awaken Count ──
    lines.append("## 14. Win Rate by Awaken Count")
    lines.append("")
    lines.append("| Awakens | Games | Wins | Win Rate |")
    lines.append("|---------|-------|------|----------|")
    for aw in sorted(set(g["awakens_count"] for g in games)):
        group = [g for g in games if g["awakens_count"] == aw]
        group_wins = sum(1 for g in group if g["fb_won"])
        lines.append(f"| {aw}       | {len(group):5d} | {group_wins:4d} | {group_wins/len(group)*100:6.1f}%  |")
    lines.append("")

    # ── Section: Full-Path Games Doom+ES ──
    lines.append("## Full-Path Games Analysis")
    lines.append("")
    fp_games = [g for g in games if g["full_path"]]
    if fp_games:
        lines.append(f"- Full-path games: {len(fp_games)}/{n} ({len(fp_games)/n*100:.1f}%)")
        lines.append(f"- Avg doom (full-path): {avg([g['total_doom'] for g in fp_games]):.1f}")
        lines.append(f"- Avg doom (all games): {avg(doom_vals):.1f}")
        fp_wins = sum(1 for g in fp_games if g["fb_won"])
        lines.append(f"- Full-path win rate: {fp_wins}/{len(fp_games)} ({fp_wins/len(fp_games)*100:.1f}%)")

        t13 = sum(1 for g in fp_games if g["total_doom"] >= 30)
        t14 = sum(1 for g in fp_games if g["total_doom"] >= 35)
        lines.append(f"- T13 (doom >= 30): {t13}/{len(fp_games)} ({t13/len(fp_games)*100:.1f}%)")
        lines.append(f"- T14 (doom >= 35): {t14}/{len(fp_games)} ({t14/len(fp_games)*100:.1f}%)")
    else:
        lines.append("No full-path games found.")
    lines.append("")

    # ── Overall Assessment ──
    lines.append("## Overall Assessment")
    lines.append("")
    lines.append("| Metric | Bot | Baseline |")
    lines.append("|--------|-----|----------|")
    lines.append(f"| Gates peak    | {avg([g['gates_peak'] for g in games]):.1f} | 3    |")
    lines.append(f"| Gates end     | {avg([g['gates_end'] for g in games]):.1f} | 3    |")
    lines.append(f"| SBs earned    | {avg([g['sbs_count'] for g in games]):.1f} | {BASELINE['sbs']}    |")
    lines.append(f"| Awakens       | {avg([g['awakens_count'] for g in games]):.1f} | {BASELINE['awakens']}    |")
    lines.append(f"| Rituals total | {avg([g['rituals'] for g in games]):.1f} | {BASELINE['rituals']}  |")
    lines.append(f"| Total doom    | {avg(doom_vals):.1f} | {BASELINE['doom']}   |")
    lines.append(f"| ES total      | {avg([g['es_total'] for g in games]):.1f} | {BASELINE['es']}    |")
    lines.append(f"| DMs           | {avg(dm_vals):.1f} | {BASELINE['dms']}    |")
    lines.append(f"| Win rate      | {win_rate:.1f}% | ~25% |")
    lines.append("")

    # Starting region distribution
    start_regions = Counter(g["start_region"] for g in games if g["start_region"])
    lines.append("### Starting Region Distribution")
    lines.append("")
    lines.append("| Region | Games | Pct |")
    lines.append("|--------|-------|-----|")
    for r, cnt in start_regions.most_common():
        lines.append(f"| {r:20s} | {cnt:5d} | {cnt/n*100:5.1f}% |")
    lines.append("")

    # SB earning order distribution
    lines.append("### Spellbook Distribution")
    lines.append("")
    sb_counts = Counter()
    for g in games:
        for sb in g["sbs_list"]:
            sb_counts[sb] += 1
    lines.append("| Spellbook | Games with it | Pct |")
    lines.append("|-----------|--------------|-----|")
    for sb, cnt in sb_counts.most_common():
        lines.append(f"| {sb:25s} | {cnt:12d} | {cnt/n*100:5.1f}% |")
    lines.append("")

    return "\n".join(lines)


def pick_replays(games):
    """Pick 5 representative games for replay generation."""
    picks = {}

    # 1. Win game (prefer mid-doom win)
    win_games = sorted([g for g in games if g["fb_won"]], key=lambda g: abs(g["total_doom"] - 30))
    if win_games:
        picks["final-win"] = win_games[0]

    # 2. Close loss (doom 25-30, not win)
    close_losses = sorted([g for g in games if not g["fb_won"] and 25 <= g["total_doom"] <= 30],
                          key=lambda g: -g["total_doom"])
    if close_losses:
        picks["final-close"] = close_losses[0]
    else:
        # Fallback: highest doom loss
        all_losses = sorted([g for g in games if not g["fb_won"]], key=lambda g: -g["total_doom"])
        if all_losses:
            picks["final-close"] = all_losses[0]

    # 3. Full-path game
    fp_games = [g for g in games if g["full_path"]]
    if fp_games:
        # Prefer one that's not already picked
        picked_files = {p["file"] for p in picks.values()}
        fp_remaining = [g for g in fp_games if g["file"] not in picked_files]
        if fp_remaining:
            picks["final-fullpath"] = fp_remaining[0]
        elif fp_games:
            picks["final-fullpath"] = fp_games[0]

    # 4. Ghato but no 3g game
    no3g = [g for g in games if g["ghato_no_3g"]]
    if no3g:
        picked_files = {p["file"] for p in picks.values()}
        no3g_remaining = [g for g in no3g if g["file"] not in picked_files]
        if no3g_remaining:
            picks["final-no3g"] = no3g_remaining[0]
        elif no3g:
            picks["final-no3g"] = no3g[0]
    else:
        # Fallback: game where max gates < 3
        low_gate = [g for g in games if g["gates_peak"] < 3]
        if low_gate:
            picked_files = {p["file"] for p in picks.values()}
            remaining = [g for g in low_gate if g["file"] not in picked_files]
            picks["final-no3g"] = remaining[0] if remaining else low_gate[0]

    # 5. Low doom game (doom < 12)
    low_doom = sorted([g for g in games if g["total_doom"] < 12], key=lambda g: g["total_doom"])
    if low_doom:
        picked_files = {p["file"] for p in picks.values()}
        remaining = [g for g in low_doom if g["file"] not in picked_files]
        if remaining:
            picks["final-low"] = remaining[0]
        elif low_doom:
            picks["final-low"] = low_doom[0]
    else:
        # Fallback: lowest doom game
        lowest = sorted(games, key=lambda g: g["total_doom"])
        picked_files = {p["file"] for p in picks.values()}
        remaining = [g for g in lowest if g["file"] not in picked_files]
        if remaining:
            picks["final-low"] = remaining[0]

    return picks


def main():
    # Find game log files — optionally filter by timestamp prefix
    ts_prefix = sys.argv[1] if len(sys.argv) > 1 else ""
    if ts_prefix:
        pattern = os.path.join(LOG_DIR, f"fb-*-{ts_prefix}*.txt")
    else:
        pattern = os.path.join(LOG_DIR, "fb-*.txt")
    files = sorted(glob.glob(pattern))
    print(f"Found {len(files)} game log files")

    # Parse all games
    games = []
    errors = []
    for i, f in enumerate(files):
        try:
            g = parse_game(f)
            games.append(g)
        except Exception as e:
            errors.append((f, str(e)))
        if (i + 1) % 50 == 0:
            print(f"  Parsed {i+1}/{len(files)}...")

    print(f"Successfully parsed {len(games)} games, {len(errors)} errors")
    if errors:
        for f, e in errors[:5]:
            print(f"  ERROR: {os.path.basename(f)}: {e}")

    # Generate stats
    md_content = format_stats(games)

    # Write output
    os.makedirs(os.path.dirname(OUTPUT_MD), exist_ok=True)
    with open(OUTPUT_MD, 'w') as f:
        f.write(md_content)
    print(f"Wrote stats to {OUTPUT_MD}")

    # Pick replay candidates
    picks = pick_replays(games)
    print(f"\n=== REPLAY CANDIDATES ===")
    for name, g in picks.items():
        print(f"{name}: {g['file']} (doom={g['total_doom']}, won={g['fb_won']}, "
              f"gates_peak={g['gates_peak']}, aps={g['aps']}, awakens={g['awakens_count']}, "
              f"full_path={g['full_path']})")

    # Write picks to JSON for replay generation
    picks_json = {name: {"file": g["file"], "doom": g["total_doom"], "won": g["fb_won"]}
                  for name, g in picks.items()}
    picks_path = os.path.join(LOG_DIR, "replay_picks.json")
    with open(picks_path, 'w') as f:
        json.dump(picks_json, f, indent=2)
    print(f"Wrote picks to {picks_path}")

    return picks


if __name__ == "__main__":
    picks = main()
