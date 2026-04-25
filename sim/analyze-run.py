#!/usr/bin/env python3
"""Analyze a batch of FB bot game logs and produce aggregate statistics.

Usage: python3 analyze-run.py <log-dir> [prefix]
"""

import os, sys, re, glob, statistics
from collections import Counter

log_dir = sys.argv[1] if len(sys.argv) > 1 else "win-logs"
prefix = sys.argv[2] if len(sys.argv) > 2 else "fb-game-"

files = sorted(glob.glob(os.path.join(log_dir, f"{prefix}*.txt")))
print(f"Found {len(files)} game logs in {log_dir}", file=sys.stderr)

def strip_html(s):
    return re.sub(r'<[^>]+>', '', s)

def avg(vals):
    return sum(vals) / len(vals) if vals else 0

def med(vals):
    return statistics.median(vals) if vals else 0

# Per-game stats
all_stats = []

for fpath in files:
    with open(fpath) as f:
        content = f.read()

    lines = content.split('\n')
    fname = os.path.basename(fpath)

    # Separate action strings from HTML log lines
    action_lines = []
    html_lines = []
    for line in lines:
        line = line.strip()
        if not line:
            continue
        if line.startswith("<div"):
            html_lines.append(line)
        else:
            action_lines.append(line)

    stats = {'file': fname}

    # --- Action counts from action strings ---
    stats['writhes'] = sum(1 for l in action_lines if l.startswith('FBWritheMainAction(FB'))
    stats['moves'] = sum(1 for l in action_lines if l.startswith('MoveAction(FB'))
    stats['recruits'] = sum(1 for l in action_lines if l.startswith('RecruitAction(FB'))
    stats['summons'] = sum(1 for l in action_lines if l.startswith('SummonAction(FB'))
    stats['builds'] = sum(1 for l in action_lines if l.startswith('BuildGateAction(FB'))
    stats['awakens'] = sum(1 for l in action_lines if l.startswith('FBAwakenGhatanothoaAction(FB'))
    stats['rituals'] = sum(1 for l in action_lines if l.startswith('RitualAction(FB'))
    stats['attacks'] = sum(1 for l in action_lines if l.startswith('AttackAction(FB'))
    stats['cotf'] = sum(1 for l in action_lines if l.startswith('FBCallOfTheFaithfulAction(FB'))
    # DMs = crater placements, NOT FBDevilsMarkDoomAction (which fires per doom point)
    stats['dms'] = sum(1 for l in action_lines if l.startswith('FBDevilsMarkPlaceCraterAction(FB'))

    # Enemy captures of FB units
    stats['fb_captured'] = 0
    for l in action_lines:
        m = re.match(r'CaptureAction\((\w+),.*FB/', l)
        if m and m.group(1) != 'FB':
            stats['fb_captured'] += 1
    # Also count CaptureTargetAction targeting FB
    for l in action_lines:
        if l.startswith('CaptureTargetAction(') and 'FB/' in l and not l.startswith('CaptureTargetAction(FB'):
            stats['fb_captured'] += 1

    # --- Per-AP tracking using PowerGatherAction as AP boundary ---
    current_ap = 1
    ap_actions = {i: {'writhes': 0, 'moves': 0, 'builds': 0, 'recruits': 0, 'summons': 0, 'attacks': 0} for i in range(1, 8)}

    # Detect placement done (StartingRegionAction is placement, first PreMainAction is AP1)
    placement_done = False
    for l in action_lines:
        if l.startswith('PowerGatherAction('):
            current_ap += 1
            continue
        if not placement_done:
            if l.startswith('PreMainAction(') or l.startswith('MainAction('):
                placement_done = True
            else:
                continue

        ap = min(current_ap, 7)
        if l.startswith('FBWritheMainAction(FB'):
            ap_actions[ap]['writhes'] += 1
        elif l.startswith('MoveAction(FB'):
            ap_actions[ap]['moves'] += 1
        elif l.startswith('BuildGateAction(FB'):
            ap_actions[ap]['builds'] += 1
        elif l.startswith('RecruitAction(FB'):
            ap_actions[ap]['recruits'] += 1
        elif l.startswith('SummonAction(FB'):
            ap_actions[ap]['summons'] += 1
        elif l.startswith('AttackAction(FB'):
            ap_actions[ap]['attacks'] += 1

    stats['ap_actions'] = ap_actions
    stats['num_aps'] = current_ap - 1  # number of completed APs

    # --- From HTML lines ---
    plain_lines = [strip_html(h) for h in html_lines]

    # Winner - check for "Firstborn" in won lines
    stats['fb_won'] = False
    stats['winner'] = "unknown"
    for pl in reversed(plain_lines):
        if 'won' in pl:
            stats['winner'] = pl.strip()
            if 'Firstborn' in pl:
                stats['fb_won'] = True
            break

    # FB final doom from "revealed" line (ES doom at game end)
    fb_es_doom = 0
    fb_es_count = 0
    for pl in plain_lines:
        m = re.search(r'Firstborn revealed.*for (\d+) Doom', pl)
        if m:
            fb_es_doom = int(m.group(1))
            fb_es_count = len(re.findall(r'\[(\d+)\]', pl))

    # Gate doom from "got X Doom" lines
    fb_gate_doom = 0
    for pl in plain_lines:
        m = re.search(r'Firstborn got (\d+) Doom', pl)
        if m:
            fb_gate_doom += int(m.group(1))

    # Ritual doom from "performed the ritual" lines
    fb_ritual_doom = 0
    fb_ritual_es = 0
    for pl in plain_lines:
        m = re.search(r'Firstborn performed the ritual for (\d+) Power and gained (\d+) Doom', pl)
        if m:
            fb_ritual_doom += int(m.group(2))
            if 'Elder Sign' in pl:
                es_m = re.search(r'(\d+) Elder Sign', pl)
                fb_ritual_es += int(es_m.group(1)) if es_m else 1

    stats['doom_total'] = fb_gate_doom + fb_ritual_doom + fb_es_doom
    stats['doom_gates'] = fb_gate_doom
    stats['doom_rituals'] = fb_ritual_doom
    stats['doom_es'] = fb_es_doom
    stats['es_count'] = fb_es_count
    stats['ritual_es'] = fb_ritual_es

    # Ritual gates from RitualAction(FB, cost, gates)
    ritual_gates = []
    for l in action_lines:
        m = re.match(r'RitualAction\(FB, (\d+), (\d+)\)', l)
        if m:
            ritual_gates.append(int(m.group(2)))
    stats['ritual_gates'] = ritual_gates

    # SBs
    sb_names = []
    for l in action_lines:
        m = re.match(r'SpellbookAction\(FB, (\w+),', l)
        if m:
            sb_names.append(m.group(1))
    stats['sb_names'] = sb_names
    stats['sbs'] = len(sb_names)

    # Gates lost to DM (crater destroying gate)
    stats['gates_lost_to_dm'] = sum(1 for pl in plain_lines
        if 'Gate in' in pl and 'destroyed by' in pl and 'Crater' in pl and 'Firstborn' in pl)

    # Placement region
    for l in action_lines:
        m = re.match(r'StartingRegionAction\(FB, (\w+)\)', l)
        if m:
            stats['placement'] = m.group(1)
            break

    all_stats.append(stats)

# --- Aggregate Statistics ---
n = len(all_stats)

# === OUTPUT ===
print(f"# FB Bot Run 10 — {n} Games — 2026-04-22\n")

# Action Comparison Table
print("## Action Comparison Table\n")
print(f"| {'Action':<35} | {'Bot Avg':>8} | {'Bot Med':>8} | {'Baseline':>8} | {'Delta':>8} |")
print(f"|{'-'*37}|{'-'*10}|{'-'*10}|{'-'*10}|{'-'*10}|")
metrics = [
    ('Writhes', 'writhes', 7.2),
    ('Moves', 'moves', 0.8),
    ('Recruits', 'recruits', 3.2),
    ('Summons', 'summons', 2.3),
    ('Builds', 'builds', 1.9),
    ('Enemy captures of FB cultists', 'fb_captured', 1.0),
    ('Gates lost (DM)', 'gates_lost_to_dm', 0),
    ('Attacks', 'attacks', None),
    ('Call of the Faithful', 'cotf', None),
]
for name, key, baseline in metrics:
    vals = [s[key] for s in all_stats]
    a = avg(vals)
    m = med(vals)
    if baseline is not None:
        bl = f"{baseline:.1f}"
        delta = f"{a - baseline:+.1f}"
    else:
        bl = "—"
        delta = "—"
    print(f"| {name:<35} | {a:>8.1f} | {m:>8.1f} | {bl:>8} | {delta:>8} |")

# Gates at end of each AP
print(f"\n## Per-AP Action Averages\n")
print(f"| {'AP':<5} | {'Writhes':>8} | {'Moves':>8} | {'Builds':>8} | {'Recruits':>8} | {'Summons':>8} | {'Attacks':>8} |")
print(f"|{'-'*7}|{'-'*10}|{'-'*10}|{'-'*10}|{'-'*10}|{'-'*10}|{'-'*10}|")
for ap in range(1, 7):
    w = [s['ap_actions'][ap]['writhes'] for s in all_stats if ap <= s['num_aps']]
    mv = [s['ap_actions'][ap]['moves'] for s in all_stats if ap <= s['num_aps']]
    b = [s['ap_actions'][ap]['builds'] for s in all_stats if ap <= s['num_aps']]
    r = [s['ap_actions'][ap]['recruits'] for s in all_stats if ap <= s['num_aps']]
    sm = [s['ap_actions'][ap]['summons'] for s in all_stats if ap <= s['num_aps']]
    at = [s['ap_actions'][ap]['attacks'] for s in all_stats if ap <= s['num_aps']]
    if w:
        games = len(w)
        print(f"| AP{ap:<3} | {avg(w):>8.1f} | {avg(mv):>8.1f} | {avg(b):>8.1f} | {avg(r):>8.1f} | {avg(sm):>8.1f} | {avg(at):>8.1f} | ({games} games)")

# Writhe/Move chart vs baseline
print(f"\n## Writhe/Move Chart (Bot vs Baseline)\n")
print(f"| {'AP':<5} | {'Bot W':>6} | {'Bot M':>6} | {'Base W':>7} | {'Base M':>7} |")
print(f"|{'-'*7}|{'-'*8}|{'-'*8}|{'-'*9}|{'-'*9}|")
baseline_writhes = {1: 1.3, 2: 0.6, 3: 1.0, 4: 1.7, 5: 2.0, 6: 1.0}
baseline_moves = {1: 0.1, 2: 0.0, 3: 0.0, 4: 0.4, 5: 0.1, 6: 0.0}
for ap in range(1, 7):
    w = [s['ap_actions'][ap]['writhes'] for s in all_stats if ap <= s['num_aps']]
    mv = [s['ap_actions'][ap]['moves'] for s in all_stats if ap <= s['num_aps']]
    if w:
        bw = baseline_writhes.get(ap, 0)
        bm = baseline_moves.get(ap, 0)
        print(f"| AP{ap:<3} | {avg(w):>6.1f} | {avg(mv):>6.1f} | {bw:>7.1f} | {bm:>7.1f} |")
total_w = [s['writhes'] for s in all_stats]
total_m = [s['moves'] for s in all_stats]
print(f"| {'Total':<5} | {avg(total_w):>6.1f} | {avg(total_m):>6.1f} | {'7.6':>7} | {'0.7':>7} |")

# Awaken distribution
print(f"\n## Awaken Distribution\n")
awakens = [s['awakens'] for s in all_stats]
awaken_counts = Counter(awakens)
print(f"Average: {avg(awakens):.1f} (baseline: 3)")
print(f"| {'Awakens':>8} | {'Games':>6} | {'Pct':>6} |")
print(f"|{'-'*10}|{'-'*8}|{'-'*8}|")
for k in sorted(awaken_counts.keys()):
    print(f"| {k:>8} | {awaken_counts[k]:>6} | {100*awaken_counts[k]/n:>5.1f}% |")

# Doom distribution
print(f"\n## Doom Distribution\n")
dooms = [s['doom_total'] for s in all_stats]
print(f"| {'Metric':<20} | {'Value':>8} | {'Baseline':>8} |")
print(f"|{'-'*22}|{'-'*10}|{'-'*10}|")
print(f"| {'Average':<20} | {avg(dooms):>8.1f} | {'35':>8} |")
print(f"| {'Median':<20} | {med(dooms):>8.1f} | {'—':>8} |")
print(f"| {'Min':<20} | {min(dooms):>8} | {'—':>8} |")
print(f"| {'Max':<20} | {max(dooms):>8} | {'—':>8} |")
print(f"| {'Std Dev':<20} | {statistics.stdev(dooms):>8.1f} | {'—':>8} |")

print(f"\nDoom breakdown (avg):")
print(f"| {'Source':<20} | {'Avg':>8} |")
print(f"|{'-'*22}|{'-'*10}|")
print(f"| {'Gate doom':<20} | {avg([s['doom_gates'] for s in all_stats]):>8.1f} |")
print(f"| {'Ritual doom':<20} | {avg([s['doom_rituals'] for s in all_stats]):>8.1f} |")
print(f"| {'ES doom':<20} | {avg([s['doom_es'] for s in all_stats]):>8.1f} |")

print(f"\nDoom histogram:")
print(f"| {'Range':<8} | {'Count':>6} | {'Pct':>6} | {'Bar':<30} |")
print(f"|{'-'*10}|{'-'*8}|{'-'*8}|{'-'*32}|")
buckets = [('0-9', 0, 10), ('10-14', 10, 15), ('15-19', 15, 20), ('20-24', 20, 25),
           ('25-29', 25, 30), ('30-34', 30, 35), ('35+', 35, 999)]
for label, lo, hi in buckets:
    count = sum(1 for d in dooms if lo <= d < hi)
    bar = '#' * count
    print(f"| {label:<8} | {count:>6} | {100*count/n:>5.1f}% | {bar:<30} |")

# Ritual detail
print(f"\n## Ritual Detail\n")
rituals = [s['rituals'] for s in all_stats]
print(f"| {'Metric':<30} | {'Value':>8} | {'Baseline':>8} |")
print(f"|{'-'*32}|{'-'*10}|{'-'*10}|")
print(f"| {'Total rituals':<30} | {sum(rituals):>8} | {'—':>8} |")
print(f"| {'Avg/game':<30} | {avg(rituals):>8.1f} | {'4.5':>8} |")
print(f"| {'Median':<30} | {med(rituals):>8.1f} | {'—':>8} |")
print(f"| {'Games with 0 rituals':<30} | {sum(1 for r in rituals if r == 0):>8} | {'0':>8} |")

print(f"\nRitual count distribution:")
ritual_counts = Counter(rituals)
for k in sorted(ritual_counts.keys()):
    print(f"  {k} rituals: {ritual_counts[k]} games ({100*ritual_counts[k]/n:.1f}%)")

# Rituals with 3+ gates
all_ritual_gates = []
for s in all_stats:
    all_ritual_gates.extend(s['ritual_gates'])
if all_ritual_gates:
    r3plus = sum(1 for g in all_ritual_gates if g >= 3)
    print(f"\nRitual gate counts: {len(all_ritual_gates)} total rituals")
    gate_dist = Counter(all_ritual_gates)
    for k in sorted(gate_dist.keys()):
        print(f"  {k} gates: {gate_dist[k]} rituals ({100*gate_dist[k]/len(all_ritual_gates):.1f}%)")
    print(f"  Rituals with 3+ gates: {r3plus} ({100*r3plus/len(all_ritual_gates):.1f}%)")

# DM detail
print(f"\n## Devil's Mark Detail\n")
dms = [s['dms'] for s in all_stats]
print(f"| {'Metric':<30} | {'Value':>8} | {'Baseline':>8} |")
print(f"|{'-'*32}|{'-'*10}|{'-'*10}|")
print(f"| {'Total DMs':<30} | {sum(dms):>8} | {'—':>8} |")
print(f"| {'Avg/game':<30} | {avg(dms):>8.1f} | {'3':>8} |")
print(f"| {'Median':<30} | {med(dms):>8.1f} | {'—':>8} |")

dm_counts = Counter(dms)
print(f"\nDM distribution:")
for k in sorted(dm_counts.keys()):
    print(f"  {k} DMs: {dm_counts[k]} games ({100*dm_counts[k]/n:.1f}%)")

# Win rate
print(f"\n## Win Rate\n")
fb_wins = sum(1 for s in all_stats if s['fb_won'])
print(f"**FB wins: {fb_wins}/{n} ({100*fb_wins/n:.1f}%)**\n")
print(f"Winner distribution:")
print(f"| {'Faction':<25} | {'Wins':>5} | {'Pct':>6} |")
print(f"|{'-'*27}|{'-'*7}|{'-'*8}|")
winners = Counter()
for s in all_stats:
    w = s['winner']
    for faction in ['Firstborn', 'Great Cthulhu', 'Crawling Chaos', 'Yellow Sign', 'Black Goat',
                     'Opener of the Way', 'Sleeper', 'Windwalker', 'The Ancients', 'Tombstalker', 'Humanity']:
        if faction in w:
            winners[faction] += 1
for faction, count in winners.most_common():
    print(f"| {faction:<25} | {count:>5} | {100*count/n:>5.1f}% |")

# SBs
print(f"\n## Spellbooks\n")
sbs = [s['sbs'] for s in all_stats]
print(f"Avg SBs: {avg(sbs):.1f} (baseline: 6)")
sb_counts = Counter(sbs)
for k in sorted(sb_counts.keys()):
    print(f"  {k} SBs: {sb_counts[k]} games ({100*sb_counts[k]/n:.1f}%)")

all_sb_names = Counter()
for s in all_stats:
    for sb in s['sb_names']:
        all_sb_names[sb] += 1
print(f"\nSB acquisition frequency:")
print(f"| {'Spellbook':<25} | {'Games':>6} | {'Pct':>6} |")
print(f"|{'-'*27}|{'-'*8}|{'-'*8}|")
for sb, count in all_sb_names.most_common():
    print(f"| {sb:<25} | {count:>6} | {100*count/n:>5.1f}% |")

# ES
print(f"\n## Elder Signs\n")
es = [s['es_count'] for s in all_stats]
print(f"Avg ES: {avg(es):.1f} (baseline: 7)")
es_counts = Counter(es)
for k in sorted(es_counts.keys()):
    print(f"  {k} ES: {es_counts[k]} games ({100*es_counts[k]/n:.1f}%)")

# Tactic adherence
print(f"\n## Tactic Adherence Summary\n")
print(f"| {'Tactic':<40} | {'Adherence':>10} | {'Notes':<30} |")
print(f"|{'-'*42}|{'-'*12}|{'-'*32}|")

tests = [
    ('Writhe-heavy (5+/game)', lambda s: s['writhes'] >= 5, 'Baseline: 7.2 avg'),
    ('Low moves (<=2/game)', lambda s: s['moves'] <= 2, 'Baseline: 0.8 avg'),
    ('Builds 2+ gates', lambda s: s['builds'] >= 2, 'Baseline: 1.9 avg'),
    ('Awakens Ghatanothoa', lambda s: s['awakens'] >= 1, 'Required'),
    ('3+ rituals', lambda s: s['rituals'] >= 3, 'Baseline: 4-5'),
    ('Rituals on 3+ gates', lambda s: any(g >= 3 for g in s['ritual_gates']), 'Critical for doom'),
    ('3+ Devil Marks', lambda s: s['dms'] >= 3, 'Baseline: 3'),
    ('6 spellbooks', lambda s: s['sbs'] >= 6, 'All SBs acquired'),
    ('Doom 25+', lambda s: s['doom_total'] >= 25, 'Competitive threshold'),
    ('Doom 30+', lambda s: s['doom_total'] >= 30, 'Win threshold'),
]
for name, test, notes in tests:
    count = sum(1 for s in all_stats if test(s))
    print(f"| {name:<40} | {100*count/n:>9.1f}% | {notes:<30} |")

# Placement
print(f"\n## Placement Distribution\n")
placements = Counter(s.get('placement', '?') for s in all_stats)
for region, count in placements.most_common():
    print(f"  {region}: {count} ({100*count/n:.1f}%)")

# Num APs
print(f"\n## Game Length (APs)\n")
aps = [s['num_aps'] for s in all_stats]
ap_counts = Counter(aps)
print(f"Avg APs: {avg(aps):.1f}")
for k in sorted(ap_counts.keys()):
    print(f"  {k} APs: {ap_counts[k]} games ({100*ap_counts[k]/n:.1f}%)")

# Replay candidates
print(f"\n## Replay Candidates\n")
sorted_by_doom = sorted(all_stats, key=lambda s: s['doom_total'])

low = [s for s in sorted_by_doom if s['doom_total'] < 12]
pick_low = low[len(low)//2] if low else sorted_by_doom[0]
print(f"Low doom (<12):    {pick_low['file']} — doom {pick_low['doom_total']}")

mid = [s for s in sorted_by_doom if 18 <= s['doom_total'] <= 22]
pick_mid = mid[len(mid)//2] if mid else min(all_stats, key=lambda s: abs(s['doom_total'] - 20))
print(f"Mid doom (18-22):  {pick_mid['file']} — doom {pick_mid['doom_total']}")

high = [s for s in sorted_by_doom if 27 <= s['doom_total'] <= 30]
pick_high = high[len(high)//2] if high else min(all_stats, key=lambda s: abs(s['doom_total'] - 28))
print(f"High doom (27-30): {pick_high['file']} — doom {pick_high['doom_total']}")

wins = [s for s in all_stats if s['fb_won']]
if wins:
    pick_win = wins[0]
    print(f"FB Win:            {pick_win['file']} — doom {pick_win['doom_total']}")
else:
    # Pick highest doom as "best" game
    pick_best = sorted_by_doom[-1]
    print(f"FB Win:            NONE — best game: {pick_best['file']} — doom {pick_best['doom_total']}")

print(f"\nDone. Processed {n} games.", file=sys.stderr)
