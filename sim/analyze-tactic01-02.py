#!/usr/bin/env python3
"""Analyze Tactic 01+02: Ghato writhe to enemy gate → handle gate state.
Only counts writhes in the AP AFTER awakening (not same AP)."""

import os, re, glob, sys, time
from collections import defaultdict

LOG_DIR = os.path.join(os.path.dirname(__file__), "win-logs")

cutoff = time.time() - 300
files = sorted(
    [f for f in glob.glob(os.path.join(LOG_DIR, "fb-*.txt"))
     if os.path.getmtime(f) > cutoff],
    key=os.path.getmtime
)

print(f"Found {len(files)} recent game logs")

WRITHE_USE = re.compile(r'Firstborn used Writhe rolling (\d+) dice')
WRITHE_RELOCATE = re.compile(r"Firstborn Writhe: relocated ([\w' -]+) from ([\w ]+) to ([\w ]+)")
AWAKEN_GHATO = re.compile(r'Firstborn awakened Ghatanothoa')
FB_CAPTURE = re.compile(r'Firstborn captured')
FB_COF = re.compile(r'Firstborn Call of the Faithful')
FB_RECRUIT = re.compile(r'Firstborn recruited (\w+) in ([\w ]+)')
FB_BATTLE = re.compile(r'Firstborn (?:attacked|battled)')
FB_SUMMON = re.compile(r'Firstborn summoned ([\w\' -]+) in ([\w ]+)')
FB_BUILD = re.compile(r'Firstborn built a gate')
FB_GAIN_GATE = re.compile(r'Firstborn gained control of the gate in ([\w ]+)')
FB_RAN_OUT = re.compile(r'Firstborn ran out of power')
FB_NO_POWER = re.compile(r'Firstborn had no power')
TURN = re.compile(r'^Turn (\d+)$')
POWER_GATHER = re.compile(r'POWER GATHER')
GATE_BUILT = re.compile(r'([\w ]+) built a gate in ([\w ]+)')
GATE_GAINED = re.compile(r'([\w ]+) gained control of the gate in ([\w ]+)')
GATE_CHANGED = re.compile(r'([\w ]+) changed control of the gate in ([\w ]+)')
GATE_LOST = re.compile(r'([\w ]+) lost control of the gate in ([\w ]+)')

total = 0
excluded = 0
writhed_ghato = 0
ghato_not_pained = 0
ghato_to_enemy_gate = 0
ghato_to_empty_land = 0
ghato_to_own = 0
ghato_to_other = 0
next_actions = defaultdict(int)
gate_gained_count = 0

for fpath in files:
    total += 1
    with open(fpath, 'r') as f:
        content = f.read()

    lines_raw = re.findall(r"<div class='p'>(.*?)</div>", content)
    lines = [re.sub(r'<[^>]+>', '', l).strip() for l in lines_raw]

    # Find Ghato awakening
    awaken_idx = None
    for i, line in enumerate(lines):
        if AWAKEN_GHATO.search(line):
            awaken_idx = i
            break

    if awaken_idx is None:
        excluded += 1
        continue

    # Find the NEXT AP boundary after awakening (POWER GATHER or Turn N)
    next_ap_start = None
    for i in range(awaken_idx + 1, len(lines)):
        if POWER_GATHER.search(lines[i]):
            next_ap_start = i
            break

    if next_ap_start is None:
        ghato_not_pained += 1
        continue

    # Build gate state at the time of the NEXT AP
    gates_at_writhe = {}
    for i, line in enumerate(lines):
        if i > next_ap_start + 30:
            break
        m = GATE_BUILT.search(line)
        if m: gates_at_writhe[m.group(2)] = m.group(1)
        m = GATE_GAINED.search(line)
        if m: gates_at_writhe[m.group(2)] = m.group(1)
        m = GATE_CHANGED.search(line)
        if m: gates_at_writhe[m.group(2)] = m.group(1)
        m = GATE_LOST.search(line)
        if m and gates_at_writhe.get(m.group(2)) == m.group(1):
            gates_at_writhe[m.group(2)] = None

    # Find first writhe with Ghato relocation AFTER the next AP starts
    ghato_dest = None
    writhe_start = None
    for i in range(next_ap_start, len(lines)):
        if WRITHE_USE.search(lines[i]):
            for j in range(i + 1, min(i + 30, len(lines))):
                m = WRITHE_RELOCATE.search(lines[j])
                if m and 'Ghatanothoa' in m.group(1):
                    ghato_dest = m.group(3)
                    writhe_start = i
                    break
                if WRITHE_USE.search(lines[j]) or TURN.match(lines[j]) or POWER_GATHER.search(lines[j]):
                    break
            if ghato_dest:
                break
        # Don't search past the NEXT power gather (2 APs ahead)
        if i > next_ap_start + 5 and POWER_GATHER.search(lines[i]):
            break

    if ghato_dest is None:
        ghato_not_pained += 1
        continue

    writhed_ghato += 1

    # Update gate state to time of actual writhe (covers changes during this AP)
    for i in range(next_ap_start, writhe_start):
        line = lines[i]
        m = GATE_BUILT.search(line)
        if m: gates_at_writhe[m.group(2)] = m.group(1)
        m = GATE_GAINED.search(line)
        if m: gates_at_writhe[m.group(2)] = m.group(1)
        m = GATE_CHANGED.search(line)
        if m: gates_at_writhe[m.group(2)] = m.group(1)
        m = GATE_LOST.search(line)
        if m and gates_at_writhe.get(m.group(2)) == m.group(1):
            gates_at_writhe[m.group(2)] = None

    # Classify destination
    dest_owner = gates_at_writhe.get(ghato_dest)
    if dest_owner and dest_owner != 'Firstborn':
        ghato_to_enemy_gate += 1
        is_enemy_gate = True
    elif dest_owner == 'Firstborn':
        ghato_to_own += 1
        is_enemy_gate = False
    elif ghato_dest in gates_at_writhe:
        ghato_to_other += 1
        is_enemy_gate = False
    else:
        ghato_to_empty_land += 1
        is_enemy_gate = False

    if not is_enemy_gate:
        continue

    # Find writhe end
    writhe_end = writhe_start + 1
    for i in range(writhe_start + 1, min(writhe_start + 50, len(lines))):
        line = lines[i]
        if 'Writhe:' in line and 'Firstborn' in line: writhe_end = i + 1; continue
        if 'rolled' in line and 'Firstborn' in line: writhe_end = i + 1; continue
        if 'rerolled' in line and 'Firstborn' in line: writhe_end = i + 1; continue
        if ('lost control' in line or 'gained control' in line) and 'gate' in line: writhe_end = i + 1; continue
        break

    # Find FB's NEXT action
    fb_next = None
    gate_gained = False
    for i in range(writhe_end, min(writhe_end + 150, len(lines))):
        line = lines[i]

        m = FB_GAIN_GATE.search(line)
        if m and m.group(1) == ghato_dest:
            gate_gained = True

        if POWER_GATHER.search(line):
            fb_next = 'turn_ended'
            break

        if 'Firstborn' not in line:
            continue

        if FB_RAN_OUT.search(line) or FB_NO_POWER.search(line):
            fb_next = 'no_power'
            break
        elif FB_CAPTURE.search(line):
            fb_next = 'capture'
            break
        elif FB_COF.search(line):
            fb_next = 'cof'
            break
        elif FB_RECRUIT.search(line):
            fb_next = 'recruit'
            break
        elif FB_BATTLE.search(line):
            fb_next = 'battle'
            break
        elif WRITHE_USE.search(line):
            # Check kill vs move
            is_kill = False
            for j in range(i + 1, min(i + 30, len(lines))):
                if re.search(r'Firstborn Writhe: eliminated', lines[j]): is_kill = True; break
                if re.search(r'Firstborn Writhe: .* replaced', lines[j]): is_kill = True; break
                if WRITHE_USE.search(lines[j]) or TURN.match(lines[j]): break
            fb_next = 'writhe-kill' if is_kill else 'writhe-move'
            break
        elif FB_SUMMON.search(line):
            fb_next = 'summon'
            break
        elif FB_BUILD.search(line):
            fb_next = 'build_gate'
            break

    if fb_next is None:
        fb_next = 'unknown'

    next_actions[fb_next] += 1
    if gate_gained:
        gate_gained_count += 1

# Print decision tree
included = total - excluded
print(f"""
Decision Tree: Tactic 01+02 ({total} games, next-AP only)
{'='*60}

{total} games
├── Excluded (no Ghato awaken): {excluded}
└── Included: {included}
    ├── Ghato not writhed in next AP: {ghato_not_pained}
    ���── Ghato writhed in next AP: {writhed_ghato}
        ├── To enemy gate: {ghato_to_enemy_gate} ({ghato_to_enemy_gate*100//max(writhed_ghato,1)}%)
        ├── To empty land: {ghato_to_empty_land} ({ghato_to_empty_land*100//max(writhed_ghato,1)}%)
        ├── To own gate: {ghato_to_own}
        └── To other/abandoned: {ghato_to_other}
""")

if ghato_to_enemy_gate > 0:
    print(f"After Ghato arrives at enemy gate ({ghato_to_enemy_gate} cases):")
    print(f"{'─'*50}")
    for action, count in sorted(next_actions.items(), key=lambda x: -x[1]):
        pct = count * 100 // ghato_to_enemy_gate
        bar = '█' * (pct // 2)
        correct = '✓' if action in ('capture', 'cof', 'recruit', 'battle', 'writhe-kill') else ''
        print(f"  {action:20s} {count:3d} ({pct:2d}%) {bar} {correct}")

    correct_total = sum(next_actions.get(a, 0) for a in ('capture', 'cof', 'recruit', 'battle', 'writhe-kill'))
    print(f"\nCorrect response rate: {correct_total}/{ghato_to_enemy_gate} = {correct_total*100//ghato_to_enemy_gate}%")
    print(f"Gate gained at destination: {gate_gained_count}")
