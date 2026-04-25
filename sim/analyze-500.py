#!/usr/bin/env python3
"""Analyze 500-game run outcomes for FB bot."""
import os, re, glob, sys
from collections import Counter, defaultdict

LOG_DIR = os.path.join(os.path.dirname(__file__), "win-logs")

# Get files from this run (timestamp prefix 177696)
games = sorted(glob.glob(os.path.join(LOG_DIR, "fb-game-177696*.txt")))
wins = sorted(glob.glob(os.path.join(LOG_DIR, "fb-win-177696*.txt")))
all_files = games + wins

print(f"Total files: {len(all_files)} ({len(wins)} wins, {len(games)} non-wins)")

# Categorize each game by FB's final state
categories = Counter()
cat_files = defaultdict(list)
doom_dist = []
gate_dist = []
awaken_dist = []
round_dist = []

for fpath in all_files:
    is_win = "fb-win" in fpath
    fname = os.path.basename(fpath)

    with open(fpath) as f:
        content = f.read()

    # Split into actions and log
    parts = content.split("\n\n", 1)
    actions = parts[0].strip().split("\n") if parts else []
    log_html = parts[1] if len(parts) > 1 else ""

    # Extract FB doom from log
    fb_doom = 0
    fb_es = 0
    fb_gates = 0
    fb_awakens = 0
    ritual_count = 0
    round_num = 0
    max_gates = 0

    # Count from actions
    for a in actions:
        if a.startswith("PowerGatherAction"):
            round_num += 1
        if "FBAwakenGhatanothoaAction" in a:
            fb_awakens += 1
        if "BuildGateAction(FB" in a:
            fb_gates += 1

    # Parse doom from log (look for final doom values)
    plain = re.sub(r'<[^>]+>', '', log_html)

    # Count FB rituals
    ritual_count = len(re.findall(r'Firstborn.*ritual', plain, re.IGNORECASE))

    # Extract doom from scoring lines
    doom_matches = re.findall(r'Firstborn.*?(\d+)\s*doom', plain)
    if doom_matches:
        fb_doom = int(doom_matches[-1])

    # Count max gates from gate actions
    gate_builds = len([a for a in actions if "BuildGateAction(FB" in a])
    gate_captures = len([a for a in actions if "CaptureAction(FB" in a])
    gate_losses = len([a for a in actions if a.startswith("CaptureAction(") and "FB" not in a.split(",")[0] and
                       len(a.split(",")) > 1])

    # Categorize
    if is_win:
        cat = "WIN"
    elif fb_awakens >= 3 and ritual_count >= 2:
        cat = "CLOSE (3awk+2rit)"
    elif fb_awakens >= 2 and ritual_count >= 1:
        cat = "PARTIAL (2awk+rit)"
    elif fb_awakens >= 1 and gate_builds >= 3:
        cat = "FULLPATH (awk+3g)"
    elif fb_awakens >= 1:
        cat = "GHATO (awk only)"
    elif gate_builds >= 2:
        cat = "2GATES (no awk)"
    else:
        cat = "EARLY_FAIL"

    categories[cat] += 1
    cat_files[cat].append(fpath)
    awaken_dist.append(fb_awakens)
    round_dist.append(round_num)

print()
print("=== OUTCOME CATEGORIES ===")
for cat, count in sorted(categories.items(), key=lambda x: -x[1]):
    pct = count * 100 / len(all_files)
    print(f"  {cat:25s}: {count:4d} ({pct:5.1f}%)")

print()
print(f"Awaken distribution: {Counter(awaken_dist).most_common()}")
print(f"Round distribution: min={min(round_dist)}, max={max(round_dist)}, avg={sum(round_dist)/len(round_dist):.1f}")

# Pick representative files for each category
print()
print("=== REPRESENTATIVE FILES ===")
for cat in ["WIN", "CLOSE (3awk+2rit)", "PARTIAL (2awk+rit)", "FULLPATH (awk+3g)", "GHATO (awk only)", "2GATES (no awk)", "EARLY_FAIL"]:
    files = cat_files.get(cat, [])
    if files:
        # Pick middle file
        rep = files[len(files)//2]
        print(f"  {cat:25s}: {os.path.basename(rep)}")
