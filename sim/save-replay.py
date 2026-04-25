#!/usr/bin/env python3
"""Convert sim action log to watchable HTML replay.
Uses a working replay file as template to ensure all assets present."""
import sys, os, re

if len(sys.argv) < 3:
    print("Usage: save-replay.py <action-log.txt> <output.html> [description]")
    sys.exit(1)

action_log = sys.argv[1]
output_html = sys.argv[2]
desc = sys.argv[3] if len(sys.argv) > 3 else "Bot replay"

# Use the user's working replay as base template (has embedded assets + html-quine)
# BUT replace the inline JS with current build's JS so code matches sim
sim_dir = os.path.dirname(os.path.abspath(__file__))
solo_dir = os.path.join(sim_dir, "..", "solo")
current_js = os.path.join(solo_dir, "target", "scala-2.13", "cthulhu-wars-solo-hrf-fastopt", "main.js")

template_candidates = [
    # FB-enabled template (has fb-background + all FB assets embedded)
    os.path.expanduser("~/My Drive/Personal/Games/Cthulhu Wars/Firstborn/Bot/cthulhu-wars-1.18.015-replay-2026-04-14-22-32.html"),
    os.path.expanduser("~/My Drive/Personal/Games/Cthulhu Wars/Firstborn/Bot/cthulhu-wars-1.18.015-replay-2026-04-14-19-36.html"),
    os.path.expanduser("~/My Drive/Personal/Games/Cthulhu Wars/Firstborn/Bot/cthulhu-wars-1.18.015-replay-2026-04-14-18-17.html"),
    # TS fallback — will crash if game uses FB-specific assets
    os.path.expanduser("~/My Drive/Personal/Games/Cthulhu Wars/Tombstalker/Bot/cthulhu-wars-1.18-replay-2026-03-28-20-23.html"),
]
template = None
for t in template_candidates:
    if os.path.exists(t):
        template = t
        break

if not template:
    print("Error: No replay template found")
    sys.exit(1)

version_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "solo", "common.sbt")

# Read action log (lines before first blank line, skip HTML div lines)
actions = []
with open(action_log) as f:
    for line in f:
        if line.strip() == "":
            break
        if not line.strip().startswith("<div"):
            actions.append(line.rstrip())

# Get factions
factions = []
for a in actions:
    m = re.match(r"StartingRegionAction\((\w+),", a)
    if m:
        factions.append(m.group(1))

# Get version
version = "1.18"
if os.path.exists(version_file):
    with open(version_file) as f:
        for line in f:
            m = re.search(r'version\s*:=\s*"([^"]+)"', line)
            if m:
                version = m.group(1)
                break

# Read template
with open(template) as f:
    html = f.read()

# Build new lobby div
faction_str = "-".join(factions)
player_count = len(factions)
seating = " ".join(factions)
users = "\n".join(f"            user {f} player-{f.lower()}" for f in factions)

new_lobby = f"""        <div id="lobby" style="display: none" >
            meta cthw
            version 1.13
            title Cthulhu Wars {version} Replay
{users}
            seating {seating}
            options Opener4P10Gates MapEarth35 PlayerCount
        </div>"""

# Build new replay div
# Ensure StartAction is first if not already present
if actions and not actions[0].startswith("StartAction"):
    actions.insert(0, "StartAction")

# Detect AP boundaries: find the first FB main action AFTER each PowerGatherAction.
# PowerGather itself is too early (doom phase happens between gather and actions).
# We look for the first FB-specific action (Writhe, Build, Move, Recruit, Awaken,
# Summon, CoF, etc.) after each power gather as the true AP-action-start.
FB_ACTION_PREFIXES = ("FBWritheMainAction", "BuildGateAction(FB", "MoveAction(FB",
                       "RecruitAction(FB", "SummonAction(FB", "FBAwakenGhatanothoaAction",
                       "FBCallOfTheFaithfulMainAction", "FBTheEyeOpensMainAction",
                       "EndTurnAction(FB", "PassAction(FB")
ap_boundaries = []
ap_num = 1
found_gather = False
for idx, a in enumerate(actions):
    if a.startswith("PowerGatherAction"):
        found_gather = True
        ap_num += 1
    elif found_gather and any(a.startswith(p) for p in FB_ACTION_PREFIXES):
        ap_boundaries.append((ap_num, idx))
        found_gather = False

# Extract trace faction marker (set by SimRunner when --trace is active).
# SimRunner emits it via writeLog so it ends up inside a <div class='p'>...</div>.
trace_faction = "BOT"
with open(action_log) as f:
    for line in f:
        m = re.search(r"TRACE_FACTION=(\w+)", line)
        if m:
            trace_faction = m.group(1)
            break

# Extract weight log entries from after the blank line (HTML log section)
# Supports both legacy "[TS BOT" prefix and new "[BOT XX" prefix.
weight_lines = []
with open(action_log) as f:
    past_blank = False
    for line in f:
        if not past_blank:
            if line.strip() == "":
                past_blank = True
            continue
        # Capture all weight-related lines (chosen, alternatives, breakdowns)
        if "[BOT " in line or "[TS BOT" in line or "[OPT " in line or "[WHY]" in line or "[ALT]" in line:
            clean = re.sub(r'<[^>]+>', '', line.strip())
            clean = clean.replace('&gt;', '>').replace('&lt;', '<').replace('&nbsp;', ' ').replace('&amp;', '&')
            if clean.strip():
                weight_lines.append(clean.strip())

new_replay = f"""        <div id="replay" style="display: none" >
            Cthulhu Wars {version} Replay
            {faction_str} Opener4P10Gates MapEarth35 PlayerCount({player_count})
""" + "\n".join("            " + a for a in actions) + """
        </div>"""

# Replace inline JS with current build's JS (so replay code matches sim)
if os.path.exists(current_js):
    with open(current_js) as f:
        new_js = f.read()
    # The inline JS starts after <script id="script" type="text/javascript" >
    # and ends before </script> (the main game script block)
    js_start = html.find('<script id="script"')
    if js_start >= 0:
        js_content_start = html.find('>', js_start) + 1
        js_end = html.find('</script>', js_content_start)
        if js_end >= 0:
            html = html[:js_content_start] + "\n" + new_js + "\n        " + html[js_end:]
            print("Replaced inline JS with current build")

# Replace lobby div
lobby_pattern = re.compile(r'<div id="lobby"[^>]*>.*?</div>', re.DOTALL)
html = lobby_pattern.sub(new_lobby, html, count=1)

# Replace replay div
replay_pattern = re.compile(r'<div id="replay"[^>]*>.*?</div>', re.DOTALL)
html = replay_pattern.sub(new_replay, html, count=1)

# Add replay controls overlay
# - Speed control (slow/medium/fast)
# - Pause/Resume that works AFTER the game's own start button is pressed
# - Back button: reloads page with #skip=N, auto-clicks start, fast-forwards, then pauses
controls = """
        <!-- REPLAY CONTROLS -->
        <script>
        (function() {
            var replaySpeed = 'medium';
            var paused = false;
            var pendingTimeouts = [];
            var origSetTimeout = window.setTimeout;
            var actionCount = 0;

            var mult = { slow: 50, medium: 10, fast: 1 };

            window.setTimeout = function(fn, delay) {
                actionCount++;

                if (paused) {
                    pendingTimeouts.push({ fn: fn, delay: delay });
                    return -1;
                }

                var m = mult[replaySpeed];
                if (m === undefined) m = 10;
                var adj;
                if (replaySpeed === 'fast') {
                    adj = Math.max(delay * m, 1);
                } else if (replaySpeed === 'medium') {
                    adj = Math.max(delay * m, 80);
                } else {
                    adj = Math.max(delay * m, 400);
                }
                return origSetTimeout.call(window, fn, adj);
            };

            function resumePending() {
                var p = pendingTimeouts.splice(0);
                p.forEach(function(t) {
                    var m = mult[replaySpeed] || 10;
                    var minDelay = replaySpeed === 'fast' ? 1 : (replaySpeed === 'medium' ? 80 : 400);
                    origSetTimeout.call(window, t.fn, Math.max(t.delay * m, minDelay));
                });
            }

            var panel, pauseBtn, speedBtn, counterSpan;

            function updateUI() {
                if (pauseBtn) {
                    pauseBtn.textContent = paused ? 'Resume' : 'Pause';
                    pauseBtn.style.background = paused ? '#363' : '#a33';
                }
                if (speedBtn) {
                    speedBtn.textContent = 'Speed: ' + replaySpeed.charAt(0).toUpperCase() + replaySpeed.slice(1);
                }
                if (counterSpan) {
                    counterSpan.textContent = '#' + actionCount;
                }
            }

            function createControls() {
                panel = document.createElement('div');
                panel.style.cssText = 'position:fixed;bottom:10px;right:10px;z-index:99999;background:#222;color:#fff;padding:8px 12px;border-radius:6px;font-family:monospace;font-size:13px;display:flex;gap:8px;align-items:center;box-shadow:0 2px 8px rgba(0,0,0,0.5);';

                var label = document.createElement('span');
                label.textContent = 'v""" + version + """';
                label.style.cssText = 'color:#666;margin-right:4px;font-size:11px;';

                pauseBtn = document.createElement('button');
                pauseBtn.style.cssText = 'background:#555;color:#fff;border:1px solid #888;padding:4px 10px;border-radius:4px;cursor:pointer;font-family:monospace;min-width:70px;';
                pauseBtn.onclick = function() {
                    paused = !paused;
                    if (!paused) resumePending();
                    updateUI();
                };

                speedBtn = document.createElement('button');
                speedBtn.style.cssText = 'background:#555;color:#fff;border:1px solid #888;padding:4px 10px;border-radius:4px;cursor:pointer;font-family:monospace;min-width:110px;';
                speedBtn.onclick = function() {
                    if (replaySpeed === 'slow') replaySpeed = 'medium';
                    else if (replaySpeed === 'medium') replaySpeed = 'fast';
                    else replaySpeed = 'slow';
                    updateUI();
                };

                // Back not possible without game engine changes — use Pause + restart instead

                counterSpan = document.createElement('span');
                counterSpan.style.cssText = 'color:#666;font-size:11px;min-width:40px;text-align:right;';
                origSetTimeout.call(window, function tick() {
                    updateUI();
                    origSetTimeout.call(window, tick, 500);
                }, 500);

                var weightsBtn = document.createElement('button');
                weightsBtn.textContent = 'Weights';
                weightsBtn.style.cssText = 'background:#555;color:#fff;border:1px solid #888;padding:4px 10px;border-radius:4px;cursor:pointer;font-family:monospace;';
                var weightGroups = [];
                var currentGroupIdx = 0;

                function formatLine(line, container) {
                    var div = document.createElement('div');
                    // Supports both new "[BOT XX @N]" and legacy "[TS BOT @N]" prefixes
                    if (line.indexOf('[BOT ') >= 0 || line.indexOf('[TS BOT') >= 0) {
                        div.style.cssText = 'color:#fff;margin-top:8px;font-weight:bold;font-size:12px;border-bottom:1px solid #444;padding-bottom:3px;';
                        div.textContent = line.replace(/\[(?:TS )?BOT[^\]]*\] /, 'CHOSE: ');
                    } else if (line.indexOf('[OPT ') >= 0) {
                        var cm = line.match(/\[OPT (#[a-f0-9]+)\]/);
                        var oc = cm ? cm[1] : '#aaa';
                        var ot = line.replace(/\[OPT #[a-f0-9]+\] /, '');
                        if (oc === '#4a4') { ot = '>>> ' + ot; div.style.cssText = 'color:#4a4;padding-left:12px;font-size:11px;font-weight:bold;'; }
                        else if (oc === '#aa4') { div.style.cssText = 'color:#aa4;padding-left:12px;font-size:11px;'; }
                        else { div.style.cssText = 'color:#a44;padding-left:12px;font-size:11px;'; }
                        div.textContent = ot;
                    } else if (line.indexOf('[WHY]') >= 0) {
                        div.style.cssText = 'color:#8ac;padding-left:28px;font-size:10px;';
                        div.textContent = line.replace('[WHY] ', '');
                    } else if (line.indexOf('[ALT]') >= 0) {
                        var at = line.replace('[ALT] ', '');
                        if (at.indexOf('breakdown') >= 0) {
                            div.style.cssText = 'color:#a86;padding-left:8px;font-size:10px;margin-top:4px;border-top:1px dashed #555;padding-top:2px;';
                        } else {
                            div.style.cssText = 'color:#a86;padding-left:28px;font-size:10px;';
                        }
                        div.textContent = at;
                    } else {
                        div.style.cssText = 'color:#999;padding-left:8px;font-size:10px;';
                        div.textContent = line;
                    }
                    container.appendChild(div);
                }

                function showWeightGroup(idx) {
                    var el = document.getElementById('bot-weight-content');
                    if (!el) return;
                    el.innerHTML = '';
                    // Previous
                    if (idx > 0) {
                        var prevLabel = document.createElement('div');
                        prevLabel.style.cssText = 'color:#666;font-size:10px;margin-bottom:4px;';
                        prevLabel.textContent = 'PREVIOUS DECISION:';
                        el.appendChild(prevLabel);
                        weightGroups[idx-1].forEach(function(line) { formatLine(line, el); });
                        var sep = document.createElement('div');
                        sep.style.cssText = 'border-top:2px solid #666;margin:8px 0;';
                        el.appendChild(sep);
                    }
                    // Current
                    if (idx < weightGroups.length) {
                        var curLabel = document.createElement('div');
                        curLabel.style.cssText = 'color:#aaa;font-size:10px;margin-bottom:4px;';
                        curLabel.textContent = 'CURRENT DECISION:';
                        el.appendChild(curLabel);
                        weightGroups[idx].forEach(function(line) { formatLine(line, el); });
                    }
                    // Counter
                    var ctr = document.createElement('div');
                    ctr.style.cssText = 'color:#666;font-size:10px;margin-top:8px;text-align:right;';
                    ctr.textContent = 'Decision ' + (idx+1) + ' / ' + weightGroups.length;
                    el.appendChild(ctr);
                }

                var weightActionNums = []; // action number for each group

                function buildWeightPanel() {
                    var lines = window.botWeightLines || [];
                    var currentGroup = [];
                    for (var gi = 0; gi < lines.length; gi++) {
                        if ((lines[gi].indexOf('[BOT ') >= 0 || lines[gi].indexOf('[TS BOT') >= 0) && currentGroup.length > 0) {
                            weightGroups.push(currentGroup);
                            currentGroup = [];
                        }
                        currentGroup.push(lines[gi]);
                    }
                    if (currentGroup.length > 0) weightGroups.push(currentGroup);
                    // Extract action numbers from [TS BOT @N] headers
                    for (var wi = 0; wi < weightGroups.length; wi++) {
                        var header = weightGroups[wi][0] || '';
                        var m = header.match(/@(\d+)\]/);
                        weightActionNums.push(m ? parseInt(m[1]) : 0);
                    }
                    showWeightGroup(0);
                    // Sync: use requestAnimationFrame + innerText search
                    function syncWeights() {
                        var gameCounter = 0;
                        var bodyText = document.body.innerText || '';
                        // Find all "N / M" patterns and pick the one with largest M (the game counter)
                        var matches = bodyText.match(/\d+\s*\/\s*\d+/g);
                        if (matches) {
                            for (var mi = 0; mi < matches.length; mi++) {
                                var parts = matches[mi].split('/');
                                var num = parseInt(parts[0].trim());
                                var den = parseInt(parts[1].trim());
                                if (den > 100 && num > gameCounter) {
                                    gameCounter = num;
                                }
                            }
                        }
                        if (gameCounter > 0 && weightActionNums.length > 0) {
                            var bestIdx = 0;
                            for (var bi = 0; bi < weightActionNums.length; bi++) {
                                if (weightActionNums[bi] <= gameCounter) bestIdx = bi;
                                else break;
                            }
                            if (bestIdx !== currentGroupIdx) {
                                currentGroupIdx = bestIdx;
                                showWeightGroup(currentGroupIdx);
                            }
                        }
                        requestAnimationFrame(syncWeights);
                    }
                    requestAnimationFrame(syncWeights);
                }
                var weightsPanelBuilt = false;
                weightsBtn.onclick = function() {
                    var wp = document.getElementById('bot-weights');
                    if (wp) {
                        wp.style.display = wp.style.display === 'none' ? 'block' : 'none';
                        if (!weightsPanelBuilt) { buildWeightPanel(); weightsPanelBuilt = true; }
                    }
                };
                // Open weights panel by default
                origSetTimeout.call(window, function() {
                    var wp = document.getElementById('bot-weights');
                    if (wp) { wp.style.display = 'block'; buildWeightPanel(); weightsPanelBuilt = true; }
                }, 500);

                // AP navigation buttons (above main controls)
                var apPanel = document.createElement('div');
                apPanel.style.cssText = 'position:fixed;bottom:50px;right:10px;z-index:99999;background:#1a1a2e;color:#fff;padding:6px 10px;border-radius:6px;font-family:monospace;font-size:11px;display:flex;gap:4px;align-items:center;box-shadow:0 2px 8px rgba(0,0,0,0.5);flex-wrap:wrap;max-width:400px;';
                var apLabel = document.createElement('span');
                apLabel.textContent = 'Jump to:';
                apLabel.style.cssText = 'color:#888;margin-right:4px;';
                apPanel.appendChild(apLabel);

                var apBoundaries = """ + str([[ap, idx] for ap, idx in ap_boundaries]) + """;
                var ffTarget = -1;
                var activeApBtn = null;

                apBoundaries.forEach(function(entry) {
                    var apNum = entry[0];
                    var stepIdx = entry[1];
                    var btn = document.createElement('button');
                    btn.textContent = 'AP' + apNum;
                    btn.dataset.apNum = apNum;
                    btn.dataset.stepIdx = stepIdx;
                    btn.style.cssText = 'background:#334;color:#aaf;border:1px solid #556;padding:2px 8px;border-radius:3px;cursor:pointer;font-family:monospace;font-size:11px;';
                    btn.onclick = function() {
                        if (btn.disabled) return;
                        // Unhighlight previous active button
                        if (activeApBtn && activeApBtn !== btn) {
                            activeApBtn.style.background = '#334';
                            activeApBtn.textContent = 'AP' + activeApBtn.dataset.apNum;
                        }
                        ffTarget = stepIdx;
                        replaySpeed = 'fast';
                        activeApBtn = btn;
                        if (paused) {
                            paused = false;
                            resumePending();
                        }
                        updateUI();
                        btn.style.background = '#553';
                        btn.textContent = 'AP' + apNum + '...';
                    };
                    apPanel.appendChild(btn);
                });

                document.body.appendChild(apPanel);

                // Grey out AP buttons that have been passed
                function updateApButtons() {
                    var btns = apPanel.querySelectorAll('button');
                    btns.forEach(function(b) {
                        var step = parseInt(b.dataset.stepIdx);
                        if (step && actionCount > step) {
                            b.disabled = true;
                            b.style.background = '#222';
                            b.style.color = '#555';
                            b.style.cursor = 'default';
                            b.style.borderColor = '#333';
                        }
                    });
                }

                // Patch setTimeout to auto-pause at ffTarget + update AP buttons
                var _prevSetTimeout = window.setTimeout;
                window.setTimeout = function(fn, delay) {
                    // Check if reached fast-forward target
                    if (ffTarget >= 0 && actionCount >= ffTarget) {
                        ffTarget = -1;
                        paused = true;
                        replaySpeed = 'medium';
                        if (activeApBtn) {
                            activeApBtn.style.background = '#334';
                            activeApBtn.textContent = 'AP' + activeApBtn.dataset.apNum;
                            activeApBtn = null;
                        }
                        updateUI();
                    }
                    // Periodically grey out passed AP buttons
                    if (actionCount % 50 === 0) updateApButtons();
                    return _prevSetTimeout.call(window, fn, delay);
                };

                panel.appendChild(label);
                panel.appendChild(pauseBtn);
                panel.appendChild(speedBtn);
                panel.appendChild(weightsBtn);
                panel.appendChild(counterSpan);
                document.body.appendChild(panel);
                updateUI();

            }

            if (document.readyState === 'complete') createControls();
            else window.addEventListener('load', createControls);
        })();
        </script>
        <!-- END REPLAY CONTROLS -->
"""
# Add weight log panel if weights were captured
if weight_lines:
    weight_log_js = "\n".join(f'"{line.replace(chr(34), "").replace(chr(92), "")}",' for line in weight_lines)
    panel_label = f"{trace_faction} Decision Weights (click to close)"
    weight_panel = """
        <!-- BOT WEIGHT LOG PANEL -->
        <div id="bot-weights" style="position:fixed;bottom:0;left:0;z-index:99998;background:rgba(0,0,0,0.92);color:#aaa;padding:8px;font-family:monospace;font-size:10px;height:312px;overflow-y:auto;width:504px;display:none;">
            <div style="color:#fff;font-size:13px;margin-bottom:5px;cursor:pointer;" onclick="document.getElementById('bot-weights').style.display='none'">""" + panel_label + """</div>
            <div id="bot-weight-content"></div>
        </div>
        <script>
            window.botWeightLines = [
""" + weight_log_js + """
            ];
        </script>
        <script>
        (function() {

            // Build all weight content on load — no setTimeout hooks
            function buildWeights() {
                var el = document.getElementById('bot-weight-content');
                if (!el) return;
                for (var i = 0; i < weightLines.length; i++) {
                    var line = weightLines[i];
                    var div = document.createElement('div');
                    if (line.indexOf('[TS BOT]') >= 0) {
                        div.style.cssText = 'color:#8f8;margin-top:6px;border-top:1px solid #333;padding-top:3px;';
                    } else if (line.indexOf('>>') >= 0) {
                        div.style.cssText = 'color:#ff8;';
                    } else {
                        div.style.cssText = 'color:#888;padding-left:10px;';
                    }
                    div.textContent = line;
                    el.appendChild(div);
                }
            }

            // Weight panel toggled via button in controls bar
        })();
        </script>
        <!-- END TS WEIGHT LOG PANEL -->
"""
    controls = weight_panel + controls

html = html.replace("</body>", controls + "    </body>")

# Write output
os.makedirs(os.path.dirname(os.path.abspath(output_html)), exist_ok=True)
with open(output_html, "w") as f:
    f.write(html)

print(f"Replay saved: {output_html} (v{version}, {faction_str}, {player_count}p)")
