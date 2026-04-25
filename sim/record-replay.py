#!/usr/bin/env python3
"""Record an HTML replay as MP4 video using Playwright + ffmpeg.

Usage: record-replay.py <replay.html> <output.mp4> [--speed fast|medium] [--duration 120]

Opens the replay in headless Chromium, clicks Start, sets speed,
records until game ends or duration exceeded, then encodes to MP4.
"""
import sys, os, time, subprocess

if len(sys.argv) < 3:
    print("Usage: record-replay.py <replay.html> <output.mp4> [--speed fast|medium] [--duration 120]")
    sys.exit(1)

replay_path = os.path.abspath(sys.argv[1])
output_path = sys.argv[2]
speed = "fast"
max_duration = 600  # seconds (10 minutes)

for i, arg in enumerate(sys.argv):
    if arg == "--speed" and i + 1 < len(sys.argv):
        speed = sys.argv[i + 1]
    if arg == "--duration" and i + 1 < len(sys.argv):
        max_duration = int(sys.argv[i + 1])

from playwright.sync_api import sync_playwright

print(f"Recording {replay_path} → {output_path}")
print(f"Speed: {speed}, Max duration: {max_duration}s")

with sync_playwright() as p:
    browser = p.chromium.launch(headless=True)
    context = browser.new_context(
        viewport={"width": 1280, "height": 800},
        record_video_dir="/tmp/replay-video/",
        record_video_size={"width": 1280, "height": 800}
    )
    page = context.new_page()

    # Load the replay file
    page.goto(f"file://{replay_path}")
    page.wait_for_load_state("networkidle")
    time.sleep(2)

    # CW replay menu: div.option elements with onclick handlers
    # Close weights panel if open (it covers the Play button)
    time.sleep(3)
    try:
        page.evaluate("var w = document.getElementById('bot-weights'); if (w) w.style.display = 'none';")
    except:
        pass

    # Click "Play" to unpause
    time.sleep(1)
    try:
        play = page.locator("div.option", has_text="Play").first
        play.click()
        print("Clicked Play")
        time.sleep(1)
    except Exception as e:
        print(f"Play click error: {e}")

    # Set speed to Fast via the replay controls panel
    # The speed button cycles: slow → medium → fast
    try:
        speed_btns = page.locator("button:has-text('Speed')")
        count = speed_btns.count()
        print(f"Found {count} speed button(s)")
        for bi in range(count):
            btn = speed_btns.nth(bi)
            if btn.is_visible():
                for _ in range(5):
                    txt = btn.text_content() or ""
                    if "Fast" in txt:
                        break
                    btn.click()
                    time.sleep(0.5)
                print(f"Speed button {bi} set to: {btn.text_content()}")
    except Exception as e:
        print(f"Could not set speed: {e}")

    # Wait for game to complete or timeout
    print(f"Recording... (max {max_duration}s)")
    start_time = time.time()
    last_check = ""
    stall_count = 0

    while time.time() - start_time < max_duration:
        time.sleep(2)
        # Check if game ended (look for "won" text in the page)
        try:
            body_text = page.locator("body").text_content() or ""
            if " won" in body_text[-500:] or "Hooray" in body_text[-500:] or "Game Over" in body_text[-500:] or "Save replay" in body_text[-500:]:
                print("Game ended!")
                time.sleep(3)  # Record a few more seconds
                break
            # Check for stall (same content)
            check = body_text[-100:]
            if check == last_check:
                stall_count += 1
                if stall_count > 15:  # 30 seconds of no change
                    print(f"Stall detected at {time.time()-start_time:.0f}s, stopping")
                    print(f"Last 300 chars: {body_text[-300:]}")
                    page.screenshot(path="/tmp/replay-stall.png")
                    print("Screenshot saved to /tmp/replay-stall.png")
                    # Check JS console for errors
                    errors = page.evaluate("window.__replayErrors || []")
                    if errors:
                        print(f"JS errors: {errors}")
                    break
            else:
                stall_count = 0
            last_check = check
        except:
            pass

    elapsed = time.time() - start_time
    print(f"Recording complete ({elapsed:.0f}s)")

    # Close and save video
    page.close()
    context.close()
    browser.close()

# Find the recorded video and convert to MP4
import glob
videos = glob.glob("/tmp/replay-video/*.webm")
if videos:
    latest_video = max(videos, key=os.path.getmtime)
    print(f"Converting {latest_video} → {output_path}")
    subprocess.run([
        "ffmpeg", "-y", "-i", latest_video,
        "-c:v", "libx264", "-preset", "fast", "-crf", "34",
        "-r", "15",
        "-movflags", "+faststart",
        output_path
    ], capture_output=True)
    size_mb = os.path.getsize(output_path) / (1024 * 1024)
    print(f"Done! {output_path} ({size_mb:.1f} MB)")
    # Cleanup
    os.remove(latest_video)
else:
    print("No video file found!")
