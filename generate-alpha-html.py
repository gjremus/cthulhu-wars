#!/usr/bin/env python3
"""Generate TchoTchoAlphaV1.html — single-file standalone build for local use."""

import re
import base64
import os
from pathlib import Path

SOLO_DIR = Path("/Users/gremus/Claude-Projects/cthulhu-wars-TchoTcho_Cats_Yuggoth/solo")
JS_PATH = SOLO_DIR / "target/scala-2.13/cthulhu-wars-solo-hrf-fastopt.js"
INDEX_PATH = SOLO_DIR / "index.html"
OUTPUT_PATH = Path("/Users/gremus/Google Drive/My Drive/Personal/Games/Cthulhu Wars/Tcho-Tcho/TchoTchoAlphaV1.html")

print("Reading index.html...")
html = INDEX_PATH.read_text(encoding="utf-8")

print(f"Reading JS ({JS_PATH.stat().st_size // 1024 // 1024}MB)...")
js_content = JS_PATH.read_text(encoding="utf-8")

# Replace the script src tag with inlined JS
script_pattern = re.compile(
    r'<script[^>]+id="script"[^>]+src="[^"]+"[^>]*></script>',
    re.IGNORECASE
)
inlined_script = f'<script id="script" type="text/javascript">\n{js_content}\n</script>'
html = script_pattern.sub(lambda m: inlined_script, html)

# Inline webp images as base64 data URIs
def inline_webp(match):
    src = match.group(1)
    img_path = SOLO_DIR / src.lstrip("./")
    if img_path.exists():
        data = base64.b64encode(img_path.read_bytes()).decode("ascii")
        ext = img_path.suffix.lstrip(".")
        mime = "image/svg+xml" if ext == "svg" else f"image/{ext}"
        return f'src="data:{mime};base64,{data}"'
    return match.group(0)  # leave as-is if file not found

print("Inlining local image assets...")
html = re.sub(r'src="(webp/[^"]+)"', inline_webp, html)

# Update the data-server attribute to offline mode
html = html.replace('data-server="###SERVER-URL###"', 'data-server=""')
html = html.replace('data-online="true"', 'data-online="false"')

print(f"Writing to {OUTPUT_PATH}...")
OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
OUTPUT_PATH.write_text(html, encoding="utf-8")

size_mb = OUTPUT_PATH.stat().st_size / 1024 / 1024
print(f"Done! File size: {size_mb:.1f}MB")
print(f"Output: {OUTPUT_PATH}")
