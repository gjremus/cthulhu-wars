# Live Values Override System

## Overview
Designer Simple Updates now take effect in the live game **WITHOUT** requiring manual `faction-values.py sync && apply --deploy`.

## Architecture

### 1. Generation (`faction-values.py`)
The generation script creates:

- **FactionValuesMapping**: Static mapping from designer API paths to FactionValues field names
  ```scala
  "units.rows.csun0002.dice" -> "Meteorite_combat"
  "units.rows.csun0002.cost" -> "Meteorite_cost"
  ```

- **Dynamic defs**: Each value is a `def` that calls `LiveValues.get()`
  ```scala
  def Meteorite_combat = LiveValues.get("CS", "Meteorite_combat", 0)
  ```

### 2. Runtime Loading (`LiveValues.scala`)
On game startup (`CthulhuWarsSolo.setupUI()`):
1. Fetches `/designer/api/live/<ACR>/values` for CS, DGT, FBE, FM, PRM, XSS
2. Parses designer paths using FactionValuesMapping
3. Stores overrides in memory
4. `LiveValues.get()` returns override OR compiled default

### 3. Designer API
- **Write**: Designer "Push simple update" → `/opt/cwo/designer/data/factions/<fid>/live-values.json`
- **Read**: Game fetches `GET /designer/api/live/<ACR>/values` on startup
- **Format**: `{"values": {"units.rows.csun0002.dice": 1}}`

## Usage

### Owner Workflow (Simple Update)
1. Edit values in designer (e.g., change Meteorite combat: 0 → 1)
2. Click "Push simple update" in designer
3. **Done!** Next game reload will use the new value

### Verification
1. Make a change in designer and push it
2. Check the API: `curl https://cwo.freeddns.org/designer/api/live/CS/values`
3. Reload game in browser (hard refresh: Cmd+Shift+R)
4. Start a CS game and verify the changed value works

## Affected Factions
- ✅ CS (Colour Out of Space)
- ✅ DGT (Tribulation)
- ✅ FBE (Faceless Blight)
- ✅ FM (Fleshmason)
- ✅ PRM (Primogenitor)
- ✅ XSS (Xiombarg)

Original factions (GC, CC, BG, YS, etc.) are **NOT** affected - they continue to use static compiled values.

## Technical Details

### Files Modified
- `/Users/gremus/Claude-Projects/cthulhu-wars-tools/scripts/faction-values.py` - Generation script
- `/Users/gremus/Claude-Projects/cw-homebrew-wt/solo/FactionValues.scala` - Generated (defs + mapping)
- `/Users/gremus/Claude-Projects/cw-homebrew-wt/solo/LiveValues.scala` - Runtime loader (NEW)
- `/Users/gremus/Claude-Projects/cw-homebrew-wt/solo/CthulhuWarsSolo.scala` - Startup hook

### Deployed
- Commit: `ce549ff9` (Live Values system)
- Version: Homebrew v 2.312
- Deploy: 2026-10-15 14:53:04

### Fallback Behavior
- If API fetch fails → uses compiled defaults (no error)
- If mapping missing → uses compiled defaults
- If override invalid → uses compiled defaults
- **Result**: System degrades gracefully, never breaks games

## Testing Checklist
- [ ] Push a numeric change from designer (e.g., CS Meteorite dice)
- [ ] Verify API returns the new value
- [ ] Reload game in browser
- [ ] Start a CS game
- [ ] Verify the changed value appears in gameplay/overlay
- [ ] Test with multiple factions
- [ ] Test when API is unavailable (should fall back to defaults)

## Future Enhancements
If needed:
- Add console logging for which values were overridden
- Add in-game indicator when live values are active
- Add admin panel to view/clear live values
- Add version tracking for live values
