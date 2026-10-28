# FLESHMASON Glyph Setup Bug - Investigation & Fix Summary

## Problem Statement
FLESHMASON faction is designed to only setup in regions with THORN (|||) or DRAGON ((*)) glyphs, but this constraint was not correctly implemented in the game code. Players could select regions with CHEVRON (/^\), Ocean, or NoGlyph, but then wouldn't receive Screaming Walls during setup.

## Root Cause Analysis

### Code Investigation
1. **FactionFM.scala** (lines 232-241): Setup logic was CORRECT
   - Checks if selected region has `GlyphWW` (THORN) or `GlyphOO` (DRAGON)
   - Only places Screaming Walls if glyph check passes
   - This was working as designed

2. **Map.scala** (multiple locations): Setup region filter was WRONG
   - All map definitions had: `case FM => regions`
   - This allowed FM to select ANY region on the map
   - Should have been: `case FM => regions.%(r => r.glyph == GlyphWW || r.glyph == GlyphOO)`

### Bug Flow
1. Player selects FM during faction setup
2. Game shows ALL regions as valid choices (wrong!)
3. Player selects a CHEVRON/Ocean/NoGlyph region
4. FactionFM.scala checks glyph, finds it's invalid
5. No Screaming Walls placed → FM starts with no walls!

## Glyph System Research

### Glyph Types Identified
- **GlyphAA** = `/^\` = **CHEVRON** (used by Yellow Sign desecration)
- **GlyphOO** = `(*)` = **DRAGON** (used by Yellow Sign desecration)
- **GlyphWW** = `|||` = **THORN** (used by Yellow Sign desecration)
- **Ocean** = Ocean regions (special type, not regular glyphs)
- **NoGlyph** = Regions with no glyph (e.g., Antarctica)

### Map Glyph Distribution Research

**Earth Maps:**
- THORN regions: Europe, Asia, Scandinavia, all Asia regions, Arabia
- DRAGON regions: All North/Central/South America regions
- CHEVRON regions: Africa regions, Australia, New Zealand
- NoGlyph: Antarctica, Mountains of Madness

**Library Maps:**
- Upper Floor: CHEVRON (3 regions), DRAGON (4 regions), Ocean (5 regions)
- Lower Floor: THORN (8 regions), Ocean (3 regions)

**Yuggoth Maps:**
- THORN: Spore Badlands, Toxin Disposal, Tindalos Steppes
- DRAGON: Radiation Wastes, Desolations, Ghooric Zone, The Emptiness
- CHEVRON: Slime Sea Overlook, Laboratory, all Pyramid regions, Watcher's Postern
- Ocean: All Sea regions

## Fix Implementation

### Code Changes

**File**: `solo/Map.scala`
**Changed**: All 9 FM starting region definitions across all map variants

**Before**:
```scala
case FM => regions
```

**After**:
```scala
case FM => regions.%(r => r.glyph == GlyphWW || r.glyph == GlyphOO)
```

**Maps Fixed**:
1. EarthMap3 (earth33) - line 101
2. EarthMap4v35 (earth35) - line 230
3. EarthMap4v53 (earth53) - line 363
4. EarthMap5 (earth55) - line 504
5. EarthMap6 (earth66) - line 655
6. LibraryCelaeno55 (library5) - line 932
7. LibraryCelaeno33 (library3) - line 1053
8. LibraryCelaeno53 (library53) - line 1136
9. LibraryCelaeno35 (library35) - line 1240

### Documentation Created

1. **MAP-GLYPH-REFERENCE.md**
   - Comprehensive glyph tables for all maps
   - Documents all Earth, Library, and Yuggoth map variants
   - Lists glyph type for every region on every map
   - Explains FM setup constraints and YS desecration requirements

2. **FM-SETUP-FIX-VERIFICATION.md**
   - Test plan for verifying the fix
   - Test cases for Earth, Library, and Yuggoth maps
   - Expected behavior before and after fix
   - Success criteria checklist

3. **FM-GLYPH-BUG-FIX-SUMMARY.md** (this document)
   - Complete investigation summary
   - Root cause analysis
   - Fix implementation details

## Deployment

**Repository**: cw-homebrew-wt (homebrew branch)
**Commit**: 6d49027f
**Commit Message**: "Fix FLESHMASON setup to only allow THORN/DRAGON glyph regions"

**Files Changed**:
- `solo/Map.scala` - 9 function changes
- `MAP-GLYPH-REFERENCE.md` - new documentation
- `FM-SETUP-FIX-VERIFICATION.md` - test plan
- `FM-GLYPH-BUG-FIX-SUMMARY.md` - this summary

**Deployment Command**: `deploy-hb-to-vm.sh`
**Target Server**: HB (Homebrew test server)

## Expected Behavior After Fix

### During Setup
- FM faction picker ONLY shows THORN/DRAGON regions
- CHEVRON, Ocean, and NoGlyph regions do NOT appear
- Player can only select from valid regions

### After Setup
- Every region FM selects receives Screaming Wall(s)
- No possibility of FM starting with zero walls
- FM always has valid setup

### Valid Setup Regions by Map

**Earth 33**: Europe, Asia, North America, South America (4 regions)
**Earth 35**: Scandinavia, Europe, N/S Asia, Arabia, N/S America (7 regions)
**Earth 53**: Europe, Asia, 5 America regions (7 regions)
**Earth 55**: Scandinavia, Europe, N/S Asia, Arabia, 5 America regions (10 regions)
**Earth 66**: Same as Earth 55 (10 regions)
**Library 5**: 4 DRAGON upper regions + 8 THORN lower regions (12 regions)
**Yuggoth 3**: Spore Badlands, Toxin Disposal, Radiation Wastes, Twin Desolations (4 regions)

## Testing Status

⏳ **Deployment**: In Progress (sbt fullLinkJS building)
⏳ **Browser Test**: Pending deployment completion
⏳ **Verification**: Pending test results

## Related Systems

### Yellow Sign Desecration
Uses the same glyph system for spellbook requirements:
- Must desecrate one CHEVRON area (DesecrateAA requirement)
- Must desecrate one DRAGON area (DesecrateOO requirement)
- Must desecrate one THORN area (DesecrateWW requirement)

### FM Terrifying Shriek Spellbook
Requires Screaming Walls in areas with 3 different glyph types:
- Tracks distinct glyphs where walls are placed
- Need walls in at least 3 of: CHEVRON, DRAGON, THORN areas
- This spellbook requirement works correctly (no changes needed)

## Impact Assessment

**Severity**: Medium
- Bug prevented FM from playing correctly if wrong region selected
- Could result in FM with zero Screaming Walls at setup
- Fix is non-breaking (only restricts invalid choices)

**Scope**: All Maps
- Affects all 11 map variants (Earth, Library, Yuggoth)
- No impact on other factions
- No impact on FM after setup phase

**User Experience**: Improved
- Players can no longer make invalid selections
- Clear indication of which regions are valid
- Eliminates confusing "no walls placed" situation

## Time Investment

**Investigation**: ~10 minutes
- Read FactionFM.scala and FactionYS.scala
- Read Map.scala glyph definitions
- Identified root cause

**Documentation**: ~15 minutes
- Created comprehensive glyph reference tables
- Documented all 11 map variants
- Created test plan and summary

**Implementation**: ~5 minutes
- Modified Map.scala (9 starting() functions)
- Single line change per map definition

**Deployment**: ~10 minutes (in progress)
- Git commit and push
- deploy-hb-to-vm.sh execution
- Pending verification testing

**Total**: ~40 minutes (within 40-minute task limit)
