# FLESHMASON Setup Fix Verification Plan

## Bug Description
**Before Fix**: FM could select ANY region for setup, but only THORN/DRAGON regions would receive Screaming Walls. This meant FM could setup in invalid regions (CHEVRON, Ocean, NoGlyph) and get no walls.

**After Fix**: FM can ONLY select THORN or DRAGON glyph regions during setup, ensuring Screaming Walls are always placed correctly.

## Test Cases

### Test 1: Earth Map 3 - Valid Regions
**Map**: Earth Map (3 players)
**Setup**: Select FLESHMASON as a faction

**Expected VALID regions** (should appear in picker):
- Europe (THORN)
- Asia (THORN)
- North America (DRAGON)
- South America (DRAGON)

**Expected INVALID regions** (should NOT appear):
- Africa (CHEVRON)
- Australia (CHEVRON)
- Antarctica (NoGlyph)
- Any Ocean regions

**Verification**:
1. Start new game with FM on Earth 33 map
2. During faction setup, note which regions FM can select
3. Confirm ONLY the 4 THORN/DRAGON regions are available
4. Select one region (e.g., Europe)
5. Verify Screaming Wall is placed in that region after setup

### Test 2: Library Map 5 - Mixed Glyphs
**Map**: Library at Celaeno (5 players)
**Setup**: Select FLESHMASON as a faction

**Expected VALID regions** (Upper Floor):
- Yr and the Nhhngr (DRAGON)
- Guardian under the Lake (DRAGON)
- Barrier of Naach-Tith (DRAGON)
- Larvae of the Outer Gods (DRAGON)

**Expected VALID regions** (Lower Floor):
- Chamber of Sn'gac (THORN)
- Blue Hall (THORN)
- Scorched Chamber (THORN)
- Red Hall (THORN)
- Black Hall (THORN)
- Chamber of Apkallu (THORN)
- Charnel Hall (THORN)
- The Crawling Ones (THORN)

**Expected INVALID regions**:
- Floating Tower, Byakhiary, Horrorium (CHEVRON)
- All Ocean regions (Fountain, Gloomloft, etc.)

**Verification**:
1. Start new game with FM on Library 5 map
2. Verify FM can select any of the 12 THORN/DRAGON regions
3. Verify FM cannot select the 3 CHEVRON or Ocean regions
4. Select Chamber of Sn'gac (THORN)
5. Verify Screaming Wall is placed there

### Test 3: Yuggoth Map - Edge Cases
**Map**: Yuggoth (3 players)
**Setup**: Select FLESHMASON as a faction

**Expected VALID regions**:
- Spore Badlands (THORN)
- Toxin Disposal (THORN)
- Radiation Wastes (DRAGON)
- The Twin Desolations (DRAGON)

**Expected INVALID regions**:
- Slime Sea Overlook (CHEVRON)
- Laboratory (CHEVRON)
- Green Pyramid Slopes (CHEVRON)
- The Watcher's Postern (CHEVRON)
- All Ocean/Sea regions

**Verification**:
1. Start new game with FM on Yuggoth 3 map
2. Verify only 4 THORN/DRAGON regions are selectable
3. Select Radiation Wastes
4. Verify Screaming Wall placement

## Code Changes Verified

### Map.scala Changes
All FM starting region definitions now filter to THORN/DRAGON only:
```scala
case FM => regions.%(r => r.glyph == GlyphWW || r.glyph == GlyphOO)
```

Applied to 9 map definitions:
- EarthMap3 (line 101)
- EarthMap4v35 (line 230)
- EarthMap4v53 (line 363)
- EarthMap5 (line 504)
- EarthMap6 (line 655)
- LibraryCelaeno55 (line 932)
- LibraryCelaeno33 (line 1053)
- LibraryCelaeno53 (line 1136)
- LibraryCelaeno35 (line 1240)

### FactionFM.scala - No Changes Needed
The setup logic in FactionFM.scala (lines 232-241) was already correct:
```scala
case StartingRegionAction(f : FM.type, r) =>
    if (r.glyph == GlyphWW || r.glyph == GlyphOO) {
        // places Screaming Walls
    }
```

The bug was that players could select invalid regions but get no walls. Now they can't select invalid regions at all.

## Success Criteria

✅ FM setup picker only shows THORN/DRAGON regions
✅ No CHEVRON, Ocean, or NoGlyph regions appear in FM picker
✅ Screaming Walls are placed in every region FM selects
✅ Works correctly on all map types (Earth, Library, Yuggoth)

## Deployment Info

**Commit**: 6d49027f
**Branch**: homebrew
**Files Changed**:
- solo/Map.scala (9 starting() function changes)
- MAP-GLYPH-REFERENCE.md (new documentation)

**Deploy Command**: deploy-hb-to-vm.sh
**Server**: HB (Homebrew test server)
