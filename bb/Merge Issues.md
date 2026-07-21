# Merge Issues — Fixes Build into More Neutral Units

**Date:** 2026-05-14
**Source:** /Users/gremus/cthulhu-wars Library at Celaeno/solo (fixes-only build)
**Target:** /Users/gremus/cthulhu-wars More Neutral Units/solo
**Common ancestor:** GitHub library-at-celaeno branch

**KEY FINDING:** The MNU build predates the common ancestor. The ancestor already has fixes (Glaaki=7, named tuple classes, Writhe undo de-dup, etc.) that MNU is missing. ALL rollback guide sections must be checked against MNU, not just the 5-file diff on top of ancestor.

## Files merged (ancestor→fixes diff)
- [x] MapExpansion.scala — §27 Barrier menu cleanup
- [x] Game.scala — §26 Gather Power ordering, fbInfernalPactFlippedTomes var
- [x] FactionFB.scala — v4 IP tome flipping
- [x] Bot3.scala — devourRanking function
- [x] BotFB.scala — v5.6-5.8 scoring, evalBattle/evalLate split for JVM bytecode limit

## Rollback guide sections — ancestor fixes missing from MNU
These fixes exist in the ancestor but NOT in MNU (MNU branched before them):
- [x] §4 FB Devil's Mark glyph union (FactionFB.scala) — handler + menu label
- [x] §5 Library Tome retention rule (MapExpansion.scala) — holder keeps tome, overdue on gate loss
- [x] §6 Librarian agony multi-faction assignment (MapExpansion.scala, Bot3, BotFB, BotMaps, Serialize) — full rewrite
- [x] §7 FB Writhe undo de-dup — ALREADY in MNU (fromUndo param approach)
- [x] §8 TS Glaaki awaken cost 6→7 (FactionTS.scala, overlay.scala, BotTS.scala)
- [x] §9 Mobile faction-status panel (CthulhuWarsSolo.scala) — doomS includes tsStack + libraryTomeStr + tomeStr
- [x] §10 Library-map perspective placement (CthulhuWarsSolo.scala) — classShrink, regionGeom, directionalSpill, passesCaps
- [x] §12 FB tuples → named case classes (FactionFB.scala, Game.scala, Battle.scala, Serialize.scala)
- [x] §14 DS Cosmic Ruler crash (FactionDS.scala) — log before eliminate
- [x] §15 Battle.scala eliminate→log crashes — log before eliminate in EliminateNoWay + CosmicRulerDeclineNoWay
- [x] §17 SpendOnLibrarianAction empty Ask crash (MapExpansion.scala) — guard empty ask
- [x] §18 NM/iGOO picking boost (BotTS, BotFB, BotDS) — jitter diversification applied
- [x] §19 Bot loops — §19a done (empty elim loop break in §6c), §19b done (IP cancel per-turn flag)
- [x] §20 Latent iGOO stale reference (IGOOs.scala) — Side.forces cleanup after iGOO eliminate
- [x] §23 GOO sprite sizing floor (CthulhuWarsSolo.scala) — GOOs render ≥10% larger than cultists
- [x] §25 v4 FB rule changes — §25a IP removed from doom phase, §25b TwoFacedown SBR deferred to EndAction

## §26 Gather Power ordering — DIFFERENT APPROACH

**Rollback guide approach:** Moved raise-to-half to a separate `RaiseToHalfPowerAction` after `AfterPowerGatherAction`.

**MNU approach (per user instruction 2026-05-14):** Keep raise-to-half INSIDE gather power as the last step. The fix is that TS Shepherd of the Crypt (which fires from `AfterPowerGatherAction` via FactionTS.scala) runs BEFORE raise-to-half because `AfterPowerGatherAction` is dispatched before raise-to-half in the Game.scala handler. No separate `RaiseToHalfPowerAction` needed.

**For the other build:** The TS Shepherd gather power piece should be moved to BEFORE raise-to-half, instead of moving raise-to-half outside of gather power. See separate "TS Gather Power Issues Rollback.md" for details.

## Additional fixes applied
- [x] §2/§3 Broken SVG silhouettes — copied 8 fixed SVGs from ancestor (bg-fungi, ww-gnoph-keh, ow-spawn, 5x AN)
- [x] §11 Custodian/Librarian placement — already correct in MNU (base layout engine, no special handling)

## Issues found during merge
1. MNU predates ancestor — scope is much larger than the 5-file diff
2. §26 implemented differently per user instruction (see above)
