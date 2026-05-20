# TS Gather Power Ordering — Rollback Guide for Other Build

**Date:** 2026-05-14
**Applies to:** /Users/gremus/cthulhu-wars Library at Celaeno/solo (fixes build)

## Problem
TS Shepherd of the Crypt power gain was happening AFTER raise-to-half, meaning the Shepherd bonus wasn't included in the max power calculation for the floor.

## Original Fix (in fixes build)
Moved raise-to-half to a separate `RaiseToHalfPowerAction` that runs after `AfterPowerGatherAction`. This puts it after Shepherd but also after gather power entirely.

## Correct Fix (per user 2026-05-14)
Raise-to-half should stay WITHIN gather power as the last step. Instead, move the TS Shepherd power gain to BEFORE raise-to-half.

### How to apply to the fixes build

**`solo/Game.scala` — AfterPowerGatherAction handler:**
Remove `RaiseToHalfPowerAction` dispatch. Move the raise-to-half logic back into AfterPowerGatherAction, AFTER MaoCeremony but as the last thing before `BeforeFirstPlayerAction`.

The flow should be:
1. `PowerGatherAction` → power calculations → triggers() → `AfterPowerGatherAction`
2. `AfterPowerGatherAction` → TS Shepherd fires (FactionTS.scala intercepts) → returns → MaoCeremony → raise-to-half → `BeforeFirstPlayerAction`

This keeps raise-to-half inside the gather power sequence while ensuring Shepherd runs before it.

**`solo/Game.scala` — Remove RaiseToHalfPowerAction:**
Delete the `case object RaiseToHalfPowerAction extends ForcedAction` declaration and its handler. Put the raise-to-half logic (max/min calculation, humanity check, power floor) at the end of the `AfterPowerGatherAction` handler in Game.scala, right before `BeforeFirstPlayerAction(factions)`.
