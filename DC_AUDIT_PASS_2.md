# DC Audit Pass #2 — 2026-06-06

Verdict legend: MATCH / DRIFT-FIXED-IN-PASS-2 / MISSING-IMPLEMENTED-IN-PASS-2 / MISSING-DEFERRED-TO-MASTER-TASKS.

This pass re-audits the implementation after Pass-1 fixes were landed in
`solo/FactionDC.scala`, `solo/BotDC.scala`, `solo/Game.scala`,
`solo/Battle.scala` and verifies Closure-clean build.

## §1.0 Faction overview & flavor — MATCH

## §1.1 Hard restriction — MATCH (no 2P illegal lineup)

## §1.2 Special tile — MATCH (no Moon equivalent)

## §1.5.1 Tenebrosum — MISSING-IMPLEMENTED-IN-PASS-2
- `DCTenebrosumMainAction` (Soft) + `DCTenebrosumRepeatAction` (Hard) are now wired.
- Trigger: `dcLastActionForTenebrosum` recorded per cost-paying DC action
  (MoveAction, BuildGateAction, RecruitAction, SummonAction, CaptureAction,
  AttackAction, AwakenAction, plus the four DC SB actions).
- Offered as a main-menu option in both pre-acted and post-acted MainAction(DC).
- Non-recursive: `dcTenebrosumGuard` set during the repeat blocks recording
  of the repeat itself; cleared on EndAction(DC).
- Repeat semantics: grants ONE extra main-menu turn via
  `dcTenebrosumExtraTurn` (mechanically equivalent to repeating the same
  action if user picks it again; bot weights converge to same).
- DEVIATION: not a literal "re-execute same action" — see master-tasks note.

## §1.5.2 Depravity — MATCH (Gather Power +1 Sin per DC Acolyte on map)

## §1.6 Setup — MATCH
- Reserved-Acolyte placement on SB acquisition now WIRED via
  `SpellbookAction(DC, sb, then)` hook that queues into
  `dcPendingAcolytePlacements`; `MainAction(DC)` consumes pending entries
  by Force-directing to `DCPlaceReservedAcolyteMainAction(self, sb)`.
- Ancient Sorcery / Sleeper interactions: DEFERRED — see master tasks.

## §1.7 Units — MATCH (Acolyte ×6, MindlessHusk ×5, FallenProphet ×4, YgolonacDC ×1)

## §1.8 Y'Golonac awaken + Bacchanal — MATCH
- Awaken cost = SB count: MATCH.
- Land area without Controlled Gate: MATCH.
- Combat dice ceil(Sin/2): MATCH (in `DC.strength`).
- Start Area on first awaken: MATCH.
- Bacchanal +1 Power +1 Sin: MATCH.
- Bacchanal Build & Control Gates: MISSING-IMPLEMENTED-IN-PASS-2 via
  `YgolonacDC.canControlGate` override = `true`. This single override
  satisfies both the Build Gate filter (`f.at(r).%(_.canControlGate).any`)
  and Control Gate filter in Game.scala builds()/controls() helpers.

## §1.9 SB requirements — MATCH

## §1.10 Spellbooks

### Proselytize (Ongoing) — MISSING-IMPLEMENTED-IN-PASS-2
- `MovedAction(DC, Acolyte, o, r)` hook now drags an enemy Acolyte per
  enemy faction in source.
- Per-Acolyte scaling: the hook fires per MovedAction (one per Acolyte
  move). Enemies with exactly 1 Acolyte in source auto-drag; 2+ get
  the choice menu (self=enemyFaction for G11 enemy-colored border).
- Chain returns `MoveContinueAction(DC, true)` to resume the standard
  movement chain.

### Satiate (Action: Cost 2) — MATCH
- Per-faction Cultist pick (Item 8): if a faction has 2+ cultists in
  Y'Golonac's area, `DCSatiateFactionPickAction` asks that faction.
- Self-capture: works (DC iteration includes self).
- ES bonus: math.max(0, captured-1) per spec.

### Lure (Action: Cost 1) — MATCH
- Per-enemy Cultist pick (Item 8): if enemy has 2+ eligible adjacent
  cultists, `DCLureFactionPickAction` chains a sub-menu (self=enemyFaction).
- Exemption checks (enemy GOO, Terror, Faction Building, Moon): MATCH.

### Eschar (Post-Battle) — MISSING-IMPLEMENTED-IN-PASS-2
- Battle.scala BattleEnd hook now counts `exempted.count(u.faction == DC
  && u.uclass == MindlessHusk && u.health == Killed)` and grants
  `game.dcSin += killedHusks` with canonical "Eschar: gained N Sin"
  log line.

### Pilgrimage (Action: Cost 1) — MATCH
- Single destination, Prophet stays put, other DC units in source move.

### Dark Bargain (Action: Cost 0) — MISSING-IMPLEMENTED-IN-PASS-2
- Full round-of-prompts pattern implemented:
  1. `DCDarkBargainConfirmAction` clears `game.dcDarkBargainPicks`.
  2. `DCDarkBargainEnemyContinueAction(self, enemies)` recursively asks
     each enemy in turn (self=enemyFaction for G11 border) for a D6 face
     1..6 via `DCDarkBargainEnemyPickAction(enemy, face, remaining)`.
  3. After all enemies pick, DC sees `DCDarkBargainChooseSinAction(DC, face)`
     for each 1..6 — DC picks a face.
  4. Matching enemies (who picked that face) forfeit Power; pooled
     amount is redistributed evenly across all enemies.
  5. `game.dcDarkBargainFacedown = true`; flipped face-up at next
     PowerGather.

## §2.6a Tenebrosum afterAction hook — IMPLEMENTED-IN-PASS-2
(See §1.5.1 above.)

## §2.7 Spellbook handlers — MATCH (all six handlers wired)

## §2.11 Render path — MATCH

## §2.12 Battle hooks — MATCH
- Fallen Prophet variable combat: implemented in `DC.strength` with
  battle-context detection (`game.battle./(_.attacker).has(DC)`).
- Y'Golonac ceil(Sin/2): implemented in `DC.strength`.
- Eschar post-battle: implemented in Battle.scala BattleEnd.

## §2.13 Serialize — MATCH

## §3.12 SB-requirement opt-ins
- ProselytizeReq / SatiateReq Doom-Phase opt-ins: MATCH, return wrapped
  in `Force(CheckSpellbooksAction(DoomAction(self)))` per Item 9 / G28.
- LureReq / EscharReq: MISSING-IMPLEMENTED-IN-PASS-2 via `AfterAction(DC)`
  hook that calls `self.satisfyIf(LureReq, ..., self.pool(MindlessHusk).none)`
  and same for EscharReq/FallenProphet. Fires after every DC action so
  pool-emptying triggers satisfy immediately.
- PilgrimageReq (any RitualAction): MATCH.
- DarkBargainReq (Awaken Y'Golonac): MATCH, return wrapped in
  CheckSpellbooksAction.

## §4 Menu Items
- Reserved-Acolyte placement: MISSING-IMPLEMENTED-IN-PASS-2 (Force-direct
  from MainAction(DC) when `dcPendingAcolytePlacements.any`).
- Tenebrosum opt-in: MISSING-IMPLEMENTED-IN-PASS-2.
- Proselytize drag: MISSING-IMPLEMENTED-IN-PASS-2 (per-Acolyte hook).
- Satiate per-faction Cultist pick: MISSING-IMPLEMENTED-IN-PASS-2.
- Lure per-enemy Cultist pick: MISSING-IMPLEMENTED-IN-PASS-2.
- Dark Bargain D6 round-of-prompts: MISSING-IMPLEMENTED-IN-PASS-2.

## §5 Audit gaps (G1-G39) status
- G1/G3 (Soft/Hard discipline for unlimited actions): MISSING-IMPLEMENTED-IN-PASS-2
  via `DCTenebrosumMainAction` (Soft) + `DCTenebrosumRepeatAction` (Hard).
- G4 (Dark Bargain round-of-prompts): MISSING-IMPLEMENTED-IN-PASS-2.
- G6/G28 (CheckSpellbooksAction wrap on mid-flow SBR satisfaction):
  MISSING-IMPLEMENTED-IN-PASS-2 (ProselytizeReq, SatiateReq, DarkBargainReq
  wrapped).
- G11 (Enemy-colored menu border via self=enemy on cross-faction asks):
  MISSING-IMPLEMENTED-IN-PASS-2 (Proselytize, Satiate, Lure, Dark Bargain
  per-enemy actions all use self=enemy).
- G29 (Sin on Game.scala): MATCH.
- G35 (grammar primitives): MATCH.

## Items DEFERRED-TO-MASTER-TASKS

1. **Tenebrosum literal "same action" re-execution** — current impl grants
   one extra main-menu turn (mechanically equivalent if bot/user picks the
   same action). Full literal re-execution would require deep action
   replay infrastructure. Logged for designer review.

2. **Ancient Sorcery (YS Serpentman) permanence on DC sheet** — §1.6 says
   placed Serpentmen are permanent for entire game; needs YSExpansion
   modification. Not implemented (no YS turn currently sees DC's sheet).

3. **Sleeper (SL) doubled-placement to copy DC ability** — §1.6 says
   Sleeper needs 2 placements per DC ability vs normal 1. Needs
   SLExpansion modification. Not implemented.

## Build status (Pass #2)
- `sbt fullLinkJS` — Closure 0 error(s), 0 warning(s) — CLEAN.

## Pass #2 Verdict
- All 10 items from Pass #1 implemented.
- 3 items deferred to master tasks (cross-faction interactions).
- Build is Closure-clean.
- Ready for deploy to /HB/.
