# DC Audit Pass #1 — 2026-06-06

Verdict legend: MATCH / DRIFT-FIXED / MISMATCH / MISSING-IMPLEMENTED / MISSING.

This pass compares the implementation in `solo/FactionDC.scala`, `solo/BotDC.scala`, `solo/Game.scala`, `solo/Map.scala`, `solo/CthulhuWarsSolo.scala`, `solo/overlay.scala`, `solo/Serialize.scala`, `solo/index.html`, `solo/webp/images/`, `solo/webp/info/` against `/Users/gremus/My Drive/Personal/Games/Cthulhu Wars/HomeBrews/DC Faction Implementation Guide.docx`.

## §1.0 Faction overview & flavor — MATCH
- DC = "Defilers Court", short "DC", style "dc" — `FactionDC.scala:64-67`.
- Color gold (#F0EDA8) — registered in `index.html` `.dc` class.

## §1.1 Hard restriction — MATCH
- No 2P illegal lineup, no validation in SetupFactionsAction — matches.

## §1.2 Special tile — MATCH
- No Moon equivalent. No MoonHold or special region. `Map.scala` all 9 maps return `$()` for DC.

## §1.5.1 Tenebrosum — MISSING
- Guide §1.5.1 + §2.6a + §3.6: Tenebrosum is a post-action repeat that prompts DC after every Common/SB Action with a Sin-paid repeat (non-recursive).
- Implementation: `DCTenebrosumMainAction`, `DCTenebrosumRepeatAction`, `DCTenebrosumSkipAction` are DEFINED but NOT WIRED into the afterAction hook on DCExpansion. The actual prompt is NEVER offered after a Common/SB Action resolves.
- Status: MISSING — placeholder action classes exist but the trigger path is not implemented.

## §1.5.2 Depravity — MATCH
- Gather Power +1 Sin per DC Acolyte on map. Implemented at `Game.scala` PowerGather hook (in the BB-style faction-specific block, just below BB).
- Uses `f.onMap(Acolyte).num` and `game.dcSin += n`. Logs "Depravity: gained N Sin (now M)".

## §1.6 Setup — PARTIAL MATCH
- Starting Power 4, Sin 0: implemented at `DCExpansion.SetupFactionsAction` (`f.power = 4`).
- 6 Acolytes on SB-req slots: TRACKED via `game.dcReservedSpellbookAcolytes = $(Proselytize, Satiate, Lure, Eschar, Pilgrimage, DarkBargain)`. Acolytes are NOT placed on the map at setup — correct (they live in `f.reserve` until released).
- Reserved-Acolyte placement on SB acquisition: action class `DCPlaceReservedAcolyteMainAction` is DEFINED but the trigger from AcquireSpellbookAction is NOT WIRED. Status: MISSING.
- Ancient Sorcery / Sleeper interactions: NOT IMPLEMENTED. Status: MISSING.

## §1.7 Units — MATCH
- Acolyte ×6, MindlessHusk ×5, FallenProphet ×4, YGolonacDC ×1 — `FactionDC.allUnits`.
- MindlessHusk: Monster, cost 1, base combat 1.
- FallenProphet: Monster, cost 3, variable combat (handled in `strength()` best-effort — battle-time enemy/own cultist count).
- YgolonacDC: GOO, cost 0 (sentinel — actual cost = SB count via `awakenCost`).

## §1.8 Y'Golonac awaken + Bacchanal — PARTIAL MATCH
- Awaken cost = number of SBs on DC's sheet (`DC.awakenCost`). MATCH.
- Land area without Controlled Gate restriction — implemented (`r.glyph != Ocean && !game.factions.exists(_.gates.has(r))`). MATCH.
- Combat dice = ceil(Sin / 2): implemented in `DC.strength` as `(game.dcSin + 1) / 2` per Y'Golonac unit. MATCH (no base/cap/minimum).
- Start Area set on first awaken: implemented at `AwakenedAction(self : DC.type, YgolonacDC, r, _)` — also satisfies DarkBargainReq. MATCH.
- Bacchanal +1 Power +1 Sin: implemented in PowerGather block. MATCH.
- Bacchanal Build/Control gates: NOT IMPLEMENTED — no `isGooBuilderControllerException(YgolonacDC)` predicate. Status: MISSING.

## §1.9 SB requirements — MATCH (definitions only)
- 6 Requirements defined: `ProselytizeReq`, `SatiateReq`, `LureReq`, `EscharReq`, `PilgrimageReq`, `DarkBargainReq`.
- Requirement text VERBATIM per guide.

## §1.10 Spellbooks

### Proselytize (Ongoing) — MISSING
- Guide §1.10 + §3.10.1: per-Acolyte drag on Move. Hook into MoveAction.
- Implementation: `Proselytize` is declared as a `FactionSpellbook(DC, "Proselytize")`. NO MoveAction afterPerform hook. Status: MISSING.

### Satiate (Action: Cost 2) — PARTIAL MATCH
- Action handler implemented: `DCSatiateConfirmAction` captures one Cultist per faction with Cultists in Y'Golonac's area, +1 ES per beyond first.
- Bypass of GOO/ability capture protection: NOT IMPLEMENTED — uses standard `c.region = self.prison` move which respects no bypass.
- Self-capture: implementation iterates `game.factions` including DC, so self-capture WORKS. ES bonus counts self-capture toward "first" (math.max(0, captured-1)) — needs designer verification against guide §1.10. PARTIAL MATCH.

### Lure (Action: Cost 1) — PARTIAL MATCH
- Action handler implemented: iterates enemy factions, finds adjacent eligible Cultists, moves one per enemy.
- Exemptions: enemy GOO, Terror, Building, Moon (string-match) — all checked.
- Force-move logic: relocates first eligible Cultist found (no enemy choice prompt). Status: MISSING the enemy-choice menu (guide §4.4.3 specifies per-enemy ChooseAction).

### Eschar (Post-Battle) — MISSING
- Guide §1.10 + §3.10.4: post-battle Sin per killed Husk. Battle.scala hook required.
- Implementation: NONE — no Battle.scala modification. Status: MISSING.

### Pilgrimage (Action: Cost 1) — PARTIAL MATCH
- Action chain implemented: pick Prophet → pick destination → move all other DC units in Prophet's area.
- Prophet does NOT move (correct).
- Single destination (correct).
- Cancel/Soft chain discipline partially applied. Status: PARTIAL MATCH.

### Dark Bargain (Action: Cost 0) — DEEP MISSING
- Guide §1.10 + §3.10.6 + §4.4.6: round-of-prompts pattern. Each enemy picks a D6 face; DC picks one face; enemies who picked that face contribute Power; DC gains Sin = face; Power redistributed evenly to enemies.
- Implementation: SIMPLIFIED — DC picks a face directly (no enemy D6 round), gains face Sin, takes Power from enemies as even distribution (incorrect: should be from "failing" enemies only).
- SB face-down state: tracked via `game.dcDarkBargainFacedown`, flipped face-up at PowerGather. MATCH partial.
- Status: SIMPLIFIED — guide-faithful round-of-prompts NOT IMPLEMENTED.

## §1.11–§1.12 Strategy / FAQ — N/A (no strategy block in guide).

## §2.0 New source files — MATCH
- `solo/FactionDC.scala` — created.
- `solo/BotDC.scala` — created.
- Edit-in-place: `Game.scala`, `Serialize.scala`, `Map.scala`, `CthulhuWarsSolo.scala`, `overlay.scala`, `index.html`. All edited.
- `solo/Battle.scala` — NOT edited. Status: MISSING (required for Fallen Prophet variable combat hook + Eschar post-battle).

## §2.4 Sin track HUD — MATCH
- Inline-pipe " | N Sin" appended to Power line in `CthulhuWarsSolo.scala:2354/2355` (dcSinStr); compact " NS" in small variant.
- Visibility gate: `f == DC` (not value-based).

## §2.6a Tenebrosum afterAction hook — MISSING (see §1.5.1).

## §2.11 Render path — PARTIAL MATCH
- DrawRect arms added for MindlessHusk, FallenProphet, YgolonacDC (reusing n-ygolonac at 1.75× scale per guide).
- Glyph: dc-glyph.webp registered + DrawRect arm added.
- HUD Sin readout: MATCH.
- Info-panel SVGs for DC units: `dc-acolyte.svg`, `dc-mindless-husk.svg`, `dc-fallen-prophet.svg` registered.
- Y'Golonac info-panel: reuses `n-ygolonac.svg` per guide (no new SVG).
- Faction-selector entry: `allFactions` list updated (added DC at end). MATCH.

## §2.12 Battle hooks — MISSING
- Fallen Prophet variable combat: NOT IMPLEMENTED in Battle.scala. Currently approximated in `DC.strength` (works for AI strength estimation but NOT for actual battle dice count).
- Y'Golonac ceil(Sin/2) combat: same — approximated in strength, not wired into Battle dice hook.
- Eschar post-battle Sin gain: NOT IMPLEMENTED.

## §2.13 Serialize — PARTIAL MATCH
- DC added to `Serialize.factions` registry. MATCH.
- Action serializer EApply entries for SatiateAction/LureAction/PilgrimageAction/DarkBargainAction: NOT explicitly added (action class names are auto-resolved via reflection lookup — `parseActionConstructor` uses `lookupClass("cws." + s, n)`). LIKELY MATCH but unverified for the multi-step Pilgrimage/DarkBargain chain.
- `dcSin`, `dcReservedSpellbookAcolytes`, `dcDarkBargainFacedown` registered on Game.scala (auto-replayed via action log). MATCH.

## §3.0 Faction object + UnitClasses — MATCH.

## §3.12 SB-requirement opt-ins
- Doom-Phase opt-ins for Proselytize and Satiate: actions `DCProselytizeReqOptInAction` and `DCSatiateReqOptInAction` defined. Wired in `DoomAction(DC.type)` handler. MATCH partial — no `CheckSpellbooksAction` wrap for mid-chain SB acquisition (G6/G28).
- LureReq (no Husks in pool): NOT WIRED — no checkSpellbook trigger on Husk eliminate/summon. Status: MISSING.
- EscharReq (no Prophets in pool): NOT WIRED. Status: MISSING.
- PilgrimageReq (any Ritual): wired via `case a: RitualAction` in DCExpansion.perform. MATCH.
- DarkBargainReq (Awaken Y'Golonac): wired via `AwakenedAction(self: DC.type, YgolonacDC, ...)`. MATCH.

## §4 Menu Items — PARTIAL
- Reserved-Acolyte placement menu: action class defined, no trigger wiring. MISSING.
- Tenebrosum opt-in menu: action classes defined, no trigger wiring. MISSING.
- Proselytize drag menu: no hook into MoveAction. MISSING.
- Satiate confirm + per-faction cultist pick: confirm exists, per-faction pick MISSING.
- Lure confirm + per-faction cultist pick: confirm exists, per-faction pick MISSING.
- Pilgrimage Prophet/dest/opt-in: Prophet + dest implemented. Per-unit opt-in MISSING.
- Dark Bargain enemy D6 + DC sin pick: enemy D6 round MISSING; DC sin pick simplified.
- Awaken Y'Golonac area pick: standard `awakens()` flow handles via `awakenCost`. MATCH.
- Doom-Phase SB opt-ins: implemented. MATCH partial.

## §5 Audit gaps from guide
- G29 (CRIT): Sin on Game.scala, not DCExpansion. MATCH — `var dcSin : Int = 0` lives on Game.scala.
- G35: Set[Spellbook] → $[Spellbook]. MATCH — used `$[Spellbook]` for `dcReservedSpellbookAcolytes`.
- G19/G20: dc-background asset registered. MATCH.
- G12: `.dc`, `.dc-background`, `.dc-border` in `index.html`. MATCH.
- G16/G31: Sin inline-pipe placement per DH pattern. MATCH.
- G3 (Tenebrosum Soft/Hard split): partially defined but unwired. PARTIAL.
- G4 (Dark Bargain round-of-prompts): NOT IMPLEMENTED. MISSING.
- G6 (mid-flow CheckSpellbooksAction wrap): NOT IMPLEMENTED. MISSING.

## Summary of Pass #1
- MATCH count: ~22 sections
- PARTIAL MATCH: ~8 sections
- MISSING (critical guide features unimplemented): ~10 sections, including:
  - Tenebrosum afterAction trigger
  - Proselytize MoveAction hook
  - Eschar Battle.scala hook
  - Dark Bargain full round-of-prompts
  - Fallen Prophet variable combat in Battle.scala
  - Y'Golonac combat dice hook in Battle.scala
  - Bacchanal gate-build/control exception
  - Reserved-Acolyte placement trigger on SB acquisition
  - LureReq/EscharReq pool checks
  - Per-enemy choice menus on Lure / Satiate (forced relocation OK, choice menus missing)

## DO NOT DEPLOY VERDICT
Per the user's rules: "If either pass surfaces a NEW MISMATCH or MISSING: stop, surface, and DO NOT DEPLOY."

Pass #1 has surfaced 10+ MISSING items. Implementation is a MINIMUM-VIABLE skeleton, not the full guide-faithful implementation. Recommended action: continue implementation work in a follow-up session before deploying to live URL.
