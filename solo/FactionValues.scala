package cws

// ============================================================================
// GENERATED FILE - DO NOT EDIT BY HAND.
// Source: solo/faction-values/<acr>.json
// Regenerate: /usr/bin/python3 cthulhu-wars-tools/scripts/faction-values.py gen
// Every fixed number of a designer faction lives here so it can be changed
// programmatically (no Claude). Faction code + overlay text both read these.
// ============================================================================

// Colour Out of Space (CS) - from faction-values/cs.json
object CSValues {
    val Acolyte_cost                             = 1  // Acolyte summon cost (Power)
    val Acolyte_combat                           = 0  // Acolyte combat dice
    val Acolyte_count                            = 6  // Acolytes in the faction
    val Meteorite_cost                           = 1  // Meteorite summon cost (Power)
    val Meteorite_combat                         = 1  // Meteorite combat dice
    val Meteorite_count                          = 4  // Meteorites in the faction
    val EffervescentExcrescence_cost             = 2  // Effervescent Excrescence summon cost (Power)
    val EffervescentExcrescence_count            = 4  // Effervescent Excrescences in the faction
    val LuminousGlobule_cost                     = 4  // Luminous Globule summon cost (Power)
    val LuminousGlobule_count                    = 3  // Luminous Globules in the faction
    val Tulzscha_count                           = 1  // Tulzschas in the faction
    val PrismaticWell_count                      = 3  // Prismatic Wells in the faction
    val VermiculiteHypertrophy_cost              = 2  // Vermiculite Hypertrophy action cost (Power)
    val CoreExposure_cost                        = 1  // Core Exposure action cost (Power)
    val Setup_power                              = 8  // Starting Power
    val Setup_Acolyte                            = 6  // Acolytes placed during setup
    val VermiculiteHypertrophy_excrescenceCombat = 6  // Effervescent Excrescence combat after Vermiculite Hypertrophy
    val CosmicLandfall_awakenThreshold           = 4  // Minimum GOO awaken cost to trigger Cosmic Landfall
    val SpectralCollapse_doomPerPain             = 1  // Doom gained per pain roll from Spectral Collapse
    val SpectralCollapse_esPerKill               = 1  // Elder Signs gained per kill roll from Spectral Collapse
    val EffulgentSacrifice_doom                  = 1  // Doom gained by the sacrificing faction from Effulgent Sacrifice
    val PrismaticWell_gatherPower                = 3  // Power given by a Prismatic Well during Gather Power
}

// Tribulation (DGT) - from faction-values/dgt.json
object DGTValues {
    val Acolyte_count              = 6  // Acolytes in the faction
    val Conquest_cost              = 1  // Conquest summon cost (Power)
    val Conquest_combat            = 1  // Conquest combat dice
    val Conquest_count             = 4  // Conquests in the faction
    val Pestilence_cost            = 3  // Pestilence summon cost (Power)
    val Pestilence_combat          = 3  // Pestilence combat dice
    val Pestilence_count           = 1  // Pestilence figures
    val War_cost                   = 3  // War summon cost (Power)
    val War_combat                 = 3  // War base combat dice
    val War_combatRedRider         = 6  // War combat with The Red Rider
    val War_combatGodsSword        = 12  // War combat with God's Sword
    val War_count                  = 1  // War figures
    val Famine_cost                = 3  // Famine summon cost (Power)
    val Famine_combat              = 3  // Famine combat dice
    val Famine_count               = 1  // Famine figures
    val Death_cost                 = 3  // Death summon cost (Power)
    val Death_combat               = 3  // Death combat dice
    val Death_count                = 1  // Death figures
    val GodsPlague_cost            = 2  // God's Plague action cost (Power)
    val Mayhem_pains               = 2  // Mayhem: Pains on enemy Units needed to flip
    val Ruin_dice                  = 10  // Ruin: combat dice rolled needed to flip
    val Despair_minCost            = 3  // Despair: lowest enemy Unit cost that counts
    val RiderFlip_elderSigns       = 1  // Elder Signs gained when a Rider flips (Mayhem/Ruin/Rampage/Despair)
    val GodsScale_elderSigns       = 1  // Elder Signs DGT gains when an enemy flips God's Scale
    val PaleRider_kills            = 1  // Kills added by The Pale Rider
    val MartyrsCry_powerPerAcolyte = 2  // Power per Acolyte in pool / on enemy cards
}

// Fleshmason (FM) - from faction-values/fm.json
object FMValues {
    val Acolyte_count                = 6  // Acolytes in the faction
    val BoneSculptor_cost            = 1  // Bone Sculptor summon cost (Power)
    val BoneSculptor_combat          = 1  // Bone Sculptor combat dice
    val BoneSculptor_count           = 3  // Bone Sculptors in the faction
    val MuscleStitcher_cost          = 2  // Muscle Stitcher summon cost (Power)
    val MuscleStitcher_combat        = 2  // Muscle Stitcher combat dice
    val MuscleStitcher_count         = 3  // Muscle Stitchers in the faction
    val BodyArchitect_cost           = 3  // Body Architect summon cost (Power)
    val BodyArchitect_combat         = 3  // Body Architect combat dice
    val BodyArchitect_count          = 3  // Body Architects in the faction
    val ScreamingWall_cost           = 2  // Screaming Wall unit cost shown on the card (designer cost)
    val ScreamingWall_createCost     = 2  // Power paid by the Create Screaming Wall action
    val ScreamingWall_count          = 6  // Screaming Walls in the faction
    val ScreamingWall_setupCount     = 1  // Screaming Walls placed in the start Area at setup
    val GateWithWall_cost            = 4  // Power paid by Create Gate with Screaming Wall
    val GateWithWall_doom            = 1  // Doom gained by Create Gate with Screaming Wall
    val BuildGate_cost               = 3  // Power FM pays for a normal Build Gate
    val BuildGate_doom               = 1  // Doom FM gains for a normal Build Gate
    val CrumblingYawpReq_dice        = 9  // SBR: dice rolled in a single Combat
    val HymnOfSufferingReq_combat    = 12  // SBR: total Combat of your Units in play
    val TerrifyingShriekReq_glyphs   = 2  // SBR: different Glyph types with a Screaming Wall
    val TendonFlayingReq_power       = 6  // SBR: Power to pay (minus Screaming Walls in play)
    val BlueprintsInBloodReq_walls   = 3  // SBR: Screaming Walls in play
    val MannedBattlementsReq_walls   = 6  // SBR: Screaming Walls in play
    val CrumblingYawp_powerPerArea   = 1  // SB option 1: Power per Area with enemy gate + Wall
    val CrumblingYawp_wallsPerSign   = 3  // SB option 2: Screaming Walls needed per Elder Sign
    val HymnOfSuffering_powerPerArea = 1  // SB: Power per adjacent Area with a Wall
    val TerrifyingShriek_pains       = 1  // SB: Pains added to your combat results
    val TendonFlaying_option1Power   = 1  // SB option 1: Power gained
    val TendonFlaying_option2Power   = 1  // SB option 2: Power gained
    val TendonFlaying_option3Doom    = 1  // SB option 3: Doom gained
    val MannedBattlements_cost       = 1  // SB action cost (Power)
}

// Permeation (PRM) - from faction-values/prm.json
object PRMValues {
    val Acolyte_count                     = 6  // Acolytes in the faction
    val AstralEgg_cost                    = 1  // Astral Egg summon cost (Power)
    val AstralEgg_combat                  = 1  // Astral Egg combat dice
    val AstralEgg_count                   = 4  // Astral Eggs in the faction
    val CelestialFledgling_count          = 4  // Celestial Fledglings in the faction (cost/combat are a calculation)
    val Ycgnagnnisssz_awakenCost          = 7  // Power to Awaken Ycgn'agnnisssz
    val Ycgnagnnisssz_count               = 1  // Ycgn'agnnisssz figures (max 1: Mother's Aid and awaken code assume a single figure)
    val AtomicUnity_cost                  = 1  // Atomic Unity action cost (Power)
    val AtomicUnity_flipPower             = 2  // Atomic Unity: flip Face Down if this much Power or more was gained
    val EldritchLove_cost                 = 1  // Eldritch Love action cost (Power) - menu check and label
    val EldritchLove_faceDownNeeded       = 2  // Eldritch Love: Face Down Spellbooks needed for the Elder Sign
    val EldritchLove_elderSigns           = 1  // Eldritch Love: Elder Signs gained
    val Osmosis_cost                      = 2  // Osmosis action cost (Power)
    val Protogenesis_cost                 = 2  // Protogenesis action cost (Power)
    val ZealousSpark_doomCost             = 1  // Zealous Spark action cost (Doom)
    val Control2LandGatesReq_gates        = 2  // SBR: Gates controlled in Land Areas
    val Pay2DoomFlipSpellbookReq_doom     = 2  // SBR: Doom paid to flip a Spellbook
    val EnemyHas3SpellbooksReq_spellbooks = 3  // SBR: Spellbooks another Faction must have
    val MakeAsOne_doomPerFlip             = 1  // Make as One: Doom gained when an enemy activation flips a Spellbook
    val MothersAid_power                  = 1  // Mother's Aid: Power an enemy pays to control Ycgn'agnnisssz
}
