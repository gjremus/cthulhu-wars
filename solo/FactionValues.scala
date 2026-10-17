package cws

// ============================================================================
// GENERATED FILE - DO NOT EDIT BY HAND.
// Source: solo/faction-values/<acr>.json
// Regenerate: /usr/bin/python3 cthulhu-wars-tools/scripts/faction-values.py gen
// Every fixed number of a designer faction lives here so it can be changed
// programmatically (no Claude). Faction code + overlay text both read these.
// ============================================================================

// Designer path mappings for runtime live value overrides
object FactionValuesMapping {
    val designerMappings: Map[String, Map[String, String]] = Map(
        "CS" -> Map(
            "sb.rows.csb00002.cost" -> "VermiculiteHypertrophy_cost",
            "sb.rows.csb00002.text (6 combat)" -> "VermiculiteHypertrophy_excrescenceCombat",
            "sb.rows.csb00003.text (4 power or more)" -> "CosmicLandfall_awakenThreshold",
            "sb.rows.csb00004.text (1 ES for kills)" -> "SpectralCollapse_esPerKill",
            "sb.rows.csb00004.text (1 doom for pains)" -> "SpectralCollapse_doomPerPain",
            "sb.rows.csb00005.text (Gain 1 doom)" -> "EffulgentSacrifice_doom",
            "sb.rows.csb00006.cost" -> "CoreExposure_cost",
            "ufa.text (3 power during gather power)" -> "PrismaticWell_gatherPower",
            "units.rows.00oaxmlf.qty" -> "PrismaticWell_count",
            "units.rows.csun0001.qty" -> "Acolyte_count",
            "units.rows.csun0002.cost" -> "Meteorite_cost",
            "units.rows.csun0002.dice" -> "Meteorite_combat",
            "units.rows.csun0002.qty" -> "Meteorite_count",
            "units.rows.csun0003.cost" -> "EffervescentExcrescence_cost",
            "units.rows.csun0003.dice" -> "EffervescentExcrescence_combat",
            "units.rows.csun0003.qty" -> "EffervescentExcrescence_count",
            "units.rows.csun0004.cost" -> "LuminousGlobule_cost",
            "units.rows.csun0004.qty" -> "LuminousGlobule_count",
            "units.rows.csun0005.awakenPower" -> "Tulzscha_awakenCost",
            "units.rows.csun0005.qty" -> "Tulzscha_count",
        ),
        "DGT" -> Map(
            "sb.rows.m3eWVNBs.costB" -> "GodsPlague_cost",
            "units.rows.g8O850uw.qty" -> "Acolyte_count",
            "units.rows.qvhv0d59.cost" -> "Death_cost",
            "units.rows.qvhv0d59.dice" -> "Death_combat",
            "units.rows.qvhv0d59.qty" -> "Death_count",
            "units.rows.qww12fzx.cost" -> "Famine_cost",
            "units.rows.qww12fzx.dice" -> "Famine_combat",
            "units.rows.qww12fzx.qty" -> "Famine_count",
            "units.rows.rf2wp9q1.cost" -> "War_cost",
            "units.rows.rf2wp9q1.dice" -> "War_combat",
            "units.rows.rf2wp9q1.qty" -> "War_count",
            "units.rows.vcAgzUS2.cost" -> "Conquest_cost",
            "units.rows.vcAgzUS2.dice" -> "Conquest_combat",
            "units.rows.vcAgzUS2.qty" -> "Conquest_count",
            "units.rows.wrXwlATc.cost" -> "Pestilence_cost",
            "units.rows.wrXwlATc.dice" -> "Pestilence_combat",
            "units.rows.wrXwlATc.qty" -> "Pestilence_count",
        ),
        "FBE" -> Map(
            "units.rows.m9m23bqh.qty" -> "Acolyte_count",
            "units.rows.mlbgdkt6.cost" -> "FungalThrall_cost",
            "units.rows.mlbgdkt6.dice" -> "FungalThrall_combat",
            "units.rows.mlbgdkt6.qty" -> "FungalThrall_count",
            "units.rows.mz7tc21j.awakenPower" -> "Byagoona_awakenCost",
            "units.rows.mz7tc21j.qty" -> "Byagoona_count",
        ),
        "FM" -> Map(
            "sb.rows.P5xvUlGE.cost" -> "MannedBattlements_cost",
            "sbr.rows.4Eec7O6j.num" -> "SBR2Req_num",
            "sbr.rows.BKlmq0eO.num" -> "SBR1Req_num",
            "sbr.rows.eeSN2BEc.num" -> "SBR5Req_num",
            "sbr.rows.nAcx996R.num" -> "SBR6Req_num",
            "sbr.rows.q-KAJEVp.num" -> "SBR3Req_num",
            "sbr.rows.xap0JS7H.num" -> "SBR4Req_num",
            "setup.units.a1b2c3d4.qty" -> "ScreamingWall_setupCount",
            "units.rows.NqnUIibv.cost" -> "BoneSculptor_cost",
            "units.rows.NqnUIibv.dice" -> "BoneSculptor_combat",
            "units.rows.NqnUIibv.qty" -> "BoneSculptor_count",
            "units.rows.e5f6g7h8.cost" -> "BodyArchitect_cost",
            "units.rows.e5f6g7h8.dice" -> "BodyArchitect_combat",
            "units.rows.e5f6g7h8.qty" -> "BodyArchitect_count",
            "units.rows.hbTu1pzr.qty" -> "Acolyte_count",
            "units.rows.i9j0k1l2.cost" -> "ScreamingWall_cost",
            "units.rows.i9j0k1l2.qty" -> "ScreamingWall_count",
            "units.rows.iR1pmQZ_.cost" -> "MuscleStitcher_cost",
            "units.rows.iR1pmQZ_.dice" -> "MuscleStitcher_combat",
            "units.rows.iR1pmQZ_.qty" -> "MuscleStitcher_count",
        ),
        "PRM" -> Map(
            "sb.rows.L9gu0vGJ.cost" -> "ZealousSpark_doomCost",
            "sb.rows.T-iXgz5Y.cost" -> "Osmosis_cost",
            "sb.rows.bKgvlWki.cost" -> "EldritchLove_cost",
            "sb.rows.oMygUypd.cost" -> "AtomicUnity_cost",
            "sb.rows.zYRp7m6e.cost" -> "Protogenesis_cost",
            "units.rows.ZUV9Z7MR.cost" -> "AstralEgg_cost",
            "units.rows.ZUV9Z7MR.dice" -> "AstralEgg_combat",
            "units.rows.ZUV9Z7MR.qty" -> "AstralEgg_count",
            "units.rows.psohIigk.qty" -> "Acolyte_count",
            "units.rows.puHilxia.qty" -> "CelestialFledgling_count",
            "units.rows.yg4kP9qZ.awakenPower" -> "Ycgnagnnisssz_awakenCost",
            "units.rows.yg4kP9qZ.qty" -> "Ycgnagnnisssz_count",
        ),
        "SW" -> Map(
            "ae.rows.1y8bfa91.qty" -> "Hunger_actionPhaseGain",
            "ae.rows.Y8NGqHCs.qty" -> "Hunger_awakenGain",
            "ae.rows.mq5x48fu.qty" -> "Hunger_killedGain",
            "ae.rows.rdnrwvr2.qty" -> "Hunger_bottomlessPitSpend",
            "ae.rows.t9erkfnh.qty" -> "Hunger_painedGain",
            "ae.rows.xm6rstsh.qty" -> "Hunger_discountedRitualGain",
            "sb.rows.5EwQF2ol.text (Divide the current value of Hunger Track by 3, rounded down)" -> "Starvation_esDivisor",
            "sb.rows.dVPTSOJ1.text (Action: Cost 1)" -> "BottomlessPit_cost",
            "sb.rows.dVPTSOJ1.text (Sphinx Worshippers gain 1 power and 1 Elder Sign when an opponent uses this ability)" -> "BottomlessPit_swBonusES",
            "sb.rows.dVPTSOJ1.text (gain elder signs equal to its printed cost - 1)" -> "BottomlessPit_costMinus",
            "sb.rows.guDpNAbs.effect" -> "MummiesCurse_pains",
            "sb.rows.guDpNAbs.effect2" -> "LifeFromDeath_cultists",
            "sb.rows.xLzQzD_m.cost" -> "GhoulQueen_cost",
            "sbr.rows.JUkyIx9V.num" -> "HungerReq4_threshold",
            "sbr.rows.hPZlRNxH.num" -> "HungerReq2_threshold",
            "setup.aeStart" -> "Setup_aeStart",
            "setup.power" -> "Setup_power",
            "setup.units.eYZ6L7qA.qty" -> "Setup_Acolyte",
            "ufa.effect" -> "Kidnap_minMummies",
            "units.rows.bs8MvPnK.awakenPower" -> "BlackSphinx_awakenBaseCost",
            "units.rows.bs8MvPnK.awakenReq (Pay 3 less the monster is kidnapped)" -> "BlackSphinx_kidnapDiscount",
            "units.rows.bs8MvPnK.qty" -> "BlackSphinx_count",
            "units.rows.bs8MvPnK.resultsCalc (Adds pains = 1 + current Hunger Track)" -> "BlackSphinx_painsBase",
            "units.rows.kf9FMV9m.cost" -> "HybridMummy_cost",
            "units.rows.kf9FMV9m.dice" -> "HybridMummy_combat",
            "units.rows.kf9FMV9m.qty" -> "HybridMummy_count",
            "units.rows.pgBdiXoY.cost" -> "PharoahKephren_cost",
            "units.rows.pgBdiXoY.dice" -> "PharoahKephren_combat",
            "units.rows.pgBdiXoY.qty" -> "PharoahKephren_count",
            "units.rows.qn1EvCxR.cost" -> "QueenNitokris_cost",
            "units.rows.qn1EvCxR.qty" -> "QueenNitokris_count",
            "units.rows.qn1EvCxR.resultsCalc (2+Current Hunger Track)" -> "QueenNitokris_combatBase",
            "units.rows.uobjhDTS.kills" -> "Acolyte_kills",
            "units.rows.uobjhDTS.pains" -> "Acolyte_pains",
            "units.rows.uobjhDTS.qty" -> "Acolyte_count",
        ),
        "XSS" -> Map(
            "sb.rows.xsb00004.cost" -> "Tsunami_cost",
            "setup.power" -> "Setup_power",
            "setup.units.xsu00001.qty" -> "Setup_Acolyte",
            "units.rows.xun00001.qty" -> "Acolyte_count",
            "units.rows.xun00002.cost" -> "AmphibianCrawler_cost",
            "units.rows.xun00002.dice" -> "AmphibianCrawler_combat",
            "units.rows.xun00002.qty" -> "AmphibianCrawler_count",
            "units.rows.xun00003.cost" -> "Twister_cost",
            "units.rows.xun00003.dice (Land)" -> "Twister_combatLand",
            "units.rows.xun00003.dice (other)" -> "Twister_combatOther",
            "units.rows.xun00003.qty" -> "Twister_count",
            "units.rows.xun00004.cost" -> "EyeOfTheStorm_cost",
            "units.rows.xun00004.dice (Sea)" -> "EyeOfTheStorm_combatSea",
            "units.rows.xun00004.dice (other)" -> "EyeOfTheStorm_combatOther",
            "units.rows.xun00004.qty" -> "EyeOfTheStorm_count",
            "units.rows.xun00005.awakenPower" -> "Petrichor_awakenCost",
            "units.rows.xun00005.qty" -> "Petrichor_count",
        ),
    )
}

// Colour Out of Space (CS) - from faction-values/cs.json
object CSValues {
    def Acolyte_count                            = LiveValues.get("CS", "Acolyte_count", 6)  // Acolytes in the faction
    def Meteorite_cost                           = LiveValues.get("CS", "Meteorite_cost", 1)  // Meteorite summon cost (Power)
    def Meteorite_combat                         = LiveValues.get("CS", "Meteorite_combat", 0)  // Meteorite combat dice
    def Meteorite_count                          = LiveValues.get("CS", "Meteorite_count", 4)  // Meteorites in the faction
    def EffervescentExcrescence_cost             = LiveValues.get("CS", "EffervescentExcrescence_cost", 2)  // Effervescent Excrescence summon cost (Power)
    def EffervescentExcrescence_count            = LiveValues.get("CS", "EffervescentExcrescence_count", 4)  // Effervescent Excrescences in the faction
    def LuminousGlobule_cost                     = LiveValues.get("CS", "LuminousGlobule_cost", 4)  // Luminous Globule summon cost (Power)
    def LuminousGlobule_count                    = LiveValues.get("CS", "LuminousGlobule_count", 3)  // Luminous Globules in the faction
    def Tulzscha_count                           = LiveValues.get("CS", "Tulzscha_count", 1)  // Tulzschas in the faction
    def PrismaticWell_count                      = LiveValues.get("CS", "PrismaticWell_count", 3)  // Prismatic Wells in the faction
    def VermiculiteHypertrophy_cost              = LiveValues.get("CS", "VermiculiteHypertrophy_cost", 2)  // Vermiculite Hypertrophy action cost (Power)
    def CoreExposure_cost                        = LiveValues.get("CS", "CoreExposure_cost", 1)  // Core Exposure action cost (Power)
    def VermiculiteHypertrophy_excrescenceCombat = LiveValues.get("CS", "VermiculiteHypertrophy_excrescenceCombat", 6)  // Effervescent Excrescence combat after Vermiculite Hypertrophy
    def CosmicLandfall_awakenThreshold           = LiveValues.get("CS", "CosmicLandfall_awakenThreshold", 4)  // Minimum GOO awaken cost to trigger Cosmic Landfall
    def SpectralCollapse_doomPerPain             = LiveValues.get("CS", "SpectralCollapse_doomPerPain", 1)  // Doom gained per pain roll from Spectral Collapse
    def SpectralCollapse_esPerKill               = LiveValues.get("CS", "SpectralCollapse_esPerKill", 1)  // Elder Signs gained per kill roll from Spectral Collapse
    def EffulgentSacrifice_doom                  = LiveValues.get("CS", "EffulgentSacrifice_doom", 1)  // Doom gained by the sacrificing faction from Effulgent Sacrifice
    def PrismaticWell_gatherPower                = LiveValues.get("CS", "PrismaticWell_gatherPower", 3)  // Power given by a Prismatic Well during Gather Power
    def EffervescentExcrescence_combat           = LiveValues.get("CS", "EffervescentExcrescence_combat", 2)  // Effervescent Excrescence combat dice
    def Tulzscha_awakenCost                      = LiveValues.get("CS", "Tulzscha_awakenCost", 5)  // Power to Awaken Tulzscha
}

// Tribulation (DGT) - from faction-values/dgt.json
object DGTValues {
    def Acolyte_count              = LiveValues.get("DGT", "Acolyte_count", 6)  // Acolytes in the faction
    def Conquest_cost              = LiveValues.get("DGT", "Conquest_cost", 1)  // Conquest summon cost (Power)
    def Conquest_combat            = LiveValues.get("DGT", "Conquest_combat", 1)  // Conquest combat dice
    def Conquest_count             = LiveValues.get("DGT", "Conquest_count", 4)  // Conquests in the faction
    def Pestilence_cost            = LiveValues.get("DGT", "Pestilence_cost", 3)  // Pestilence summon cost (Power)
    def Pestilence_combat          = LiveValues.get("DGT", "Pestilence_combat", 3)  // Pestilence combat dice
    def Pestilence_count           = LiveValues.get("DGT", "Pestilence_count", 1)  // Pestilence figures
    def War_cost                   = LiveValues.get("DGT", "War_cost", 3)  // War summon cost (Power)
    def War_combat                 = LiveValues.get("DGT", "War_combat", 3)  // War base combat dice
    def War_combatRedRider         = LiveValues.get("DGT", "War_combatRedRider", 6)  // War combat with The Red Rider
    def War_combatGodsSword        = LiveValues.get("DGT", "War_combatGodsSword", 12)  // War combat with God's Sword
    def War_count                  = LiveValues.get("DGT", "War_count", 1)  // War figures
    def Famine_cost                = LiveValues.get("DGT", "Famine_cost", 3)  // Famine summon cost (Power)
    def Famine_combat              = LiveValues.get("DGT", "Famine_combat", 3)  // Famine combat dice
    def Famine_count               = LiveValues.get("DGT", "Famine_count", 1)  // Famine figures
    def Death_cost                 = LiveValues.get("DGT", "Death_cost", 3)  // Death summon cost (Power)
    def Death_combat               = LiveValues.get("DGT", "Death_combat", 3)  // Death combat dice
    def Death_count                = LiveValues.get("DGT", "Death_count", 1)  // Death figures
    def GodsPlague_cost            = LiveValues.get("DGT", "GodsPlague_cost", 2)  // God's Plague action cost (Power)
    def Mayhem_pains               = LiveValues.get("DGT", "Mayhem_pains", 2)  // Mayhem: Pains on enemy Units needed to flip
    def Ruin_dice                  = LiveValues.get("DGT", "Ruin_dice", 10)  // Ruin: combat dice rolled needed to flip
    def Despair_minCost            = LiveValues.get("DGT", "Despair_minCost", 3)  // Despair: lowest enemy Unit cost that counts
    def RiderFlip_elderSigns       = LiveValues.get("DGT", "RiderFlip_elderSigns", 1)  // Elder Signs gained when a Rider flips (Mayhem/Ruin/Rampage/Despair)
    def GodsScale_elderSigns       = LiveValues.get("DGT", "GodsScale_elderSigns", 1)  // Elder Signs DGT gains when an enemy flips God's Scale
    def PaleRider_kills            = LiveValues.get("DGT", "PaleRider_kills", 1)  // Kills added by The Pale Rider
    def MartyrsCry_powerPerAcolyte = LiveValues.get("DGT", "MartyrsCry_powerPerAcolyte", 2)  // Power per Acolyte in pool / on enemy cards
}

// Faceless Blight (FBE) - from faction-values/fbe.json
object FBEValues {
    def Acolyte_count       = LiveValues.get("FBE", "Acolyte_count", 6)  // Acolytes in the faction
    def FungalThrall_count  = LiveValues.get("FBE", "FungalThrall_count", 10)  // Fungal Thralls in the faction
    def FungalThrall_cost   = LiveValues.get("FBE", "FungalThrall_cost", 2)  // Fungal Thrall summon cost (Power)
    def FungalThrall_combat = LiveValues.get("FBE", "FungalThrall_combat", 2)  // Fungal Thrall combat dice
    def Byagoona_count      = LiveValues.get("FBE", "Byagoona_count", 1)  // Byagoonas in the faction
    def Byagoona_awakenCost = LiveValues.get("FBE", "Byagoona_awakenCost", 8)  // Power to Awaken Byagoona
}

// Fleshmason (FM) - from faction-values/fm.json
object FMValues {
    def Acolyte_count                = LiveValues.get("FM", "Acolyte_count", 6)  // Acolytes in the faction
    def BoneSculptor_cost            = LiveValues.get("FM", "BoneSculptor_cost", 1)  // Bone Sculptor summon cost (Power)
    def BoneSculptor_combat          = LiveValues.get("FM", "BoneSculptor_combat", 1)  // Bone Sculptor combat dice
    def BoneSculptor_count           = LiveValues.get("FM", "BoneSculptor_count", 3)  // Bone Sculptors in the faction
    def MuscleStitcher_cost          = LiveValues.get("FM", "MuscleStitcher_cost", 2)  // Muscle Stitcher summon cost (Power)
    def MuscleStitcher_combat        = LiveValues.get("FM", "MuscleStitcher_combat", 2)  // Muscle Stitcher combat dice
    def MuscleStitcher_count         = LiveValues.get("FM", "MuscleStitcher_count", 3)  // Muscle Stitchers in the faction
    def BodyArchitect_cost           = LiveValues.get("FM", "BodyArchitect_cost", 3)  // Body Architect summon cost (Power)
    def BodyArchitect_combat         = LiveValues.get("FM", "BodyArchitect_combat", 3)  // Body Architect combat dice
    def BodyArchitect_count          = LiveValues.get("FM", "BodyArchitect_count", 3)  // Body Architects in the faction
    def ScreamingWall_cost           = LiveValues.get("FM", "ScreamingWall_cost", 2)  // Screaming Wall unit cost shown on the card (designer cost)
    def ScreamingWall_createCost     = LiveValues.get("FM", "ScreamingWall_createCost", 2)  // Power paid by the Create Screaming Wall action
    def ScreamingWall_count          = LiveValues.get("FM", "ScreamingWall_count", 6)  // Screaming Walls in the faction
    def ScreamingWall_setupCount     = LiveValues.get("FM", "ScreamingWall_setupCount", 1)  // Screaming Walls placed in the start Area at setup
    def GateWithWall_cost            = LiveValues.get("FM", "GateWithWall_cost", 4)  // Power paid by Create Gate with Screaming Wall
    def BuildGate_cost               = LiveValues.get("FM", "BuildGate_cost", 3)  // Power FM pays for a normal Build Gate
    def CrumblingYawpReq_dice        = LiveValues.get("FM", "CrumblingYawpReq_dice", 9)  // SBR: dice rolled in a single Combat
    def HymnOfSufferingReq_combat    = LiveValues.get("FM", "HymnOfSufferingReq_combat", 12)  // SBR: total Combat of your Units in play
    def TerrifyingShriekReq_glyphs   = LiveValues.get("FM", "TerrifyingShriekReq_glyphs", 3)  // SBR: different Glyph types with a Screaming Wall
    def TendonFlayingReq_power       = LiveValues.get("FM", "TendonFlayingReq_power", 6)  // SBR: Power to pay (minus Screaming Walls in play)
    def BlueprintsInBloodReq_walls   = LiveValues.get("FM", "BlueprintsInBloodReq_walls", 3)  // SBR: Screaming Walls in play
    def MannedBattlementsReq_walls   = LiveValues.get("FM", "MannedBattlementsReq_walls", 6)  // SBR: Screaming Walls in play
    def CrumblingYawp_powerPerArea   = LiveValues.get("FM", "CrumblingYawp_powerPerArea", 1)  // SB option 1: Power per Area with enemy gate + Wall
    def CrumblingYawp_wallsPerSign   = LiveValues.get("FM", "CrumblingYawp_wallsPerSign", 3)  // SB option 2: Screaming Walls needed per Elder Sign
    def HymnOfSuffering_powerPerArea = LiveValues.get("FM", "HymnOfSuffering_powerPerArea", 1)  // SB: Power per adjacent Area with a Wall
    def TerrifyingShriek_pains       = LiveValues.get("FM", "TerrifyingShriek_pains", 1)  // SB: Pains added to your combat results
    def TendonFlaying_option1Power   = LiveValues.get("FM", "TendonFlaying_option1Power", 1)  // SB option 1: Power gained
    def TendonFlaying_option2Power   = LiveValues.get("FM", "TendonFlaying_option2Power", 1)  // SB option 2: Power gained
    def TendonFlaying_option3Doom    = LiveValues.get("FM", "TendonFlaying_option3Doom", 1)  // SB option 3: Doom gained
    def MannedBattlements_cost       = LiveValues.get("FM", "MannedBattlements_cost", 1)  // SB action cost (Power)
    def SBR1Req_num                  = LiveValues.get("FM", "SBR1Req_num", 1)  // SBR 1: number in "Roll 9 or more dice in a single Combat"
    def SBR2Req_num                  = LiveValues.get("FM", "SBR2Req_num", 2)  // SBR 2: number in "Your Units in play have 12 or more Combat"
    def SBR3Req_num                  = LiveValues.get("FM", "SBR3Req_num", 3)  // SBR 3: number in "The Screaming Wall is in Areas with 2 of the following Glyphs:"
    def SBR4Req_num                  = LiveValues.get("FM", "SBR4Req_num", 4)  // SBR 4: number in "As an Action, Pay 6 Power, minus the number of Screaming Walls in play"
    def SBR5Req_num                  = LiveValues.get("FM", "SBR5Req_num", 5)  // SBR 5: number in "Have 3 Screaming Walls in Play"
    def SBR6Req_num                  = LiveValues.get("FM", "SBR6Req_num", 6)  // SBR 6: number in "Have 6 Screaming Walls in Play"
}

// Permeation (PRM) - from faction-values/prm.json
object PRMValues {
    def Acolyte_count                     = LiveValues.get("PRM", "Acolyte_count", 6)  // Acolytes in the faction
    def AstralEgg_cost                    = LiveValues.get("PRM", "AstralEgg_cost", 1)  // Astral Egg summon cost (Power)
    def AstralEgg_combat                  = LiveValues.get("PRM", "AstralEgg_combat", 1)  // Astral Egg combat dice
    def AstralEgg_count                   = LiveValues.get("PRM", "AstralEgg_count", 4)  // Astral Eggs in the faction
    def CelestialFledgling_count          = LiveValues.get("PRM", "CelestialFledgling_count", 4)  // Celestial Fledglings in the faction (cost/combat are a calculation)
    def Ycgnagnnisssz_awakenCost          = LiveValues.get("PRM", "Ycgnagnnisssz_awakenCost", 7)  // Power to Awaken Ycgn'agnnisssz
    def Ycgnagnnisssz_count               = LiveValues.get("PRM", "Ycgnagnnisssz_count", 1)  // Ycgn'agnnisssz figures (max 1: Mother's Aid and awaken code assume a single figure)
    def AtomicUnity_cost                  = LiveValues.get("PRM", "AtomicUnity_cost", 1)  // Atomic Unity action cost (Power)
    def AtomicUnity_flipPower             = LiveValues.get("PRM", "AtomicUnity_flipPower", 2)  // Atomic Unity: flip Face Down if this much Power or more was gained
    def EldritchLove_cost                 = LiveValues.get("PRM", "EldritchLove_cost", 1)  // Eldritch Love action cost (Power) - menu check and label
    def EldritchLove_faceDownNeeded       = LiveValues.get("PRM", "EldritchLove_faceDownNeeded", 2)  // Eldritch Love: Face Down Spellbooks needed for the Elder Sign
    def EldritchLove_elderSigns           = LiveValues.get("PRM", "EldritchLove_elderSigns", 1)  // Eldritch Love: Elder Signs gained
    def Osmosis_cost                      = LiveValues.get("PRM", "Osmosis_cost", 2)  // Osmosis action cost (Power)
    def Protogenesis_cost                 = LiveValues.get("PRM", "Protogenesis_cost", 2)  // Protogenesis action cost (Power)
    def ZealousSpark_doomCost             = LiveValues.get("PRM", "ZealousSpark_doomCost", 1)  // Zealous Spark action cost (Doom)
    def Control2LandGatesReq_gates        = LiveValues.get("PRM", "Control2LandGatesReq_gates", 2)  // SBR: Gates controlled in Land Areas
    def Pay2DoomFlipSpellbookReq_doom     = LiveValues.get("PRM", "Pay2DoomFlipSpellbookReq_doom", 2)  // SBR: Doom paid to flip a Spellbook
    def EnemyHas3SpellbooksReq_spellbooks = LiveValues.get("PRM", "EnemyHas3SpellbooksReq_spellbooks", 3)  // SBR: Spellbooks another Faction must have
    def MakeAsOne_doomPerFlip             = LiveValues.get("PRM", "MakeAsOne_doomPerFlip", 1)  // Make as One: Doom gained when an enemy activation flips a Spellbook
    def MothersAid_power                  = LiveValues.get("PRM", "MothersAid_power", 1)  // Mother's Aid: Power an enemy pays to control Ycgn'agnnisssz
}

// Sphinx Worshippers (SW) - from faction-values/sw.json
object SWValues {
    def Acolyte_count               = LiveValues.get("SW", "Acolyte_count", 6)  // Acolytes in the faction
    def HybridMummy_cost            = LiveValues.get("SW", "HybridMummy_cost", 2)  // Hybrid Mummy summon cost (Power)
    def HybridMummy_combat          = LiveValues.get("SW", "HybridMummy_combat", 2)  // Hybrid Mummy combat dice
    def HybridMummy_count           = LiveValues.get("SW", "HybridMummy_count", 4)  // Hybrid Mummies in the faction
    def PharoahKephren_cost         = LiveValues.get("SW", "PharoahKephren_cost", 3)  // Pharoah Kephren summon cost (Power)
    def PharoahKephren_combat       = LiveValues.get("SW", "PharoahKephren_combat", 3)  // Pharoah Kephren combat dice
    def PharoahKephren_count        = LiveValues.get("SW", "PharoahKephren_count", 1)  // Pharoah Kephrens in the faction
    def QueenNitokris_cost          = LiveValues.get("SW", "QueenNitokris_cost", 4)  // Queen Nitokris summon cost (Power)
    def QueenNitokris_combatBase    = LiveValues.get("SW", "QueenNitokris_combatBase", 2)  // Queen Nitokris automatic Pains base (before adding current Hunger)
    def QueenNitokris_count         = LiveValues.get("SW", "QueenNitokris_count", 1)  // Queen Nitokrises in the faction
    def BlackSphinx_awakenBaseCost  = LiveValues.get("SW", "BlackSphinx_awakenBaseCost", 8)  // Base Power to Awaken Black Sphinx (before subtracting sacrificed monster's cost)
    def BlackSphinx_kidnapDiscount  = LiveValues.get("SW", "BlackSphinx_kidnapDiscount", 3)  // Extra discount to the awaken cost if the sacrificed monster was kidnapped
    def BlackSphinx_painsBase       = LiveValues.get("SW", "BlackSphinx_painsBase", 1)  // Black Sphinx automatic Pains base (before adding current Hunger)
    def BlackSphinx_count           = LiveValues.get("SW", "BlackSphinx_count", 1)  // Black Sphinxes in the faction
    def Setup_power                 = LiveValues.get("SW", "Setup_power", 8)  // Starting Power
    def Setup_aeStart               = LiveValues.get("SW", "Setup_aeStart", 0)  // Starting Hunger value
    def Setup_Acolyte               = LiveValues.get("SW", "Setup_Acolyte", 6)  // Acolytes placed in the start Area at setup
    def HungerReq2_threshold        = LiveValues.get("SW", "HungerReq2_threshold", 2)  // SBR: Hunger Track value to reach
    def HungerReq4_threshold        = LiveValues.get("SW", "HungerReq4_threshold", 4)  // SBR: Hunger Track value to reach
    def Hunger_awakenGain           = LiveValues.get("SW", "Hunger_awakenGain", 1)  // Hunger gained when Black Sphinx is Awakened
    def Hunger_actionPhaseGain      = LiveValues.get("SW", "Hunger_actionPhaseGain", 1)  // Hunger gained at start of Action Phase while Black Sphinx is on the map
    def Hunger_painedGain           = LiveValues.get("SW", "Hunger_painedGain", 1)  // Hunger gained when Black Sphinx is Pained
    def Hunger_killedGain           = LiveValues.get("SW", "Hunger_killedGain", 3)  // Hunger gained when Black Sphinx is Killed or eliminated
    def Hunger_bottomlessPitSpend   = LiveValues.get("SW", "Hunger_bottomlessPitSpend", 1)  // Hunger spent (reduced) when Bottomless Pit is performed
    def Hunger_discountedRitualGain = LiveValues.get("SW", "Hunger_discountedRitualGain", 1)  // Hunger gained when Starvation's ritual discount is used (same event as the ae row and sb.rows.5EwQF2ol.text 'Advance the Hunger track by 1')
    def Kidnap_minMummies           = LiveValues.get("SW", "Kidnap_minMummies", 2)  // UFA: minimum Hybrid Mummies (with Nitokris) required to hold a Kidnapped monster
    def GhoulQueen_cost             = LiveValues.get("SW", "GhoulQueen_cost", 1)  // SB action cost (Power)
    def BottomlessPit_cost          = LiveValues.get("SW", "BottomlessPit_cost", 1)  // SB action cost (Power)
    def BottomlessPit_costMinus     = LiveValues.get("SW", "BottomlessPit_costMinus", 1)  // Elder Signs gained = sacrificed monster's printed cost minus this
    def BottomlessPit_swBonusPower  = LiveValues.get("SW", "BottomlessPit_swBonusPower", 1)  // Power SW gains when an opponent uses Bottomless Pit
    def BottomlessPit_swBonusES     = LiveValues.get("SW", "BottomlessPit_swBonusES", 1)  // Elder Signs SW gains when an opponent uses Bottomless Pit
    def MummiesCurse_pains          = LiveValues.get("SW", "MummiesCurse_pains", 3)  // SB (side A): Pains added to battle results
    def LifeFromDeath_cultists      = LiveValues.get("SW", "LifeFromDeath_cultists", 3)  // SB (side B): Cultists gained replacing the sacrificed Hybrid Mummy
    def Starvation_esDivisor        = LiveValues.get("SW", "Starvation_esDivisor", 3)  // SB: Hunger Track value is divided by this (rounded down) for Elder Signs gained
    def Acolyte_pains               = LiveValues.get("SW", "Acolyte_pains", 0)  // Acolyte fixed Pain results
    def Acolyte_kills               = LiveValues.get("SW", "Acolyte_kills", 0)  // Acolyte fixed Kill results
}

// Xyrious Storm (XSS) - from faction-values/xss.json
object XSSValues {
    def Setup_power               = LiveValues.get("XSS", "Setup_power", 8)  // Starting Power
    def Setup_Acolyte             = LiveValues.get("XSS", "Setup_Acolyte", 6)  // Acolytes placed during setup
    def Acolyte_count             = LiveValues.get("XSS", "Acolyte_count", 6)  // Acolytes in the faction
    def AmphibianCrawler_count    = LiveValues.get("XSS", "AmphibianCrawler_count", 2)  // Amphibian Crawlers in the faction
    def AmphibianCrawler_cost     = LiveValues.get("XSS", "AmphibianCrawler_cost", 1)  // Amphibian Crawler summon cost (Power)
    def AmphibianCrawler_combat   = LiveValues.get("XSS", "AmphibianCrawler_combat", 0)  // Amphibian Crawler combat dice
    def Twister_count             = LiveValues.get("XSS", "Twister_count", 4)  // Twisters in the faction
    def Twister_cost              = LiveValues.get("XSS", "Twister_cost", 2)  // Twister summon cost (Power)
    def Twister_combatLand        = LiveValues.get("XSS", "Twister_combatLand", 3)  // Twister combat dice in Land Areas
    def Twister_combatOther       = LiveValues.get("XSS", "Twister_combatOther", 1)  // Twister combat dice in other Areas
    def EyeOfTheStorm_count       = LiveValues.get("XSS", "EyeOfTheStorm_count", 3)  // Eye of the Storms in the faction
    def EyeOfTheStorm_cost        = LiveValues.get("XSS", "EyeOfTheStorm_cost", 3)  // Eye of the Storm summon cost (Power)
    def EyeOfTheStorm_combatSea   = LiveValues.get("XSS", "EyeOfTheStorm_combatSea", 4)  // Eye of the Storm combat dice in Sea Areas
    def EyeOfTheStorm_combatOther = LiveValues.get("XSS", "EyeOfTheStorm_combatOther", 1)  // Eye of the Storm combat dice in other Areas
    def Petrichor_count           = LiveValues.get("XSS", "Petrichor_count", 1)  // Petrichors in the faction
    def Petrichor_awakenCost      = LiveValues.get("XSS", "Petrichor_awakenCost", 8)  // Power to Awaken Petrichor
    def Tsunami_cost              = LiveValues.get("XSS", "Tsunami_cost", 1)  // Tsunami action cost (Power)
}
