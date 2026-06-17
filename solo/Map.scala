package cws

import hrf.colmat._

object EarthMap3 extends Board {
    val id = "earth33"
    val name = "Earth Map (3 players)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Europe = Area("Europe", GlyphWW)
    val Asia = Area("Asia", GlyphWW)
    val Africa = Area("Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmerica = Area("South America", GlyphOO)
    val NorthAmerica = Area("North America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Europe, Asia, Africa, NorthAmerica, SouthAmerica, Australia, Antarctica)
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthAmerica, Australia)
    val west = $(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = $(Europe, Asia, Africa, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmerica, NorthAtlantic, Europe, Asia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmerica, ArcticOcean, Europe, Asia, Africa, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, Africa, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, Asia)
        case IndianOcean => $(Antarctica, SouthAtlantic, Africa, Asia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Europe => $(ArcticOcean, NorthAtlantic, Asia)
        case Asia => $(ArcticOcean, NorthAtlantic, NorthPacific, IndianOcean, Europe, Africa)
        case Africa => $(NorthAtlantic, Asia, SouthAtlantic, IndianOcean)
        case NorthAmerica => $(ArcticOcean, NorthAtlantic, SouthAmerica, NorthPacific)
        case SouthAmerica => $(NorthAmerica, NorthAtlantic, SouthAtlantic, SouthPacific, NorthPacific)
        case Australia => $(SouthPacific, IndianOcean)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case GC => $(SouthPacific)
        case CC => $(Asia)
        case BG => $(Africa)
        case YS => $(Europe)
        case SL => $(NorthAmerica)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
        // Tombstalker (TS): starting areas are ocean-only non-faction regions (Gla'aki requires ocean gate)
        case TS => nonFactionRegions.%(_.glyph == Ocean)
        // Firstborn (FB): starting areas are ANY area on the map. The faction card only
        // requires "an empty area" — meaning any area not already chosen by another faction.
        // The .diff(starting.values.$) in Game.scala (line ~1554) filters out areas already
        // taken. Bug 43 fix: was nonFactionRegions which incorrectly excluded printed-glyph
        // areas (e.g., Europe, Africa) even when those factions weren't in the game.
        // Same pattern as OW which uses `regions`.
        case FB => regions
        // Daemon Sultan (DS): no starting area restrictions
        case DS => $()
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Europe => (1240,245)
        case Asia => (1605, 350)
        case Africa => (1110, 480)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmerica => (590, 680)
        case NorthAmerica => (380, 290)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (215, 707)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap4v35 extends Board {
    val id = "earth35"
    val name = "Earth Map (4 players 3/5 variant)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Scandinavia = Area("Scandinavia", GlyphWW)
    val Europe = Area("Europe", GlyphWW)
    val NorthAsia = Area("North Asia", GlyphWW)
    val SouthAsia = Area("South Asia", GlyphWW)
    val Arabia = Area("Arabia", GlyphWW)
    val EastAfrica = Area("East Africa", GlyphAA)
    val WestAfrica = Area("West Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmerica = Area("South America", GlyphOO)
    val NorthAmerica = Area("North America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmerica, SouthAmerica, Australia, Antarctica)
    
    // It is therefore a faction region, not a non-faction region.
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, SouthAmerica, Australia)
    val west = $(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = $(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmerica, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => $(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Scandinavia => $(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => $(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => $(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => $(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => $(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => $(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => $(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmerica => $(ArcticOcean, NorthAtlantic, SouthAmerica, NorthPacific)
        case SouthAmerica => $(NorthAmerica, NorthAtlantic, SouthAtlantic, SouthPacific, NorthPacific)
        case Australia => $(SouthPacific, IndianOcean)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case GC => $(SouthPacific)
        // CC's printed starting glyph on the Earth board is in Arabia (not SouthAsia).
        // Fixes the cross-build bug where FB Devil's Mark / BB SBR for starting glyphs /
        // TT Idolatry / etc. did not treat Arabia as having a faction starting glyph
        // when CC was not in the game.
        case CC => $(SouthAsia)
        case BG => $(WestAfrica)
        case YS => $(Europe)
        case SL => $(NorthAmerica)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
        // Tombstalker (TS): starting areas are ocean-only non-faction regions (Gla'aki requires ocean gate)
        case TS => nonFactionRegions.%(_.glyph == Ocean)
        // Firstborn (FB): starting areas are ANY area on the map. The faction card only
        // requires "an empty area" — meaning any area not already chosen by another faction.
        // The .diff(starting.values.$) in Game.scala (line ~1554) filters out areas already
        // taken. Bug 43 fix: was nonFactionRegions which incorrectly excluded printed-glyph
        // areas (e.g., Europe, Africa) even when those factions weren't in the game.
        // Same pattern as OW which uses `regions`.
        case FB => regions
        // Daemon Sultan (DS): no starting area restrictions
        case DS => $()
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmerica => (590, 680)
        case NorthAmerica => (380, 290)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (215, 707)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap4v53 extends Board {
    val id = "earth53"
    val name = "Earth Map (4 players 5/3 variant)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Europe = Area("Europe", GlyphWW)
    val Asia = Area("Asia", GlyphWW)
    val Africa = Area("Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmericaWest = Area("South America West", GlyphOO)
    val SouthAmericaEast = Area("South America East", GlyphOO)
    val NorthAmericaWest = Area("North America West", GlyphOO)
    val NorthAmericaEast = Area("North America East", GlyphOO)
    val CentralAmerica = Area("Central America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)
    val NewZealand = Area("New Zealand", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Europe, Asia, Africa, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand)
    val west = $(ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = $(Europe, Asia, Africa, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmericaWest, NorthAmericaEast, NorthAtlantic, Europe, Asia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Europe, Asia, Africa, SouthAtlantic, SouthAmericaEast)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, Africa, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, Asia)
        case IndianOcean => $(Antarctica, SouthAtlantic, Africa, Asia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Europe => $(ArcticOcean, NorthAtlantic, Asia)
        case Asia => $(ArcticOcean, NorthAtlantic, NorthPacific, IndianOcean, Europe, Africa)
        case Africa => $(NorthAtlantic, Asia, SouthAtlantic, IndianOcean)
        case NorthAmericaWest => $(ArcticOcean, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => $(ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => $(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => $(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => $(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => $(IndianOcean, NewZealand)
        case NewZealand => $(IndianOcean, SouthPacific, Australia)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case GC => $(SouthPacific)
        case CC => $(Asia)
        case BG => $(Africa)
        case YS => $(Europe)
        case SL => $(NorthAmericaWest)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
        // Tombstalker (TS): starting areas are ocean-only non-faction regions (Gla'aki requires ocean gate)
        case TS => nonFactionRegions.%(_.glyph == Ocean)
        // Firstborn (FB): starting areas are ANY area on the map. The faction card only
        // requires "an empty area" — meaning any area not already chosen by another faction.
        // The .diff(starting.values.$) in Game.scala (line ~1554) filters out areas already
        // taken. Bug 43 fix: was nonFactionRegions which incorrectly excluded printed-glyph
        // areas (e.g., Europe, Africa) even when those factions weren't in the game.
        // Same pattern as OW which uses `regions`.
        case FB => regions
        // Daemon Sultan (DS): no starting area restrictions
        case DS => $()
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Europe => (1240,245)
        case Asia => (1605, 350)
        case Africa => (1110, 480)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670, 615)
        case CentralAmerica => (305, 410)
        case NorthAmericaWest => (330, 180)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (125, 685)
        case NewZealand => (265, 710)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap5 extends Board {
    val id = "earth55"
    val name = "Earth Map (5 players)"

    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Scandinavia = Area("Scandinavia", GlyphWW)
    val Europe = Area("Europe", GlyphWW)
    val NorthAsia = Area("North Asia", GlyphWW)
    val SouthAsia = Area("South Asia", GlyphWW)
    val Arabia = Area("Arabia", GlyphWW)
    val EastAfrica = Area("East Africa", GlyphAA)
    val WestAfrica = Area("West Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmericaWest = Area("South America West", GlyphOO)
    val SouthAmericaEast = Area("South America East", GlyphOO)
    val NorthAmericaWest = Area("North America West", GlyphOO)
    val NorthAmericaEast = Area("North America East", GlyphOO)
    val CentralAmerica = Area("Central America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)
    val NewZealand = Area("New Zealand", GlyphAA)

    val regions = $(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    
    // It is therefore a faction region, not a non-faction region.
    val nonFactionRegions = $(NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand)
    val west = $(ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = $(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => $(NorthAmericaWest, NorthAmericaEast, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => $(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmericaEast)
        case SouthAtlantic => $(Antarctica, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => $(ArcticOcean, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => $(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Scandinavia => $(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => $(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => $(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => $(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => $(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => $(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => $(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmericaWest => $(ArcticOcean, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => $(ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => $(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => $(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => $(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => $(IndianOcean, NewZealand)
        case NewZealand => $(IndianOcean, SouthPacific, Australia)
        case Antarctica => $(SouthPacific, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case GC => $(SouthPacific)
        // CC's printed starting glyph on the Earth board is in Arabia (not SouthAsia).
        // Fixes the cross-build bug where FB Devil's Mark / BB SBR for starting glyphs /
        // TT Idolatry / etc. did not treat Arabia as having a faction starting glyph
        // when CC was not in the game.
        case CC => $(SouthAsia)
        case BG => $(WestAfrica)
        case YS => $(Europe)
        case SL => $(NorthAmericaWest)
        case WW => $(ArcticOcean, Antarctica)
        case OW => regions
        case AN => nonFactionRegions
        // Tombstalker (TS): starting areas are ocean-only non-faction regions (Gla'aki requires ocean gate)
        case TS => nonFactionRegions.%(_.glyph == Ocean)
        // Firstborn (FB): starting areas are ANY area on the map. The faction card only
        // requires "an empty area" — meaning any area not already chosen by another faction.
        // The .diff(starting.values.$) in Game.scala (line ~1554) filters out areas already
        // taken. Bug 43 fix: was nonFactionRegions which incorrectly excluded printed-glyph
        // areas (e.g., Europe, Africa) even when those factions weren't in the game.
        // Same pattern as OW which uses `regions`.
        case FB => regions
        // Daemon Sultan (DS): no starting area restrictions
        case DS => $()
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case ArcticOcean => (933, 77)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (690, 390)
        case SouthAtlantic => (855, 665)
        case Antarctica => (970, 815)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670, 615)
        case CentralAmerica => (305, 410)
        case NorthAmericaWest => (330, 180)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (125, 685)
        case NewZealand => (265, 710)
        case _ => throw new Error("Unknown region " + r)
    }
}

object EarthMap6 extends Board {
    val id = "earth66"
    val name = "Earth Map (6 players)"

    val BeringSea = Area("Bering Sea", Ocean)
    val ArcticOcean = Area("Arctic Ocean", Ocean)
    val Scandinavia = Area("Scandinavia", GlyphWW)
    val Europe = Area("Europe", GlyphWW)
    val NorthAsia = Area("North Asia", GlyphWW)
    val SouthAsia = Area("South Asia", GlyphWW)
    val Arabia = Area("Arabia", GlyphWW)
    val EastAfrica = Area("East Africa", GlyphAA)
    val WestAfrica = Area("West Africa", GlyphAA)
    val NorthAtlantic = Area("North Atlantic", Ocean)
    val MediterraneanSea = Area("Mediterranean Sea", Ocean)
    val SouthAtlantic = Area("South Atlantic", Ocean)
    val Antarctica = Area("Antarctica", NoGlyph)
    val MountainsOfMadness = Area("Mountains of Madness", NoGlyph)
    val SouthPacific = Area("South Pacific", Ocean)
    val SouthAmericaWest = Area("South America West", GlyphOO)
    val SouthAmericaEast = Area("South America East", GlyphOO)
    val NorthAmericaWest = Area("North America West", GlyphOO)
    val NorthAmericaEast = Area("North America East", GlyphOO)
    val CentralAmerica = Area("Central America", GlyphOO)
    val NorthPacific = Area("North Pacific", Ocean)
    val IndianOcean = Area("Indian Ocean", Ocean)
    val Australia = Area("Australia", GlyphAA)
    val NewZealand = Area("New Zealand", GlyphAA)

    val regions = $(BeringSea, ArcticOcean, NorthAtlantic, MediterraneanSea, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmericaWest, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica, MountainsOfMadness)
    
    // It is therefore a faction region, not a non-faction region.
    val nonFactionRegions = $(BeringSea, NorthAtlantic, MediterraneanSea, SouthAtlantic, NorthPacific, IndianOcean, Scandinavia, NorthAsia, Arabia, EastAfrica, NorthAmericaEast, CentralAmerica, SouthAmericaWest, SouthAmericaEast, Australia, NewZealand, Antarctica)
    val west = $(BeringSea, ArcticOcean, NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, NorthAtlantic, MediterraneanSea, Australia, NewZealand, SouthPacific, SouthAmericaWest, SouthAmericaEast, SouthAtlantic, Antarctica)
    val east = $(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean, MountainsOfMadness)

    def connected(region : Region) = region match {
        case BeringSea => $(ArcticOcean, NorthAmericaEast, NorthAmericaWest, NorthPacific)
        case ArcticOcean => $(BeringSea, NorthAmericaEast, NorthAtlantic, Scandinavia, NorthAsia)
        case NorthAtlantic => $(NorthPacific, NorthAmericaWest, NorthAmericaEast, CentralAmerica, ArcticOcean, Scandinavia, Europe, MediterraneanSea, WestAfrica, SouthAtlantic, SouthAmericaEast)
        case MediterraneanSea => $(NorthAtlantic, Europe, Arabia, WestAfrica)
        case SouthAtlantic => $(Antarctica, MountainsOfMadness, SouthPacific, SouthAmericaWest, SouthAmericaEast, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => $(BeringSea, NorthAmericaWest, CentralAmerica, NorthAtlantic, SouthAmericaWest, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => $(MountainsOfMadness, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia, NewZealand)
        case SouthPacific => $(Antarctica, SouthAtlantic, SouthAmericaWest, NorthPacific, IndianOcean, NewZealand)
        case Scandinavia => $(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => $(NorthAtlantic, Scandinavia, NorthAsia, Arabia, MediterraneanSea)
        case NorthAsia => $(ArcticOcean, BeringSea, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => $(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => $(MediterraneanSea, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => $(NorthAtlantic, MediterraneanSea, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => $(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmericaWest => $(BeringSea, NorthAtlantic, NorthPacific, NorthAmericaEast, CentralAmerica)
        case NorthAmericaEast => $(BeringSea, ArcticOcean, NorthAtlantic, NorthAmericaWest)
        case CentralAmerica => $(NorthAtlantic, NorthPacific, NorthAmericaWest, SouthAmericaWest, SouthAmericaEast)
        case SouthAmericaWest => $(NorthPacific, SouthPacific, CentralAmerica, SouthAmericaEast, SouthAtlantic)
        case SouthAmericaEast => $(NorthAtlantic, SouthAtlantic, CentralAmerica, SouthAmericaWest)
        case Australia => $(IndianOcean, NewZealand)
        case NewZealand => $(IndianOcean, SouthPacific, Australia)
        case Antarctica => $(SouthPacific, SouthAtlantic, MountainsOfMadness)
        case MountainsOfMadness => $(Antarctica, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case GC => $(SouthPacific)
        // CC's printed starting glyph on the Earth board is in Arabia (not SouthAsia).
        // Fixes the cross-build bug where FB Devil's Mark / BB SBR for starting glyphs /
        // TT Idolatry / etc. did not treat Arabia as having a faction starting glyph
        // when CC was not in the game.
        case CC => $(SouthAsia)
        case BG => $(WestAfrica)
        case YS => $(Europe)
        case SL => $(NorthAmericaWest)
        case WW => $(ArcticOcean, MountainsOfMadness)
        case OW => regions
        case AN => nonFactionRegions
        // Tombstalker (TS): starting areas are ocean-only non-faction regions (Gla'aki requires ocean gate)
        case TS => nonFactionRegions.%(_.glyph == Ocean)
        // Firstborn (FB): starting areas are ANY area on the map. The faction card only
        // requires "an empty area" — meaning any area not already chosen by another faction.
        // The .diff(starting.values.$) in Game.scala (line ~1554) filters out areas already
        // taken. Bug 43 fix: was nonFactionRegions which incorrectly excluded printed-glyph
        // areas (e.g., Europe, Africa) even when those factions weren't in the game.
        // Same pattern as OW which uses `regions`.
        case FB => regions
        // Daemon Sultan (DS): no starting area restrictions
        case DS => $()
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        case BeringSea => (85, 55)
        case ArcticOcean => (1075, 60)
        case Scandinavia => (1135, 165)
        case Europe => (1110, 255)
        case NorthAsia => (1595, 150)
        case SouthAsia => (1620, 360)
        case Arabia => (1455, 460)
        case EastAfrica => (1235, 665)
        case WestAfrica => (1115, 525)
        case NorthAtlantic => (665, 385)
        case MediterraneanSea => (1130, 360)
        case SouthAtlantic => (855, 665)
        case Antarctica => (870, 845)
        case MountainsOfMadness => (1240, 855)
        case SouthPacific => (540, 830)
        case SouthAmericaWest => (550, 675)
        case SouthAmericaEast => (670, 615)
        case CentralAmerica => (305, 410)
        case NorthAmericaWest => (330, 180)
        case NorthAmericaEast => (565, 255)
        case NorthPacific => (105, 355)
        case IndianOcean => (1610, 730)
        case Australia => (125, 685)
        case NewZealand => (265, 710)
        case _ => throw new Error("Unknown region " + r)
    }
}


// [2026-05-23] Canonical region color palette for Earth boards — Option B fix
// for the portrait/mobile glyph-misplacement bug. The placement bitmaps
// (earth33/35/53/55-place.webp) store each region as a distinct solid color
// separated by black GAP pixels. Hard-coding the canonical color per region
// per board means the glyph-placement algorithm never has to SAMPLE the
// bitmap at the gate XY to learn the region's color — eliminating the entire
// class of "sampled at a boundary or rotated coord → wrong region" bugs.
// Colors were sampled from the unrotated landscape bitmap at each board's
// gateXYO; verified visually distinct per region.
object EarthRegionPalette {
    private[cws] val byBoard : Map[String, Map[Region, Int]] = {
        import EarthMap3._
        val earth33 = Map[Region, Int](
            ArcticOcean -> 0xafbeb3, Europe -> 0x857d3f, Asia -> 0xba441e, Africa -> 0x9f9a7f,
            NorthAtlantic -> 0x505c70, SouthAtlantic -> 0x597f7d, Antarctica -> 0xcdac85,
            SouthPacific -> 0x405061, SouthAmerica -> 0xcc4438, NorthAmerica -> 0xd8952d,
            NorthPacific -> 0x274a66, IndianOcean -> 0x506a8c, Australia -> 0x8d6919,
        )
        val earth35 = {
            import EarthMap4v35._
            Map[Region, Int](
                ArcticOcean -> 0xafbeb3, Scandinavia -> 0xa3741a, Europe -> 0x857d3f,
                NorthAsia -> 0xba441e, SouthAsia -> 0x670f0b, Arabia -> 0xcba750,
                EastAfrica -> 0x9d6d63, WestAfrica -> 0x9f9a7f,
                NorthAtlantic -> 0x505c70, SouthAtlantic -> 0x597f7d, Antarctica -> 0xcdac85,
                SouthPacific -> 0x405061, SouthAmerica -> 0xcc4438, NorthAmerica -> 0xd8952d,
                NorthPacific -> 0x274a66, IndianOcean -> 0x506a8c, Australia -> 0x8d6919,
            )
        }
        val earth53 = {
            import EarthMap4v53._
            Map[Region, Int](
                ArcticOcean -> 0xafbeb3, Europe -> 0x857d3f, Asia -> 0xba441e, Africa -> 0x9f9a7f,
                NorthAtlantic -> 0x505c70, SouthAtlantic -> 0x597f7d, Antarctica -> 0xcdac85,
                SouthPacific -> 0x405061, SouthAmerica -> 0xcc4438, NorthAmerica -> 0xa6905f,
                NorthPacific -> 0x274a66, IndianOcean -> 0x506a8c, Australia -> 0x8d6919,
            )
        }
        val earth55 = {
            import EarthMap5._
            Map[Region, Int](
                ArcticOcean -> 0xafbeb3, Europe -> 0x857d3f, Asia -> 0x670f0b, Africa -> 0x9f9a7f,
                NorthAtlantic -> 0x505c70, SouthAtlantic -> 0x597f7d, Antarctica -> 0xcdac85,
                SouthPacific -> 0x405061,
                SouthAmericaWest -> 0xcc4438, SouthAmericaEast -> 0x6b5540,
                CentralAmerica -> 0xd8952d,
                NorthAmericaWest -> 0xa6905f, NorthAmericaEast -> 0xc87c5c,
                NorthPacific -> 0x274a66, IndianOcean -> 0x506a8c,
                Australia -> 0x8a3a0f, NewZealand -> 0x8d6919,
            )
        }
        Map(
            "earth33" -> earth33,
            "earth35" -> earth35,
            "earth53" -> earth53,
            "earth55" -> earth55,
        )
    }
    def get(boardId : String, r : Region) : Option[Int] =
        byBoard.get(boardId).flatMap(_.get(r))
}


// ══════════════════════════════════════════════════════════════════════════════
// LIBRARY AT CELAENO — Two-floor map with Archway and Stairwell adjacency
// ══════════════════════════════════════════════════════════════════════════════

// Canonical region color palette for the library placement bitmaps.
// Each entry maps a Region → the (r,g,b) packed-int color used to mark that
// region's pixels in library{3,5,35,53}-place.webp. Colors were extracted by
// pixel-sampling library5-place.webp at each region's hand-coded gateXY
// (verified to be inside the right region on 5U/5L). User-confirmed that the
// same palette applies to 3U/3L/35/53 bitmaps as well — only the regions'
// PIXEL POSITIONS shift between bitmaps, not their colors. The pre-pass in
// CthulhuWarsSolo.scala uses this table to identify region pixels by color
// instead of by seed XY, which previously mis-identified regions on 3U/3L
// where the hand-coded XY would land on a neighbor's color (e.g., Lake of
// Hali Overlook gate appearing way to the left on 3p).
object RegionPalette {
    private[cws] val table : Map[Region, Int] = Map(
        LibraryCelaeno55.FloatingTower       -> 0xfec800,
        LibraryCelaeno55.Byakhiary           -> 0x6e5424,
        LibraryCelaeno55.Horrorium           -> 0xdc2928,
        LibraryCelaeno55.Fountain            -> 0x0150c7,
        LibraryCelaeno55.YrAndTheNhhngr      -> 0xff9a06,
        LibraryCelaeno55.GuardianUnderLake   -> 0x00ff01,
        LibraryCelaeno55.Gloomloft           -> 0x808080,
        LibraryCelaeno55.CursedHall          -> 0xfcff00,
        LibraryCelaeno55.BarrierOfNaachTith  -> 0x638c28,
        LibraryCelaeno55.LarvaeOfOuterGods   -> 0xdc6400,
        LibraryCelaeno55.LakeOfHaliOverlook  -> 0x962d90,
        LibraryCelaeno55.LakeOfHaliBalcony   -> 0x01ffff,
        LibraryCelaeno55.ChamberOfSngac      -> 0xf93cc9,
        LibraryCelaeno55.Oubliette           -> 0xf9e53c,
        LibraryCelaeno55.BlueHall            -> 0x011896,
        LibraryCelaeno55.ScorchedChamber     -> 0xcffa3c,
        LibraryCelaeno55.PorphyrHall         -> 0x2c0839,
        LibraryCelaeno55.RedHall             -> 0xc18c3c,
        LibraryCelaeno55.BlackHall           -> 0xbe97b9,
        LibraryCelaeno55.ChamberOfApkallu    -> 0xff84ad,
        LibraryCelaeno55.Hyperquarium        -> 0x01b58c,
        LibraryCelaeno55.CharnelHall         -> 0xfbc386,
        LibraryCelaeno55.TheCrawlingOnes     -> 0x5079dd,
    )
    def get(r : Region) : Option[Int] = table.get(r)
    def getOrElse(r : Region, fallback : => Int) : Int = table.getOrElse(r, fallback)
}

object LibraryCelaeno55 extends Board {
    val id = "library5"
    val name = "Library at Celaeno (5 players)"
    override val isLibraryMap = true
    override def silenceTokenMax(f : Faction) : Int = 1
    override val unitScale : Double = 2.0

    // ── UPPER FLOOR ──
    val FloatingTower = Area("Floating Tower", GlyphAA)
    val Byakhiary = Area("Byakhiary", GlyphAA)
    val Horrorium = Area("Horrorium", GlyphAA)
    val Fountain = Area("Fountain", Ocean)
    val YrAndTheNhhngr = Area("Yr and the Nhhngr", GlyphOO)
    val GuardianUnderLake = Area("Guardian under the Lake", GlyphOO)
    val BarrierOfNaachTith = Area("Barrier of Naach-Tith", GlyphOO)
    val Gloomloft = Area("Gloomloft", Ocean)
    val CursedHall = Area("Cursed Hall", Ocean)
    val LarvaeOfOuterGods = Area("Larvae of the Outer Gods", GlyphOO)
    val LakeOfHaliOverlook = Area("Lake of Hali Overlook", Ocean)
    val LakeOfHaliBalcony = Area("Lake of Hali Balcony", Ocean)

    // ── LOWER FLOOR ──
    val ChamberOfSngac = Area("Chamber of Sn'gac", GlyphWW)
    val Oubliette = Area("Oubliette", Ocean)
    val BlueHall = Area("Blue Hall", GlyphWW)
    val ScorchedChamber = Area("Scorched Chamber", GlyphWW)
    val PorphyrHall = Area("Porphyr Hall", Ocean)
    val RedHall = Area("Red Hall", GlyphWW)
    val BlackHall = Area("Black Hall", GlyphWW)
    val ChamberOfApkallu = Area("Chamber of Apkallu", GlyphWW)
    val Hyperquarium = Area("Hyperquarium", Ocean)
    val CharnelHall = Area("Charnel Hall", GlyphWW)
    val TheCrawlingOnes = Area("The Crawling Ones", GlyphWW)

    val upperRegions = $(FloatingTower, Byakhiary, Horrorium, Fountain, YrAndTheNhhngr,
        GuardianUnderLake, BarrierOfNaachTith, Gloomloft, CursedHall, LarvaeOfOuterGods,
        LakeOfHaliOverlook, LakeOfHaliBalcony)
    val lowerRegions = $(ChamberOfSngac, Oubliette, BlueHall, ScorchedChamber, PorphyrHall,
        RedHall, BlackHall, ChamberOfApkallu, Hyperquarium, CharnelHall, TheCrawlingOnes)

    val regions = upperRegions ++ lowerRegions

    // Tome regions — excluded from OW/AN starting
    val tomeRegions = $(YrAndTheNhhngr, GuardianUnderLake, BarrierOfNaachTith, LarvaeOfOuterGods)
    val nonFactionRegions = regions.diff($(FloatingTower, Fountain, BlueHall, ChamberOfSngac, Hyperquarium, LakeOfHaliOverlook, Oubliette)).diff(tomeRegions)

    val west = upperRegions
    val east = lowerRegions

    // 55 arch regions: mutually adjacent for movement, NOT retreat
    // 5L boards: FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ScorchedChamber
    override val archways : Set[Region] = Set(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ScorchedChamber)
    private val arch55 = $(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ScorchedChamber)

    // Helper: for an arch region, return all other arch regions + explicit adjacencies (deduped)
    private def withArch(self : Region, explicit : $[Region]) : $[Region] =
        (arch55.%(r => r != self) ++ explicit).distinct

    // Base adjacency WITHOUT arch expansion — used for retreat/pain
    def baseConnected(region : Region) : $[Region] = region match {
        case FloatingTower => $(Byakhiary, Horrorium)
        case Byakhiary => $(FloatingTower, YrAndTheNhhngr, ChamberOfSngac)
        case Horrorium => $(FloatingTower, GuardianUnderLake, BlueHall)
        case Fountain => $(YrAndTheNhhngr, GuardianUnderLake, Gloomloft, Oubliette)
        case YrAndTheNhhngr => $(Byakhiary, Fountain, BarrierOfNaachTith)
        case GuardianUnderLake => $(Horrorium, Fountain, LarvaeOfOuterGods)
        case BarrierOfNaachTith => $(YrAndTheNhhngr, LakeOfHaliOverlook, ScorchedChamber)
        case Gloomloft => $(Fountain, CursedHall)
        case CursedHall => $(Gloomloft, LakeOfHaliOverlook, LakeOfHaliBalcony)
        case LarvaeOfOuterGods => $(GuardianUnderLake, LakeOfHaliBalcony, TheCrawlingOnes)
        case LakeOfHaliOverlook => $(BarrierOfNaachTith, CursedHall, LakeOfHaliBalcony, Hyperquarium)
        case LakeOfHaliBalcony => $(LakeOfHaliOverlook, CursedHall, LarvaeOfOuterGods)
        case ChamberOfSngac => $(ScorchedChamber, Oubliette, Byakhiary)
        case Oubliette => $(ChamberOfSngac, ScorchedChamber, PorphyrHall, BlackHall, Fountain)
        case BlueHall => $(BlackHall, RedHall, Horrorium)
        case ScorchedChamber => $(ChamberOfSngac, Oubliette, PorphyrHall, Hyperquarium, ChamberOfApkallu, BarrierOfNaachTith)
        case PorphyrHall => $(ScorchedChamber, Oubliette, BlackHall, CharnelHall, Hyperquarium)
        case RedHall => $(BlackHall, BlueHall)
        case BlackHall => $(BlueHall, RedHall, Oubliette, PorphyrHall, CharnelHall)
        case ChamberOfApkallu => $(ScorchedChamber, Hyperquarium)
        case Hyperquarium => $(ChamberOfApkallu, PorphyrHall, CharnelHall, LakeOfHaliOverlook)
        case CharnelHall => $(BlackHall, PorphyrHall, Hyperquarium, TheCrawlingOnes)
        case TheCrawlingOnes => $(CharnelHall, LarvaeOfOuterGods)
    }

    // Full adjacency including archway connections — used for movement
    def connected(region : Region) = {
        val base = baseConnected(region)
        if (archways.contains(region)) withArch(region, base) else base
    }

    // Retreat uses base adjacency only (no archway expansion)
    override def connectedForRetreat(region : Region) : $[Region] = baseConnected(region)

    def distance(a : Region, b : Region) =
        if (a == b) 0
        else if (connected(a).contains(b)) 1
        else if (connected(a)./~(connected).contains(b)) 2
        else if (connected(a)./~(connected)./~(connected).contains(b)) 3
        else 4

    def starting(faction : Faction) = faction match {
        case GC => $(Hyperquarium)
        case CC => $(BlueHall)
        case BG => $(Fountain)
        case YS => $(FloatingTower)
        case SL => $(ChamberOfSngac)
        case WW => $(LakeOfHaliOverlook, Oubliette)
        case OW => regions.diff(tomeRegions)
        case AN => nonFactionRegions
        case TS => nonFactionRegions.%(_.glyph == Ocean)
        case FB => regions.diff(tomeRegions)
        case DS => $()
    }

    def gateXYO(r : Region) : (Int, Int) = r match {
        // Upper floor (5U) — gate positions adjusted to avoid region artwork glyphs
        case FloatingTower => (806, 480)          // bottom of circular area
        case Byakhiary => (449, 311)              // upper right, inside region
        case Horrorium => (1632, 312)             // upper right, inside region
        case Fountain => (808, 746)
        case YrAndTheNhhngr => (380, 700)         // top right corner
        case GuardianUnderLake => (1600, 700)     // top right corner
        case Gloomloft => (854, 1100)             // moved up so gate doesn't cover region name
        case CursedHall => (800, 1360)
        case BarrierOfNaachTith => (158, 1260)    // top left, inside region
        case LarvaeOfOuterGods => (1441, 1430)
        case LakeOfHaliOverlook => (718, 1622)
        case LakeOfHaliBalcony => (1118, 1566)    // top middle
        // Lower floor (5L) — offset by 1792 (upper floor height)
        case ChamberOfSngac => (388, 2087)          // top middle
        case Oubliette => (907, 2213)
        case BlueHall => (1464, 2156)
        case ScorchedChamber => (422, 2694)
        case PorphyrHall => (907, 2686)
        case RedHall => (1578, 2380)              // top right
        case BlackHall => (1278, 2533)
        case ChamberOfApkallu => (387, 3238)
        case Hyperquarium => (904, 3156)
        case CharnelHall => (1321, 3132)
        case TheCrawlingOnes => (1559, 3214)
        case _ => throw new Error("Unknown region " + r)
    }

    override def gateXYOHorizontal(r : Region) : (Int, Int) = {
        // Horizontal layout: lower LEFT (x=0..1790), upper RIGHT (x=1791..3581)
        // Vertical image: 1791x3584, upper y=0..1791, lower y=1792..3583
        val (vx, vy) = gateXYO(r)
        if (vy < 1792) {
            // Upper floor → RIGHT side in horizontal
            (vx + 1791, vy)
        } else {
            // Lower floor → LEFT side in horizontal
            (vx, vy - 1792)
        }
    }
}

object LibraryCelaeno33 extends Board {
    val id = "library3"
    val name = "Library at Celaeno (3 players)"
    override val isLibraryMap = true
    override def silenceTokenMax(f : Faction) : Int = 1
    override val unitScale : Double = 2.0

    import LibraryCelaeno55.{FloatingTower, Fountain, YrAndTheNhhngr, GuardianUnderLake,
        BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, LakeOfHaliOverlook,
        ChamberOfSngac, Oubliette, BlueHall, ChamberOfApkallu, BlackHall,
        Hyperquarium, TheCrawlingOnes, tomeRegions}

    val regions = $(FloatingTower, Fountain, YrAndTheNhhngr, GuardianUnderLake,
        BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, LakeOfHaliOverlook,
        ChamberOfSngac, Oubliette, BlueHall, ChamberOfApkallu, BlackHall,
        Hyperquarium, TheCrawlingOnes)

    // BlueHall (CC's pre-printed glyph) added to align with LibraryCelaeno55 / 53 —
    // AN/TS should be locked out of every faction starting area including CC's.
    val nonFactionRegions = regions.diff($(FloatingTower, Fountain, BlueHall, ChamberOfSngac, Hyperquarium, LakeOfHaliOverlook, Oubliette)).diff(tomeRegions)
    val west = $(FloatingTower, Fountain, YrAndTheNhhngr, GuardianUnderLake, BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, LakeOfHaliOverlook)
    val east = $(ChamberOfSngac, Oubliette, BlueHall, ChamberOfApkallu, BlackHall, Hyperquarium, TheCrawlingOnes)

    // 3L arch regions (no ScorchedChamber in 3L; ChamberOfApkallu replaces it)
    override val archways : Set[Region] = Set(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ChamberOfApkallu)
    private val arch33 = $(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ChamberOfApkallu)
    private def withArch(self : Region, explicit : $[Region]) : $[Region] =
        (arch33.%(r => r != self) ++ explicit).distinct

    def baseConnected(region : Region) : $[Region] = region match {
        case FloatingTower => $(YrAndTheNhhngr, GuardianUnderLake)
        case Fountain => $(YrAndTheNhhngr, GuardianUnderLake, Gloomloft, Oubliette)
        case YrAndTheNhhngr => $(FloatingTower, Fountain, BarrierOfNaachTith, ChamberOfSngac)
        case GuardianUnderLake => $(FloatingTower, Fountain, LarvaeOfOuterGods, BlueHall)
        case BarrierOfNaachTith => $(YrAndTheNhhngr, LakeOfHaliOverlook, ChamberOfApkallu)
        case Gloomloft => $(Fountain, LakeOfHaliOverlook)
        case LakeOfHaliOverlook => $(BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, Hyperquarium)
        case LarvaeOfOuterGods => $(GuardianUnderLake, LakeOfHaliOverlook, TheCrawlingOnes)
        case ChamberOfSngac => $(ChamberOfApkallu, Oubliette, YrAndTheNhhngr)
        case Oubliette => $(ChamberOfSngac, ChamberOfApkallu, Hyperquarium, BlueHall, BlackHall, TheCrawlingOnes, Fountain)
        case BlueHall => $(Oubliette, BlackHall, GuardianUnderLake)
        case BlackHall => $(BlueHall, Oubliette, TheCrawlingOnes)
        case ChamberOfApkallu => $(ChamberOfSngac, Oubliette, Hyperquarium, BarrierOfNaachTith)
        case Hyperquarium => $(ChamberOfApkallu, Oubliette, TheCrawlingOnes, LakeOfHaliOverlook)
        case TheCrawlingOnes => $(Hyperquarium, Oubliette, BlackHall, LarvaeOfOuterGods)
    }

    def connected(region : Region) = {
        val base = baseConnected(region)
        if (archways.contains(region)) withArch(region, base) else base
    }

    override def connectedForRetreat(region : Region) : $[Region] = baseConnected(region)

    def distance(a : Region, b : Region) =
        if (a == b) 0 else if (connected(a).contains(b)) 1
        else if (connected(a)./~(connected).contains(b)) 2
        else if (connected(a)./~(connected)./~(connected).contains(b)) 3 else 4

    def starting(faction : Faction) = faction match {
        case GC => $(Hyperquarium); case CC => $(BlueHall); case BG => $(Fountain); case YS => $(FloatingTower)
        case SL => $(ChamberOfSngac); case WW => $(LakeOfHaliOverlook, Oubliette)
        case OW => regions.diff(tomeRegions); case AN => nonFactionRegions
        case TS => nonFactionRegions.%(_.glyph == Ocean); case FB => regions.diff(tomeRegions); case DS => $()
        case _ => regions
    }

    // 3U+3L: lower floor uses 3L gate positions
    def gateXYO(r : Region) : (Int, Int) = Library3LGates.gateXYO(r)
    override def gateXYOHorizontal(r : Region) : (Int, Int) = Library3LGates.gateXYOHorizontal(r)
}

object LibraryCelaeno53 extends Board {
    val id = "library53"
    val name = "Library at Celaeno (4 players, 5L3U)"
    override val isLibraryMap = true
    override def silenceTokenMax(f : Faction) : Int = 1
    override val unitScale : Double = 2.0

    import LibraryCelaeno55.{FloatingTower, Fountain, YrAndTheNhhngr, GuardianUnderLake,
        BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, LakeOfHaliOverlook,
        ChamberOfSngac, Oubliette, BlueHall, ScorchedChamber, PorphyrHall,
        RedHall, BlackHall, ChamberOfApkallu, Hyperquarium, CharnelHall,
        TheCrawlingOnes, tomeRegions}

    // 3U + 5L
    val regions = $(FloatingTower, Fountain, YrAndTheNhhngr, GuardianUnderLake,
        BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, LakeOfHaliOverlook,
        ChamberOfSngac, Oubliette, BlueHall, ScorchedChamber, PorphyrHall,
        RedHall, BlackHall, ChamberOfApkallu, Hyperquarium, CharnelHall, TheCrawlingOnes)

    val nonFactionRegions = regions.diff($(FloatingTower, Fountain, BlueHall, ChamberOfSngac, Hyperquarium, LakeOfHaliOverlook, Oubliette)).diff(tomeRegions)
    val west = $(FloatingTower, Fountain, YrAndTheNhhngr, GuardianUnderLake, BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, LakeOfHaliOverlook)
    val east = $(ChamberOfSngac, Oubliette, BlueHall, ScorchedChamber, PorphyrHall, RedHall, BlackHall, ChamberOfApkallu, Hyperquarium, CharnelHall, TheCrawlingOnes)

    // 5L arch regions (same as 55)
    override val archways : Set[Region] = Set(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ScorchedChamber)
    private val arch53 = $(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ScorchedChamber)
    private def withArch(self : Region, explicit : $[Region]) : $[Region] =
        (arch53.%(r => r != self) ++ explicit).distinct

    def baseConnected(region : Region) : $[Region] = region match {
        case FloatingTower => $(YrAndTheNhhngr, GuardianUnderLake)
        case Fountain => $(YrAndTheNhhngr, GuardianUnderLake, Gloomloft, Oubliette)
        case YrAndTheNhhngr => $(FloatingTower, Fountain, BarrierOfNaachTith, ChamberOfSngac)
        case GuardianUnderLake => $(FloatingTower, Fountain, LarvaeOfOuterGods, BlueHall)
        case BarrierOfNaachTith => $(YrAndTheNhhngr, LakeOfHaliOverlook, ChamberOfApkallu)
        case Gloomloft => $(Fountain, LakeOfHaliOverlook)
        case LakeOfHaliOverlook => $(BarrierOfNaachTith, Gloomloft, LarvaeOfOuterGods, Hyperquarium)
        case LarvaeOfOuterGods => $(GuardianUnderLake, LakeOfHaliOverlook, TheCrawlingOnes)
        case ChamberOfSngac => $(ScorchedChamber, Oubliette, YrAndTheNhhngr)
        case Oubliette => $(ChamberOfSngac, ScorchedChamber, PorphyrHall, BlackHall, Fountain)
        case BlueHall => $(BlackHall, RedHall, GuardianUnderLake)
        case ScorchedChamber => $(ChamberOfSngac, Oubliette, PorphyrHall, Hyperquarium, ChamberOfApkallu, BarrierOfNaachTith)
        case PorphyrHall => $(ScorchedChamber, Oubliette, BlackHall, CharnelHall, Hyperquarium)
        case RedHall => $(BlackHall, BlueHall)
        case BlackHall => $(BlueHall, RedHall, Oubliette, PorphyrHall, CharnelHall)
        case ChamberOfApkallu => $(ScorchedChamber, Hyperquarium)
        case Hyperquarium => $(ChamberOfApkallu, PorphyrHall, CharnelHall, LakeOfHaliOverlook)
        case CharnelHall => $(BlackHall, PorphyrHall, Hyperquarium, TheCrawlingOnes)
        case TheCrawlingOnes => $(CharnelHall, LarvaeOfOuterGods)
    }

    def connected(region : Region) = {
        val base = baseConnected(region)
        if (archways.contains(region)) withArch(region, base) else base
    }

    override def connectedForRetreat(region : Region) : $[Region] = baseConnected(region)

    def distance(a : Region, b : Region) =
        if (a == b) 0 else if (connected(a).contains(b)) 1
        else if (connected(a)./~(connected).contains(b)) 2
        else if (connected(a)./~(connected)./~(connected).contains(b)) 3 else 4

    def starting(faction : Faction) = faction match {
        case GC => $(Hyperquarium); case CC => $(BlueHall); case BG => $(Fountain)
        case YS => $(FloatingTower); case SL => $(ChamberOfSngac)
        case WW => $(LakeOfHaliOverlook, Oubliette)
        case OW => regions.diff(tomeRegions); case AN => nonFactionRegions
        case TS => nonFactionRegions.%(_.glyph == Ocean); case FB => regions.diff(tomeRegions); case DS => $()
        case _ => regions
    }

    def gateXYO(r : Region) : (Int, Int) = LibraryCelaeno55.gateXYO(r)
    override def gateXYOHorizontal(r : Region) : (Int, Int) = LibraryCelaeno55.gateXYOHorizontal(r)
}

// 3L lower floor gate positions — regions are in different positions than 5L
object Library3LGates {
    import LibraryCelaeno55._
    // Vertical coords: lower floor offset by 1792
    def gateXYO(r : Region) : (Int, Int) = r match {
        case ChamberOfSngac => (367, 1792 + 493)
        case Oubliette => (907, 1792 + 762)
        case BlueHall => (1397, 1792 + 323)
        case BlackHall => (1340, 1792 + 781)
        case ChamberOfApkallu => (361, 1792 + 1186)
        case Hyperquarium => (907, 1792 + 1359)
        case TheCrawlingOnes => (1450, 1792 + 1340)
        case _ => LibraryCelaeno55.gateXYO(r)  // upper floor regions unchanged
    }
    def gateXYOHorizontal(r : Region) : (Int, Int) = {
        val (vx, vy) = gateXYO(r)
        if (vy < 1792) (vx + 1791, vy) else (vx, vy - 1792)
    }
}

object LibraryCelaeno35 extends Board {
    val id = "library35"
    val name = "Library at Celaeno (4 players, 3L5U)"
    override val isLibraryMap = true
    override def silenceTokenMax(f : Faction) : Int = 1
    override val unitScale : Double = 2.0

    import LibraryCelaeno55.{FloatingTower, Byakhiary, Horrorium, Fountain, YrAndTheNhhngr,
        GuardianUnderLake, BarrierOfNaachTith, Gloomloft, CursedHall, LarvaeOfOuterGods,
        LakeOfHaliOverlook, LakeOfHaliBalcony,
        ChamberOfSngac, Oubliette, BlueHall, ChamberOfApkallu, BlackHall,
        Hyperquarium, TheCrawlingOnes, tomeRegions}

    // 5U upper (all 12) + 3L lower (7 regions)
    val regions = $(FloatingTower, Byakhiary, Horrorium, Fountain, YrAndTheNhhngr,
        GuardianUnderLake, BarrierOfNaachTith, Gloomloft, CursedHall, LarvaeOfOuterGods,
        LakeOfHaliOverlook, LakeOfHaliBalcony,
        ChamberOfSngac, Oubliette, BlueHall, ChamberOfApkallu, BlackHall,
        Hyperquarium, TheCrawlingOnes)

    // BlueHall (CC's pre-printed glyph) added to align with LibraryCelaeno55 / 53.
    val nonFactionRegions = regions.diff($(FloatingTower, Fountain, BlueHall, ChamberOfSngac, Hyperquarium, LakeOfHaliOverlook, Oubliette)).diff(tomeRegions)
    val west = $(FloatingTower, Byakhiary, Horrorium, Fountain, YrAndTheNhhngr, GuardianUnderLake, BarrierOfNaachTith, Gloomloft, CursedHall, LarvaeOfOuterGods, LakeOfHaliOverlook, LakeOfHaliBalcony)
    val east = $(ChamberOfSngac, Oubliette, BlueHall, ChamberOfApkallu, BlackHall, Hyperquarium, TheCrawlingOnes)

    // 3L arch regions (no ScorchedChamber; ChamberOfApkallu replaces it)
    override val archways : Set[Region] = Set(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ChamberOfApkallu)
    private val arch35 = $(FloatingTower, Fountain, ChamberOfSngac, Oubliette, BlueHall, BlackHall, ChamberOfApkallu)
    private def withArch(self : Region, explicit : $[Region]) : $[Region] =
        (arch35.%(r => r != self) ++ explicit).distinct

    def baseConnected(region : Region) : $[Region] = region match {
        case FloatingTower => $(Byakhiary, Horrorium)
        case Byakhiary => $(FloatingTower, YrAndTheNhhngr, ChamberOfSngac)
        case Horrorium => $(FloatingTower, GuardianUnderLake, BlueHall)
        case Fountain => $(YrAndTheNhhngr, GuardianUnderLake, Gloomloft, Oubliette)
        case YrAndTheNhhngr => $(Byakhiary, Fountain, BarrierOfNaachTith)
        case GuardianUnderLake => $(Horrorium, Fountain, LarvaeOfOuterGods)
        case BarrierOfNaachTith => $(YrAndTheNhhngr, LakeOfHaliOverlook, ChamberOfApkallu)
        case Gloomloft => $(Fountain, CursedHall)
        case CursedHall => $(Gloomloft, LakeOfHaliOverlook, LakeOfHaliBalcony)
        case LarvaeOfOuterGods => $(GuardianUnderLake, LakeOfHaliBalcony, TheCrawlingOnes)
        case LakeOfHaliOverlook => $(BarrierOfNaachTith, CursedHall, LakeOfHaliBalcony, Hyperquarium)
        case LakeOfHaliBalcony => $(LakeOfHaliOverlook, CursedHall, LarvaeOfOuterGods)
        case ChamberOfSngac => $(ChamberOfApkallu, Oubliette, Byakhiary)
        case Oubliette => $(ChamberOfSngac, ChamberOfApkallu, Hyperquarium, BlueHall, BlackHall, TheCrawlingOnes, Fountain)
        case BlueHall => $(Oubliette, BlackHall, Horrorium)
        case BlackHall => $(BlueHall, Oubliette, TheCrawlingOnes)
        case ChamberOfApkallu => $(ChamberOfSngac, Oubliette, Hyperquarium, BarrierOfNaachTith)
        case Hyperquarium => $(ChamberOfApkallu, Oubliette, TheCrawlingOnes, LakeOfHaliOverlook)
        case TheCrawlingOnes => $(Hyperquarium, Oubliette, BlackHall, LarvaeOfOuterGods)
    }

    def connected(region : Region) = {
        val base = baseConnected(region)
        if (archways.contains(region)) withArch(region, base) else base
    }

    override def connectedForRetreat(region : Region) : $[Region] = baseConnected(region)

    def distance(a : Region, b : Region) =
        if (a == b) 0 else if (connected(a).contains(b)) 1
        else if (connected(a)./~(connected).contains(b)) 2
        else if (connected(a)./~(connected)./~(connected).contains(b)) 3 else 4

    def starting(faction : Faction) = faction match {
        case GC => $(Hyperquarium); case CC => $(BlueHall); case BG => $(Fountain)
        case YS => $(FloatingTower); case SL => $(ChamberOfSngac)
        case WW => $(LakeOfHaliOverlook, Oubliette)
        case OW => regions.diff(tomeRegions); case AN => nonFactionRegions
        case TS => nonFactionRegions.%(_.glyph == Ocean); case FB => regions.diff(tomeRegions); case DS => $()
        case _ => regions
    }

    def gateXYO(r : Region) : (Int, Int) = Library3LGates.gateXYO(r)
    override def gateXYOHorizontal(r : Region) : (Int, Int) = Library3LGates.gateXYOHorizontal(r)
}
