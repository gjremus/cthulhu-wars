package cws

import scala.scalajs._
import scala.scalajs.js.annotation._

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import html._

import util.canvas._

class InfoOverlay(overlay : html.Element) {
    var showing : |[String] = None
    var soonHide : Int = 0
    var soonShow : Int = 0
    var toShow : |[String] = None

    overlay.parentElement.parentElement.style.display = "none"

    overlay.parentElement.parentElement.addEventListener("click", (_ : dom.Event) => hide(), true)

    private var list : $[dom.HTMLTableElement] = $
    private var old : |[dom.HTMLTableElement] = None
    private var children : $[html.Div] = $
    private var prev : $[html.Div] = $

    def show(tableHtml : String) {
        if (showing.has(tableHtml))
            return

        prev = children
        children = $

        showing = |(tableHtml)

        overlay.parentElement.parentElement.style.display = ""

        list = 20.to(100).reverse.%(_ % 2 == 0).$./ { z =>
            val p = dom.document.createElement("div").asInstanceOf[html.Div]
            p.style.position = "absolute"
            p.style.fontSize = "" + z + "%"
            p.style.opacity = "0"
            p.style.height = "100%"
            p.style.overflowY = "hidden"
            p.innerHTML = tableHtml

            overlay.appendChild(p)
            children :+= p

            p.children(0).as[dom.HTMLTableElement].get
        }

        old = None
    }

    def hide() {
        showing = None

        overlay.parentElement.parentElement.style.display = "none"

        while (overlay.hasChildNodes())
            overlay.removeChild(overlay.lastChild)

        list = $
        prev = $
        children = $
    }

    def removePrev() {
        prev.foreach { c => overlay.removeChild(c) }
        prev = $
    }

    private def readjust() {
        if (soonHide == 1)
            hide()

        if (soonHide > 1)
            soonHide -= 1

        if (soonShow == 3 && toShow.any) {
            show(toShow.get)
            toShow = None
        }

        if (soonShow == 1) {
            removePrev()
        }

        if (list.any && soonHide == 0) {
            val target = overlay.clientHeight

            val best = list.%(_.clientHeight <= target).starting.|(list.minBy(_.clientHeight))

            if (old.has(best).not || soonShow == 1) {
                old = |(best)

                list.foreach { p =>
                    if (p == best && soonShow <= 1) {
                        p.parentElement.style.opacity = "1"
                        p.parentElement.style.pointerEvents = ""
                        p.parentElement.style.overflowY = "auto"
                        p.parentElement.style.height = (p.clientHeight <= target).??("100%")
                    }
                    else {
                        p.parentElement.style.opacity = "0"
                        p.parentElement.style.pointerEvents = "none"
                        p.parentElement.style.overflowY = "hidden"
                        p.parentElement.style.height = "100%"
                    }
                }
            }
        }

        if (soonShow > 1)
            soonShow -= 1

        dom.window.requestAnimationFrame { _ =>
            readjust()
        }
    }

    readjust()
}

object RitualTrackOverlay {
    var numPlayers : Int = 4
    var markerIndex : Int = 0
    var trackLength : Int = 9
    var ritualHistory : $[String] = $          // faction style strings, in order
    var ritualHistoryCeremony : $[Boolean] = $ // true when that ritual was a Ceremony of Annihilation
    // Tombstalker (TS): indices on the ritual track where pure Death's Head hecatomb rituals occurred (no marker advance)
    var tsPureDHMarkerIndices : $[Int] = $

    // Circle centers as (xPct, yPct) of image dimensions, for each track index
    // Positions derived from pixel clustering of track images (threshold=180).
    // Last entry = Instant Death position.
    // Positions derived from brightness-peak analysis of track images.
    // Upper-row y shifted ~9% below number label so numbers show above glyphs (same as 3p fix).
    // Lower-row y set to orb-body center from analysis (~65%).
    // Last entry = Instant Death ellipse center (x=center of ID text, y=40% to cover text).
    // 3p: top row x equal-spacing (confirmed correct); bottom row x = original near-values +2% right shift.
    // ID: x=90% so right edge touches track edge; y=43% (lowered from 40%).
    val positions3p : $[(Double, Double)] = $(
        (33.8, 47.0), (41.0, 65.0), (48.7, 47.0),
        (56.0, 65.0), (63.6, 47.0), (71.0, 65.0), (90.0, 51.0)
    )
    // 4p: top row x shifted right +2% from first, spacing expanded ~1.5% per gap; y top=53% (confirmed).
    // Bottom row y raised to 76% (was 68%: user confirmed too high by ~1/5-1/6 glyph height).
    // ID x=89% so right edge near track edge.
    val positions4p : $[(Double, Double)] = $(
        (30.5, 53.0), (37.4, 76.0), (43.6, 53.0), (49.3, 76.0),
        (55.8, 53.0), (62.0, 76.0), (69.9, 53.0), (77.7, 76.0), (89.0, 49.0)
    )
    // 5p: circle 1 x shifted right +3% (was at number-label position); circle 1 y raised to top-row level.
    // Bottom row y=72% (was 68%, still a little too high per user).
    // ID x=90% so right edge touches track edge.
    // 5p: strict alternating pattern. Top row (odd glyphs) y=53%, bottom row (even) y=72%.
    // Circle 1 x confirmed correct. Circles 2-10 shifted right +3.2% to give uniform ~6% gap from circle 1.
    val positions5p : $[(Double, Double)] = $(
        (28.0, 53.0), (34.1, 72.0), (40.6, 53.0), (46.6, 72.0),
        (51.9, 53.0), (57.7, 72.0), (63.5, 53.0), (69.9, 72.0),
        (76.1, 53.0), (82.6, 72.0), (90.0, 40.0)
    )

    def positions = numPlayers match {
        case 3 => positions3p
        case 5 => positions5p
        case _ => positions4p
    }

    def trackImageId = "ritual-track-" + numPlayers + "p"
}

// Tombstalker (TS) Cursed Tomes overlay: tracks tome ownership per faction and tome text descriptions (I-XI)
object TSCursedTomesOverlay {
    // Map from faction style to list of (tomeNum, isFaceDown)
    var factionTomes : scala.collection.immutable.Map[String, $[(Int, Boolean)]] = scala.collection.immutable.Map()
    var tomesOnCard : Int = 0

    // Tome text styled like menu/log text: faction-colored unit names, doom/elder-sign
    // colored resource names. Inline HTML lets the row td color attribute act only as the
    // fallback (face-down rows use the parent color via opacity, not by overwriting spans).
    private val P  = """<span class="power">Power</span>"""
    private val P1 = """<span class="power">1 Power</span>"""
    val tomeFactionPowerBlock : String = {
        val curse = """<span class="ability-color">Cursed Tomes</span>""" +
                    """<span class="cost-color"> (Ongoing)</span>""" +
                    """<span class="nt">: When you perform a Ritual of Annihilation you may remove any of your Tomes on your Faction Sheet from the game.</span>"""
        val final_rev = """<span class="ability-color">The Final Revelation</span>""" +
                        """<span class="cost-color"> (Game End)</span>""" +
                        """<span class="nt">: If at the end of the game, if the Instant Death marker has not been reached on the Ritual Track, each player loses 1 Doom for each face-down Tome that they have on their Faction Sheet.</span>"""
        s"""<tr><td style="padding:8px 12px;border-top:1px solid grey;font-size:88%;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">$curse</td></tr>""" +
        s"""<tr><td style="padding:8px 12px;font-size:88%;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">$final_rev</td></tr>"""
    }
    private val TS = """<span class="ts">Tombstalker</span>"""
    private val TH = """<span class="ts">Tomb-Herd</span>"""
    private val DT = """<span class="ts">Deep Tendril</span>"""
    private val DH = """<span class="ts">Death's Head</span>"""
    private val ES = """<span class="es">Elder Sign</span>"""
    private val D  = """<span class="doom">Doom</span>"""
    private val D1 = """<span class="doom">1 Doom</span>"""
    private val RT = """<span class="doom">Ritual Track</span>"""

    val tomeTexts : scala.collection.immutable.Map[Int, String] = scala.collection.immutable.Map(
        1  -> s"Gain $P1. $TS places a $TH at their Controlled Gate if able.",
        2  -> s"Gain $P1. $TS places a $TH at their Controlled Gate if able.",
        3  -> s"Gain $P1. $TS places a $DT at their Controlled Gate if able.",
        4  -> s"Gain $P1. $TS places a $DT at their Controlled Gate if able.",
        5  -> s"Gain $P1. $TS gains $D1.",
        6  -> s"Gain $P1. $TS gains $D1.",
        7  -> s"Gain $P1. $TS gains $DH equal to the number of $TH in play.",
        8  -> s"Gain $P1. $TS gains $DH equal to the number of $TH in play.",
        9  -> s"Gain $P1. $TS gains an $ES.",
        10 -> s"Gain $P1. $TS gains an $ES.",
        11 -> s"Gain $P1. $TS gains $D equal to the $RT marker minus 5."
    )
}

// [v2.4.10] Bubastis Moon placement bitmap.
//
// Per BB rule design and the in-game Moon overlay: when many Earth Cats sit on
// the Moon, their sprites must scatter INSIDE the moon disc — not all over the
// surrounding container. The previous polar-scatter math used 70% of the moon
// image height as its scatter radius, which sent sprite centres beyond the
// image edges and produced "cats spread across the whole screen" behaviour.
//
// Fix (2026-06-02): use a dedicated placement BITMAP — same approach as
// earth*-place.webp / library*-place.webp. The bitmap is map-shaped (so the
// circle's centre can be calibrated against the map for future re-use), with
// a single solid-colour magenta (#FF00FF) circle covering 90% of the moon
// disc's diameter. Magenta is verified distinct from every Earth and Library
// region colour in EarthRegionPalette / RegionPalette.
//
// Two bitmaps:
//   bb-moon-h-place.webp  — horizontal landscape (1791×894), circle at
//                           (75%, 25%) of the map = top-right of map view.
//   bb-moon-place.webp    — vertical portrait    (894×1791), circle at
//                           (75%, 25%) of the rotated canvas.
//
// At runtime this object samples the chosen bitmap once, finds every magenta
// pixel, normalises each pixel position to a fraction of the bitmap circle's
// bounding box, and exposes those normalised points so the overlay layer can
// map them onto the moon disc image. This guarantees every sprite lands
// strictly inside the moon disc.
object MoonPlacement {
    private var pointsHorizontal : scala.Option[Array[(Double, Double)]] = scala.None
    private var pointsVertical   : scala.Option[Array[(Double, Double)]] = scala.None

    private def sample(assetId : String) : Array[(Double, Double)] = {
        val img = dom.document.getElementById(assetId).asInstanceOf[html.Image]
        if (img == null || img.width == 0 || img.height == 0) return Array.empty
        val w = img.width
        val h = img.height
        val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
        canvas.width = w
        canvas.height = h
        val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        ctx.drawImage(img, 0, 0)
        val data = ctx.getImageData(0, 0, w, h).data
        // First pass: find bounding box of magenta circle.
        var minX = w; var maxX = -1; var minY = h; var maxY = -1
        var y = 0
        while (y < h) {
            var x = 0
            while (x < w) {
                val i = (y * w + x) * 4
                val r = data(i)
                val g = data(i + 1)
                val b = data(i + 2)
                if (r > 200 && g < 60 && b > 200) {
                    if (x < minX) minX = x
                    if (x > maxX) maxX = x
                    if (y < minY) minY = y
                    if (y > maxY) maxY = y
                }
                x += 1
            }
            y += 1
        }
        if (maxX < 0 || maxY < 0) return Array.empty
        val bw = (maxX - minX).max(1).toDouble
        val bh = (maxY - minY).max(1).toDouble
        // Second pass: every 6th pixel — plenty of candidates without O(W·H) cost
        // each render. Stored as (xFrac, yFrac) in [0..1] of the circle's bbox.
        val buf = scala.collection.mutable.ArrayBuffer.empty[(Double, Double)]
        var py = minY
        while (py <= maxY) {
            var px = minX
            while (px <= maxX) {
                val i = (py * w + px) * 4
                val r = data(i)
                val g = data(i + 1)
                val b = data(i + 2)
                if (r > 200 && g < 60 && b > 200) {
                    buf += (((px - minX) / bw, (py - minY) / bh))
                }
                px += 6
            }
            py += 6
        }
        buf.toArray
    }

    def horizontal : Array[(Double, Double)] = {
        if (pointsHorizontal.isEmpty)
            pointsHorizontal = scala.Some(sample("bb-moon-h-place"))
        pointsHorizontal.get
    }
    def vertical : Array[(Double, Double)] = {
        if (pointsVertical.isEmpty)
            pointsVertical = scala.Some(sample("bb-moon-place"))
        pointsVertical.get
    }

    def scatter(n : Int, useHorizontal : Boolean, seed : Int) : Array[(Double, Double)] = {
        val pool = if (useHorizontal) horizontal else vertical
        if (pool.isEmpty || n <= 0) return Array.empty
        val rng = new scala.util.Random(seed)
        val out = scala.collection.mutable.ArrayBuffer.empty[(Double, Double)]

        val textZones : Array[(Double, Double, Double, Double)] = Array(
            (0.13, 0.57, 0.88, 0.90)
        )

        def inTextZone(xf : Double, yf : Double) : Boolean = {
            var z = 0
            while (z < textZones.length) {
                val (x1, y1, x2, y2) = textZones(z)
                if (xf >= x1 && xf <= x2 && yf >= y1 && yf <= y2) return true
                z += 1
            }
            false
        }

        def centerWeight(xf : Double, yf : Double) : Double = {
            val dx = (xf - 0.5) * 2.0
            val dy = (yf - 0.5) * 2.0
            val dist = Math.sqrt(dx * dx + dy * dy)
            val w = 1.0 - 0.6 * dist * dist
            if (w < 0.1) 0.1 else w
        }

        val innerPool = pool.filter { case (xf, yf) => !inTextZone(xf, yf) }
        val usePool = if (innerPool.nonEmpty) innerPool else pool

        val startIdx = rng.nextInt(usePool.length)
        out += usePool(startIdx)
        var i = 1
        while (i < n) {
            var best : (Double, Double) = usePool(rng.nextInt(usePool.length))
            var bestScore = -1.0
            var tries = 0
            while (tries < 60) {
                val cand = usePool(rng.nextInt(usePool.length))
                var minD2 = Double.MaxValue
                var k = 0
                while (k < out.length) {
                    val (ox, oy) = out(k)
                    val dx = cand._1 - ox
                    val dy = cand._2 - oy
                    val d2 = dx * dx + dy * dy
                    if (d2 < minD2) minD2 = d2
                    k += 1
                }
                val score = minD2 * centerWeight(cand._1, cand._2)
                if (score > bestScore) {
                    bestScore = score
                    best = cand
                }
                tries += 1
            }
            out += best
            i += 1
        }
        out.toArray
    }
}

// Faceless Blight (FBE) — Faction-Card dice-pool detail overlay state (Task 3.11.9 / §4.0.7).
// FBE's pseudo-currency is the list of dice on its Faction Card (§1.6/§2.4), stored as
// pip values 1-6 on game.fbeCardDice. This object mirrors the Cursed-Tomes / dynamic-overlay
// pattern (TSCursedTomesOverlay): the main render loop in CthulhuWarsSolo.scala copies the
// live dice into FBEFactionCardOverlay.dice each tick, and info("FBE","FactionCard") renders
// the detail panel. Faces follow FBEExpansion.face: pip 6 = Kill, 4-5 = Pain, 1-3 = Miss.
// Byagoona's combat = the count of Kill+Pain faces (pip >= 4). All mutations to the underlying
// list flow through Hard actions only (Task 3.11.2); this object is render-only state.
object FBEFactionCardOverlay {
    // Pip values (1-6) currently on the FBE Faction Card, copied from displayGame each tick.
    var dice : $[Int] = $
    // Whether FBE is in the current game (drives whether the strip/overlay renders at all).
    var inGame : Boolean = false
    // True once any dice source is acquired (Awaken / Changeling Adherents), so the strip can
    // show "0" rather than hide entirely, per FCG learning #9.
    var sourceAcquired : Boolean = false
}

object Overlays {
    val overlay = new InfoOverlay(dom.document.getElementById("overlay").asInstanceOf[html.Element])
    var temp = false

    // [2026-04-03] Force-refresh the overlay if it's currently showing
    def refreshIfShowing() {
        if (overlay.showing.any) {
            // Regenerate content by re-clicking with the same args
            // Clear showing so show() doesn't early-return
            val wasShowing = overlay.showing
            overlay.showing = None
            // Re-show with current data
            val text = info($("RoA"))
            if (text.any) {
                overlay.show(text.get)
            }
        }
    }

    @JSExportTopLevel("onExternalClick")
    def onExternalClick(s : Any*) {
        temp = false

        println("onExternalClick args: " + s.mkString(" | "))
        val text = info(s.$)
        println("onExternalClick text.any=" + text.any)

        overlay.soonHide = 0
        overlay.soonShow = 0

        if (text.none)
            overlay.hide()
        else {
            overlay.toShow = text
            overlay.soonShow = 3
            overlay.soonHide = 0
        }
    }

    @JSExportTopLevel("onExternalOver")
    def onExternalOver(s : Any*) {
        // println("onExternalOver " + s)

        if (overlay.showing.none || overlay.soonHide > 0) {
            overlay.toShow = info(s.$)
            if (overlay.toShow.any) {
                overlay.soonShow = 30
                overlay.soonHide = 0
                temp = true
            }
        }
    }

    @JSExportTopLevel("onExternalOut")
    def onExternalOut(s : Any*) {
        // println("onExternalOut " + s)

        if (overlay.showing.any || overlay.soonShow > 0) {
            if (temp) {
                overlay.soonHide = 20
                overlay.soonShow = 0
                temp = false
            }
        }
    }

    def imageSource(id : String) = hrf.web.getElem(id).as[dom.html.Image]./(_.src).|!("unknown image source " + id)



    // Fix HB-77 (2026-06-06): Y'Golonac dynamic awaken-cost display reads the
    // current DC spellbook count from the live Game. Set by the main render
    // loop on each draw (CthulhuWarsSolo.scala uses displayGame).
    var currentGame : |[Game] = None

    implicit class ElementString(val s : String) extends AnyVal {
        def & = "<span style=inline-block>" + s + "</span>"
    }

    def info(s : $[Any]) : |[String] = (s @@ {
        case $("GC") => faction(GC, "info:gc-background", Immortal, "Ongoing", "Once Cthulhu has Awakened, he costs only 4 Power each subsequent time he is Awakened. Whenever you Awaken any Great Old One, gain <span class=es>1 Elder Sign.</span>",
            $(), $(
            (Acolyte,   6, "1", "0", s"""<div class=p>Spellbook: ${reference(GC, Dreams)}</div>"""),
            (DeepOne,   4, "1", "1", s"""<div class=p>Spellbook: ${reference(GC, Devolve)}</div>"""),
            (Shoggoth,  2, "2", "2", s"""<div class=p>Spellbook: ${reference(GC, Absorb)}</div>"""),
            (Starspawn, 2, "3", "3", s"""<div class=p>Spellbook: ${reference(GC, Regenerate)}</div>"""),
            (Cthulhu,   1, "10/4", "6", s"""
                <div class=p>${cost(s"How to Awaken ${Cthulhu.name}:")}</div>
                <div class=p>${cost("1)")} There must be a Gate in Great Cthulhu's starting Area (Can be abandoned or enemy-Controlled).</div>
                <div class=p>${cost("2)")} If this is the first Awakening, pay <span class=cost-color>10 Power</span>. Otherwise pay <span class=cost-color>4 Power</span>.</div>
                <div class=p>${cost("3)")} ${Cthulhu.name} appears in its starting Area (Remember to gain <span class=es>1 Elder Sign</span>).</div>
                <div class=p>${ref(Devour)} ${cost("(Pre-Battle):")} The enemy player chooses and Eliminates one of his Monsters or Cultists in the Battle.</div>
                <div class=p>Spellbooks: ${reference(GC, Submerge)}, ${reference(GC, YhaNthlei)}</div>"""
            ),
        ))

        case $("GC", FirstDoomPhase.text) => requirement("Receive this Spellbook in the first Doom Phase.<br/>Receive <span class=es>1 Elder Sign.</span>")
        case $("GC", KillDevour1.text) => requirement("Kill and/or Devour an enemy Unit in a Battle.<br/><br/>* You may earn both Spellbooks in a single Battle, if you Kill and/or Devour 3 or more Units.")
        case $("GC", KillDevour2.text) => requirement("Kill and/or Devour 2 enemy Units in a Battle.<br/><br/>* You may earn both Spellbooks in a single Battle, if you Kill and/or Devour 3 or more Units.")
        case $("GC", AwakenCthulhu.text) => requirement("Awaken Cthulhu.")
        case $("GC", OceanGates.text) => requirement("Control 3 Gates in Ocean/sea Areas<br/>OR<br/>4 Gates exist in Ocean/sea Areas.")
        case $("GC", FiveSpellbooks.text) => requirement("This must be the last Faction Spellbook you receive.<br/>It must be taken during the Doom Phase.<br/>Receive <span class=es>1 Elder Sign.</span>")

        case $("GC", Dreams.name) => spellbook(Dreams.name, "Action: Cost 2", "Choose an Area containing an enemy’s Acolyte Cultist. Your enemy must Eliminate one of his Acolyte Cultists from that Area and replace it with one from your Pool.")
        case $("GC", Devolve.name) => spellbook(Devolve.name, "Ongoing", "After any player's Action, replace one or more of your Acolyte Cultists anywhere on the Map with Deep Ones from your Pool.")
        case $("GC", Absorb.name) => spellbook(Absorb.name, "Pre-Battle", "If a Shoggoth is present, Eliminate one or more of your Monsters or Cultists in the Battle. For each Unit so removed, add 3 dice to the Shoggoth's Combat for that Battle.")
        case $("GC", Regenerate.name) => spellbook(Regenerate.name, "Post-Battle", "Assign up to 2 Kill or Pain Battle results to the same Starspawn. If 2 Kills are applied, the Starspawn is Killed. On any other combination of Kill or Pain results, the Starspawn is only Pained.")
        case $("GC", Submerge.name) => spellbook(Submerge.name, "Action: Cost 1", "If Cthulhu is in an ocean or sea Area, remove him from the Map and place him on your Faction Card, along with any or all of your Units in the Area. Later, as a 0-cost Action, you may place Cthulhu, plus all accompanying Units, into any Area.")
        case $("GC", YhaNthlei.name) => spellbook(YhaNthlei.name, "Gather Power Phase", "During Gather Power, if Cthulhu is in play, gain 1 Power for each enemy-controlled Gate in an ocean or sea Area.")


        case $("CC") => faction(CC, "info:cc-background", Flight, "Ongoing", "All your units can fly (even Cultists). When moved, they can travel 2 Areas. They can fly over Areas containing enemy Units.",
            $(Madness), $(
            (Acolyte,       6, "1", "0", ""),
            (Nightgaunt,    3, "1", "0", s"""<div class=p>Spellbook: ${reference(CC, Abduct)}</div>"""),
            (FlyingPolyp,   3, "2", "1", s"""<div class=p>Spellbook: ${reference(CC, Invisibility)}</div>"""),
            (HuntingHorror, 2, "3", "2", s"""<div class=p>Spellbook: ${reference(CC, SeekAndDestroy)}</div>"""),
            (Nyarlathotep,  1, "10", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Nyarlathotep.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate.</div>
                <div class=p>${cost("2)")} Pay ${power(10)}. Nyarlathotep appears at the controlled Gate.</div>
                <div class=p>${combat} Equals the total of your own Faction Spellbooks plus the Faction Spellbooks of your opponent in the Battle.</div>
                <div class=p>${ref(Harbinger)} ${cost("(Post-Battle):")} If Nyarlathotep is in a Battle in which one or more enemy Great Old Ones are Pained or Killed, you receive Power equal to half the cost to Awaken those Great Old Ones. Per enemy Great Old One, you may choose to receive 2 Elder Signs instead of Power. Harbringer takes effect even if Nyarlathotep is Killed or Pained in the Battle himself.</div>
                <div class=p>Spellbooks: ${reference(CC, Emissary)}, ${reference(CC, ThousandForms)}</div>"""
            ),
        ))

        case $("CC", Pay4Power.text) => requirement("As your Action, pay 4 Power.*<br/><br/>* You may earn both these Spellbooks in a single Round by paying 10 Power.")
        case $("CC", Pay6Power.text) => requirement("As your Action, pay 6 Power.*<br/><br/>* You may earn both these Spellbooks in a single Round by paying 10 Power.")
        case $("CC", Gates3Power12.text) => requirement("Control three Gates OR have 12 Power.")
        case $("CC", Gates4Power15.text) => requirement("Control four Gates OR have 15 Power.")
        case $("CC", CaptureCultist.text) => requirement("Capture an enemy Cultist.")
        case $("CC", AwakenNyarlathotep.text) => requirement("Awaken Nyarlathotep.")

        case $("CC", Abduct.name) => spellbook(Abduct.name, "Pre-Battle", "Eliminate one or more Nightgaunts from the Battle. For each one Eliminated, your enemy must Eliminate one of his own Monsters or Cultists from the Battle.")
        case $("CC", Invisibility.name) => spellbook(Invisibility.name, "Pre-Battle", "Select one Monster or Cultist (from either Faction) for each Flying Polyp present and \"exempt\" it. The selected Unit does not participate in the rest of the Battle.")
        case $("CC", SeekAndDestroy.name) => spellbook(SeekAndDestroy.name, "Pre-Battle", "Immediately move any or all Hunting Horrors from any Area to the Battle Area.")
        case $("CC", Emissary.name) => spellbook(Emissary.name, "Post-Battle", "Unless an enemy Great Old One is involved in the Battle, a Kill applied to Nyarlathotep becomes a Pain. If Nyarlathotep cannot be Pained due to being surrounded, he is not Eliminated.")
        case $("CC", ThousandForms.name) => spellbook(ThousandForms.name, "Action: Cost 0", s"If Nyarlathotep is in play, roll 1d6. Your foes lose that much Power between them; they have 1 minute to decide how much each loses. If they cannot agree, you get the rolled number as Power added to your total. This spellbook cannot be used again this Action Phase.")
        case $("CC", Madness.name) => spellbook(Madness.name, "Post-Battle", "After all Pain results have been assigned, you, rather than the Units' owners, choose the Area(s) to which all Pained Units will go. You may apply these results in any order (rather than the normal 'attacker first, then defender'), but you must still follow all other rules. Do this even for Battles in which you did not participate.")


        case $("BG") => faction(BG, "info:bg-background", Fertility, "Ongoing", "You may Summon Monsters as an Unlimited Action.",
            $(BloodSacrifice), $(
            (Acolyte,       6, "1", "0/1", s"""<div class=p>${combat} 1 with ${reference(BG, Frenzy)}</div>"""),
            (Ghoul,         2, "1/0", "0", s"""
                <div class=p>${cost("Cost:")} 0 with ${reference(BG, ThousandYoung)}.</div>
                <div class=p>Spellbook: ${reference(BG, Necrophagy)}</div>"""
            ),
            (Fungi,         4, "2/1", "1", s"""
                <div class=p>${cost("Cost:")} 1 with ${reference(BG, ThousandYoung)}.</div>
                <div class=p>Spellbook: ${reference(BG, Ghroth)}</div>"""
            ),
            (DarkYoung,     3, "3/2", "2", s"""
                <div class=p>${cost("Cost:")} 2 with ${reference(BG, ThousandYoung)}.</div>
                <div class=p>Spellbook: ${reference(BG, RedSign)}</div>"""
            ),
            (ShubNiggurath, 1, "8", "?", s"""
                <div class=p>${cost(s"How to Awaken ${ShubNiggurath.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate, and at least 2 Cultists on the Map &mdash; they can be in any Area(s).</div>
                <div class=p>${cost("2)")} Pay ${power(8)}.</div>
                <div class=p>${cost("3)")} Remove your 2 Cultists, then place Shub-Niggurath at your Controlled Gate.</div>
                <div class=p>${combat} Equals to the sum of your Controlled Gates and in-play Cultists. If you have The Red Sign, add another +1 for each Dark Young you have in play.</div>
                <div class=p>${ref(Avatar)} ${cost("(Action: Cost 1):")} Choose an Area and a Faction. Swap the location of Shub-Niggurath with that of a Monster or Cultist in the chosen Area. The owner of the chosen Faction chooses which Unit to relocate.</div>"""
            ),
        ))

        case $("BG", Spread4.text) => requirement("Have units in 4 Areas.")
        case $("BG", Spread6.text) => requirement("Have units in 6 Areas.")
        case $("BG", Spread8.text) => requirement("Have units in 8 Areas.")
        case $("BG", SpreadSocial.text) => requirement("Share Areas with all enemies. Gain 1 Power per enemy player.")
        case $("BG", EliminateTwoCultists.text) => requirement("As your Action, for a Round, Eliminate 2 of your Cultists from any Area(s) on the Map.")
        case $("BG", AwakenShubNiggurath.text) => requirement("Awaken Shub-Niggurath.")

        case $("BG", ThousandYoung.name) => spellbook(ThousandYoung.name, "Ongoing", "If Shub-Niggurath is in play, Ghouls, Fungi, and Dark Young cost 1 fewer Power each to Summon.")
        case $("BG", Frenzy.name) => spellbook(Frenzy.name, "Battle", "Your Cultists now have 1 Combat.")
        case $("BG", Necrophagy.name) => spellbook(Necrophagy.name, "Post-Battle", "Move any or all Ghouls (who did not partecipate in the Battle) from any Area to the Battle Area, even if your Faction was not involved in the Battle. For each Ghoul so moved, both sides in the Battle suffer an additional Pain result.")
        case $("BG", Ghroth.name) => spellbook(Ghroth.name, "Action: Cost 2", "Roll a die. If the roll is less than or equal to the number of Areas containing Fungi, your enemies must collectively Eliminate Cultists equal to the die roll. They have 1 minute to decide how to distribute these Eliminations. If time runs out, you choose for them. If the roll is greater than the number of Areas with Fungi, place 1 Acolyte from any Faction's pool anywhere on the map.")
        case $("BG", RedSign.name) => spellbook(RedSign.name, "Ongoing", "Dark Young can Create and Control Gates. Each Dark Young adds 1 to Shub-Niggurath's Combat and each provides 1 Power during the Gather Power Phase. They do not act as Cultists with respect to any other purpose.")
        case $("BG", BloodSacrifice.name) => spellbook(BloodSacrifice.name, "Doom Phase", "If Shub-Niggurath is in play during the Doom Phase, you can choose to Eliminate one of your Cultists (from anywhere on the map). If you do, gain <span class=es>1 Elder Sign.</span>")


        case $("YS") => faction(YS, "info:ys-background", Feast, "Gather Power Phase", "During Gather Power, you gain 1 Power for each Area containing both a Desecration Token and one or more of your units.",
            $(), $(
            (Acolyte,   6, "1",  "0", s"""<div class=p>Spellbook: ${reference(YS, Passion)}</div>"""),
            (Undead,    6, "1", "1-", s"""
                <div class=p>${combat} Roll 1 die less than the total Undead in the area.</div>
                <div class=p>Spellbook: ${reference(YS, Zingaya)}</div>"""),
            (Byakhee,   4, "2", "1+", s"""
                <div class=p>${combat} Roll 1 die more than the total Byakhee in the area.</div>
                <div class=p>Spellbook: ${reference(YS, Shriek)}</div>"""),
            (KingInYellow, 1, "4", "0", s"""
                <div class=p>${cost(s"How to Awaken the ${KingInYellow.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Unit in an Area lacking a Gate.</div>
                <div class=p>${cost("2)")} Pay ${power(4)}. ${KingInYellow.name} appears in that Area.</div>
                <div class=p>${ref(Desecrate)} ${cost("(Action: Cost 2):")} If the King is in an Area with no Desecration Token, roll 1 die and compare to your total units in the Area (including the King). On a roll equal or less than your unit total, place a Desecration Token in the Area. If you succeed or fail, place a Monster or Cultist with a cost of 2 or less in the Area.</div>
                <div class=p>Spellbook: ${reference(YS, ScreamingDead)}</div>"""
            ),
            (Hastur, 1, "10", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Hastur.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate and the King in Yellow in the same area.</div>
                <div class=p>${cost("2)")} Pay ${power(10)}. ${Hastur.name} appears in the King's Area.</div>
                <div class=p>${combat} Equals the current Cost of a Ritual of Annihilation.</div>
                <div class=p>${ref(Vengeance)} ${cost("(Post-Battle):")} If Hastur is involved in a Battle, choose which Combat results are applied to which enemy Unit.</div>
                <div class=p>Spellbooks: ${reference(YS, ThirdEye)}, ${reference(YS, HWINTBN)}</div>"""
            ),
        ))

        case $("YS", Provide3Doom.text) => requirement("As your Action for a round, select another player.<br/>That player gains three Doom points.")
        case $("YS", AwakenKing.text) => requirement("Awaken the King in Yellow.")
        case $("YS", DesecrateAA.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-aa")} class=inline-glyph />")
        case $("YS", DesecrateOO.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-oo")} class=inline-glyph />")
        case $("YS", DesecrateWW.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-ww")} class=inline-glyph />")
        case $("YS", AwakenHastur.text) => requirement("Awaken Hastur. Also receive <span class=es>1 Elder Sign</span>.")

        case $("YS", Passion.name) => spellbook(Passion.name, "Ongoing", "When one or more of your Cultists are Eliminated by an enemy (Killed, Captured, etc.), gain 1 Power total.")
        case $("YS", Zingaya.name) => spellbook(Zingaya.name, "Action: Cost 1", "If Undead are in an Area with enemy Acolyte Cultists, your enemy must Eliminate an Acolyte Cultist. Then, place an Undead in the Area.")
        case $("YS", Shriek.name) => spellbook(Shriek.name, "Action: Cost 1", "Move any or all Byakhee from their current Area(s) to any one Area on the Map.")
        case $("YS", ScreamingDead.name) => spellbook(ScreamingDead.name, "Action: Cost 1", "Move the King in Yellow to an adjacent Area. Any Undead in the same Area can move with him for free. You may then take a second, different Action. You may NOT take He Who is Not to be Named as your second Action.")
        case $("YS", ThirdEye.name) => spellbook(ThirdEye.name, "Ongoing", "If Hastur is in play, the cost of Desecration is reduced to 1. If the Desecration succeeds, you also get 1 Elder Sign.")
        case $("YS", HWINTBN.name) => spellbook(HWINTBN.name, "Action: Cost 1", "Move Hastur to any Area containing a Cultist of any Faction. You may then take a second, different Action. You may NOT take The Screaming Dead as your second Action.")


        case $("SL") => slFactionOverlay(false, false)
        case $("SL", easierSBR : Boolean, energyNexusPreBattle : Boolean) => slFactionOverlay(easierSBR, energyNexusPreBattle)

        case $("SL", Pay3SomeoneGains3.text) => requirement("As your action spend 3 Power. Select another player.<br/>He gains 3 Power.")
        case $("SL", Pay3EverybodyGains1.text) => requirement("As your action spend 3 Power.<br/>Each other player gains 1 Power.")
        case $("SL", Pay3EverybodyLoses1.text) => requirement("As your action spend 3 Power.<br/>Each other player loses 1 Power.")
        // Easier SBR variants
        case $("SL", "Pay your last power") => requirement("Pay your last power.")
        case $("SL", "Pay your last 2 power") => requirement("Pay your last 2 power.")
        case $("SL", "Pay your last 3 power") => requirement("Pay your last 3 power.")

        case $("SL", Roll6DiceInBattle.text) => requirement("Roll 6 or more combat dice in a single Battle.")
        case $("SL", PerformRitual.text) => requirement("Perform a Ritual of Annihilation.")
        case $("SL", AwakenTsathoggua.text) => requirement("Awaken Tsathoggua.")

        case $("SL", Burrow.name) => spellbook(Burrow.name, "Ongoing", "After a Move Action in which you spend 2 or more Power moving Units, regain 1 Power.")
        case $("SL", EnergyNexus.name) => spellbook(EnergyNexus.name, "Ongoing", "Just before a Battle in an Area containing a Wizard, you may take one Action that originates in the Area for its normal Power cost. The Battle proceeds once that Action is finished, starting with Pre-Battle Spellbooks and abilities.")
        case $("SL", EnergyNexus.name, true) => spellbook(EnergyNexus.name, "Pre-Battle", "Before a Battle in an Area containing a Wizard, you may take one Action that originates in the Area for its normal Power cost. The Battle proceeds once that Action is finished.")
        case $("SL", AncientSorcery.name) => spellbook(AncientSorcery.name, "Action: Cost 1", "Remove a Serpent Man from the Map and place him on an enemy's Faction Card. You now have access to that Faction's Unique Ability until the end of the next Doom Phase. At that point, gain 1 Power and replace the Serpent Man anywhere on the Map. If a Faction's Unique Ability mentions a Great Old One, it is also considered to include Tsathoggua.")
        case $("SL", CaptureMonster.name) => spellbook(CaptureMonster.name, "Action: Cost 1", "Tsathoggua can Capture Enemy Monsters in the same manner as Cultists are Captured. They are sacrificed for 1 Power in the next Gather Power Phase.")
        case $("SL", DemandSacrifice.name) => spellbook(DemandSacrifice.name, "Pre-Battle", "If Tsathoggua is in play, your enemy chooses ONE of the following options before a Battle with you:<br/>1) You gain <span class=es>1 Elder Sign</span>.<br/>OR<br/>2) All of their Kill results against your Units in this Battle count as Pains instead.")
        case $("SL", CursedSlumber.name) => spellbook(CursedSlumber.name, "Action: Cost 1", "Remove your Controlled Gate and its Cultist from the map and place it on your Faction Card. This Gate and Cultist still provide Power and Doom points, but are immune to enemy abilities. As a Cost 1 Action, return the Gate and Cultist to any Area lacking a Gate. You may only have one Gate on your Faction Card at a time.")

        // Sleeper buff option overlays
        case $("SleeperEasierSBR") => spellbook("Easier Spellbook Requirements", "Variant", "The 3 power related Spellbook Requirements are changed from their current 'Pay 3 power, enemies gain/ lose power' to 'Pay your last power; Pay your last 2 power, and Pay your last 3 power.'")
        case $("SleeperEnergyNexusPreBattle") => spellbook("Energy Nexus - Pre Battle", "Variant", "Energy Nexus is changed from an interrupt that occurs before Pre-battle, to a standard Pre-battle power (for example, Cthulhu's Devour will trigger before Energy Nexus).")


        case $("WW") => faction(WW, "info:ww-background", Hibernate, "Action: Cost 0", "Add +1 Power to your total for each enemy Great Old One in play (but not more than your current Power). You can take no further Actions during this Action Phase. At the start of the next Gather Power Phase, do NOT lose your Power, but add it to your total.",
            $(IceAge, Herald), $(
            (Acolyte,   6, "1", "0", s"""<div class=p>Spellbook: ${reference(WW, Cannibalism)}</div>"""),
            (Wendigo,   4, "1", "1", s"""<div class=p>Spellbooks: ${reference(WW, Cannibalism)}, ${reference(WW, Howl)}</div>"""),
            (GnophKeh,  4, "?", "3", s"""
                <div class=p>${cost("Cost:")} Equals the number of Gnoph-Kehs in your Unit pool.</div>
                <div class=p>${reference(WW, Berserkergang)}</div>"""
            ),
            (RhanTegoth, 1, "6", "3", s"""
                <div class=p>${cost(s"How to Awaken ${RhanTegoth.name}:")}</div>
                <div class=p>${cost("1)")} Pay ${power(6)}.</div>
                <div class=p>${cost("2)")} ${RhanTegoth.name} appears in an Area containing the Windwalker Glyph.</div>
                <div class=p>${ref(Eternal)} ${cost("(Post-Battle):")} If ${RhanTegoth.name} receives a Pain or a Kill, you may pay 1 Power to cancel its effect on him. He can only receive one combat result per Battle.</div>"""
            ),
            (Ithaqua, 1, "6", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Ithaqua.name}:")}</div>
                <div class=p>${cost("1)")} ${RhanTegoth.name} has been awakened (he need not be in play).</div>
                <div class=p>${cost("2)")} A Gate must exist in an Area marked with your Glyph. You need not control the Gate.</div>
                <div class=p>${cost("3)")} Pay ${power(6)} and replace the Gate with ${Ithaqua.name}.</div>
                <div class=p>${combat} Equal to half of your opponent's Doom, rounded up.</div>
                <div class=p>${ref(Ferox)} ${cost("(Ongoing):")} While ${Ithaqua.name} is in play, your Cultists cannot be captured by enemy Monsters or Terrors. They are still vulnerable to enemy Great Old Ones.</div>
                <div class=p>Spellbook: ${reference(WW, ArcticWind)}</div>"""
            ),
        ))

        case $("WW", FirstPlayer.text) => requirement("You are the First Player.")
        case $("WW", OppositeGate.text) => requirement("A Gate exists in the Area marked with the Windwalker Glyph and in which you did not start.")
        case $("WW", AnytimeGainElderSigns.text) => requirement("Take this spellbook at any time. Gain <span class=es>1 Elder Sign</span> for each enemy player with 6 Spellbooks on their Faction Card, to a maximum of <span class=es>3 Elder Signs</span>.")
        case $("WW", AnotherFactionAllSpellbooks.text) => requirement("Another Faction has 6 Spellbooks on their Faction Card.")
        case $("WW", AwakenIthaqua.text) => requirement("Awaken Ithaqua.")
        case $("WW", AwakenRhanTegoth.text) => requirement("Awaken Rhan Tegoth.")

        case $("WW", Cannibalism.name) => spellbook(Cannibalism.name, "Post-Battle", "After all Battle results have been applied, if one or more enemy Units were killed, you may place a Wendigo or Acolyte from your Pool into the Battle Area. You may do this even if you were not involved in the Battle.")
        case $("WW", Howl.name) => spellbook(Howl.name, "Pre-Battle", "Before Battle, if any Wendigos are present, you may force the enemy to Retreat one Unit (of their choice) out of the Area to an adjacent Area. This is not a Pain - the Unit may be moved to an Area containing your Units.")
        case $("WW", Berserkergang.name) => spellbook(Berserkergang.name, "Post-Battle", "For each Gnoph-Keh assigned a Kill in Battle, the enemy must Eliminate 1 Monster or Cultist.")
        case $("WW", ArcticWind.name) => spellbook(ArcticWind.name, "Ongoing", "When Ithaqua uses Move Action, any or all your Units in the same Area can Move with him for no additional cost.")
        case $("WW", IceAge.name) => spellbook(IceAge.name, "Action: Cost 1", "Place or move your Ice Age token to any Area. When an enemy Faction takes any Action ending in the Ice Age Area, they must pay +1 Power.")
        case $("WW", Herald.name) => spellbook(Herald.name, "Doom Phase", "Pay 5 Power for Windwalker's Ritual of Annihilation, regardless of the number indicated on the Ritual track.")


        case $("OW") => owFactionOverlay(false, false)
        case $("OW", cheapMutants : Boolean, yogCurseDie : Boolean) => owFactionOverlay(cheapMutants, yogCurseDie)

        case $("OW", EightGates.text) => requirement("There are 8 Gates on the Map.")
        case $("OW", TenGates.text) => requirement("There are 10 Gates on the Map.")
        case $("OW", TwelveGates.text) => requirement("There are 12 Gates on the Map.")
        case $("OW", UnitsAtEnemyGates.text) => requirement("You have Units in at least 2 Areas containing<br/>enemy-Controlled Gates.")
        // OpenerCheapMutants variant: same SBR with threshold raised to 3.
        case $("OW", "Units at 3 enemy gates") => requirement("You have Units in at least 3 Areas containing<br/>enemy-Controlled Gates.")
        case $("OW", LoseUnitInBattle.text) => requirement("Lose 1 of your own Units in Battle.")
        case $("OW", GooMeetsGoo.text) => requirement("Your Great Old One is in the same Area with<br/>an enemy Great Old One.")
        case $("OW", AwakenYogSothoth.text) => requirement("Awaken Yog-Sothoth.")

        case $("OW", TheyBreakThrough.name) => spellbook(TheyBreakThrough.name, "Ongoing", "You can Summon Monsters at enemy-Controlled and Abandoned Gates. You do not need to have any Units present in the Area.")
        case $("OW", TheyBreakThrough.name, true) => spellbook(TheyBreakThrough.name, "Ongoing", "You can Summon Monsters at enemy-Controlled and Abandoned Gates. You do not need to have any Units present in the Area.")
        case $("OW", MillionFavoredOnes.name) => spellbook(MillionFavoredOnes.name, "Post-Battle", "After Pains and Kills are resolved, replace any or all surviving Acolytes to Mutants, Mutants with Abominations, and Abominations with Spawns of Yog-Sothoth. You can replace a Spawn of Yog-Sothoth with as many Mutants as are in your Pool.")
        case $("OW", ChannelPower.name) => spellbook(ChannelPower.name, "Battle", "After rolling Battle dice, you may pay 1 Power to reroll all dice which were not Kills or Pains. You may do this more than once.")
        case $("OW", DreadCurse.name) => spellbook(DreadCurse.name, "Action: Cost 2", "Select an Area and roll 1 Battle die per Abomination and Spawn of Yog-Sothoth in play. Apply the results as Kills and Pains to enemy Factions in the Area. You choose which Faction receives which results, but they choose which of their Units receives each result. No Battle-type abilities apply. You choose to which Area each Unit is Pained, ignoring normal Pain rules.")
        case $("OW", DreadCurse.name, true) => spellbook(DreadCurse.name, "Action: Cost 2", "Select an Area and roll 1 Battle die per Abomination and Spawn of Yog-Sothoth in play. Apply the results as Kills and Pains to enemy Factions in the Area. You choose which Faction receives which results, but they choose which of their Units receives each result. No Battle-type abilities apply. You choose to which Area each Unit is Pained, ignoring normal Pain rules.<br/><br/>Also receive one battle die if Yog-Sothoth is in play. For each pain or kill assigned to a Great Old One, receive 1 Elder Sign.")
        case $("OW", DragonAscending.name) => spellbook(DragonAscending.name, "Once Only", "Once during the game (at any time), set your Power to be equal to the current Power of one chosen enemy Faction.")
        case $("OW", DragonDescending.name) => spellbook(DragonDescending.name, "Once Only", "Once during the game when you perform a Ritual of Annihilation, you receive twice the normal Doom points.")

        // Opener Buff option overlays
        case $("OpenerCheapMutants") => spellbook("Cheap Mutants + Harder SBR", "Variant", "Mutants cost 1 power. Units at 2 enemy gates requires 3.")
        case $("OpenerYogCurseDie") => spellbook("Yog Curse Die + DC GOO ES", "Variant", "Yog Sothoth contributes 1 die to Dread Curse of Azazoth when in play. If an enemy Great Old One is pained or killed by Dread Curse, Opener recives an Elder Sign.")

        // Sleeper buff option overlays
        case $("SleeperEasierSBR") => spellbook("Easier Spellbook Requirements", "Variant", "The 3 power related Spellbook Requirements are changed from their current 'Pay 3 power, enemies gain/ lose power' to 'Pay your last power; Pay your last 2 power, and Pay your last 3 power.'")
        case $("SleeperEnergyNexusPreBattle") => spellbook("Energy Nexus - Pre Battle", "Variant", "Energy Nexus is changed from an interrupt that occurs before Pre-battle, to a standard Pre-battle power (for example, Cthulhu's Devour will trigger before Energy Nexus).")

        // DS alternate spellbooks option overlay
        case $("DSAlternateSpellbooks") => spellbook("Daemon Sultan - Alternate Spellbooks", "", "<span class=ability-color>Traitors!</span>, <span class=ability-color>Fiendish Growth</span>, and <span class=ability-color>Undirected Energy</span> are replaced with:<br/><br/><span class=ability-color>Omnipotence</span> <span class=cost-color>(Action Cost: 1)</span><br/>Flip this spellbook facedown. Move any of your Avatars in play to any Areas on the Map.<br/><br/><span class=ability-color>Fiendish Spawn</span> <span class=cost-color>(Pre-Battle)</span><br/>Flip this spellbook facedown. If Avatar Antithesis is in a Battle, you may place up to two Larva of your choice from your pool into Avatar Antithesis's Area.<br/><br/><span class=ability-color>Directed Energy</span> <span class=cost-color>(Post-Battle)</span><br/>Flip this spellbook facedown. After Pains and Kills are resolved, if Avatar Thesis is in a Battle and survived, gain 1 power for each Controlled Chaos Gate in play.")


        case $("AN") => anFactionOverlay(false)
        case $("AN", altSB : Boolean) => anFactionOverlay(altSB)

        case $("AN", CathedralAA.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-aa")} class=inline-glyph />")
        case $("AN", CathedralOO.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-oo")} class=inline-glyph />")
        case $("AN", CathedralWW.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-ww")} class=inline-glyph />")
        case $("AN", CathedralNG.text) => requirement(s"A Cathedral is in an Area without<br/>any of these Glyphs: <img src=${imageSource("sign-aa")} class=inline-glyph /><img src=${imageSource("sign-oo")} class=inline-glyph /><img src=${imageSource("sign-ww")} class=inline-glyph />")
        case $("AN", GiveWorstMonster.text) => requirement("As your Action, each enemy Summons their lowest cost Monster at their Controlled Gate for free.")
        case $("AN", GiveBestMonster.text) => requirement("As your Action, each enemy Summons their highest cost Monster at their Controlled Gate for free.")

        case $("AN", Brainless.name) => spellbook(Brainless.name, "Ongoing", "Reanimated now cost 1 Power to Summon. They may only Move, Capture, or declare Battle if they share an Area with one or more of your non-Reanimated Units.")
        case $("AN", Festival.name) => spellbook(Festival.name, "Ongoing", "Un-Men now cost 0 Power to Summon. When you Summon an Un-Man, also select an enemy to gain 1 Power.")
        case $("AN", Extinction.name) => spellbook(Extinction.name, "Ongoing", "Yothans now cost 3 Power to Summon. When a Yothan is Killed or Eliminated, remove it permanently from the game.")
        case $("AN", UnholyGround.name) => spellbook(UnholyGround.name, "Post Battle", "If there is a Cathedral in the Battle Area, you may choose to remove a Cathedral from anywhere. If you do, an enemy Great Old One in the Battle must be eliminated by its owner.")
        case $("AN", Consecration.name) => spellbook(Consecration.name, "Doom Phase", "When you perform a Ritual of Annihilation, gain <span style=es>1 Elder Sign</span> if at least one Cathedral is in play. If all four Cathedrals are in play, gain <span style=es>2 Elder Signs</span> instead.")
        case $("AN", WorshipServices.name) => spellbook(WorshipServices.name, "Gather Power Phase", "Gain 1 Power for each Cathedral that shares an Area with an enemy Gate. Those enemies each gain 1 Power.")
        // Alt Ancients (AA) replacement spellbooks
        case $("AN", HolyGround.name) => spellbook(HolyGround.name, "Ongoing (Action Phase Only)", "Your Cathedrals count as 3 Cost Great Old One Units with 0 Combat. They cannot be assigned a Pain, Moved or Retreated but can be Killed like any other Unit.")
        case $("AN", Sanguinessence.name) => spellbook(Sanguinessence.name, "Ongoing", "Any time you Kill 1 or more enemy Units in or adjacent to an Area with a Cathedral, gain 1 Doom. If you Killed any Great Old Ones, gain an Elder Sign instead.")
        case $("AN", Crusade.name) => spellbook(Crusade.name, "Ongoing", "Declaring Battle against a player with equal or more Power than you costs 0 Power.")

            case $("DS") => dsFactionOverlay(false)
            case $("DS", altSB : Boolean) => dsFactionOverlay(altSB)

            case $("DS", OneLarvaEach.text) => requirement("You have one of each Larva type in play.")
            case $("DS", AbandonedGateGP.text) => requirement("An abandoned Gate is on the Map during Gather Power Phase.")
            case $("DS", PowerDoomOffer.text) => requirement("During any Doom Phase, you declare that each other player chooses whether to receive 1 Power or 1 Doom. You gain everything they gain.")
            case $("DS", AwakenAvatarThesis.text) => requirement("Awaken Avatar Thesis.")
            case $("DS", AwakenAvatarAntithesis.text) => requirement("Awaken Avatar Antithesis.")
            case $("DS", AwakenAvatarSynthesis.text) => requirement("Awaken Avatar Synthesis.")

            case $("DS", Psychosis.name) => spellbook(Psychosis.name, "Action: Cost 0", "You must have an Acolyte in your pool. Select an area that has no units from any faction. Place an acolyte from your pool there. During each Doom phase, flip ALL your face-down faction spellbooks face-up again.")
            case $("DS", CosmicRuler.name) => spellbook(CosmicRuler.name, "Post-Battle", "When any Avatar is choosen to recieve a Kill or Elimination, instead you can Eliminate another Avatar in its stead, from anywhere on the map.")
            case $("DS", AnimateMatter.name) => spellbook(AnimateMatter.name, "Action: Cost 1", "Flip this Spellbook face down. Move a controlled Chaos Gate from its Area to an adjacent Area, taking its Cultist along. You cannot move the Gate to another player’s Start Area. If the new Area has an existing physical Gate, replace that Gate with the Chaos Gate.")
            case $("DS", ChaosGateSB.name) => spellbook(ChaosGateSB.name, "Action: Cost 1", "Flip this Spellbook face down, then place a Chaos Gate in any area without a Gate.")
            case $("DS", Consummation.name) => spellbook(Consummation.name, "Action: Cost 1", "Flip this spellbook face-down. Then flip one of your face-down faction spellbooks face-up again, permitting its re-use this turn.")
            case $("DS", FiendishGrowth.name) => spellbook(FiendishGrowth.name, "Action: Cost 1", "Flip this spellbook face down if Avatar Antithesis is in play. Place 1 free Monster or Acolyte from your pool in Avatar Antithesis' Area per Faction with Units in that Area, including yours.")
            case $("DS", Traitors.name) => spellbook(Traitors.name, "Action: Cost 1", "Flip this Spellbook face down. Replace your controlled Chaos Gate with a Normal Gate, and replace its Controlling Cultist with another player's Cultist from his Pool. Your cultist returns to your Pool. If you have no other Units in the Area, gain an Elder Sign.")
            case $("DS", UndirectedEnergy.name) => spellbook(UndirectedEnergy.name, "Action: Cost 1", "Flip this spellbook face-down if Avatar Thesis is in play. Gain 1 Power per faction with units in Avatar Thesis' area, including yours.")

            // DS Alternate spellbook overlays
            case $("DS", Omnipotence.name) => spellbook(Omnipotence.name, "Action: Cost 1", "Flip this spellbook facedown. Move any of your Avatars in play to any Areas on the Map.")
            case $("DS", FiendishSpawn.name) => spellbook(FiendishSpawn.name, "Pre-Battle", "Flip this spellbook facedown. If Avatar Antithesis is in a Battle, you may place up to two Larva of your choice from your pool into Avatar Antithesis's Area.")
            case $("DS", DirectedEnergy.name) => spellbook(DirectedEnergy.name, "Post-Battle", "Flip this spellbook facedown. After Pains and Kills are resolved, if Avatar Thesis is in a Battle and survived, gain 1 power for each Controlled Chaos Gate in play.")


        case $(_, MaoCeremony.name) => spellbook(MaoCeremony.name, "Ongoing", "At the end of Gather Power, after Power has been added (i.e., before Determine First Player), you may choose to sacrifice 1 or more of your own Cultists, adding 1 Power apiece to your total.")
        case $(_, Recriminations.name) => spellbook(Recriminations.name, "Action: Cost 1", "Remove any spellbook (including this one) from your Faction Card and replace it with another available spellbook.")
        case $(_, Shriveling.name) => spellbook(Shriveling.name, "Pre-Battle", "Select an enemy Monster or Cultist in the Battle. That Unit is Eliminated, and the owner receives Power equal to the Unit's cost.")
        case $(_, StarsAreRight.name) => spellbook(StarsAreRight.name, "Ongoing", "During the Doom Phase, if you turn in Elder Signs for Doom points, you also immediately receive Power equal to their face value.")
        case $(_, UmrAtTawil.name) => spellbook(UmrAtTawil.name, "Ongoing", "Gates now cost you 2 Power to Build.")
        case $(_, Undimensioned.name) => spellbook(Undimensioned.name, "Action: Cost 2", "Rearrange your Units among their Areas as you see fit. You may completely empty an Area, but you may not move to any new Areas.")


        case $("Ghast") => loyaltyCard(GhastCard.name, GhastCard.quantity, GhastCard.cost, GhastCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place all 4 Ghasts at your controlled Gate(s).", "Hordeling", "Ongoing", "When you spend 2 Power to Summon Ghasts, all Ghasts in your pool are immediately placed on the map at any Gate(s) you control.")
        case $("Gug") => loyaltyCard(GugCard.name, GugCard.quantity, GugCard.cost, GugCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Gug at your controlled Gate.", "Clumsy", "Ongoing", "A Gug cannot Capture a Cultist.")
        case $("Shantak") => loyaltyCard(ShantakCard.name, ShantakCard.quantity, ShantakCard.cost, ShantakCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Shantak at your controlled Gate.", "Horror Steed", "Ongoing", "When Moving a Shantak, it can reach any Area on the Map. Additionally, the Shantak may carry one of your Cultists along for free.")
        case $("Voonith") => loyaltyCard(VoonithCard.name, VoonithCard.quantity, VoonithCard.cost, VoonithCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Voonith at your controlled Gate.", "Vicious", "Post-Battle", "For each Kill scored fewer than the number of Vooniths in the Battle, add 1 Kill. (Example: 2 Vooniths in Battle, roll 1 Kill = add 1 extra for total of 2; roll no Kills = add 2 Kills.)")

        case $("Star Vampire") => loyaltyCard(StarVampireCard.name, StarVampireCard.quantity, StarVampireCard.cost, StarVampireCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Star Vampire at your controlled Gate.", "Vampirism", "Ongoing", "Roll each Star Vampire's combat dice separately. Each Pain rolled drains 1 Power from the enemy Faction. Each Kill rolled drains 1 Doom from the enemy Faction. Drained points transfer to you immediately. If the enemy lacks Power or Doom, you get nothing. Pains and Kills still count toward your Battle results.")

        case $("Dimensional Shambler") => loyaltyCard(DimensionalShamblerCard.name, DimensionalShamblerCard.quantity, DimensionalShamblerCard.cost, DimensionalShamblerCard.combat, "Pay 2 Doom to obtain this Loyalty Card, then place 1 Dimensional Shambler onto your Faction Card.", "Walk Between Worlds", "Ongoing", "When Summoning a Dimensional Shambler, place it onto your Faction Card. After any Action (by any player), you may place one or more Dimensional Shamblers from your Faction Card into any Area. Once placed, Dimensional Shamblers remain on the Map (until Killed or otherwise Eliminated).")
        case $("Gnorri") => loyaltyCard(GnorriCard.name, GnorriCard.quantity, GnorriCard.cost, GnorriCard.combat, "Pay 2 Doom to obtain this Loyalty Card, then place 1 Gnorri at your Controlled Gates.", "Grottos", "Doom Phase", "Having 2 Gnorri in play earns 1 extra Doom point during the Doom Phase; having 3 Gnorri in play earns 2 extra Doom points instead.")

        // ── NEW TERRORS ──
        case $("Dhole") => loyaltyCard("Dhole", 1, 4, 5, "Pay 2 Doom + 2 Power. Place at your Controlled Gate.", "Planetary Destruction", "Post-Battle", "If the Dhole is Killed or Eliminated in Battle, its owner earns 2 Elder Signs. The opponent also receives the owner's choice of either 2 Doom or 2 Power.")
        case $("Great Race of Yith") => loyaltyCard("Great Race of Yith", 1, 4, 3, "Pay 2 Doom + 2 Power. Place at your Controlled Gate.", "Possession", "Ongoing and Gather Power Phase", "Can Capture an enemy Cultist in its Area regardless of any enemy Units, Great Old Ones, or Windwalker's Ferox ability. During the Gather Power Phase, if in play, its owner earns 1 additional Power per Captured Cultist (beyond the normal 1 Power per owned Cultist).")
        case $("Quachil Uttaus") => loyaltyCard("Quachil Uttaus", 1, 4, 1, "Pay 2 Doom + 2 Power. Place at your Controlled Gate.", "Dust to Dust", "Post-Battle", "When an enemy Unit is Killed or Eliminated in a Battle involving Quachil Uttaus, that Unit's owner must choose one option: (1) permanently remove one of their lost units from the game, OR (2) the Quachil Uttaus owner receives an Elder Sign.")
        case $("The Shadow Pharaoh") => loyaltyCard("The Shadow Pharaoh", 1, 2, 0, "Pay 2 Doom + 2 Power (CC: 0 Doom, others gain 1 ES). Place at your Controlled Gate.", "Hebephrenia", "Ongoing", "Gates cannot be Controlled by any Faction in the Shadow Pharaoh's Area. When it enters an Area, any occupying Unit immediately Abandons the Gate. (Yog-Sothoth unaffected.)")
        case $("Hound of Tindalos") => loyaltyCard("Hound of Tindalos", 1, 4, 4, "Pay 2 Doom + 2 Power (Terror). Place at ANY Gate on the map.", "Cronophage", "Ongoing", "Cannot perform Move Action by itself. Moves for free whenever you Move any other Unit, teleporting directly from an Area with a Gate to another Area with a Gate – neither need be Controlled by you or anyone.<br><br><b>Angles of Time</b> (Ongoing): Cannot be assigned a Kill in Battle. However, if ever in an Area without a Gate (due to Pain, gate destruction/movement, etc.), it is Eliminated. Also Eliminated if it cannot be Pained due to enemy presence in adjacent Areas per normal Pain rules.")
        case $("Brown Jenkin") => loyaltyCard("Brown Jenkin", 1, 2, 0, "Pay 2 Doom + 2 Power. Place at your Controlled Gate.", "Loathsome Titter", "Gather Power Phase", "If Brown Jenkin shares an area with an enemy-controlled Gate during Gather Power, gain 2 Power plus 1 more Power per enemy Cultist in the area.<br><br><b>Familiar</b> (Ongoing): If Brown Jenkin is killed or eliminated, as soon as you have at least 2 Power, pay 2 Power and place Brown Jenkin at your Controlled Gate. This doesn't count as an Action and is not optional.")
        case $("Elder Shoggoth") => loyaltyCard("Elder Shoggoth", 1, 4, 2, "Pay 2 Doom + 2 Power. Place at your Controlled Gate.", "Prime Cause", "Post-Battle", "In a Battle involving Elder Shoggoth, choose any of your Units (including it) and replace with any Unit from your Pool. You can gain a previously Awakened Faction GOO but must fulfill other requirements (e.g., Cthulhu needs his Start Area + gate). If replacing Elder Shoggoth itself, pay nothing and no enemy gains reward. Otherwise: (1) Pay half the new Unit's Power cost rounded down. (2) If new Unit is a Terror, enemy gains 1 Doom. (3) If new Unit is a Great Old One, enemy gains 1 Elder Sign.")

        // ── NEW MONSTERS ──
        case $("Moonbeast") => loyaltyCard("Moonbeast", 4, 2, 0, "Pay 2 Doom. Place onto enemy Spellbook.", "Blasphemous Obeisance", "Ongoing", "When Summoned, place on an enemy's Faction Spellbook. While there, that Spellbook cannot be used (it still counts for other purposes such as Unlimited Battle, and winning the game). Next Doom Phase, remove all Moonbeasts from enemy Spellbooks and place them into any Map Areas with a Controlled Gate(s). A victim can prematurely return a Moonbeast to the Map by spending 1 Doom point at any time (not counting as an Action).<br/><br/>Blocking the following Spellbooks will have the following exceptions to the block effects:<br/>Submerge: Still allows Unsubmerge.<br/>Cursed Slumber: Allows return to map.<br/>Red Sign: Does not force Dark Young to abandon gates they control.<br/>Ancient Sorcery: Does not immediately return Serpent Men to map. If Sleeper is already hibernated, does not activate Sleeper or deduct hibernation power bonus.<br/>Ice Age: Marker remains, power penalty is blocked.")
        case $("Giant Blind Albino Penguins") => loyaltyCard("Giant Blind Albino Penguins", 2, 1, -2, "Pay 2 Doom. Place at Controlled Gate.", "Laughingstock", "Pre-Battle", "Move one or more Penguins to any Battle area, even if you're not involved. If you are in the Battle, the Penguin fights for you. If not, you choose which side it joins, and it belongs to that player until battle's end.")
        case $("Elder Thing") => loyaltyCard("Elder Thing", 3, 2, 2, "Pay 2 Doom. Place at Controlled Gate.", "Mind Control", "Ongoing", "If an Elder Thing shares an Area with an enemy Great Old One, the latter may not use its Special Ability.")
        case $("Leng Spider") => loyaltyCard("Leng Spider", 3, 2, 1, "Pay 2 Doom. Place at Controlled Gate.", "Bloodthirst", "Ongoing", "If a Leng Spider is involved in a Battle, you may exchange two Pain results for a Kill before results are assigned. You may do this once per Leng Spider in the Battle. Each use may be applied to your results OR your opponent's.")
        case $("Satyr") => loyaltyCard("Satyr", 3, 2, 1, "Pay 2 Doom. Place 1 Satyr + 1 Acolyte.", "Fecund", "Ongoing", "When you summon a Satyr, also place an Acolyte Cultist from your Pool into the same Area.")
        case $("Servitor of the Outer Gods") => loyaltyCard("Servitor of the Outer Gods", 3, 2, -1, "Pay 2 Doom. Choose an enemy faction to receive this card and its units. Summon cost: 1 Power each.", "Adulation", "Ongoing", "You may not Summon any Monsters except Servitors if any Servitors remain in your Pool. (You may still place other Monsters via abilities or means other than the Summon Action.)")
        case $("Insects from Shaggai") => loyaltyCard("Insects from Shaggai", 3, 2, 0, "Pay 2 Doom. Place in any Area.", "Mind Parasite", "Ongoing", "All Acolyte Cultists not on a Gate who share an Area with an Insect from Shaggai are Controlled by you during the Action Phase for limited purposes: (1) Only you can Move them; (2) They fight on your side in Battle. They don't benefit from any Faction's Spellbooks. They can only be Captured by you if their true Faction permits it and cannot be Captured by their true Faction (though they could be targeted by a Spellbook or Killed in battle). These Cultists are not Controlled by you during Gather Power or Doom Phases \u2013 they provide Power and Doom to their true Faction.")

        // ── iGOO overlays — no-param (setup menu ? click) ──
        case $("Byatis") => loyaltyCardIGOO(ByatisCard.name, "" + ByatisCard.cost, "" + ByatisCard.combat, false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Byatis in the Area containing the Gate.", "Toad of Berkeley", "Ongoing", "Byatis cannot Move nor be moved by movement-type abilities (like Arctic Winds or Submerge). He can still be Pained. During the Doom Phase, if no enemy Units are in his Area, receive 1 Elder Sign.", "Byatis survives a Battle in which at least one enemy Unit is Killed", "God of Forgetfulness", "Action: Cost 1", "Select all enemy Cultists in an Area adjacent to Byatis; those Cultists are moved into Byatis's Area.")
        case $("Abhoth") => loyaltyCardIGOO(AbhothCard.name, "" + AbhothCard.cost, "Equals Filth Tokens in play", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Abhoth in the Area containing the Gate.", "Filth", "Action: Cost 1", "Place a Filth Token in any Area. Filth Tokens act as Combat 0 Monsters belonging to Abhoth's Faction but never Move nor take Actions. They can be Killed or Pained normally and are affected by Spellbooks/abilities.", "EITHER your Faction has 4+ different Monster types in play (including Filth) OR 8+ total Monsters in play (including Filth)", "The Brood", "Ongoing", "Gates in Areas containing Filth Tokens don't count during the Doom Phase. Does not apply to Abhoth's Faction.")
        case $("Daoloth") => loyaltyCardIGOO("Daoloth", "4", "0", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Daoloth in the Area containing the Gate.", "Cosmic Unity", "Pre-Battle", "In a Battle involving Daoloth, choose one enemy Great Old One – it rolls no Combat dice (it still gets its Battle Ability, if any).", "A Great Old One is Killed anywhere on the Map", "Interdimensional", "Ongoing", "When Daoloth enters an Area without a Gate, immediately place a Gate there.")
        case $("Nyogtha") => loyaltyCardIGOO("Nyogtha", "4", "4 attacking / 1 defending", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Nyogtha in the Area containing the Gate.", "From Below", "Ongoing", "Nyogtha is two Units. Any Common Action involving one can be applied simultaneously to the other at no extra cost. When one Moves, so may the other. When one Captures a Cultist, so may the other. If Battle is declared in one's Area, you can also declare Battle in the other's Area for free. You only lose the Loyalty Card if both Nyogtha Units are Killed.", "Nyogtha survives a Battle against an enemy Great Old One", "Nightmare Web", "Ongoing", "If one Nyogtha Unit is in your Pool, you can Awaken it for 2 Power, placing it in any Area where you have a Unit.")
        case $("Tulzscha") => loyaltyCardIGOO("Tulzscha", "4", "1", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At end of Gather Power Phase, gain 1 Doom if at least one Faction has more Doom than you, gain 1 Elder Sign if at least one Faction has more Elder Signs than you, gain 1 Power if at least one Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power", "Ceremony of Annihilation", "Doom Phase", "When performing a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual of Annihilation marker position, then advance the marker 1 step. You earn no extra Doom points nor Elder Signs.")
        case $("Y'Golonac") => loyaltyCardIGOO("Y'Golonac", "2", "1", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 2 Power, and place Y'Golonac in the Area containing the Gate.", "Orifices", "Post-Battle", "If Y'Golonac is Killed in Battle, select a surviving enemy Terror, Monster, or Cultist – replace it with Y'Golonac and give the Loyalty Card to that player. If no enemies survived, Y'Golonac dies normally and the card returns to the general Pool.", "You have just received Y'Golonac as a result of his Orifices ability", "The Revelations", "Doom Phase", "Every player except you gets 1 Elder Sign. This is not optional.")
        case $("Yig") => loyaltyCardIGOO("Yig", "4", "2", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Yig in the Area containing the Gate.", "Snakebite", "Post-Battle", "If any of your Cultists are killed in a Battle, the enemy receives 1 extra Kill result.", "Remove a Controlled Gate from the map.", "Messenger of Yig", "Doom Phase", "Each Doom Phase, every other faction decides whether to donate 1 Power to you. For each faction that doesn't, you gain 1 Doom.")
        case $("Mother Hydra") => loyaltyCardIGOO("Mother Hydra", "4", "6 minus enemy Units (min 1)", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Mother Hydra in the Area containing the Gate.", "The Agony Sting", "Action: Cost 1", "Choose any Ocean Area. All enemy Cultists must move to adjacent Land Areas.", "No GOOs in Ocean, or an enemy has no GOOs in Ocean.", "The Zygote", "Action: Cost 1", "Place each Acolyte from Pool onto the board, one at a time.")
        case $("Father Dagon") => loyaltyCardIGOO("Father Dagon", "4", "2 land / 6 ocean", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Father Dagon in the Area containing the Gate.", "Tsunami", "Action: Cost 1", "Choose a Land Area adjacent to Ocean. All Cultists must move to adjacent Ocean Areas.", "Have 8 Units in Ocean Areas.", "The Innsmouth Look", "Doom Phase", "Remove one Acolyte permanently. Gain 6 Power. Not optional.")
        case $("Cthugha") => loyaltyCardIGOO("Cthugha", "6 minus GOO cost", "Matches enemy GOO combat", false, "Replace your GOO with Cthugha. If cost is negative, gain Power.", "Fire Vampires", "Post-Battle", "Spare killed enemies for 1 Power each.", "Kill an enemy GOO in Battle.", "Firestorm", "Post-Battle", "When you spare a killed enemy, also gain 1 Elder Sign.")
        case $("Ghatanothoa") => loyaltyCardIGOO("Ghatanothoa (IGOO)", "4", "Enemy Cultists on map", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Ghatanothoa in the Area containing the Gate.", "Mummify", "Action: Cost 1", "Enemy Cultists sharing Area with Ghatanothoa are Mummified.", "Fewer than 6 total Gates and Cultists on the map at the Doom Phase, or pay 4 Power.", "Execration of Mu", "Ongoing", "Mummify is now instant/ongoing.")
        case $("The Bloated Woman") => loyaltyCardIGOO("The Bloated Woman", "4", "1", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place the Bloated Woman in the Area containing the Gate.", "The Velvet Fan", "Post-Battle", "Choose one Killed or Eliminated enemy Monster or Cultist and place it on this card.", "Have all 6 Faction Spellbooks.", "Disaster Looms", "Gather Power Phase", "Controlled Gates earn 1 Elder Sign instead of 2 Power.")
        case $("Azathoth") => loyaltyCardIGOO("Azathoth", "1d6+2 (8+ Power)", "Glyph position on Doom track", false, "Roll 1d6+2, lose that much Power. Enemies choose die faces. Place glyph on sum.", "Daemon Sultan", "Ongoing", "If Azathoth is assigned a Kill, roll 1 die. Lower marker by that amount. If 0, Azathoth is Killed.", "Every Faction must have a GOO in play.", "Nuclear Chaos", "Action: Cost 0", "All players roll 1d6. Highest gets Power, lowest gets ES. Owner may adjust +/-1.")
        case $("Atlach-Nacha") => loyaltyCardIGOO("Atlach-Nacha", "6", "4", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Atlach-Nacha in the Area containing the Gate.", "Spinnerets", "Action: Cost 1", "If Atlach-Nacha is in an Area that is lacking a Web Token, place one there.", "Place 6 Web Tokens in 6 different Areas.", "Cosmic Web", "Immediate", "Your Faction wins the game immediately.")
        case $("Bokrug") => loyaltyCardIGOO("Bokrug", "6", "0", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Bokrug in the Area containing the Gate.", "Ghosts of Ib", "Ongoing", "If Bokrug is killed, you do not lose this Loyalty Card. Instead, place Bokrug's figure on this card. At the end of the next Doom Phase, return Bokrug to any Area of the Map that does not contain any enemy Units. If there are no such areas, Bokrug remains on this card until the next Doom Phase.", "Give Bokrug to another player.", "Doom that Came to Sarnath", "Doom Phase", "An enemy chooses a unit to eliminate or an Elder Sign to discard.")
        case $("Gla'aki") => loyaltyCardIGOO("Gla'aki (IGOO)", "6", "0", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Gla'aki in the Area containing the Gate.", "Tomb Herd", "Gather Power", "Gain Power equal to cultists in your pool.", "Reach 0 power during action phase before any other faction.", "Green Decay", "Gather Power", "Captured enemy cultists give ES instead of Power.")
        // ── NEW IGOOs (menu overlays — 2-param: name + spellbook boolean) ──
        case $("Yig", _ : Boolean) => loyaltyCardIGOO("Yig", "4", "2", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Yig in the Area containing the Gate.", "Snakebite", "Post-Battle", "Your Cultists are now poisonous. If any of your Cultists are killed in a Battle, the enemy receives 1 extra Kill result (he only takes 1 extra Kill regardless of how many Cultists die).", "Remove a Controlled Gate from the map (Cultist does not die).", "Messenger of Yig", "Doom Phase", "Each Doom Phase, every other faction decides whether to donate 1 Power to you. For each faction that doesn't, you gain 1 Doom.")
        case $("Mother Hydra", _ : Boolean) => loyaltyCardIGOO("Mother Hydra", "4", "6 minus the number of enemy Units in the Area (minimum 1)", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Mother Hydra in the Area containing the Gate.", "The Agony Sting", "Action: Cost 1", "Choose any Ocean Area. All enemy Cultists in that Area must be moved into adjacent Land Areas by their owners (your Cultists are immune). In a dispute over who moves first, you decide.", "Have no Great Old Ones in the Ocean, or an enemy has no Great Old Ones in the Ocean.", "The Zygote", "Action: Cost 1", "Place each Acolyte Cultist from your Pool onto the board, one at a time, each to any Area where you have a Unit or Gate.")
        case $("Father Dagon", _ : Boolean) => loyaltyCardIGOO("Father Dagon", "4", "2 on Land, 6 in an Ocean Area", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Father Dagon in the Area containing the Gate.", "Tsunami", "Action: Cost 1", "Choose a Land Area adjacent to an Ocean Area. All Cultists in the Area must be moved into adjacent Ocean Areas by their owners (this includes your Cultists). In a dispute over who moves first, you decide.", "Have 8 Units in Ocean Areas.", "The Innsmouth Look", "Doom Phase", "During the Doom Phase, remove one of your Acolyte Cultists from the map and out of the game permanently. Gain 6 Power. This is not optional.")
        case $("Cthugha", _ : Boolean) => loyaltyCardIGOO("Cthugha", "6 minus your GOO's Awakening Cost (if negative, gain Power)", "Equals the Combat of an enemy Great Old One in the Battle (your choice). If none are present, Cthugha's Combat is 0.", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay Power equal to 6 minus your Great Old One's Awakening Cost. Replace your Great Old One with Cthugha.", "Fire Vampires", "Post-Battle", "If Cthugha is involved in a Battle, after all Kill results are assigned, you may choose to &quot;spare&quot; one or more Killed enemy Units, by reducing their loss to a Pain instead. For each Killed enemy Unit you spare in this way, you gain 1 Power.", "Kill an enemy Great Old One in Battle.", "Firestorm", "Post-Battle", "If Cthugha is involved in a Battle, when you spare a Killed enemy, you also receive 1 Elder Sign.")
        case $("Ghatanothoa", _ : Boolean) => loyaltyCardIGOO("Ghatanothoa (IGOO)", "4", "Equal to the number of Cultists your opponent has on the Map", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Ghatanothoa in the Area containing the Gate.", "Mummify", "Action: Cost 1", "Any enemy Acolyte Cultists sharing an Area with Ghatanothoa are immediately &quot;Mummified.&quot; Lay Mummified Cultist figures on their sides. Such Cultists cannot use the Move Action, do not participate in Battle, and produce no Power during the next Gather Power Phase. During the Doom Phase, stand any Mummified Cultists back up. (A Mummified Cultist can be Captured. If a Cultist Controlled a Gate before becoming Mummified, it retains Control of that Gate.)", "Fewer than 6 total Gates and Cultists on the map at the Doom Phase, or pay 4 Power.", "Execration of Mu", "Ongoing", "The Mummify ability is no longer an Action. It now occurs instantly when any enemy Cultist shares an Area with Ghatanothoa.")
        case $("The Bloated Woman", _ : Boolean) => loyaltyCardIGOO("The Bloated Woman", "4", "1", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place the Bloated Woman in the Area containing the Gate.<br>Crawling Chaos (CC): Pay 3 Power at a Controlled Gate. Each other Faction receives 1 Elder Sign.", "The Velvet Fan", "Post-Battle", "After any Battle involving the Bloated Woman, choose one Killed or Eliminated enemy Monster or Cultist and place it on this card; that Unit is considered to be out of play. If a Unit is later Recruited or Summoned from this card, its owner pays its Power cost directly to you. Units remain on this card if the Bloated Woman is Killed or Eliminated — they may still be Recruited or Summoned from this card, but no one will be paid Power for this while she is out of play. If she is later returned to play, her controller will receive payments as described above. There is no limit to the number of Units you may have on this card.", "Have all 6 Faction Spellbooks.", "Disaster Looms", "Gather Power Phase", "Your Controlled Gates now earn 1 Elder Sign instead of 2 Power. They still earn 1 Doom during the Doom Phase.")
        case $("Bokrug", _ : Boolean) => loyaltyCardIGOO("Bokrug", "6", "0", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Bokrug in the Area containing the Gate.", "Ghosts of Ib", "Ongoing", "If Bokrug is killed, you do not lose this Loyalty Card. Instead, place Bokrug's figure on this card. At the end of the next Doom Phase, return Bokrug to any Area of the Map that does not contain any enemy Units. If there are no such areas, Bokrug remains on this card until the next Doom Phase.", "Give Bokrug to another player.", "Doom that Came to Sarnath", "Doom Phase", "Choose one: an enemy chooses a monster or cultist of yours to eliminate, OR an enemy chooses one of your elder signs to discard.")
        case $("Bokrug", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Bokrug", "6", "0", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Bokrug in the Area containing the Gate.", "Ghosts of Ib", "Doom Phase", "During the Doom Phase, if Bokrug is NOT on the map, place Bokrug in any Area without enemy Units. No gate or own-unit required.", "Give Bokrug to another player.", "Doom that Came to Sarnath", "Doom Phase", "Choose one: an enemy chooses a monster or cultist of yours to eliminate, OR an enemy chooses one of your elder signs to discard.", facedown)
        case $("Gla'aki", _ : Boolean) => loyaltyCardIGOO("Gla'aki (IGOO)", "6", "0", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Gla'aki in the Area containing the Gate.", "Tomb Herd", "Gather Power Phase", "During Gather Power, BEFORE captured cultists are returned, count Cultists in your Pool and gain that much Power.", "Reach 0 Power during the Action Phase before any other faction.", "Green Decay", "Gather Power Phase", "Captured enemy Cultists give Elder Signs instead of Power during Gather Power.")
        case $("Gla'aki", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Gla'aki (IGOO)", "6", "0", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Gla'aki in the Area containing the Gate.", "Tomb Herd", "Gather Power Phase", "During Gather Power, BEFORE captured cultists are returned, count Cultists in your Pool and gain that much Power.", "Reach 0 Power during the Action Phase before any other faction.", "Green Decay", "Gather Power Phase", "Captured enemy Cultists give Elder Signs instead of Power during Gather Power.", facedown)
        case $("Atlach-Nacha", _ : Boolean) => loyaltyCardIGOO("Atlach-Nacha", "6", "4", false, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Atlach-Nacha in the Area containing the Gate.", "Spinnerets", "Action: Cost 1", "If Atlach-Nacha is in an Area that is lacking a Web Token, place one there.", "Place 6 Web Tokens in 6 different Areas.", "Cosmic Web", "Immediate", "Your Faction wins the game immediately.")
        case $("Atlach-Nacha", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Atlach-Nacha", "6", "4", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Atlach-Nacha in the Area containing the Gate.", "Spinnerets", "Action: Cost 1", "If Atlach-Nacha is in an Area that is lacking a Web Token, place one there.", "Place 6 Web Tokens in 6 different Areas.", "Cosmic Web", "Immediate", "Your Faction wins the game immediately.", facedown)
        case $("Azathoth", _ : Boolean) => loyaltyCardIGOO("Azathoth", "1d6+2 (8+ Power required)", "Equal to the position of the Azathoth Glyph on the Doom track", false, "1. You must have 8+ Power and your Great Old One at your Controlled Gate.<br>2. Roll 1d6+2, lose that much Power.<br>3. All enemies choose and reveal a die face (1-6). Each receives Power equal to their choice. Player(s) with lowest score lose 2 Doom.<br>4. Total all dice. Place Azathoth glyph on that spot on the Doom track. Combat equals glyph position.", "Daemon Sultan", "Ongoing", "If Azathoth is assigned a Kill in Battle, roll 1 die. Lower the Azathoth marker by the amount shown on the die. If the marker reaches 0, Azathoth is Killed.", "Every Faction must have a Great Old One in play.", "Nuclear Chaos", "Action: Cost 0", "Every player rolls 1d6. Player(s) with the highest roll gain Power equal to their die roll. Player(s) with the lowest roll gain that many Elder Signs. Azathoth's owner may add or subtract 1 from their own die roll. Flip this spellbook face down until next turn.")
        // Faction card variants with spellbook state
        case $("Yig", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Yig", "4", "2", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Yig in the Area containing the Gate.", "Snakebite", "Post-Battle", "Your Cultists are now poisonous. If any of your Cultists are killed in a Battle, the enemy receives 1 extra Kill result (he only takes 1 extra Kill regardless of how many Cultists die).", "Remove a Controlled Gate from the map (Cultist does not die).", "Messenger of Yig", "Doom Phase", "Each Doom Phase, every other faction decides whether to donate 1 Power to you. For each faction that doesn't, you gain 1 Doom.", facedown)
        case $("Mother Hydra", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Mother Hydra", "4", "6 minus the number of enemy Units in the Area (minimum 1)", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Mother Hydra in the Area containing the Gate.", "The Agony Sting", "Action: Cost 1", "Choose any Ocean Area. All enemy Cultists in that Area must be moved into adjacent Land Areas by their owners (your Cultists are immune). In a dispute over who moves first, you decide.", "Have no Great Old Ones in the Ocean, or an enemy has no Great Old Ones in the Ocean.", "The Zygote", "Action: Cost 1", "Place each Acolyte Cultist from your Pool onto the board, one at a time, each to any Area where you have a Unit or Gate.", facedown)
        case $("Father Dagon", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Father Dagon", "4", "2 on Land, 6 in an Ocean Area", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Father Dagon in the Area containing the Gate.", "Tsunami", "Action: Cost 1", "Choose a Land Area adjacent to an Ocean Area. All Cultists in the Area must be moved into adjacent Ocean Areas by their owners (this includes your Cultists). In a dispute over who moves first, you decide.", "Have 8 Units in Ocean Areas.", "The Innsmouth Look", "Doom Phase", "During the Doom Phase, remove one of your Acolyte Cultists from the map and out of the game permanently. Gain 6 Power. This is not optional.", facedown)
        case $("Cthugha", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Cthugha", "6 minus your GOO's Awakening Cost (if negative, gain Power)", "Equals the Combat of an enemy Great Old One in the Battle (your choice). If none are present, Cthugha's Combat is 0.", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay Power equal to 6 minus your Great Old One's Awakening Cost. Replace your Great Old One with Cthugha.", "Fire Vampires", "Post-Battle", "If Cthugha is involved in a Battle, after all Kill results are assigned, you may choose to &quot;spare&quot; one or more Killed enemy Units, by reducing their loss to a Pain instead. For each Killed enemy Unit you spare in this way, you gain 1 Power.", "Kill an enemy Great Old One in Battle.", "Firestorm", "Post-Battle", "If Cthugha is involved in a Battle, when you spare a Killed enemy, you also receive 1 Elder Sign.", facedown)
        case $("Ghatanothoa", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Ghatanothoa (IGOO)", "4", "Equal to the number of Cultists your opponent has on the Map", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Ghatanothoa in the Area containing the Gate.", "Mummify", "Action: Cost 1", "Any enemy Acolyte Cultists sharing an Area with Ghatanothoa are immediately &quot;Mummified.&quot; Lay Mummified Cultist figures on their sides. Such Cultists cannot use the Move Action, do not participate in Battle, and produce no Power during the next Gather Power Phase. During the Doom Phase, stand any Mummified Cultists back up. (A Mummified Cultist can be Captured. If a Cultist Controlled a Gate before becoming Mummified, it retains Control of that Gate.)", "Fewer than 6 total Gates and Cultists on the map at the Doom Phase, or pay 4 Power.", "Execration of Mu", "Ongoing", "The Mummify ability is no longer an Action. It now occurs instantly when any enemy Cultist shares an Area with Ghatanothoa.", facedown)
        case $("The Bloated Woman", spellbook : Boolean, facedown : Boolean, capturedCount : Int, capturedList : String) => loyaltyCardIGOO("The Bloated Woman", "4", "1", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place the Bloated Woman in the Area containing the Gate.<br>Crawling Chaos (CC): Pay 3 Power at a Controlled Gate. Each other Faction receives 1 Elder Sign.", "The Velvet Fan", "Post-Battle", "After any Battle involving the Bloated Woman, choose one Killed or Eliminated enemy Monster or Cultist and place it on this card; that Unit is considered to be out of play. If a Unit is later Recruited or Summoned from this card, its owner pays its Power cost directly to you. Units remain on this card if the Bloated Woman is Killed or Eliminated — they may still be Recruited or Summoned from this card, but no one will be paid Power for this while she is out of play. If she is later returned to play, her controller will receive payments as described above. There is no limit to the number of Units you may have on this card." + (if (capturedCount > 0) "<br><br><span class=cost-color>Units on card (" + capturedCount + "):</span> " + capturedList else ""), "Have all 6 Faction Spellbooks.", "Disaster Looms", "Gather Power Phase", "Your Controlled Gates now earn 1 Elder Sign instead of 2 Power. They still earn 1 Doom during the Doom Phase.", facedown)
        case $("The Bloated Woman", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("The Bloated Woman", "4", "1", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place the Bloated Woman in the Area containing the Gate.<br>Crawling Chaos (CC): Pay 3 Power at a Controlled Gate. Each other Faction receives 1 Elder Sign.", "The Velvet Fan", "Post-Battle", "After any Battle involving the Bloated Woman, choose one Killed or Eliminated enemy Monster or Cultist and place it on this card; that Unit is considered to be out of play. If a Unit is later Recruited or Summoned from this card, its owner pays its Power cost directly to you. Units remain on this card if the Bloated Woman is Killed or Eliminated — they may still be Recruited or Summoned from this card, but no one will be paid Power for this while she is out of play. If she is later returned to play, her controller will receive payments as described above. There is no limit to the number of Units you may have on this card.", "Have all 6 Faction Spellbooks.", "Disaster Looms", "Gather Power Phase", "Your Controlled Gates now earn 1 Elder Sign instead of 2 Power. They still earn 1 Doom during the Doom Phase.", facedown)
        case $("Azathoth", spellbook : Boolean, facedown : Boolean, glyphPos : Int) => loyaltyCardIGOO("Azathoth", "1d6+2 (8+ Power required)", "Equal to the position of the Azathoth Glyph on the Doom track" + (if (glyphPos > 0) "<br><span class=cost-color>Current Combat: " + glyphPos + "</span>" else ""), spellbook, "1. You must have 8+ Power and your Great Old One at your Controlled Gate.<br>2. Roll 1d6+2, lose that much Power.<br>3. All enemies choose and reveal a die face (1-6). Each receives Power equal to their choice. Player(s) with lowest score lose 2 Doom.<br>4. Total all dice. Place Azathoth glyph on that spot on the Doom track. Combat equals glyph position.", "Daemon Sultan", "Ongoing", "If Azathoth is assigned a Kill in Battle, roll 1 die. Lower the Azathoth marker by the amount shown on the die. If the marker reaches 0, Azathoth is Killed.", "Every Faction must have a Great Old One in play.", "Nuclear Chaos", "Action: Cost 0", "Every player rolls 1d6. Player(s) with the highest roll gain Power equal to their die roll. Player(s) with the lowest roll gain that many Elder Signs. Azathoth's owner may add or subtract 1 from their own die roll. Flip this spellbook face down until next turn.", facedown)
        case $("Azathoth", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO("Azathoth", "1d6+2 (8+ Power required)", "Equal to the position of the Azathoth Glyph on the Doom track", spellbook, "1. You must have 8+ Power and your Great Old One at your Controlled Gate.<br>2. Roll 1d6+2, lose that much Power.<br>3. All enemies choose and reveal a die face (1-6). Each receives Power equal to their choice. Player(s) with lowest score lose 2 Doom.<br>4. Total all dice. Place Azathoth glyph on that spot on the Doom track. Combat equals glyph position.", "Daemon Sultan", "Ongoing", "If Azathoth is assigned a Kill in Battle, roll 1 die. Lower the Azathoth marker by the amount shown on the die. If the marker reaches 0, Azathoth is Killed.", "Every Faction must have a Great Old One in play.", "Nuclear Chaos", "Action: Cost 0", "Every player rolls 1d6. Player(s) with the highest roll gain Power equal to their die roll. Player(s) with the lowest roll gain that many Elder Signs. Azathoth's owner may add or subtract 1 from their own die roll. Flip this spellbook face down until next turn.", facedown)

    case $("Dimensional Shambler") => loyaltyCard(DimensionalShamblerCard.name, DimensionalShamblerCard.quantity, DimensionalShamblerCard.cost, DimensionalShamblerCard.combat, "Pay 2 Doom to obtain this Loyalty Card, then place 1 Dimensional Shambler onto your Faction Card.", "Walk Between Worlds", "Ongoing", "When Summoning a Dimensional Shambler, place it onto your Faction Card. After any Action (by any player), you may place one or more Dimensional Shamblers from your Faction Card into any Area. Once placed, Dimensional Shamblers remain on the Map (until Killed or otherwise Eliminated).")
    case $("High Priest") => loyaltyCard(HighPriestCard.name, HighPriestCard.quantity, HighPriestCard.cost, HighPriestCard.combat, "The High Priest is a new type of Cultist, it is Recruited like an Acolyte. Each High Priest generates 1 Power during the Gather Power Phase, can Create and Control a Gate, and can be Captured.", "Unspeakable Oath", "Ongoing", "At the end of any player's Action (even if it is not your turn), Sacrifice your High Priest (return him to your Pool) and gain 2 Power. This may also be done during the Gather Power and Doom Phases.")

        // Round 8 Bug 40: third parameter (facedown : Boolean) added for Infernal Pact strikethrough
        case $("Byatis", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(ByatisCard.name, "" + ByatisCard.cost, "" + ByatisCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Byatis in the Area containing the Gate.", "Toad of Berkeley", "Ongoing", "Byatis may not Move, nor can he be moved with movement-type abilities (such as Arctic Winds or Submerge). He can still be Pained. If there are no enemy Units in Byatis' Area during the Doom Phase, earn 1 Elder Sign.", "Byatis survives a Battle in which at least one enemy Unit is Killed", "God of Forgetfulness", "Action: Cost 1", "Select all enemy Cultists in an Area adjacent to Byatis. Those Cultists are moved into Byatis' Area.", facedown)
        case $("Abhoth", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(AbhothCard.name, "" + AbhothCard.cost, "Equals the number of Filth Tokens in play (0-12)", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Abhoth in the Area containing the Gate.", "Filth", "Action: Cost 1", "Place a Filth Token in any Area.", "Choose One: EITHER your Faction has 4+ different Monster types in play, including Filth Tokens, OR Your faction has 8+ total Monsters in play, including Filth Tokens", "The Brood", "Ongoing", "Gates in Areas containing Filth Tokens do not count during the Doom phase. Does not apply to Abhoth's Faction.", facedown)
        case $("Daoloth", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(DaolothCard.name, "" + DaolothCard.cost, "" + DaolothCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Daoloth in the Area containing the Gate.", "Cosmic Unity", "Pre-Battle", "In a Battle involving Daoloth, choose one enemy Great Old One. It rolls no Combat dice (it still gets its Battle Ability, if any).", "A Great Old One is Killed (anywhere on the map)", "Interdimensional", "Ongoing", "When Daoloth enters an Area without a Gate, immediately place a Gate there.", facedown)
        case $("Nyogtha", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(NyogthaCard.name, "" + NyogthaCard.cost, "4 if Nyogtha's Faction declared the Battle, 1 if not.", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place all Nyogtha Units from your Pool to the Area containing the Gate.", "From Below", "Ongoing", "Nyogtha is two Units. Any Common Action involving one of these Units can be applied simultaneously to the other as part of the same Action and at no extra cost. When one Nyogtha Unit Moves, so may the other. When one Captures a Cultist, so may the other. If Battle is declared in one's Area, you can also declare a Battle in the other's Area, for free. You only lose this Loyalty Card if both Nyogtha Units have been Killed.", "Nyogtha survives a Battle against an enemy Great Old One", "Nightmare Web", "Ongoing", "If one of the Nyogtha Units is in your pool, you can Awaken it for 2 Power, placing it in any Area in which you have at least one Unit.", facedown)
        case $("Tulzscha", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power.", "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs.", facedown)
        case $("Y'Golonac", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(YgolonacCard.name, "" + YgolonacCard.power, "" + YgolonacCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 2 Power, place Y'Golonac in the Area containing the Gate.", "Orifices", "Post-Battle", "If Y'Golonac is Killed in a Battle, select a surviving enemy Terror, Monster, or Cultist. Replace it with Y'Golonac, then give Y'Golonac's Loyalty Card to that player. If no enemies survived, Y'Golonac dies normally (placing this Loyalty Card in the general Pool).", "You have just received Y'Golonac as a result of his Orifices ability.", "The Revelations", "Doom Phase", "Every player except you gets 1 Elder Sign. This is not optional.", facedown)
        // Backwards compatibility: old overlays without facedown parameter
        case $("Byatis", spellbook : Boolean) => loyaltyCardIGOO(ByatisCard.name, "" + ByatisCard.cost, "" + ByatisCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Byatis in the Area containing the Gate.", "Toad of Berkeley", "Ongoing", "Byatis may not Move, nor can he be moved with movement-type abilities (such as Arctic Winds or Submerge). He can still be Pained. If there are no enemy Units in Byatis' Area during the Doom Phase, earn 1 Elder Sign.", "Byatis survives a Battle in which at least one enemy Unit is Killed", "God of Forgetfulness", "Action: Cost 1", "Select all enemy Cultists in an Area adjacent to Byatis. Those Cultists are moved into Byatis' Area.")
        case $("Abhoth", spellbook : Boolean) => loyaltyCardIGOO(AbhothCard.name, "" + AbhothCard.cost, "Equals the number of Filth Tokens in play (0-12)", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Abhoth in the Area containing the Gate.", "Filth", "Action: Cost 1", "Place a Filth Token in any Area.", "Choose One: EITHER your Faction has 4+ different Monster types in play, including Filth Tokens, OR Your faction has 8+ total Monsters in play, including Filth Tokens", "The Brood", "Ongoing", "Gates in Areas containing Filth Tokens do not count during the Doom phase. Does not apply to Abhoth's Faction.")
        case $("Daoloth", spellbook : Boolean) => loyaltyCardIGOO(DaolothCard.name, "" + DaolothCard.cost, "" + DaolothCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Daoloth in the Area containing the Gate.", "Cosmic Unity", "Pre-Battle", "In a Battle involving Daoloth, choose one enemy Great Old One. It rolls no Combat dice (it still gets its Battle Ability, if any).", "A Great Old One is Killed (anywhere on the map)", "Interdimensional", "Ongoing", "When Daoloth enters an Area without a Gate, immediately place a Gate there.")
        case $("Nyogtha", spellbook : Boolean) => loyaltyCardIGOO(NyogthaCard.name, "" + NyogthaCard.cost, "4 if Nyogtha's Faction declared the Battle, 1 if not.", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place all Nyogtha Units from your Pool to the Area containing the Gate.", "From Below", "Ongoing", "Nyogtha is two Units. Any Common Action involving one of these Units can be applied simultaneously to the other as part of the same Action and at no extra cost. When one Nyogtha Unit Moves, so may the other. When one Captures a Cultist, so may the other. If Battle is declared in one's Area, you can also declare a Battle in the other's Area, for free. You only lose this Loyalty Card if both Nyogtha Units have been Killed.", "Nyogtha survives a Battle against an enemy Great Old One", "Nightmare Web", "Ongoing", "If one of the Nyogtha Units is in your pool, you can Awaken it for 2 Power, placing it in any Area in which you have at least one Unit.")
        case $("Tulzscha", spellbook : Boolean) => loyaltyCardIGOO(TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power.", "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs.")
        case $("Y'Golonac", spellbook : Boolean) => loyaltyCardIGOO(YgolonacCard.name, "" + YgolonacCard.power, "" + YgolonacCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 2 Power, place Y'Golonac in the Area containing the Gate.", "Orifices", "Post-Battle", "If Y'Golonac is Killed in a Battle, select a surviving enemy Terror, Monster, or Cultist. Replace it with Y'Golonac, then give Y'Golonac's Loyalty Card to that player. If no enemies survived, Y'Golonac dies normally (placing this Loyalty Card in the general Pool).", "You have just received Y'Golonac as a result of his Orifices ability.", "The Revelations", "Doom Phase", "Every player except you gets 1 Elder Sign. This is not optional.")


        // ── Library at Celaeno: Silence Token overlay ──
        case $("SilenceToken", _ : Boolean) => tomeOverlay("Silence Token", "",
            "Spend to activate the Custodian or the Librarian.")

        // ── Library at Celaeno: Tome Info overlay ──
        case $("LibraryTomeInfo", _ : Boolean) => tomeOverlay("Library Tomes", "",
            "Library Tomes act as neutral Spellbooks. When you Control a Gate in an Area with a Tome, take possession of that Area's Library Tome. It may be used as a standard Spellbook.<br><br>" +
            "If you lose Control of the Tome's Gate, the Tome is considered Overdue but remains in your possession. Library Tomes may only be returned to the Map by Satisfying the Librarian's Agony.<br><br>" +
            "They cannot be otherwise voluntarily relinquished, and the player cannot be forced to return them if they choose to lose Doom or Units instead.<br><br>" +
            "When a Tome is relinquished and returned to its slot, it is immediately acquired by the Faction currently Controlling its respective Gate, if any.")

        // ── Library at Celaeno: Hint Card overlay ──
        case $("LibraryHintCard", _ : Boolean) => libraryHintCard()

        // ── Library at Celaeno: Custodian and Librarian overlays ──
        case $("Custodian", _ : Boolean) => mapUnitOverlay("Custodian",
            "<div class='p nt black-border'>Spend a silence token. Place the Custodian in any Area. Roll the Agony Die. " +
            "If the Custodian stays in the same Area, add +1 to the Agony die result, for this roll only. " +
            "The Activating player assigns the Agony result between the Factions present in the Custodian's Area. " +
            "Those Factions can satisfy one Agony for each of their Units in the Area that they move to the Oubliette, " +
            "and must satisfy as much of the Custodian's Agony as possible.</div>" +
            "<div class='p nt black-border'>No player may perform the Control Gate Action in the Custodian's Area. " +
            "However, if a Gate is already Controlled when the Custodian is Moved into its Area, that Gate may remain Controlled.</div>",
            "Agony Die faces: 1, 2, 2, 3, 3, 4")

        case $("Librarian", _ : Boolean) => mapUnitOverlay("Librarian",
            "<div class='p nt black-border'>The Librarian may only be Activated if at least one enemy Faction has an Overdue Library Tome. " +
            "Spend a silence token. Place the Librarian in an Area with units from one of those Factions (with an Overdue Library Tome). " +
            "Roll the Agony Die. If the Librarian stays in the same Area, add +1 to the Agony die result, for this roll only. " +
            "The Activating player assigns the Agony result between the Factions present in the Librarian's Area. " +
            "Those Factions can satisfy one Agony for each of their Units in the Area of their choice that they Eliminate, " +
            "for each point of Doom they choose to lose, or for each Overdue Tome they return to its slot, " +
            "and must satisfy as much of the Librarian's Agony as possible.</div>" +
            "<div class='p nt black-border'>No player may perform the Control Gate Action in the Librarian's Area. " +
            "However, if a Gate is already Controlled when the Librarian is Moved into its Area, that Gate may remain Controlled.</div>",
            "Agony Die faces: 1, 2, 2, 3, 3, 4")

        // Library Tomes
        case $("Barrier of Naach-Tith", faceUp : Boolean) => tomeOverlay("Barrier of Naach-Tith", "Always Active",
            "When an enemy declares Battle against the holder, the attacker must pay one of:<br>" +
            "• Release a Captured Cultist (any faction's) back to its owner's Pool<br>" +
            "• Discard an Elder Sign<br>" +
            "• Discard a Silence Token<br><br>" +
            "If none can be paid, the Battle is blocked.", faceUp)

        case $("Guardian under the Lake", faceUp : Boolean) => tomeOverlay("Guardian under the Lake", "Action: Cost 1 (flippable)",
            "Choose an Archway region containing enemy units. Move all units of one enemy faction " +
            "from that Archway to another Archway region. Flips face-down after use. " +
            "You may flip it face-up again (and reuse it) by releasing a Captured Cultist back to its owner's Pool, discarding an Elder Sign, or discarding a Silence Token. It also flips in the next Doom Phase.", faceUp)

        case $("Larvae of the Outer Gods", faceUp : Boolean) => tomeOverlay("Larvae of the Outer Gods", "Action: Cost 1 (flippable)",
            "If any opponent has more Power than you, gain 1 Elder Sign. " +
            "Flips face-down after use. " +
            "You may flip it face-up again (and reuse it) by releasing a Captured Cultist back to its owner's Pool, discarding an Elder Sign, or discarding a Silence Token. It also flips in the next Doom Phase.", faceUp)

        case $("Yr and the Nhhngr", faceUp : Boolean) => tomeOverlay("Yr and the Nhhngr", "Action: Cost 1 (flippable)",
            "If any opponent has more Doom than you, choose one:<br>" +
            "• Place 1 Monster from your Pool at a Controlled Gate (no additional cost)<br>" +
            "• Gain 2 Power (net +1 after the 1 Power cost)<br><br>" +
            "Flips face-down after use. You may flip it face-up again (and reuse it) by releasing a Captured Cultist back to its owner's Pool, discarding an Elder Sign, or discarding a Silence Token. It also flips in the next Doom Phase.", faceUp)

        // Tombstalker (TS): faction info card showing Death March ability, units (TombHerd, DeepTendril, Gla'aki)
        case $("TS") => faction(TS, "info:ts-background", DeathMarch, "Ongoing",
            "Increment the Death's Head each time an enemy Unit dies in any Battle. In the Doom Phase, spend 1 Death's Head to place a Tomb-Herd in any Area; repeat as much as possible. Then reset the Death's Head to 0.",
            $(), $(
            (Acolyte,    6, "1",   "0", s""""""),
            (TombHerd,   6, "2",   "3", s"""<div class=p>The first Tomb-Herd in an Area has 3 Combat. Any others have 0 Combat.</div>"""),
            (DeepTendril, 3, "3", "1-3", s"""<div class=p>Combat: 1, +1 if Gla'aki is in the same Area, +1 if in an Ocean/Sea Area.</div>"""),
            (Glaaki,     1, "7",   "?", s"""<div class=p>Combat: equals double the number of Deep Tendrils in play.</div><div class=p><b>How to Awaken Tombstalker Gla'aki:</b></div><div class=p>1) You must Control a Gate in an Ocean/Sea Area</div><div class=p>2) Pay 6 Power (you may also spend Death's Head as Power)</div><div class=p>3) Gla'aki appears in that Area</div><div class=p><span class=ability-color>Shepherd of the Crypt</span> (Gather Power Phase): choose an Area and gain 1 Power per Tomb-Herd there.</div>""")
        ))

        // Tombstalker (TS): spellbook requirement info card overlays
        case $("TS", TSAwakenGlaaki.text) => requirement("Awaken Tombstalker Gla'aki.")
        case $("TS", TSTombHerdKilled.text) => requirement("A Tomb-Herd is Killed in Battle.")
        case $("TS", TSRollKill.text) => requirement("Roll a Kill in a Battle.")
        case $("TS", TSRoll3Pains.text) => requirement("Roll 3 or more Pains in a single Battle.")
        case $("TS", TSGlaakiBattlesGOO.text) => requirement("Tombstalker Gla'aki is in a Battle with an enemy Great Old One.")
        case $("TS", TSRitualOrEnemyGate.text) => requirement("Perform a Ritual of Annihilation OR Control a Gate in an enemy faction's starting Area.")

        // Tombstalker (TS): spellbook info card overlays
        case $("TS", ElevenRevelations.name) => spellbook(ElevenRevelations.name, "Action: Cost 1", "Give an enemy your topmost Cursed Tome (I-XI) which they place on their Faction Sheet.")
        case $("TS", Oleaginous.name) => spellbook(Oleaginous.name, "Post-Battle", "Any Pains applied to Tombstalker Gla'aki and Deep Tendrils instead become Retreats (you can move them to any adjacent Areas, regardless of the presence of enemy Units).")
        case $("TS", GraspingDead.name) => spellbook(GraspingDead.name, "Action: Cost 1 Power or 2 Death's Head", "Resolve a Battle in each Area containing a Tomb-Herd and enemy Units, with you as the attacker, for free. Only your Tomb-Herd participate in the Battle.")
        case $("TS", Hecatomb.name) => spellbook(Hecatomb.name, "Doom Phase", "After placing all Tomb-Herd from Death March, but before resetting the counter to 0, you may spend residual Death's Head toward the cost of a Ritual of Annihilation as Power. If you only spent Death's Head toward the cost of a Ritual, do not advance the Ritual Marker.")
        case $("TS", GreenDecay.name) => spellbook(GreenDecay.name, "Gather Power Phase", "If Tombstalker Gla'aki is in play, captured Cultists gain you 1 Elder Sign (instead of 1 Power) during the Gather Power Phase.")
        case $("TS", Undulate.name) => spellbook(Undulate.name, "Ongoing", "Your Units may carry one of your Units of lesser cost when moving for free. This effect stacks.")


        // Firstborn (FB): faction info card showing unique ability (Writhe), units, and Crater building
        case $("FB") => faction(FB, "info:fb-background", Writhe, "Action: Cost 2",
            "Roll dice equal to your Power. For each Kill: Eliminate a Unit you control, any of your Acolytes Eliminated are instead replaced with Desiccated. For each Pain, relocate your Unit to any Area. Before applying these results, you may reroll ALL these dice once.<br/><br/><span class=ability-color>Crater</span> <span class=cost-color>(Building):</span> Any Gate (other than Yog-Sothoth) that coexists in an Area with a Crater is immediately destroyed.",
            $(), $(
            (Acolyte,        6, "1", "0", s""""""),
            (Desiccated,     6, "2", "0+", s"""<div class=p>Combat is 1 if in a land Area, 0 if in a sea Area.</div>"""),
            (RevenantOfKnaa, 2, "3", "?", s"""<div class=p>Combat equals the number of Desiccated in play.</div>"""),
            (Ghatanothoa,    1, "?", "?", s"""<div class=p>Cost: 11 minus Ritual cost. Combat equals your Power.</div><div class=p><span class=ability-color>Infernal Pact</span> (Ongoing): You may discount the cost of any Action you perform by flipping any number of your faceup spellbooks, reducing that cost by 1 per spellbook flipped.</div>""")
        ))

        // Firstborn (FB): spellbook requirement info card overlays
        case $("FB", FBNoAcolytesInStart.text) => requirement("None of your Acolytes are in your Start Area.")
        case $("FB", FBAwakenGhatanothoa.text) => requirement("Awaken Ghatanothoa.")
        case $("FB", FBTwoFacedownSpellbooks.text) => requirement("Have two facedown spellbooks.")
        case $("FB", FBSecondAwakening.text) => requirement("Awaken Ghatanothoa a second time.")
        case $("FB", FBMostDoomOrMoreGates.text) => requirement("Have more Doom than any other player OR Control more Gates than the First Player.")
        case $("FB", FBThirdAwakening.text) => requirement("Awaken Ghatanothoa a third time.")

        // Firstborn (FB): spellbook info card overlays
        case $("FB", Augury.name) => spellbook("Augury", "Ongoing", "Put a Kill on this spellbook for each Kill that was cancelled or unapplied in a Battle you were in. Whenever you roll a blank (for Battle or Writhe) you may replace it with a Kill from this Spellbook. If this spellbook is flipped, discard all dice on it.")
        case $("FB", Carnage.name) => spellbook("Carnage", "Post-Battle", "If you AND your opponent Killed or Eliminated Units in the Battle, you may pay 1 Power or flip any of your spellbooks facedown (including this one) to gain an Elder Sign.")
        case $("FB", TheEyeOpens.name) => spellbook("The Eye Opens", "Action: Cost 1", "For each Area containing a Desiccated and enemy Cultist(s): choose an Enemy Faction to Eliminate one of their Cultists in that Area then Eliminate your Desiccated in that Area to gain a Power.")
        case $("FB", CyclopeanGaze.name) => spellbook("Cyclopean Gaze", "Ongoing", "Whenever an opponent ends an Action in any Area(s) containing Revenant(s) and/or Ghatanothoa, they must Pain one of their Units from those Areas for each Revenant and/or Ghatanothoa present, with Firstborn choosing where the Units are Pained, following normal Pain rules.")
        case $("FB", DevilsMark.name) => spellbook("Devil's Mark", "Doom Phase", "Place a Crater in a LAND AREA with your Controlled Gate. Destroy all Gates you Control there &amp; gain an Elder Sign for each Gate destroyed. If the Crater was placed in an Area with a Faction Glyph (even your own) gain 1 Power for each Crater in play (including that one).")
        case $("FB", CallOfTheFaithful.name) => spellbook("Call of the Faithful", "Unlimited Action: Cost 0", "Place an Acolyte from your Pool in an Area with Ghatanothoa and/or a Revenant. You may not use this ability in an Area containing one of your Acolytes.")


        // Tcho-Tcho (TT): faction info card
        case $("TT") => faction(TT, "info:tt-background", Sycophancy, "Ongoing",
            "When an enemy player does a Ritual of Annihilation, either you gain 1 Doom, or he gains 1 fewer Doom, his choice.",
            $(), $(
            (Acolyte,       6, "1", "0",  s"""<div class=p>Spellbook: ${reference(TT, Soulless)}</div>"""),
            (HighPriest,    3, "3", "0",  s"""<div class=p><span class=ability-color>Unspeakable Oath</span> ${cost("(Ongoing):")} At the end of any player's Action (even if it is not your turn), Sacrifice your High Priest (return him to your Pool) and gain 2 Power. This may also be done during the Gather Power and Doom phase.</div><div class=p>Spellbooks: ${reference(TT, Martyrdom)}, ${reference(TT, Hierophants)}, ${reference(TT, TabletsOfTheGods)}</div>"""),
            (ProtoShoggoth, 6, "2", "1",  s"""<div class=p>Spellbook: ${reference(TT, TerrorSB)}</div>"""),
            (UbboSathla,    1, "6/0", "?",  s"""
                <div class=p>${cost("How to Awaken Ubbo-Sathla:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate and a High Priest in play (he need not be with the Gate) during the Doom Phase or the Action Phase.</div>
                <div class=p>${cost("2)")} If it is the Doom Phase, pay 0 Power; if it is the Action Phase, pay <span class=cost-color>6 Power</span>.</div>
                <div class=p>${cost("3)")} Eliminate the High Priest, then place Ubbo-Sathla at your Controlled Gate.</div>
                <div class=p>${combat} Equals the Growth counter value on the Doom track.</div>
                <div class=p><span class=ability-color>Hell's Banquet</span> ${cost("(Doom Phase):")} Once Ubbo-Sathla has been Awakened, each Doom Phase (whether or not Ubbo-Sathla is still in play), roll 1d6 and increase the Growth counter by the die roll.</div>""")
        ), "Faction Card Text reflects Tsang Tribe spellbooks regardless of which tribe was chosen, similar to real world faction card.")

        // Tcho-Tcho (TT): spellbook requirement info card overlays
        case $("TT", TTSycophancyTrigger.text)    => requirement("Another faction performs a Ritual of Annihilation OR reaches 15 Doom.")
        case $("TT", TTEarnElderSign.text)         => requirement("Earn an Elder Sign.")
        case $("TT", TTThreeElderSigns.text)       => requirement("Own 3 or more Elder Signs.")
        case $("TT", TTRemoveControlledGate.text)  => requirement("Remove a Gate you control in your Start Area.")
        case $("TT", TTGOOKilledInBattle.text)     => requirement("Any GOO is Killed in Battle.")
        case $("TT", TTAwakenUbboSathla.text)      => requirement("Awaken Ubbo-Sathla.")

        // Tcho-Tcho (TT): faction ability and shared spellbook info overlays
        case $("TT", Sycophancy.name) => spellbook(Sycophancy.name, "Doom Phase (Faction Ability)", "When an enemy player does a Ritual of Annihilation, either you gain 1 Doom, or he gains 1 fewer Doom, his choice.")
        case $("TT", Hierophants.name) => spellbook(Hierophants.name, "Ongoing (All Tribes)", "When you earn a Faction Spellbook (including this one), place a High Priest at one of your Gates. If there are no High Priests in your Pool, instead advance Ubbo-Sathla's Growth counter by 1. When this Spellbook is first taken, if you are using the High Priests expansion, then all Factions place a High Priest at one of their Gates.")
        case $("TT", Soulless.name) => spellbook(Soulless.name, "Ongoing (All Tribes)", "When Captured and Sacrificed, your Cultists provide 0 Power (instead of the normal 1 Power).")
        case $("TT", TerrorSB.name) => spellbook(TerrorSB.name, "Battle (All Tribes)", "Choose one: Your enemy's Combat total is reduced by 1 per Proto-Shoggoth in the Battle. OR Your Combat total is increased by 1 per Proto-Shoggoth in the Battle.")

        // Tcho-Tcho (TT): Tsang exclusive spellbooks
        case $("TT", Idolatry.name) => spellbook(Idolatry.name, "Action: Cost 1 (Tsang Tribe)", "Select an Area containing another Faction's starting Glyph (even if that Faction is not in play). Move any or all of your Units in adjacent Areas into the selected Area.")
        case $("TT", Martyrdom.name) => spellbook(Martyrdom.name, "Post-Battle (Tsang Tribe)", "If your High Priest is Killed, all Kills assigned to your other Units become Pains instead.")
        case $("TT", TabletsOfTheGods.name) => spellbook(TabletsOfTheGods.name, "Doom Phase (Tsang Tribe)", "When you perform a Ritual of Annihilation, you also receive 1 additional Elder Sign for each Gate at which you have any High Priests. Then, Eliminate all your High Priests. This is not optional.")

        // Tcho-Tcho (TT): Leng exclusive spellbooks
        case $("TT", DarkRituals.name) => spellbook(DarkRituals.name, "Action: Cost 0 (Leng Tribe)", "Flip this spellbook face down. All Factions with your High Priest(s) in their Start Areas must pay you 2 Power or 2 Doom (their choice). A Faction with less than 2 Power or Doom is immune. Flip this Spellbook face up again at the Doom Phase.")
        case $("TT", Fulmination.name) => spellbook(Fulmination.name, "Post-Battle (Leng Tribe)", "If Ubbo-Sathla is Killed in a Battle, you may remove it from the game permanently, and gain 1 Elder Sign for each Unit Killed (by either side) in that Battle.")
        case $("TT", SurpriseSB.name) => spellbook(SurpriseSB.name, "Action: Cost 2 (Leng Tribe)", "Choose an enemy Faction. That player selects and Eliminates one of their Acolytes; Replace it with a Proto-Shoggoth from your Pool.")

        // Tcho-Tcho (TT): Sarkomand exclusive spellbooks
        case $("TT", Doomsday.name) => spellbook(Doomsday.name, "Once Only (Sarkomand Tribe)", "If you have a Controlled Gate, flip this card face down, then immediately take from the Pool one Independent Great Old One with a cost of exactly 2 or 4 and place it at that Gate. Take its Loyalty Card.")
        case $("TT", Inerrant.name) => spellbook(Inerrant.name, "Doom Phase (Sarkomand Tribe)", "When you perform a Ritual of Annihilation, gain 1 additional Elder Sign for each enemy-Controlled Gate at which you have any Great Old Ones.")
        case $("TT", OtherworldAlliances.name) => spellbook(OtherworldAlliances.name, "Ongoing (Sarkomand Tribe)", "Your Neutral Monsters and Terrors have +1 Combat.")

        // Tcho-Tcho (TT): tribe selection overlays — all 3 spellbooks in a single table via multiSpellbook
        case $("TT-TribeLeng") =>
            multiSpellbook("",
                (DarkRituals.name, "Action: Cost 0", "Flip this spellbook face down. All Factions with your High Priest(s) in their Start Areas must pay you 2 Power or 2 Doom (their choice). A Faction with less than 2 Power or Doom is immune. Flip this Spellbook face up again at the Doom Phase."),
                (Fulmination.name, "Post-Battle", "If Ubbo-Sathla is Killed in a Battle, you may remove it from the game permanently, and gain 1 Elder Sign for each Unit Killed (by either side) in that Battle."),
                (SurpriseSB.name, "Action: Cost 2", "Choose an enemy Faction. That player selects and Eliminates one of their Acolytes; Replace it with a Proto-Shoggoth from your Pool."))
        case $("TT-TribeSarkomand", hasIGOOs : Boolean) =>
            val warning = hasIGOOs.not.??("<tr><td></td><td><div style='color:#ff4444;font-weight:bold;text-transform:uppercase;padding:4px 0 8px 0;'>Warning — Doomsday will not function without 2/4 power iGOOs.</div></td><td></td></tr>")
            multiSpellbook(warning,
                (Doomsday.name, "Once Only", "If you have a Controlled Gate, flip this card face down, then immediately take from the Pool one Independent Great Old One with a cost of exactly 2 or 4 and place it at that Gate. Take its Loyalty Card."),
                (Inerrant.name, "Doom Phase", "When you perform a Ritual of Annihilation, gain 1 additional Elder Sign for each enemy-Controlled Gate at which you have any Great Old Ones."),
                (OtherworldAlliances.name, "Ongoing", "Your Neutral Monsters and Terrors have +1 Combat."))
        case $("TT-TribeSarkomand") =>
            multiSpellbook("",
                (Doomsday.name, "Once Only", "If you have a Controlled Gate, flip this card face down, then immediately take from the Pool one Independent Great Old One with a cost of exactly 2 or 4 and place it at that Gate. Take its Loyalty Card."),
                (Inerrant.name, "Doom Phase", "When you perform a Ritual of Annihilation, gain 1 additional Elder Sign for each enemy-Controlled Gate at which you have any Great Old Ones."),
                (OtherworldAlliances.name, "Ongoing", "Your Neutral Monsters and Terrors have +1 Combat."))
        case $("TT-TribeTsang") =>
            multiSpellbook("",
                (Idolatry.name, "Action: Cost 1", "Select an Area containing another Faction's starting Glyph (even if that Faction is not in play). Move any or all of your Units in adjacent Areas into the selected Area."),
                (Martyrdom.name, "Post-Battle", "If your High Priest is Killed, all Kills assigned to your other Units become Pains instead."),
                (TabletsOfTheGods.name, "Doom Phase", "When you perform a Ritual of Annihilation, you also receive 1 additional Elder Sign for each Gate at which you have any High Priests. Then, Eliminate all your High Priests. This is not optional."))


        // Bubastis (BB): faction info card (standard and alt-spellbooks variant)
        case $("BB")                     => bbFactionOverlay(false)
        case $("BB", altSB : Boolean)    => bbFactionOverlay(altSB)

        // Defilers Court (DC): faction info card — Homebrew faction
        // Unique abilities: Tenebrosum (Ongoing) + Depravity (Gather Power).
        // Card art transcribed from Defiler_Faction_Card.png + Defiler_Spellbooks.png.
        // Fix HB-77 (2026-06-06): Y'Golonac awaken cost is dynamic — reads the
        // current DC spellbook count from Overlays.currentGame. Pattern mirrors
        // TS Glaaki's awakenCost(): live formula, not a static "?".
        case $("DC") =>
            val ygCostDisplay : String = Overlays.currentGame match {
                case Some(g) => { implicit val gg : Game = g; s"${DC.spellbooks.num} Power" }
                case None    => "?"
            }
            faction(DC, "info:dc-background", Tenebrosum, "Ongoing",
            "When you perform a Common or Spellbook Action, you may perform the same Action again (if it is available to you) using Sin as if it were Power.<br/><br/>" +
            "<span class=ability-color>Depravity</span> <span class=cost-color>(Gather Power):</span> Gain 1 Sin for each Cultist you have on the map. Sin is not Power & can be kept over multiple phases with a max equal to twice the Ritual Marker.",
            $(), $(
            (Acolyte,         6, "1", "0", s"""<div class=p>Start on Spellbook requirement slots. Released into play upon SB acquisition.</div>"""),
            (MindlessHusk,    5, "1", "1", s"""<div class=p>Spellbook: ${reference(DC, Eschar)}</div>"""),
            (FallenProphet,   4, "3", "?", s"""<div class=p>Combat: During your turn, equals the number of enemy Cultists in the Area. Any other time, equals the number of your Cultists in the Area.</div><div class=p>Spellbook: ${reference(DC, Pilgrimage)}</div>"""),
            (YgolonacDC,      1, ygCostDisplay, "?", s"""
                <div class=p>${cost(s"How to Awaken ${YgolonacDC.name}:")}</div>
                <div class=p>${cost("1)")} Pay Power equal to the number of Spellbooks on your Faction Sheet.</div>
                <div class=p>${cost("2)")} Y'Golonac appears in a LAND AREA lacking a Controlled Gate.</div>
                <div class=p>${combat} Equals half your amount of Sin (rounded up).</div>
                <div class=p><span class=ability-color>Bacchanal</span> ${cost("(Ongoing):")} Y'Golonac can Build & Control Gates, & generates 1 Power and 1 Sin in the Gather Power Phase.</div>
                <div class=p>Spellbooks: ${reference(DC, Lure)}, ${reference(DC, Eschar)}, ${reference(DC, DarkBargain)}</div>""")
        ))

        // Defilers Court (DC): spellbook requirement info card overlays
        case $("DC", ProselytizeReq.text) => requirement("You may take this spellbook in the Doom Phase. When you do, gain 2 Sin per enemy Great Old One in play.")
        case $("DC", SatiateReq.text)     => requirement("You may take this spellbook in the Doom Phase. When you do, gain 1 Power for each other Spellbook on your Faction Sheet and 1 Sin for each remaining Spellbook in your Pool.")
        case $("DC", LureReq.text)        => requirement("Have no Mindless Husks in your Pool.")
        case $("DC", EscharReq.text)      => requirement("Have no Fallen Prophets in your Pool.")
        case $("DC", PilgrimageReq.text)  => requirement("Any player performs a Ritual of Annihilation.")
        case $("DC", DarkBargainReq.text) => requirement("Awaken Y&#39;Golonac, Lord of Sin.")

        // Defilers Court (DC): spellbook info card overlays (verbatim from card art)
        case $("DC", Tenebrosum.name)  => spellbook(Tenebrosum.name,  "Ongoing (Faction Ability)", "When you perform a Common or Spellbook Action, you may perform the same Action again (if it is available to you) using Sin as if it were Power.")
        case $("DC", Depravity.name)   => spellbook(Depravity.name,   "Gather Power (Faction Ability)", "Gain 1 Sin for each Cultist you have on the map. Sin is not Power & can be kept over multiple phases with a max equal to twice the Ritual Marker.")
        case $("DC", Proselytize.name) => spellbook(Proselytize.name, "Ongoing", "Each of your Acolytes 'drag' an enemy Acolyte of each enemy Faction (of those player(s) choice) from any Areas they move from, to the Area they move to.")
        case $("DC", Satiate.name)     => spellbook(Satiate.name,     "Action: Cost 2", "Capture a Cultist from each Faction with Cultists in Y'Golonac's Area including yourself. Gain 1 Elder Sign for each Cultist captured beyond the first. This Capture cannot be protected by the presence of enemy Great Old Ones or any other abilities (like Lunacy's capture protection, or Masquerade).")
        case $("DC", Lure.name)        => spellbook(Lure.name,        "Action: Cost 1", "Each enemy Faction must move one of their Cultists from a (non-Moon) Area adjacent to Y'Golonac, into Y'Golonac's Area. Enemy Cultists in Areas containing an enemy Great Old One, Terror or Faction Buildings are exempt.")
        case $("DC", Eschar.name)      => spellbook(Eschar.name,      "Post-Battle", "Gain 1 Sin per Mindless Husk that is Killed in Battle.")
        case $("DC", Pilgrimage.name)  => spellbook(Pilgrimage.name,  "Action: Cost 1", "Choose a Fallen Prophet, then move any or all of your other Units from that Area to an adjacent Area for free.")
        case $("DC", DarkBargain.name) => spellbook(DarkBargain.name, "Action: Cost 0", "If Y'Golonac is in play, all enemies have 30 seconds to choose a number using a D6. You gain Sin equal to one of those numbers of your choice, then distribute an equal amount of Power as evenly as possible among all enemies. Flip this spellbook facedown until the Gather Power Phase.")

        // ── FACELESS BLIGHT (FBE) — Homebrew faction overlays ───────────────
        // Delegated to fbeOverlay(...) to keep the giant `info` match under the JVM
        // 64KB method-size limit (the inline cases overflowed it).
        case s2 if s2.headOption.exists(_ == "FBE") => fbeOverlay(s2)

        // Bubastis (BB): alternate spellbooks info overlay.
        // Help text shows the FULL rulebook text of both replacement spellbooks
        // (Syzygy and Carnivore), mirroring the DS Alternate Spellbooks help
        // pattern at line ~534. Verbatim wording matches the spellbook overlays
        // at lines 897-898 below.
        case $("BBAlternateSpellbooks") => spellbook("Bubastis — Alternate Spellbooks", "",
            s"<span class=ability-color>${Catabolism.name}</span> is replaced with <span class=ability-color>${Syzygy.name}</span>, and <span class=ability-color>${Ailurophobia.name}</span> is replaced with <span class=ability-color>${Carnivore.name}</span>:<br/><br/>" +
            s"<span class=ability-color>${Syzygy.name}</span> <span class=cost-color>(Doom Phase)</span><br/>" +
            "If you have no units on the Moon, gain an Elder Sign.<br/><br/>" +
            s"<span class=ability-color>${Carnivore.name}</span> <span class=cost-color>(Post-Battle)</span><br/>" +
            "Gain 1 Doom per enemy Monster Killed or Eliminated by You in Battle.")

        // (bbFactionOverlay is defined as a helper method near the bottom of this file)

        // Bubastis (BB): spellbook requirement info card overlays
        case $("BB", Pay2ForBB.text)            => requirement("As your Action, pay 2 Power.")
        case $("BB", NoEarthCatsOnMoon.text)    => requirement("None of your Earth Cats are on the Moon.")
        case $("BB", CatInEveryEnemyStart.text) => requirement("One of your Cats is in every enemy faction's Start Area.")
        case $("BB", MarsOrSaturnLost.text)     => requirement("A Cat from Mars or Saturn is Killed or Eliminated.")
        case $("BB", UranusLost.text)           => requirement("A Cat from Uranus is Killed or Eliminated.")
        case $("BB", AwakenBastet.text)         => requirement("Awaken Bastet.")

        // Bubastis (BB): spellbook info card overlays
        case $("BB", Catabolism.name)   => spellbook(Catabolism.name,   "Ongoing",         "Earth Cats can Recruit monsters, as if the monsters were cultists, instead of needing to Summon them.")
        case $("BB", Zagazig.name)      => spellbook(Zagazig.name,      "Pre-Battle",      "If a Cat from Mars is in the Battle, all rolled Pains become Kills, and all rolled Kills become Pains.")
        case $("BB", Savagery.name)     => spellbook(Savagery.name,     "Pre-Battle",      "Pay 1 Power to increase the Combat of all Cats from Saturn by 4 for this Battle.")
        case $("BB", Predator.name)     => spellbook(Predator.name,     "Post-Battle",     "If a Cat from Uranus was in the Battle, you can select one Unit lost by the enemy. He must eliminate a second Unit of that type anywhere on the Map, if possible.")
        case $("BB", Catnapping.name)   => spellbook(Catnapping.name,   "Action: Cost 1",  "Choose any or all enemy Factions in Bastet’s Area. Move all Units belonging to those Faction(s) from that Area to the Moon. When an enemy Unit on the Moon leaves, you gain the Power they spend on movement.")
        case $("BB", Ailurophobia.name) => spellbook(Ailurophobia.name, "Doom Phase",      "Gain 1 Doom per Monster Cat Variety sharing any non-Moon Area with Unit(s) from enemy Factions.")

        // Bubastis (BB): alternate spellbook info card overlays
        case $("BB", Syzygy.name)    => spellbook(Syzygy.name,    "Doom Phase",  "If you have no units on the Moon, gain an Elder Sign.")
        case $("BB", Carnivore.name) => spellbook(Carnivore.name, "Post-Battle", "Gain 1 Doom per enemy Monster Killed or Eliminated by You in Battle.")

        // Bubastis (BB): Requires Attention info card (Bastet's per-GOO doom-phase ritual)
        case $("BB", RequiresAttention.name) => spellbook(RequiresAttention.name, "Doom Phase", "If Bastet is in an Area containing an enemy Cultist, you may perform a Ritual of Annihilation. For you, this adds exactly 4 Doom plus: if Bastet's Area has an Enemy-Controlled Gate, gain 1 Elder Sign; if Bastet's Area has an Enemy Great Old One, gain 2 Elder Sign. These rewards are additive.")

        // Bubastis (BB): Moon overlay — clicking the small Moon HUD button in the
        // top-right of the map opens this LARGE-Moon view. The big Moon image
        // auto-fits inside the map area (the overlay container is the map pane,
        // sized 60vw × 60vh per index.html). The Moon disc must fit top-to-bottom
        // INSIDE that pane, NOT spill out and NOT exceed the viewport.
        //
        // Background MUST be transparent — only the round Moon PNG should show
        // through, floating against whatever is behind the overlay. No black
        // square, no dark wash, no opaque table backdrop.
        //
        // Each unit currently on the Moon is rendered as the SAME map sprite
        // image used elsewhere on the board (NOT a text list, NOT a silhouette).
        // Sprites are SCATTERED across the moon disc — placed at varied (x, y)
        // positions in a polar layout (rings of increasing radius) so the result
        // looks like a region with units strewn across it, not a straight line
        // or flex-wrapped grid. This is the closest HTML analog to drawMap's
        // findAnother random-placement behavior for normal regions; we cannot
        // re-use the canvas bitmap engine here because the overlay is HTML, but
        // the visual outcome (scattered sprites inside the region) matches.
        //
        // Click anywhere on the overlay (handled by InfoOverlay's parent
        // click-to-hide) to dismiss back to the small Moon HUD.
        //
        // Filter: only units whose region == BB.moon are passed in (see
        // CthulhuWarsSolo.scala moonFigs = factions./~(ff => ff.at(BB.moon))).
        // This overlay does NOT iterate all BB units or all cat varieties —
        // it shows exactly the units who currently live in the Moon region,
        // exactly like rendering any other region.
        //
        // Encoding: unitList is a `;`-separated list of `assetId|displayName`
        // entries, packed in CthulhuWarsSolo.scala (see moonSpriteAssetId helper).
        case $("BB", "Moon", count : Int, unitList : String) =>
            val moonSrc = imageSource("bb-moon-high-res")
            val rawEntries = if (unitList.toString.trim.nonEmpty)
                unitList.toString.split(";").toList
            else List.empty
            // Each entry: "asset-id|Display Name (FAC)|onMapH". Render the map
            // sprite image. Defensive empty fallback on missing asset ids so the
            // overlay never breaks if a future unit class slips through without a
            // sprite mapping. The 3rd field (BB Moon sizing): the unit's on-map
            // sprite height in map-pixel space. Defaults to 70 (Earth Cat height)
            // for legacy 2-field entries so old cached payloads still render.
            val parsed = rawEntries./(entry => {
                val parts = entry.split("\\|", 5)
                val assetId = if (parts.length > 0) parts(0).trim else ""
                val display = if (parts.length > 1) parts(1).trim else assetId
                val hp      = if (parts.length > 2) parts(2).trim else "alive"
                val onMapH  = if (parts.length > 3)
                    scala.util.Try(parts(3).trim.toDouble).getOrElse(70.0)
                else 70.0
                val fShort  = if (parts.length > 4) parts(4).trim else ""
                val src     = if (assetId.nonEmpty) {
                    val tint = fShort match {
                        case "GC" => CthulhuWarsSolo.Processing(|("#77a055"), |("#222222"), None)
                        case "CC" => CthulhuWarsSolo.Processing(|("#4977b3"), |("#111111"), None)
                        case "BG" => CthulhuWarsSolo.Processing(|("#cd3233"), None, |("#555555"))
                        case "YS" => CthulhuWarsSolo.Processing(|("#ffd000"), |("#663344"), None)
                        case "WW" => CthulhuWarsSolo.Processing(|("#88a9be"), |("#5577aa"), None)
                        case "SL" => CthulhuWarsSolo.Processing(|("#db6a33"), |("#4a1a1a"), None)
                        case "OW" => CthulhuWarsSolo.Processing(|("#6c4296"), None, |("#4c4c4c"))
                        case "AN" => CthulhuWarsSolo.Processing(|("#47a5bc"), |("#333333"), None)
                        case "TS" => CthulhuWarsSolo.Processing(|("#BDE0BC"), |("#333333"), None)
                        case "FB" => CthulhuWarsSolo.Processing(|("#CB307E"), |("#333333"), None)
                        case "DS" => CthulhuWarsSolo.Processing(|("#3A2825"), None, |("#120E0C"))
                        case "TT" => CthulhuWarsSolo.Processing(|("#fc9ca0"), |("#333333"), None)
                        case "BB" => CthulhuWarsSolo.Processing(|("#c8a84b"), |("#333333"), None)
                        case "DC" => CthulhuWarsSolo.Processing(|("#F0EDA8"), |("#333333"), None)
                        case "XSS" => CthulhuWarsSolo.Processing(|("#4a6b7a"), |("#333333"), None)
                        case "TB" => CthulhuWarsSolo.Processing(|("#8b6914"), |("#333333"), None)
                        case "FBE" => CthulhuWarsSolo.Processing(None, None, None, |("#3d5f1c"))
                        case _    => CthulhuWarsSolo.Processing(None, None, None)
                    }
                    CthulhuWarsSolo.getTintedAsset(assetId, tint).toDataURL("image/png")
                } else ""
                (src, display, hp, onMapH)
            }).filter { case (src, _, _, _) => src.nonEmpty }
            // [v2.4.10] Use the dedicated Moon placement bitmap (bb-moon-place /
            // bb-moon-h-place) to scatter sprites strictly inside the moon disc.
            // The bitmap drives placement just like earth*-place / library*-place
            // bitmaps drive map region placement: a single solid magenta circle
            // marks the valid zone. MoonPlacement samples that circle once and
            // hands back farthest-point-spread (xFrac, yFrac) pairs in [0..1]
            // of the circle's bounding box. We map those onto the moon image
            // rect so every cat sprite lands inside the disc.
            val n = parsed.length
            // BB Moon sizing: render each sprite at a height PROPORTIONAL to its
            // real on-map sprite height, so the Moon matches the regular map (a
            // Bastet towers over an Earth Cat, exactly as on the board) instead of
            // every unit being a flat 14%-tall sprite. The Earth Cat (on-map height
            // 70px) is the anchor: scaled to 10% moon height (reduced from 14% to
            // prevent large TB units from dominating). Other units scale relative
            // to it by the same 0.143 (= 10/70) %-per-map-pixel factor. Cap at 12%
            // to prevent large GOOs (TB Shudde M'ell Head/Segments, etc.) from dominating.
            val moonSpriteScale = 10.0 / 70.0
            def spriteHFor(onMapH : Double) : Double = (onMapH * moonSpriteScale).min(12.0)
            val useHorizontal = dom.window.innerWidth > dom.window.innerHeight
            val seed = parsed.length * 31 + parsed./({ case (s, _, _, _) => s }).mkString.hashCode
            val rawScatter = MoonPlacement.scatter(n, useHorizontal, seed)
            // Map each (xFrac, yFrac) of the circle bbox onto the moon image:
            // the moon disc fills ~76% horizontally and ~89% vertically of
            // bb-moon-high-res. Center each sprite around the disc's image
            // center and scale by the disc's coverage fraction.
            val discCx = 50.0
            val discCy = 50.0
            val discWPct = 76.0  // disc horizontal coverage of moon image
            val discHPct = 89.0  // disc vertical   coverage of moon image
            val positions : List[(Double, Double)] = rawScatter.toList./ { case (xf, yf) =>
                val dx = (xf - 0.5) * discWPct
                val dy = (yf - 0.5) * discHPct
                (discCx + dx, discCy + dy)
            }
            val unitFigures = parsed.zip(positions)./({ case ((src, display, hp, onMapH), (xPct, yPct)) =>
                val spriteH = spriteHFor(onMapH)
                println(s"[MOON UNIT SCALE] Unit: $display, onMapH: $onMapH, calculated spriteH: $spriteH%")
                val hpOverlay = hp match {
                    case "killed" => s"""<img src="${imageSource("kill")}" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none;" />"""
                    case "pained" => s"""<img src="${imageSource("pain")}" style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none;" />"""
                    case _ => ""
                }
                f"""<div style="position: absolute; left: $xPct%2.2f%%; top: $yPct%2.2f%%; transform: translate(-50%%, -50%%); height: $spriteH%2.1f%%; width: auto; pointer-events: none;">
                    <img src="$src" title="$display" style="height: 100%%; width: auto; filter: drop-shadow(0 0 0.4em rgba(0,0,0,0.95));" />$hpOverlay</div>"""
            }).mkString("")
            val figureLayer = if (count > 0)
                s"""<div style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; overflow: hidden;">$unitFigures</div>"""
            else ""
            val emptyCaption = if (count == 0)
                s"""<div style="position: absolute; bottom: 6%; left: 0; right: 0; text-align: center; color: #c8a84b; text-shadow: 0 0 4px black, 0 0 4px black; font-size: 1.4em;">The Moon is empty.</div>"""
            else ""
            // Transparent backdrop. The outer table has no background. The
            // moon image is sized so its FULL height fits inside the overlay
            // pane (which is the map pane, 60vw × 60vh). max-height: 100%
            // refers to the table cell, which fills the overlay container.
            // height: 100% on html/body chain via the cell forces the image
            // to be capped by the cell's available height; max-width: 100%
            // also caps it horizontally. Using object-fit-style sizing keeps
            // the moon centered with no letterboxing color.
            s"""<table style="background: transparent; width: 100%; height: 100%; border-collapse: collapse;">
              <tbody>
                <tr>
                  <td style="text-align: center; vertical-align: middle; padding: 0; background: transparent;">
                    <div style="position: relative; display: inline-block; max-width: 100%; max-height: 100%;">
                      <img src="$moonSrc" style="display: block; max-width: 100%; max-height: 60vh; width: auto; height: auto; background: transparent;" />
                      $figureLayer
                      $emptyCaption
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>"""

        case $("BB", "Moon") =>
            val moonSrc = imageSource("bb-moon-high-res")
            s"""<table style="background: transparent; width: 100%; height: 100%; border-collapse: collapse;">
              <tbody>
                <tr>
                  <td style="text-align: center; vertical-align: middle; padding: 0; background: transparent;">
                    <div style="position: relative; display: inline-block; max-width: 100%; max-height: 100%;">
                      <img src="$moonSrc" style="display: block; max-width: 100%; max-height: 60vh; width: auto; height: auto; background: transparent;" />
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>"""

        // The Burrowers Beneath (TB): Mantle overlay — shows units on the Mantle.
        // Mirrors the BB Moon overlay pattern.
        case $("TB", "Mantle", count : Int, unitList : String, adjAreas : String) =>
            val mantleSrc = imageSource("tb-mantle")
            val rawEntries = if (unitList.toString.trim.nonEmpty)
                unitList.toString.split(";").toList
            else List.empty
            val parsed = rawEntries./(entry => {
                val parts = entry.split("\\|", 3)
                val assetId = if (parts.length > 0) parts(0).trim else ""
                val display = if (parts.length > 1) parts(1).trim else assetId
                val onMapH  = if (parts.length > 2)
                    scala.util.Try(parts(2).trim.toDouble).getOrElse(60.0)
                else 60.0
                val src     = if (assetId.nonEmpty)
                    hrf.web.getElem(assetId).as[dom.html.Image]./(_.src).|("")
                else ""
                (assetId, src, display, onMapH)
            }).filter { case (_, src, _, _) => src.nonEmpty }
            val spriteScale = 12.0 / 60.0
            def spriteHFor(onMapH : Double) : Double = onMapH * spriteScale
            val gateEntries = parsed.filter(_._1 == "gate")
            val unitEntries = parsed.filter(_._1 != "gate")
            val gateControlled = gateEntries.exists(_._3.contains("controlled"))
            val gateControllerEntries = if (gateEntries.nonEmpty && gateControlled) unitEntries.filter(e => e._3.contains("Cadavolyte") || e._3.contains("Acolyte")) else $[(String, String, String, Double)]()
            val abandonedCultistEntries = $[(String, String, String, Double)]()
            val otherUnits = if (gateEntries.nonEmpty && gateControlled) unitEntries.filter(e => !e._3.contains("Cadavolyte") && !e._3.contains("Acolyte")) else unitEntries
            val gateFigure = gateEntries.headOption./({ case (_, src, display, onMapH) =>
                val spriteH = spriteHFor(onMapH)
                f"""<img src="$src"
                         title="$display"
                         style="position: absolute; left: 50.00%%; top: 50.00%%; transform: translate(-50%%, -50%%); height: $spriteH%2.1f%%; width: auto; pointer-events: none; filter: drop-shadow(0 0 0.4em rgba(0,0,0,0.95));" />"""
            }).|("")
            val controllerFigure = gateControllerEntries.headOption./({ case (_, src, display, onMapH) =>
                val spriteH = spriteHFor(onMapH)
                f"""<img src="$src"
                         title="$display"
                         style="position: absolute; left: 50.00%%; top: 45.00%%; transform: translate(-50%%, -50%%); height: $spriteH%2.1f%%; width: auto; pointer-events: none; filter: drop-shadow(0 0 0.4em rgba(0,0,0,0.95)); z-index: 1;" />"""
            }).|("")
            val abandonedCultistFigures = abandonedCultistEntries./({ case (_, src, display, onMapH) =>
                val spriteH = spriteHFor(onMapH)
                f"""<img src="$src"
                         title="$display"
                         style="position: absolute; left: 25.00%%; top: 50.00%%; transform: translate(-50%%, -50%%); height: $spriteH%2.1f%%; width: auto; pointer-events: none; filter: drop-shadow(0 0 0.4em rgba(0,0,0,0.95));" />"""
            }).mkString("")
            val remainingControllers = if (gateControllerEntries.nonEmpty) gateControllerEntries.drop(1) else $[( String, String, String, Double)]()
            val allOther = (remainingControllers ++ otherUnits).toList
            val n = allOther.length
            val positions : List[(Double, Double)] = if (n == 0) Nil else {
                val step = 360.0 / n
                (0 until n).toList./(i => {
                    val angle = (i * step - 90.0) * math.Pi / 180.0
                    val radius = if (n <= 3) 25.0 else 30.0
                    (50.0 + radius * math.cos(angle), 50.0 + radius * math.sin(angle))
                })
            }
            val otherFigures = allOther.zip(positions)./({ case ((_, src, display, onMapH), (xPct, yPct)) =>
                val spriteH = spriteHFor(onMapH)
                f"""<img src="$src"
                         title="$display"
                         style="position: absolute; left: $xPct%2.2f%%; top: $yPct%2.2f%%; transform: translate(-50%%, -50%%); height: $spriteH%2.1f%%; width: auto; pointer-events: none; filter: drop-shadow(0 0 0.4em rgba(0,0,0,0.95));" />"""
            }).mkString("")
            val figureLayer = if (count > 0)
                s"""<div style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; overflow: hidden;">$gateFigure$controllerFigure$abandonedCultistFigures$otherFigures</div>"""
            else ""
            val emptyCaption = if (count == 0)
                s"""<div style="position: absolute; bottom: 6%; left: 0; right: 0; text-align: center; color: white; text-shadow: 0 0 4px black, 0 0 4px black; font-size: 0.35em;">The Mantle is empty.</div>"""
            else ""
            val adjCaption = if (adjAreas.toString.trim.nonEmpty)
                s"""<div style="position: absolute; top: 4%; left: 0; right: 0; text-align: center; color: white; text-shadow: 0 0 4px black, 0 0 4px black; font-size: 0.275em;">to: ${adjAreas.toString.trim}</div>"""
            else ""
            s"""<table style="background: transparent; width: 100%; height: 100%; border-collapse: collapse;">
              <tbody>
                <tr>
                  <td style="text-align: center; vertical-align: middle; padding: 0; background: transparent;">
                    <div style="position: relative; display: inline-block; max-width: 100%; max-height: 100%;">
                      <img src="$mantleSrc" style="display: block; max-width: 100%; max-height: 60vh; width: auto; height: auto; background: transparent;" />
                      $figureLayer
                      $emptyCaption
                      $adjCaption
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>"""

        case $("TB", "Mantle") =>
            val mantleSrc = imageSource("tb-mantle")
            s"""<table style="background: transparent; width: 100%; height: 100%; border-collapse: collapse;">
              <tbody>
                <tr>
                  <td style="text-align: center; vertical-align: middle; padding: 0; background: transparent;">
                    <div style="position: relative; display: inline-block; max-width: 100%; max-height: 100%;">
                      <img src="$mantleSrc" style="display: block; max-width: 100%; max-height: 60vh; width: auto; height: auto; background: transparent;" />
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>"""

        // Tombstalker (TS): Cursed Tomes overlay for TS's own card — shows all 11 tomes (white=remaining, grey=given away)
        case $("cursed-tomes", fStyle) if fStyle.toString == "ts" =>
            val givenAway = TSCursedTomesOverlay.tomesOnCard
            val allTomes = TSCursedTomesOverlay.factionTomes
            // Show all 11 tomes in ascending order — white if still on card, grey if given away
            val onCardRows = (1 to 11)./ { n =>
                val text = "Vol. " + tomeNumToRoman(n) + " \u2014 " + TSCursedTomesOverlay.tomeTexts.getOrElse(n, "")
                val opacity = if (n <= givenAway) "0.55" else "1"
                s"""<tr><td style="color:#c8c8c8;opacity:$opacity;padding:3px 12px;font-size:90%;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">$text</td></tr>"""
            }.mkString("")
            // Show tomes held by other factions
            val factionRows = allTomes.toList.flatMap { case (fs, tomes) =>
                tomes.sortBy(_._1)./ { case (n, faceDown) =>
                    val text = "Vol. " + tomeNumToRoman(n) + " \u2014 " + TSCursedTomesOverlay.tomeTexts.getOrElse(n, "")
                    val opacity = if (faceDown) "0.55" else "1"
                    s"""<tr><td style="color:#c8c8c8;opacity:$opacity;padding:3px 12px;font-size:90%;text-shadow:1px 1px 2px rgba(0,0,0,0.85);"><span class="$fs">[$fs]</span> $text</td></tr>"""
                }
            }.mkString("")
            val anyNonTsHolder = allTomes.exists { case (fs, t) => fs != "ts" && t.nonEmpty }
            val footer = if (anyNonTsHolder) TSCursedTomesOverlay.tomeFactionPowerBlock else ""
            s"""<table class="spellbook-table"><tbody><tr><td style="color:white;padding:4px 12px;font-weight:bold;border-bottom:1px solid grey;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">Cursed Tomes</td></tr>$onCardRows$factionRows$footer</tbody></table>"""

        // Tombstalker (TS): Cursed Tomes overlay for other factions — shows tomes held (white=face-up, grey=face-down)
        case $("cursed-tomes", fStyle) =>
            val tomes = TSCursedTomesOverlay.factionTomes.getOrElse(fStyle.toString, Nil)
            if (tomes.isEmpty) ""
            else {
                val rows = tomes.sortBy(_._1)./ { case (n, faceDown) =>
                    val text = "Vol. " + tomeNumToRoman(n) + " \u2014 " + TSCursedTomesOverlay.tomeTexts.getOrElse(n, "")
                    val opacity = if (faceDown) "0.55" else "1"
                    s"""<tr><td style="color:#c8c8c8;opacity:$opacity;padding:3px 12px;font-size:90%;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">$text</td></tr>"""
                }.mkString("")
                s"""<table class="spellbook-table"><tbody><tr><td style="color:white;padding:4px 12px;font-weight:bold;border-bottom:1px solid grey;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">Cursed Tomes</td></tr>$rows${TSCursedTomesOverlay.tomeFactionPowerBlock}</tbody></table>"""
            }

        case $("RoA") =>
            val pos = RitualTrackOverlay.positions
            val marker = RitualTrackOverlay.markerIndex
            val history = RitualTrackOverlay.ritualHistory
            val trackLen = RitualTrackOverlay.trackLength
            val imgSrc = imageSource(RitualTrackOverlay.trackImageId)

            val markerDiv = if (marker < pos.length) {
                val (mx, my) = pos(marker)
                val isID = marker == pos.length - 1
                if (isID) {
                    // Ellipse centered on the stored ID position (which points to center of "Instant Death" text).
                    // width=20% of track width; aspect-ratio=3.0 gives correct height across all track aspect ratios.
                    s"""<div style="
                        position: absolute;
                        left: ${mx}%;
                        top: ${my}%;
                        width: 20%;
                        aspect-ratio: 3.0;
                        transform: translate(-50%, -50%);
                        border-radius: 50%;
                        border: 0.18em solid #ff2020;
                        box-shadow: 0 0 0.5em 0.2em rgba(255,32,32,0.8), inset 0 0 0.3em rgba(255,32,32,0.3);
                        box-sizing: border-box;
                        pointer-events: none;">
                    </div>"""
                } else {
                    s"""<div style="
                        position: absolute;
                        left: ${mx}%;
                        top: ${my}%;
                        width: 8%;
                        aspect-ratio: 1;
                        transform: translate(-50%, -50%);
                        border-radius: 50%;
                        border: 0.18em solid #ff2020;
                        box-shadow: 0 0 0.5em 0.2em rgba(255,32,32,0.8), inset 0 0 0.3em rgba(255,32,32,0.3);
                        box-sizing: border-box;
                        pointer-events: none;">
                    </div>"""
                }
            } else ""

            val ceremony = RitualTrackOverlay.ritualHistoryCeremony
            val glyphs = history.indexed./ { (style, i) =>
                val isID = i >= trackLen - 1
                // ID glyphs anchor to circle 10 (second-to-last position) and extend right
                val (gx, gy) =
                    if (isID) pos(math.min(trackLen - 2, pos.length - 1))
                    else pos(math.min(i, pos.length - 1))
                val extraOffset = if (isID) (i - (trackLen - 1)) * 9.0 + 7.0 else 0.0
                val finalX = gx + extraOffset
                val isCeremony = i < ceremony.length && ceremony(i)
                // Firstborn (FB) Bug fix Round 3: FB map glyph was shrunk to 60x60 (60% of normal)
                // so the faction icon on the map wouldn't dominate the region, but the RoA track
                // uses a separate render context — the map size change left FB's RoA track marker
                // looking tiny next to every other faction's glyph. Bump FB's RoA track glyph width
                // by 30% (6% -> 7.8%) ONLY on the RoA track; the map render still uses 60x60.
                val glyphWidthPct = if (style == "fb") "7.8%" else "6%"
                if (isCeremony) {
                    val factionColor = style match {
                        case "gc" => "#77a055"
                        case "cc" => "#4977b3"
                        case "bg" => "#cd3233"
                        case "ys" => "#ffd000"
                        case "ww" => "#88a9be"
                        case "sl" => "#db6a33"
                        case "ow" => "#6c4296"
                        case "an" => "#47a5bc"
                        case "ts" => "#BDE0BC"
                        case "fb" => "#CB307E"
                        case "ds" => "#3A2825"
                        case "tt" => "#fc9ca0"
                        case "bb" => "#c8a84b"
                        case "dc" => "#F0EDA8"
                        case "fbe" => "#3d5f1c"
                        case "xss" => "#4a6b7a"
                        case "tb" => "#8b6914"
                        case _    => "#888888"
                    }
                    val processing = CthulhuWarsSolo.Processing(None, None, None, |(factionColor))
                    val tintedCanvas = CthulhuWarsSolo.getTintedAsset("n-tulzscha", processing)
                    val tintedSrc = tintedCanvas.toDataURL("image/png")
                    s"""<img src="${tintedSrc}"
                        style="
                            position: absolute;
                            left: ${finalX}%;
                            top: ${gy}%;
                            width: ${glyphWidthPct};
                            height: auto;
                            transform: translate(-50%, -50%);
                            opacity: 0.9;
                            pointer-events: none;
                            filter: drop-shadow(0 0 0.15em rgba(0,0,0,0.9));" />"""
                } else {
                    s"""<img src="${imageSource(style + "-glyph")}"
                        style="
                            position: absolute;
                            left: ${finalX}%;
                            top: ${gy}%;
                            width: ${glyphWidthPct};
                            height: auto;
                            transform: translate(-50%, -50%);
                            opacity: 0.9;
                            pointer-events: none;
                            filter: drop-shadow(0 0 0.15em rgba(0,0,0,0.9));" />"""
                }
            }.mkString("")

            // Tombstalker (TS) Pure DH Hecatomb: render TS glyph markers on RoA track for rituals paid entirely with Death's Head
            val pureDHFixedY = RitualTrackOverlay.numPlayers match {
                case 3 => 28.0   // 3p: ID Y=51, one glyph height above
                case 5 => 17.0   // 5p: ID Y=40, one glyph height above
                case _ => 26.0   // 4p: ID Y=49, one glyph height above
            }
            val glyphWidth = 6.0
            var lastPureDHX : Double = -999.0
            var lastPureDHY : Double = -999.0
            val pureDHGlyphHtml = RitualTrackOverlay.tsPureDHMarkerIndices.map { idx =>
                val (baseX, _) = if (idx < pos.length) pos(idx) else pos(pos.length - 1)
                val dhY = pureDHFixedY
                // Check if last pure-DH glyph is in same spot — offset right if so
                val (finalX, finalY) =
                    if ((lastPureDHX - baseX).abs < 1.0 && (lastPureDHY - dhY).abs < 1.0)
                        (lastPureDHX + glyphWidth, dhY)
                    else
                        (baseX, dhY)
                lastPureDHX = finalX
                lastPureDHY = finalY
                s"""<img src="${imageSource("ts-glyph")}"
                    style="
                        position: absolute;
                        left: ${finalX}%;
                        top: ${finalY}%;
                        width: 6%;
                        height: auto;
                        transform: translate(-50%, -50%);
                        opacity: 0.85;
                        pointer-events: none;
                        filter: drop-shadow(0 0 0.15em rgba(0,0,0,0.9)) drop-shadow(0 0 0.3em rgba(100,255,100,0.6));" />"""
            }.mkString("")

            s"""<table style="background: rgba(0,0,0,0.85); width: 100%;">
              <tbody>
                <tr>
                  <td style="text-align: center; vertical-align: middle; padding: 1.5em 0.5em;">
                    <div style="position: relative; display: inline-block; max-width: 100%;">
                      <img src="${imgSrc}"
                           style="display: block; width: 100%; height: auto;" />
                      ${markerDiv}
                      ${glyphs}
                      ${pureDHGlyphHtml}
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>"""

        case s if s.headOption.exists(h => h == "XSS" || h == "TB") => xssTbOverlay(s)


        case _ =>
            // println("onExternalClick " + s.mkString(" | "))
            ""
    }).but("")

    def xssTbOverlay(s : $[Any]) : String = { println("xssTbOverlay called with: " + s.mkString(",")); s match {
        case $("XSS") =>
            faction(XSS, "info:xss-background", Precipitation, "Post-Battle",
            "You always Pain out of a Battle first, even if you are the Defender. Fully cancelled by Crawling Chaos Madness.",
            $(), $(
            (Acolyte,           6, "1", "0", s""""""),
            (AmphibianCrawler,  2, "1", "0", s"""<div class=p>Monster. 0 Combat.</div>"""),
            (Twister,           4, "2", "1-3", s"""<div class=p>Combat: 3 in Land Areas, 1 elsewhere.</div>"""),
            (EyeOfTheStorm,     3, "3", "1-4", s"""<div class=p>Combat: 4 in Sea Areas, 1 elsewhere.</div><div class=p>Spellbook: ${reference(XSS, Tsunami)}</div>"""),
            (Petrichor,         1, "8", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Petrichor.name}:")}</div>
                <div class=p>${cost("1)")} You must Control a Unit with Cost 3+ in the target Area.</div>
                <div class=p>${cost("2)")} Pay ${power(8)}. ${Petrichor.name} appears in the Area.</div>
                <div class=p>${combat} Equals the count of XSS Units in play with Cost 2 or greater (includes Petrichor).</div>
                <div class=p><span class=ability-color>Distant Thunderclap</span> ${cost("(Post-Battle, Optional):")} Assign your excess Pains to yourself. If no participating Units remain in the Battle Area and your opponent had a non-Cultist Unit present, gain an Elder Sign.</div>
                <div class=p>Spellbooks: ${reference(XSS, StaticAccumulator)}, ${reference(XSS, CloudOfAshes)}</div>""")
        ))
        case $("XSS", PetrichorBattlesAloneReq.text) => requirement("Petrichor is alone (no other XSS Units) in a Battle.")
        case $("XSS", FourGlyphFootprintReq.text) => requirement("Have Units in 4 Areas with Faction Glyphs (any in-game or off-board core faction's Start Area / Glyph).")
        case $("XSS", SeaGatesReq.text) => requirement("Control 3 Gates in Sea Areas<br/>OR<br/>Control 3 Gates in Areas with Faction Glyphs.")
        case $("XSS", LandGatesReq.text) => requirement("Control 3 Gates in Land Areas<br/>OR<br/>Control 3 Gates in Areas with Faction Glyphs.")
        case $("XSS", MonsterMassReq.text) => requirement("Have 10 Cost worth of Monsters in play (sum of Monster Costs on map).")
        case $("XSS", AwakenPetrichorReq.text) => requirement("Awaken Petrichor.")
        case $("XSS", Whirlwind.name) => spellbook(Whirlwind.name, "Post-Battle", "While in Land Areas, Twisters may Retreat to Sea Areas containing enemy Units.")
        case $("XSS", StaticAccumulator.name) => spellbook(StaticAccumulator.name, "Pre-Battle", "Move Units with total Cost up to 4 from one adjacent Area into the Battle Area.")
        case $("XSS", CloudOfAshes.name) => spellbook(CloudOfAshes.name, "Ongoing", "When one of your Monsters is Killed, you may place it on your Faction Card instead of returning it to Pool. In the Doom Phase, return one Monster from the Faction Card to a Controlled Gate; remaining Monsters return to Pool.")
        case $("XSS", Tsunami.name) => spellbook(Tsunami.name, "Action: Cost 1", "Move an Eye of the Storm from a Sea Area to an adjacent Land Area. Any or all of your other Units in the Sea Area may move with it.")
        case $("XSS", FrozenSolid.name) => spellbook(FrozenSolid.name, "Ongoing", "Your Acolytes controlling Gates in Areas with Faction Glyphs cannot be captured by Monsters. Cultist-source, GOO-ability, and Terror captures still apply.")
        case $("XSS", TorrentialDownpour.name) => spellbook(TorrentialDownpour.name, "Gather Power", "Gain 1 Power if you have Amphibian Crawlers in BOTH a Land Area AND a Sea Area. Gain +1 additional Power for each of those Amphibian Crawler Areas that contain an Enemy-Controlled Gate.")
        case $("TB") =>
            faction(TB, "info:tb-background", ThousandWrithingMaws, "Ongoing",
            "2 Power Action: may Recruit/Summon two upto 2 Cost Units of same type.<br/><br/>" +
            "<span class=ability-color>Behemoth</span> <span class=cost-color>(Ongoing):</span> Whenever TB Power reaches 0 (after Shudde M'ell has been Awakened and the Mantle is in play), place a Segment from Pool on the Mantle. Parts may be moved to the Mantle as a 0-Cost Unlimited Action.",
            $(), $(
            (Cadavolyte,        6, "2", "0", s"""<div class=p>Acolyte, but Gathers 0 Power</div>"""),
            (Tentacle,         10, "2", "0", s"""<div class=p>Cultist. CANNOT Build/Control Gates nor be Captured; Gathers 1 Power per Area containing Tentacle.</div>"""),
            (Chthonian,         5, "2", "1", s"""<div class=p>Monster</div>"""),
            (ShuddeMellHead,    1, "8", "?", s"""
                <div class=p>${cost("How to Awaken Shudde M'ell:")}</div>
                <div class=p>${cost("1)")} The Mantle must be in play.</div>
                <div class=p>${cost("2)")} Pay ${power(8)}. Shudde M'ell (Head) appears on the Mantle.</div>
                <div class=p>${combat} 3 per Part in play (Head + Segments). Up to 4 Parts = up to 12 Combat.</div>"""),
            (ShuddeMellSegment, 3, "0", "0", s"""<div class=p>GOO Part. Placed via Behemoth or Autotomy.</div>""")
        ))
        case $("TB", OverlayMantleReq.text) => requirement("Control Gates in 2 adjacent Areas, then overlay the Mantle.")
        case $("TB", TenTentaclesReq.text) => requirement("There are 10 Tentacles in play.")
        case $("TB", RemoveGatePlaceChthonianReq.text) => requirement("Gain 2 Power, remove a Gate you Control, place a Chthonian in any Area you occupy.")
        case $("TB", GatesAtGOOsReq.text) => requirement("Pay 8 Power, place Gates at every Area containing a Great Old One. Gain 1 Power per Gate placed.")
        case $("TB", AwakenShuddeMellReq.text) => requirement("Awaken Shudde M'ell.")
        case $("TB", ShuddeMellInThreeGlyphsReq.text) => requirement("Shudde M'ell Parts simultaneously in 3 specific Glyph Areas, OR Pay 6 Power as an Action.")
        case $("TB", Stalk.name) => spellbook(Stalk.name, "Ongoing", "Immediately after any faction's Move Action, you may relocate a single Cultist to a Moved Unit's Area.")
        case $("TB", Autotomy.name) => spellbook(Autotomy.name, "Post-Battle, Shudde M'ell", "In Unlimited Battles on your turn with Head present, apply 1 received Kill to any Segment in play. IF YOU DO: 1. Gain 1 Elder Sign per Segment in pool; and 2. Ignore Pains, but Retreat all your Units not Killed/Eliminated from Battle Area to 1 adjacent Area. TURN-END: place all Segments from pool on the Mantle.")
        case $("TB", Subterrane.name) => spellbook(Subterrane.name, "Ongoing", "For your Units only, any Tentacle's Area and the Mantle can be adjacent (eg. for Move, Pain).")
        case $("TB", Grasp.name) => spellbook(Grasp.name, "Battle", "If any Chthonians in Battle Area, add +1 Combat Dice per adjacent Area containing a Tentacle (Maximum of +4).")
        case $("TB", Ensnare.name) => spellbook(Ensnare.name, "Action: Cost 1", "Choose enemy in Area with Tentacle. Roll 1 D6 die. From that Area to Shudde M'ell's Head Area, the enemy must Relocate a number of their Units equal to the lesser of: the die roll; or their Power. On any Faction, you may use this only once per Action Phase.")
        case $("TB", PsychicShriek.name) => spellbook(PsychicShriek.name, "Action: Cost 1", "If Mantle in play, choose enemy (not Hibernating). Roll 2 D6 dice. They Retreat a number of their Units equal to the lesser of: the dice roll; or 2x their Power; to Areas that did not contain their Units just prior to the Shriek, nor contain your Gates. On any Faction, you may use this only once per Action Phase.")
        case _ => ""
    }}

    def combat = s"<span class=combat-color>Combat:</span>"

    def cost(s : String) = s"<span class=cost-color>${s}</span>"

    def power(n : Int) = cost(s"${n} Power")

    def loyaltyCard(name : String, quantity : Int, cost : Int, combat : Int, obtainText : String, ability : String, phase : String, abilityText : String) = s"""
        <table class="loyalty-card-table">
            <thead>
                <tr>
                    <th style=width:10%>
                    </th>
                    <th style=width:80%>
                    </th>
                    <th style=width:10%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="h2 abaddon nt">${name}<sup><span class="deh3 nt">(${quantity.toString})</span></sup></span></div>
                        <img class="img" src="${imageSource("info:" + "n-" + name.toLowerCase.replaceAll("\\s*\\([^)]*\\)", "").replace("'", "").replace(" ", "-").stripSuffix("-"))}">
                        <div>&nbsp;</div>
                        <div>
                            <span class="cost-color black-border">Cost: ${cost.toString}</span>
                        </div>
                        <div>
                            <span class="combat-color black-border">Combat: ${combat.toString}</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border">
                            <span class="nt">${obtainText}</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border">
                            <span class="ability-color">${ability} </span><span class="cost-color">(${phase})</span><span class="nt">: ${abilityText}</span>
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    // Round 8 Bug 40: added facedown parameter. When true, spellbook title and text
    // are shown with strikethrough (text-decoration: line-through) to indicate the
    // spellbook has been flipped facedown via Infernal Pact and its power is disabled.
    def loyaltyCardIGOO(name : String, cost : String, combat : String, hasSpellbook : Boolean, obtainText : String, ability : String, abilityPhase : String, abilityText : String, spellbookRequirement : String, spellbook : String, spellbookPhase : String, spellbookText : String, facedown : Boolean = false) = {
        val dimmedClass = if (hasSpellbook) "" else " dimmed"
        val strikeStyle = if (facedown) " style=\"text-decoration: line-through\"" else ""
        s"""<table class="loyalty-card-table">
            <thead>
                <tr>
                    <th style="width:10%"></th>
                    <th style="width:38%"></th>
                    <th style="width:4%"></th>
                    <th style="width:38%"></th>
                    <th style="width:10%"></th>
                </tr>
            </thead>
            <tbody>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="h2 abaddon nt">${name}</span></div>
                        <img class="img" src="${imageSource("info:" + "n-" + name.toLowerCase.replaceAll("\\s*\\([^)]*\\)", "").replace("'", "").replace(" ", "-").stripSuffix("-"))}">
                        <div>&nbsp;</div>
                        <div>
                            <span class="cost-color black-border">Cost: ${cost}</span>
                        </div>
                        <div>
                            <span class="combat-color black-border">Combat: ${combat}</span>
                        </div>
                    </td>
                    <td></td>
                    <td>
                        <div class="black-border">
                            <span class="nt">${obtainText}</span>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr>
                    <td></td>
                    <td>
                        <div class="black-border">
                            <span class="ability-color">${ability}</span>
                            <span class="cost-color"> (${abilityPhase})</span>
                            <span class="nt">: ${abilityText}</span>
                        </div>
                    </td>
                    <td></td>
                    <td>
                        <div class="black-border">
                            <span class="nt">${spellbookRequirement}:</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border"${strikeStyle}>
                            <span class="ability-color$dimmedClass">${spellbook}</span>
                            <span class="cost-color$dimmedClass"> (${spellbookPhase})</span>
                            <span class="nt$dimmedClass">: ${spellbookText}</span>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr><td colspan="4">&nbsp;</td></tr>
            </tbody>
        </table>"""
    }

    def libraryHintCard() = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:10%></th>
                    <th style=width:80%></th>
                    <th style=width:10%></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border"><span class="lb inline-block">Library at Celaeno</span></div>
                        <div class="white-border">
                            <div class="p black-border"><span class="ability-color">First Doom Phase Only</span> <span class="nt">&mdash; Gates appear at Library Tome Areas, if not already present</span></div>
                            <div class="p black-border"><span class="ability-color">All Doom Phases</span> <span class="nt">&mdash; Each player receives a silence token</span></div>
                            <div class="p black-border"><span class="ability-color">All Gather Power Phases</span> <span class="nt">&mdash; Each player loses any remaining silence token</span></div>
                        </div>
                        <div>&nbsp;</div>
                        <div class="h1 black-border"><span class="lb inline-block">Stairwells</span></div>
                        <div class="white-border">
                            <div class="p nt black-border">Certain Areas contain Stairwells; these are lettered A&ndash;F on each floor. These Areas are adjacent to their matching Areas on the other level. For instance, in the 3-Player side of the Lower Floor, the Chamber of Apkallu contains Stairwell D. It is therefore adjacent to the Area of Barrier of Naach-Tith, which is also marked with a D but is on the Upper Floor (for both 3- and 5-player sides).</div>
                            <div class="p nt black-border">A Pained Unit in a Stairwell Area can go through the Stairwell to the other Floor, because these Areas are adjacent for all purposes.</div>
                        </div>
                        <div>&nbsp;</div>
                        <div class="h1 black-border"><span class="lb inline-block">Archways</span></div>
                        <div class="white-border">
                            <div class="p nt black-border">Several Areas contain Archways. These are similar to Stairwells, except that they are not lettered. For purposes of the Move Action and movement-type abilities, each Archway Area is adjacent to EVERY OTHER Archway Area on both Floors of the Library.</div>
                            <div class="p nt black-border">However, unlike the Stairwells, the Archways are NOT adjacent when being Pained or Retreated. Players using other movement-type abilities to travel may do so freely between the two boards. For instance, Shub-Niggurath can Avatar to either side, Crawling Chaos Hunting Horrors can use Seek and Destroy to access either board, Cthulhu can use Submerge to hit both boards, etc.</div>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>
            </tbody>
        </table>"""

    def mapUnitOverlay(name : String, rulesText : String, dieText : String) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:15%></th>
                    <th style=width:70%></th>
                    <th style=width:15%></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border"><span class="lb inline-block">${name}</span></div>
                        <div class="black-border"><span class="cost-color">Action Cost: 0</span></div>
                        <div>
                            ${rulesText}
                            <div class="p black-border"><span class="cost-color">${dieText}</span></div>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>
            </tbody>
        </table>"""

    def tomeOverlay(name : String, phase : String, text : String, faceUp : Boolean = true) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:15%></th>
                    <th style=width:70%></th>
                    <th style=width:15%></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border"><span class="lb inline-block">${name}</span> <span class="cost-color inline-block">${if (phase.nonEmpty) "(" + phase + ")" else ""}</span></div>
                        <div class="white-border">
                            <div class="nt black-border" ${if (!faceUp) "style='opacity:0.4'" else ""}>${text}</div>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>
            </tbody>
        </table>"""

    def spellbookRows(name : String, phase : String, text : String) = s"""
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="ability-color inline-block">${name}</span> <span class="cost-color inline-block">(${phase})</span></div>
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>"""

    def multiSpellbook(prefix : String, entries : (String, String, String)*) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:20%></th>
                    <th style=width:60%></th>
                    <th style=width:20%></th>
                </tr>
            </thead>
            <tbody>
                ${prefix}
                ${entries./{ case (n, p, t) => spellbookRows(n, p, t) }.mkString("")}
            </tbody>
        </table>"""

    def spellbook(name : String, phase : String, text : String) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:20%>
                    </th>
                    <th style=width:60%>
                    </th>
                    <th style=width:20%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="ability-color inline-block">${name}</span> <span class="cost-color inline-block">(${phase})</span></div>
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    def requirement(text : String) = s"""
        <table class="requirement-table" style="">
            <tbody>
                <tr>
                    <td style="width:20%"></td>
                    <td style="width:60%; vertical-align:middle; text-align:center;">
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td style="width:20%"></td>
                </tr>
            </tbody>
        </table>"""

    def ref(spellbook : Spellbook) = s"""<span class=ability-color>${spellbook.name}</span>"""

    def reference(f : Faction, spellbook : Spellbook) = s"""<span class="ability-color pointer" onclick="onExternalClick('${f.short}', '${spellbook.name}')">${spellbook.name}</span>"""

    def anFactionOverlay(altSB : Boolean) = {
        // Dematerialization is AN's Unique Ability (Ongoing) — same in BOTH the standard
        // and alternate-spellbook variants. UnholyGround/Consecration/WorshipServices
        // (standard) and HolyGround/Sanguinessence/Crusade (alt-variant) are SPELLBOOKS,
        // not the Special Ability, and are referenced in the unit descriptions below.
        val abilityPhase = "Doom Phase"
        val abilityText  = "Relocate any or all of your own Units from one Area to a single other Area, anywhere on the Map."

        // The Cathedral description changes based on which spellbook variant is active
        val cathedralSBs = if (altSB) {
            s"""${reference(AN, HolyGround)}, ${reference(AN, Sanguinessence)}, ${reference(AN, Crusade)}"""
        } else {
            s"""${reference(AN, WorshipServices)}, ${reference(AN, Consecration)}, ${reference(AN, UnholyGround)}"""
        }

        faction(AN, "info:an-background", Dematerialization, abilityPhase, abilityText,
            $, $(
            (Acolyte,    6,   "1",   "0", s""""""),
            (UnMan,      3, "3/0",   "0", s"""<div class=p><span class=cost-color>Cost:</span> 0 with ${reference(AN, Festival)}</div>"""),
            (Reanimated, 3, "4/1",   "2", s"""<div class=p><span class=cost-color>Cost:</span> 1 with ${reference(AN, Brainless)}</div>"""),
            (Yothan,     3, "6/3",   "7", s"""<div class=p><span class=cost-color>Cost:</span> 3 with ${reference(AN, Extinction)}</div>"""),
            (Cathedral,  4, "3/1", "n/a", s"""
                <div class=p>You may use the Create Gate Action to Create Cathedrals instead of Gates.</div>
                <div class=p>${cost("Cost:")} 1 if built not adjacent to another Cathedral</div>
                <div class=p>Spellbooks: ${cathedralSBs}</div>
                <div class=p>${cost("Special:")} If all 4 Cathedrals are in play, you may Awaken an Independent Great Old One without your own Great Old One (when Awakening Cthugha this way, just pay 6 Power).</div>"""
            )
        ))
    }

    def slFactionOverlay(easierSBR : Boolean, energyNexusPreBattle : Boolean) = {
        def slRef(sb : Spellbook) = {
            val extra = sb match {
                case EnergyNexus if energyNexusPreBattle => ", true"
                case _ => ""
            }
            s"""<span class="ability-color pointer" onclick="onExternalClick('SL', '${sb.name}'$extra)">${sb.name}</span>"""
        }
        faction(SL, "info:sl-background", DeathFromBelow, "Doom Phase", "Place your lowest-cost Monster from your Pool into any Area containing at least 1 of your Units.",
            $(Burrow, CursedSlumber), $(
            (Acolyte,       6, "1", "0", ""),
            (Wizard,        2, "1", "0", s"""<div class=p>Spellbook: ${slRef(EnergyNexus)}</div>"""),
            (SerpentMan,    3, "2", "1", s"""<div class=p>Spellbook: ${slRef(AncientSorcery)}</div>"""),
            (FormlessSpawn, 4, "3", "?", s"""<div class=p>${combat} Equals count of Formless Spawns on the map, +1 if Tsathoggua is also on the map.</div>"""),
            (Tsathoggua,    1, "8", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Tsathoggua.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Formless Spawn on the map.</div>
                <div class=p>${cost("2)")} Pay ${power(8)}. Place Tsathoggua in the Area with the Formless Spawn.</div>
                <div class=p>${combat} Equals the opponent's current Power or 2, whichever is greater.</div>
                <div class=p>${ref(Lethargy)} ${cost("(Action: Cost 0):")} If Tsathoggua is in play, do nothing. This counts as an Action.</div>
                <div class=p>Spellbooks: ${slRef(DemandSacrifice)}, ${slRef(CaptureMonster)}</div>"""
            ),
        ))
    }

    def dsFactionOverlay(altSB : Boolean) = {
        val thesisSB = if (altSB) reference(DS, DirectedEnergy) else reference(DS, UndirectedEnergy)
        val antithesisSB = if (altSB) reference(DS, FiendishSpawn) else reference(DS, FiendishGrowth)
        faction(DS, "info:ds-background", Psychosis, "Ongoing", "Psychosis (Action: Cost 0): You must have an Acolyte in your pool. Select an area that has no units from any faction. Place an acolyte from your pool there. During each Doom phase, flip ALL your face-down faction spellbooks face-up again.",
            $(Consummation), $(
            (Acolyte,       6, "1", "0", ""),
            (LarvaThesis,   3, "1", "0", s"<div class=p>${combat} 2 if Avatar Thesis is in play.</div>"),
            (LarvaAntithesis, 3, "1", "0", s"<div class=p>${combat} 2 if Avatar Antithesis is in play.</div>"),
            (LarvaSynthesis, 3, "1", "0", s"<div class=p>${combat} 2 if Avatar Synthesis is in play.</div>"),
            (ChaosGate, 1, "n/a", "n/a", s"<div class=p>Spellbook: ${reference(DS, ChaosGateSB)}</div>"),
            (AvatarThesis, 1, "?", "?", s"""
                <div class=p>${cost("How to Awaken Avatar Thesis:")}</div>
                <div class=p>${cost("1)")} Set the Azathoth marker on the Doom track on any desired spot from 0 to 8.</div>
                <div class=p>${cost("2)")} Pay Power equal to the Azathoth marker's setting.</div>
                <div class=p>${cost("3)")} Each other player gains 1 Power OR choose an enemy player to gain 2 Power.</div>
                <div class=p>${combat} Equals the Azathoth marker's position.</div>
                <div class=p>Spellbooks: ${thesisSB}</div>
            """),
            (AvatarAntithesis, 1, "?", "?", s"""
                <div class=p>${cost("How to Awaken Avatar Antithesis:")}</div>
                <div class=p>${cost("1)")} Avatar Thesis must have been Awakened (it need not be in play).</div>
                <div class=p>${cost("2)")} Pay Power equal to 8 minus the Azathoth marker's setting.</div>
                <div class=p>${cost("3)")} Each other player gains 1 Doom OR choose an enemy player who gains an Elder Sign.</div>
                <div class=p>${combat} Equals 8 minus the Azathoth marker's position.</div>
                <div class=p>Spellbooks: ${antithesisSB}</div>
            """),
            (AvatarSynthesis, 1, "8", "?", s"""
                <div class=p>${cost(s"How to Awaken ${AvatarSynthesis.name}:")}</div>
                <div class=p>${cost("1)")} Both ${AvatarThesis.name} and ${AvatarAntithesis.name} must have been Awakened (they need not be in play).</div>
                <div class=p>${cost("2)")} Pay ${power(8)}.</div>
                <div class=p>${cost("3)")} Roll the Azathoth die. All enemies have 2 minutes to decide how to collectively lose that much Power and/or Doom, if they cannot agree then you win the game. In a 2-3 player game halve the die roll result (rounded up).</div>
                <div class=p>${combat} First roll the Azathoth die, then roll that many combat dice.</div>
                <div class=p>${ref(CosmicRuler)} ${cost("(Post-Battle):")} When any Avatar is choosen to recieve a Kill or Elimination, instead you can Eliminate another Avatar in its stead, from anywhere on the map.</div>
            """),
        ))
    }

    def bbFactionOverlay(altSB : Boolean) = {
        // Lunacy is BB's Unique Ability (Ongoing) — same in BOTH the standard
        // and alternate-spellbook variants. Catabolism (standard) and Syzygy
        // (alt-variant) are SPELLBOOKS, not the Special Ability, and belong in
        // the Spellbooks list — not at the top of the faction card.
        val lunacyPhase = "Ongoing"
        val lunacyText  = "Spellbooks and abilities that affect Cultists can target Earth Cats as if they are Acolytes. They cannot be captured as Cultists nor can they create or control Gates. This ability is not optional."
        // The variant-dependent spellbook (Catabolism or Syzygy) is rendered in
        // the Spellbooks line below, alongside the rest of BB's library that is
        // already shown via the per-unit Spellbook references.
        val variantSpellbook = if (altSB) Syzygy else Catabolism
        faction(BB, "info:bb-background", Lunacy, lunacyPhase, lunacyText,
            $(variantSpellbook), $(
            (EarthCat,      6, "1",  "0",  s"""<div class=p>Special: Generates 1 Power during the Gather Power Phase</div><div class=p>Spellbook: ${reference(BB, variantSpellbook)}</div>"""),
            (CatFromMars,   2, "2",  "1",  s"""<div class=p>Special: Generates 1 Power during the Gather Power Phase</div><div class=p>Spellbook: ${reference(BB, Zagazig)}</div>"""),
            (CatFromSaturn, 2, "3",  "2",  s"""<div class=p>Special: Generates 1 Power during the Gather Power Phase</div><div class=p>Spellbook: ${reference(BB, Savagery)}</div>"""),
            (CatFromUranus, 2, "4",  "3",  s"""<div class=p>Special: Generates 1 Power during the Gather Power Phase</div><div class=p>Spellbook: ${reference(BB, Predator)}</div>"""),
            (Bastet,        1, "6",  "1 Kill",  s"""
                <div class=p>${cost(s"How to Awaken ${Bastet.name}:")}</div>
                <div class=p>${cost("1)")} All your Cat varieties are in play.</div>
                <div class=p>${cost("2)")} Pay ${power(6)}.</div>
                <div class=p>${cost("3)")} Place ${Bastet.name} in an Area containing no enemy Units.</div>
                <div class=p>${combat} Add 1 Kill to your combat total (Bastet rolls no dice); the enemy must lower their Kill total by 1.</div>
                <div class=p>${reference(BB, RequiresAttention)} ${cost("(Doom Phase):")} During the Doom Phase, if Bastet is in an Area containing an enemy Cultist, you may perform a Ritual of Annihilation. For you, this adds exactly 4 Doom plus: if Bastet’s Area has an Enemy-Controlled Gate, gain <span class=es>1 Elder Sign</span>; if Bastet’s Area has an Enemy Great Old One, gain <span class=es>2 Elder Sign</span>. These rewards are additive.</div>""")
        ))
    }

    // Faceless Blight (FBE) — overlay dispatch helper (extracted from the giant
    // `info` match so that method stays under the JVM 64KB limit). Handles the
    // faction info card, the 6 spellbook-requirement panels, and the 7 spellbook /
    // ability info cards (verbatim from card art, §1.9 / §1.10).
    def fbeOverlay(s : $[Any]) : String = s match {
        case $("FBE") => fbeFactionOverlay()

        // Task 3.11.9 / §4.0.7 — Faction-Card dice-pool detail overlay (custom,
        // Cursed-Tomes dynamic pattern). Opened by clicking the HUD dice strip
        // (Task 3.11.1). Renders each card die by face icon (Kill crimson / Pain
        // orange / Miss dim grey) plus the live Byagoona-combat subtotal = count of
        // Kill+Pain faces (pip >= 4). State is populated from displayGame each tick
        // into FBEFactionCardOverlay (CthulhuWarsSolo.scala). Shows "0" once any
        // dice source is acquired; an empty list still renders the panel with a
        // "(no dice)" note (the strip itself hides when empty, per §4.0.7).
        case $("FBE", "FactionCard") => fbeFactionCardOverlay()

        // Task 3.11.6 / §4.0.4 — Byagoona awaken-selector "?" detail overlay.
        // Opened from the clickable "?" on Byagoona's faction-card GOO slot while he
        // is OFF the map and FBE controls >= 1 Monster. Explains the self-sacrifice
        // Awaken procedure and the live Power-cost preview "max(0, 10 - SCost)". The
        // actual Awaken is performed through FBE's MainAction menu
        // (ByagoonaAwakenMainAction); this is the informational overlay only.
        case $("FBE", "ByagoonaAwaken") => fbeByagoonaAwakenOverlay()

        case $("FBE", ChangelingAdherentsReq.text) => requirement("A total of 3 Kills are Rolled in a Battle you Participate in.")
        case $("FBE", NecromanticSporesReq.text)   =>
            val locationInfo = currentGame match {
                case Some(g) =>
                    implicit val gg : Game = g
                    val thralls = FBE.onMap(FungalThrall)
                    if (thralls.any)
                        "<br/><br/><span class=cost-color>Fungal Thralls on map (" + thralls.num + "):</span> " +
                        thralls./(t => t.region.toString).distinct./(r => r + " (" + thralls.count(_.region.toString == r) + ")").mkString(", ")
                    else
                        "<br/><br/><span class=cost-color>No Fungal Thralls on map.</span>"
                case None => ""
            }
            requirement("As an Action, Eliminate Two Fungal Thralls." + locationInfo)
        case $("FBE", ShapestealingReq.text)        => requirement("Have 3 Units in an Enemy Start Area.")
        case $("FBE", AnimatedRushReq.text)         => requirement("Have 3 Dice on your Faction Card.")
        case $("FBE", SuccorReq.text)               => requirement("Byagoona Dies in Battle. Do not fulfill if the Kill/Elimination is prevented.")
        case $("FBE", OverlordOfDeathReq.text)      => requirement("Awaken Byagoona.")

        case $("FBE", SelfConsuming.name)        => spellbook(SelfConsuming.name,        "Ongoing (Faction Ability)", "Whenever two or more Units are Killed or Eliminated as part of the same Action, Gain 1 Power. If you controlled at least three of them, also gain 1 Doom.")
        case $("FBE", ChangelingAdherents.name)  => spellbook(ChangelingAdherents.name,  "Gather Power", "Roll a die for each of your Acolytes controlling Gates and place them on your Faction Card.")
        case $("FBE", NecromanticSpores.name)    => spellbook(NecromanticSpores.name,    "Post-Battle", "Eliminate a Monster Present to create a Fungal Thrall for each enemy Unit Killed.")
        case $("FBE", Shapestealing.name)        => spellbook(Shapestealing.name,        "Pre-Battle", "Choose an Enemy Monster and roll a die from your Faction Card. If the value exceeds that Monster's Cost, it fights for you this Combat.")
        case $("FBE", AnimatedRush.name)         => spellbook(AnimatedRush.name,         "Move", "When Byagoona Moves, discard dice from your Faction Card to move twice that many Units for free.")
        case $("FBE", Succor.name)               => spellbook(Succor.name,               "Doom Phase", "Eliminate any number of your Units and roll that many dice. If the total value exceeds that of the Ritual Marker, gain an Elder Sign.")
        case $("FBE", OverlordOfDeath.name)      => spellbook(OverlordOfDeath.name,      "Ongoing", "If Byagoona is in play, Eliminate Monsters to pay Costs as though they were Power. Each Monster Eliminated is worth 1 Power.")

        case _ => ""
    }

    // Faceless Blight (FBE) — Faction-Card dice-pool detail overlay (Task 3.11.9 / §4.0.7).
    // Renders the dice currently on the FBE Faction Card (FBEFactionCardOverlay.dice — pip
    // values 1-6) as a row of face icons and reports Byagoona's live combat-dice subtotal
    // (count of Kill+Pain faces, pip >= 4). Faces per FBEExpansion.face: 6 = Kill (crimson),
    // 4-5 = Pain (orange), 1-3 = Miss (dim grey). Styled per the §4.0 KEY on the shared
    // spellbook parchment background.
    def fbeFactionCardOverlay() = {
        val dice = FBEFactionCardOverlay.dice
        // Per-die face cell: large pip value coloured by face class, with a face label.
        val cells = dice.sortBy(x => x)./{ pip =>
            val (cls, label) =
                if (pip >= 6) ("str", "Kill")
                else if (pip >= 4) ("pain", "Pain")
                else ("halfhigh", "Miss")   // Miss = dim grey per §4.0.7 / §4 KEY
            s"""<td style="padding:6px 14px;text-align:center;vertical-align:middle;">
                    <div class="$cls" style="font-size:200%;font-weight:bold;line-height:1;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">${pip}</div>
                    <div class="$cls" style="font-size:80%;">${label}</div>
                </td>"""
        }.mkString("")
        val combat = dice.count(_ >= 4)
        val diceRow =
            if (dice.any) s"""<tr>$cells</tr>"""
            else s"""<tr><td style="padding:8px 14px;color:#bbbbbb;font-style:italic;">(no dice on Faction Card)</td></tr>"""
        val header =
            s"""<tr><td colspan="${math.max(1, dice.num)}" style="padding:6px 14px;border-bottom:1px solid grey;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">""" +
            s"""<span class="ability-color">Faceless Blight</span> <span class="cost-color">Faction Card</span></td></tr>"""
        val footer =
            s"""<tr><td colspan="${math.max(1, dice.num)}" style="padding:8px 14px;border-top:1px solid grey;font-size:90%;text-shadow:1px 1px 2px rgba(0,0,0,0.85);">""" +
            s"""<span class="ability-color">${Byagoona.name}</span> combat = <span class="str">${combat}</span> """ +
            s"""<span class="nt">(Kill + Pain faces)</span></td></tr>"""
        s"""<table class="spellbook-table"><tbody>$header$diceRow$footer</tbody></table>"""
    }

    // Faceless Blight (FBE) — Byagoona awaken-selector "?" detail overlay (Task 3.11.6 / §4.0.4).
    // Informational panel for the clickable "?" on Byagoona's GOO card slot: it explains the
    // self-sacrifice Awaken procedure (§1.8) and shows the live Power-cost preview. The actual
    // Awaken is offered through FBE's MainAction menu (ByagoonaAwakenMainAction).
    def fbeByagoonaAwakenOverlay() = {
        spellbook("Awaken " + Byagoona.name, "Action",
            s"""<div class=p>${cost("How to Awaken " + Byagoona.name + ":")}</div>
                <div class=p>${cost("1)")} Eliminate one or more of your Monsters in a single Area and roll that many dice.</div>
                <div class=p>${cost("2)")} Set all of the dice on your Faction Card.</div>
                <div class=p>${cost("3)")} Pay Power equal to <span class=cost-color>max(0, 10 - the total Cost of Monsters Eliminated)</span>.</div>
                <div class=p>${cost("4)")} ${Byagoona.name} appears in the Area.</div>
                <div class=p>${combat} Equal to the number of Results (Kills and Pains) on dice on your Faction Card.</div>""")
    }

    // Faceless Blight (FBE) — Homebrew faction info card (§G14). Unique ability:
    // Self Consuming (Ongoing). Byagoona awakens via a Monster-sacrifice procedure;
    // his Combat = the count of Kill/Pain faces on the Faction-Card dice pool.
    def fbeFactionOverlay() = {
        def fbeRef(sb : Spellbook) =
            s"""<span class="ability-color pointer" onclick="onExternalClick('FBE', '${sb.name}')">${sb.name}</span>"""
        faction(FBE, "info:fbe-background", SelfConsuming, "Ongoing",
            "Whenever two or more Units are Killed or Eliminated as part of the same Action, Gain 1 Power. If you controlled at least three of them, also gain 1 Doom.",
            $(ChangelingAdherents, NecromanticSpores, Shapestealing, AnimatedRush, Succor, OverlordOfDeath), $(
            (Acolyte,      6, "1", "0", s"""<div class=p>Setup: 6 Acolytes + a Controlled Gate in an empty area not adjacent to another faction's start area. Spellbook: ${fbeRef(ChangelingAdherents)}</div>"""),
            (FungalThrall, 10, "2", "2", s"""<div class=p>Spellbooks: ${fbeRef(NecromanticSpores)}, ${fbeRef(Succor)}</div>"""),
            (Byagoona,     1, "?", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Byagoona.name}:")}</div>
                <div class=p>${cost("1)")} Eliminate one or more of your Monsters in a single area and roll that many dice.</div>
                <div class=p>${cost("2)")} Set all of the dice on your Faction Card.</div>
                <div class=p>${cost("3)")} Pay Power equal to the difference between the total Cost of Monsters Eliminated and 10 (floored at 0).</div>
                <div class=p>${cost("4)")} Byagoona appears in the Area.</div>
                <div class=p>${combat} Equal to the number of Results (Kills and Pains) on dice on your Faction Card.</div>
                <div class=p>${ref(Shapestealing)} ${cost("(Pre-Battle):")} Choose an Enemy Monster and roll a die from your Faction Card. If the value exceeds that Monster's Cost, it fights for you this Combat.</div>
                <div class=p>${ref(AnimatedRush)} ${cost("(Move):")} When Byagoona Moves, discard dice from your Faction Card to move twice that many Units for free.</div>
                <div class=p>Distributed Death ${cost("(Post-Battle):")} Prevent any number of Kills assigned to your Units by discarding that many Dice from your Faction Card.</div>
                <div class=p>Spellbooks: ${fbeRef(OverlordOfDeath)}</div>""")
        ))
    }

    def owFactionOverlay(cheapMutants : Boolean, yogCurseDie : Boolean) = {
        val mutantQty = 4
        val mutantCost = if (cheapMutants) "1" else "2"
        def owRef(sb : Spellbook) = {
            val extra = sb match {
                case TheyBreakThrough => cheapMutants.?(", true").|("")
                case DreadCurse => yogCurseDie.?(", true").|("")
                case _ => ""
            }
            s"""<span class="ability-color pointer" onclick="onExternalClick('OW', '${sb.name}'$extra)">${sb.name}</span>"""
        }
        faction(OW, "info:ow-background", BeyondOne, "Action: Cost 1", "Select one of your your Units with a Cost of 3+ in an Area that contains a Gate and no enemy Great Old Ones. Move that Unit, the Gate, and any Controlling Unit to any Area on the map that does not already have a Gate.",
            $(TheyBreakThrough, ChannelPower, DragonAscending, DragonDescending), $(
            (Acolyte,     6, "1", "0", s"""<div class=p>Spellbook: ${owRef(MillionFavoredOnes)}</div>"""),
            (Mutant,      mutantQty, mutantCost, "1", s"""<div class=p>Spellbook: ${owRef(MillionFavoredOnes)}</div>"""),
            (Abomination, 3, "3", "2", s"""<div class=p>Spellbooks: ${owRef(MillionFavoredOnes)}, ${owRef(DreadCurse)}</div>"""),
            (SpawnOW,     2, "4", "3", s"""<div class=p>Spellbooks: ${owRef(MillionFavoredOnes)}, ${owRef(DreadCurse)}</div>"""),
            (YogSothoth,  1, "6", "?", s"""
                <div class=p>${cost(s"How to Awaken ${YogSothoth.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Spawn of Yog-Sothoth on the Map.</div>
                <div class=p>${cost("2)")} Pay ${power(6)}. Replace the Spawn with Yog-Sothoth.</div>
                <div class=p>${combat} Equal to twice the number of enemy-Controlled Faction Great Old Ones in play.</div>
                <div class=p>${ref(KeyAndGate)} ${cost("(Ongoing):")} Yog-Sothoth counts as a Gate for every purpose, except for The Beyond One ability. Yog-Sothoth is not Controlled by any Cultist, and can exist in the same Area as another Gate.</div>"""
            ),
        ))
    }

    def faction(f : Faction, background : String, unique : Spellbook, uniquePhase : String, uniqueText : String, miscSpellbooks : $[Spellbook], units : $[(UnitClass, Int, String, String, String)], footer : String = "") = {
        val backgroundUrl = if (background.startsWith("data:")) background else imageSource(background)
        s"""
        <table class="faction-table" style="background-image:url(${backgroundUrl})">
            <thead>
                <tr>
                    <th style=width:9%>
                    </th>
                    <th style=width:13%>
                    </th>
                    <th style=width:4%>
                    </th>
                    <th style=width:7%>
                    </th>
                    <th style=width:7%>
                    </th>
                    <th style=width:60%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td colspan=6>
                        <div style="padding-left: 3ex; padding-right: 3ex; padding-bottom: 1ex;">
                            <div class="h1 abaddon">${f.name}</div>
                            <div class="border-outer">
                                <div class="border-inner">
                                    <span class=ability-color>${unique.name}</span>
                                    <span class=cost-color>(${uniquePhase}):</span>
                                    <span>${uniqueText}</span>
                                </div>
                            </div>
                        </div>
                    </td>
                </tr>
                ${
                    if (miscSpellbooks.any) { s"""
                        <tr>
                            <td colspan=6>
                                <div style="padding-left: 3ex; padding-right: 3ex; padding-bottom: 1ex;">
                                    Spellbooks:
                                    ${
                                        miscSpellbooks./{ sb =>
                                            s"""${reference(f, sb)}"""
                                        }.join(", ")
                                    }
                                </div>
                            </td>
                        </tr>"""
                    }
                    else
                        ""
                }
                <tr>
                    <td colspan=2>
                        <div class=h3>Unit<sup><span class="deh3">(Total)</span></sup></div>
                    </td>
                    <td>
                    </td>
                    <td>
                        <div class=h3><span class=cost-color>Cost</span></div>
                    </td>
                    <td>
                        <div class=h3><span class=combat-color>Combat</span></div>
                    </td>
                    <td>
                        <div class=h3>Notes</div>
                    </td>
                </tr>
                <tr>
                    <td colspan=6>
                        <div class="separator">
                        </div>
                    </td>
                </tr>
                ${
                    units./{ case (uc, n, c, b, t) => s"""
                        <tr>
                            <td>
                                <img class="img" src=${imageSource("info:" + f.short.toLowerCase + "-" + uc.name.toLowerCase.replace(" ", "-").replace("'", "").replace("(", "").replace(")", ""))}>
                            </td>
                            <td>
                                <div class="unit-desc">
                                    <div class="p"><span class=unit-name>${uc.name}${(n > 1).??(s"""<sup>(${n})</sup>""")}</div>
                                    <div class="p"><span class=unit-type>${uc.utype.name.replace("GOO", "Great Old One")}</div>
                                </div>
                            </td>
                            <td>
                            </td>
                            <td>
                                <span class=cost-color>${c}</span>
                            </td>
                            <td>
                                <span class=combat-color>${b}</span>
                            </td>
                            <td>
                                <div class="notes">
                                    ${t}
                                </div>
                            </td>
                        </tr>"""
                    }.join("""
                        <tr>
                            <td colspan=6>
                                <div class="separator">
                                </div>
                            </td>
                        </tr>""")
                }
                ${
                    if (footer.any) { s"""
                        <tr>
                            <td colspan=6>
                                <div class="separator">
                                </div>
                            </td>
                        </tr>
                        <tr>
                            <td colspan=6>
                                <div style="padding-left: 3ex; padding-right: 3ex; padding-top: 1ex; padding-bottom: 1ex;">
                                    <div class="p">${footer}</div>
                                </div>
                            </td>
                        </tr>"""
                    }
                    else
                        ""
                }
            </tbody>
        </table>"""
    }

}
