package cws

import hrf.colmat._
import hrf.reflect._

import fastparse._, NoWhitespace._

import scalajs.reflect._


class Serialize(val game : Game) {
    import Serialize._

    def write(o : Any) : String = o match {
        case b : Boolean => b.toString
        case n : Int => n.toString
        case s : String if s.contains("\"") => "\"\"\"" + s + "\"\"\""
        case s : String => "\"" + s + "\""
        case r : Region => r.id
        case f : Faction => f.short
        case sb : Spellbook => className(sb)
        case uc : UnitClass => className(uc)
        case ur : UnitRef => write(ur.faction) + "/" + write(ur.uclass) + "/" + write(ur.index)
        case br : BattleRoll => className(br)
        case bf : BattlePhase => className(bf)
        case o : Offer => write(o.f) + "->" + write(o.n)
        case o : AzathothOffer => "AzathothOffer(" + o.productIterator.$./(write).mkString(", ") + ")"
        case a : Action => className(a) + a.productIterator.$.some./(_./(write).mkString("(", ", ", ")")).|("")
        case es : ElderSign => "$" + es.value

        case Some(x) => "Some(" + write(x) + ")"
        case None => "None"

        case ss : List[_] => ss./(write).mkString("[", ", ", "]")

        // Firstborn (FB) helper case classes (used as fields in FB actions and state).
        // Follow the AzathothOffer pattern: function-call form with productIterator.
        // Parser reconstructs via the reflection-based EApply catch-all.
        case x : FBEyeOpensTarget => "FBEyeOpensTarget(" + x.productIterator.$./(write).mkString(", ") + ")"
        case x : FBCyclopeanGazeSource => "FBCyclopeanGazeSource(" + x.productIterator.$./(write).mkString(", ") + ")"
        case x : FBWritheKillEntry => "FBWritheKillEntry(" + x.productIterator.$./(write).mkString(", ") + ")"
        case x : FBWrithePainEntry => "FBWrithePainEntry(" + x.productIterator.$./(write).mkString(", ") + ")"

        case x => x.getClass.getSimpleName.stripSuffix("$")
    }

    trait Expr
    case class ESymbol(value : String) extends Expr
    case class EInt(value : Int) extends Expr
    case class EDouble(value : Double) extends Expr
    case class EBool(value : Boolean) extends Expr
    case class EString(value : String) extends Expr
    case object ENone extends Expr
    case class ESome(value : Expr) extends Expr
    case class EList(values : $[Expr]) extends Expr
    case class EApply(f : String, params : $[Expr]) extends Expr

    case class EElderSign(value : Int) extends Expr
    case class EOffer(a : String, b : Int) extends Expr
    case class EUnitRef(a : String, b : String, c : Int) extends Expr

    def space[* : P] = P{ CharsWhileIn(" \r\n", 0) }

    // Region names can have hyphens (Naach-Tith) and apostrophes (Sn'gac).
    // Hyphen must not be followed by > so it doesn't gobble the offer arrow ->.
    def symbol[* : P] = P{ (CharIn("A-Z") ~ (CharsWhileIn("A-Za-z0-9'") | ("-" ~ !">")).rep).! }.map(ESymbol)

    def number[* : P] = P{ ("-".? ~ CharsWhileIn("0-9")).! }.map(_.toInt).map(EInt)

    def fractional[* : P] = P{ ("-".? ~ CharsWhileIn("0-9") ~ "." ~ CharsWhileIn("0-9")).! }.map(_.toDouble).map(EDouble)

    def stringQQQ[* : P] = P{ (("\"\"\"" ~/ ((AnyChar ~ !("\"\"\"")).rep ~ AnyChar).! ~ "\"\"\"")) }.map(EString)

    def string[* : P] = P{ "\"" ~/ CharsWhile(c => c != '\"' && c != '\\', 0).! ~ "\"" }.map(EString)

    def pfalse[* : P] = P{ "false" }.map(_ => EBool(false))

    def ptrue[* : P] = P{ "true" }.map(_ => EBool(true))

    def list[* : P] = P{ "[" ~/ params ~ "]" }.map(EList)

    def some[* : P] = P{ "Some" ~ space ~ "(" ~/ expr ~ ")" }.map(o => ESome(o))

    def none[* : P] = P{ "None" }.map(o => ENone)

    def base[* : P] : P[Expr] = P{ some | none | action | symbol | fractional | number | pfalse | ptrue | list | stringQQQ | string }

    def main[* : P] : P[Expr] = P{ action | symbol }

    def action[* : P] = P{ space ~ symbol ~ space ~ "(" ~/ space ~ params ~ space ~ ")" ~ space }.map(o => EApply(o._1.value, o._2))

    def params[* : P] = P{ expr.rep(sep = ","./) }.map(_.$)


    def unitref[* : P] = P( symbol ~ "/" ~ symbol ~ "/" ~ number ).map(o => EUnitRef(o._1.value, o._2.value, o._3.value))

    def es[* : P] = P( "$" ~ ("0" | "1" | "2" | "3").! ).map(_.toInt).map(EElderSign)

    def offer[* : P] = P( symbol ~ "->" ~ number ).map(o => EOffer(o._1.value, o._2.value))

    def expr[* : P] : P[Expr] = P{ space ~ ( unitref | es | offer | base ) ~ space }


    def parseAction(ss : String) : Action = {
        // Legacy log migration: agony tally used to serialize as a Map literal
        // {F->n, F->n}; it now serializes as an offer list [F->n, F->n] so the
        // grammar doesn't have to support both `offer = symbol -> number` and
        // `pair = expr -> expr` (which were ambiguous). Rewrite legacy logs in
        // place before parsing. Only Custodian/Librarian agony fields ever
        // emitted braces, so this is the only place braces appear in old logs.
        val migrated = ss
            .replaceAll("""\{\}""", "[]")
            .replaceAll("""\{([A-Z][^{}]*)\}""", "[$1]")

        val s = migrated.replace("&gt;", ">")

        if (s.startsWith("// "))
            CommentAction(s.drop("// ".length))
        else {
            val sss =
                if (s.startsWith("AwakenAction(") && s.split(",").length == 3)
                    s.replace(")", ", -1)")
                else
                if (s.startsWith("MainDoneFertilityAction("))
                    s.replace("MainDoneFertilityAction(", "EndAction(")
                else
                    s

            parse(sss, main(_)) match {
                case Parsed.Success(a, _) => parseExpr(a).asInstanceOf[Action]
                case Parsed.Failure(label, index, extra) =>
                    println(s"[Serialize] Parse failure: $label at $index: $extra")
                    CommentAction(s"Parse error: $label at $index")
            }
        }
    }

    def parseExpr(e : Expr) : Any = e match {
        case ESymbol("UnspeakableOathThreatOfAttack") => UnspeakableOathThreatOfAttackOnHighPriest
        case ESymbol("UnspeakableOathOfAttackOnGOO") => UnspeakableOathThreatOfAttackOnGOO
        case ESymbol("UnspeakableOathOfAttackOnGate") => UnspeakableOathThreatOfAttackOnGate
        case ESymbol(s) =>
            parseFaction(s).map(_.asInstanceOf[Any])
                .orElse(parseRegion(s).map(_.asInstanceOf[Any]))
                .orElse(parseLoyaltyCard(s))
                .orElse(parseSymbol(s))
                .getOrElse(throw new IllegalArgumentException(s"Unknown symbol: $s"))

        case EInt(n) => n
        case EElderSign(v) => ElderSign(v)
        case EBool(b) => b
        case EString(s) => s
        case EOffer(a, b) => Offer(parseFaction(a).get, b)
        case EUnitRef(a, b, c) => UnitRef(parseFaction(a).get, parseSymbol(b).get.asInstanceOf[UnitClass], c)
        case ESome(e) => Some(parseExpr(e))
        case ENone => None
        case EList(l) => l.map(parseExpr)
        case EApply("AzathothOffer", ps) => AzathothOffer(parseExpr(ps(0)).asInstanceOf[Faction], parseExpr(ps(1)).asInstanceOf[Int], parseExpr(ps(2)).asInstanceOf[Int])
        // Firstborn (FB) helper case classes — explicit parser cases mirror the
        // writer cases in Serialize.write. Same pattern as AzathothOffer above.
        case EApply("FBEyeOpensTarget", ps) => FBEyeOpensTarget(parseExpr(ps(0)).asInstanceOf[Region], parseExpr(ps(1)).asInstanceOf[Faction], parseExpr(ps(2)).asInstanceOf[UnitRef])
        case EApply("FBCyclopeanGazeSource", ps) => FBCyclopeanGazeSource(parseExpr(ps(0)).asInstanceOf[Region], parseExpr(ps(1)).asInstanceOf[UnitClass])
        case EApply("FBWritheKillEntry", ps) => FBWritheKillEntry(parseExpr(ps(0)).asInstanceOf[UnitRef], parseExpr(ps(1)).asInstanceOf[Region], parseExpr(ps(2)).asInstanceOf[UnitClass], parseExpr(ps(3)).asInstanceOf[|[UnitRef]])
        case EApply("FBWrithePainEntry", ps) => FBWrithePainEntry(parseExpr(ps(0)).asInstanceOf[UnitRef], parseExpr(ps(1)).asInstanceOf[Region], parseExpr(ps(2)).asInstanceOf[Region])
        case EApply("TSPlaceTomeUnitAction", ps) if ps.length == 4 =>
            TSPlaceTomeUnitAction(parseExpr(ps(0)).asInstanceOf[Faction], parseExpr(ps(1)).asInstanceOf[UnitClass], parseExpr(ps(2)).asInstanceOf[Region], parseExpr(ps(3)).asInstanceOf[Int], parseExpr(ps(0)).asInstanceOf[Faction])
        case EApply(f, params) => params.none.?(parseSymbol(f).get).|(parseActionConstructor(f, params.num).|!("unknown class " + f).apply(params.map(parseExpr)))
    }

    def parseRegion(s : String) : |[Region] = (game.board.regions :+ SL.slumber :+ GC.deep :+ BB.moon).find(_.id == s)
}

object Serialize {
    // BUBASTIS: BB added to faction registry (Task 3.15.1)
    // Defilers Court (DC): Homebrew faction
    // Faceless Blight (FBE): Homebrew faction (§3.15.1)
    val factions = $(GC, CC, BG, YS, SL, WW, OW, AN, TS, FB, DS, TT, BB, DC, FBE) ++ $(NeutralAbhoth, LibraryFaction)

    val loyaltyCards = $(
        HighPriestCard,
        GhastCard, GugCard, ShantakCard, StarVampireCard, VoonithCard, DimensionalShamblerCard, GnorriCard,
        MoonbeastCard, AlbinoPenguinsCard, ElderThingCard, LengSpiderCard, SatyrCard, ServitorCard, InsectsFromShaggaiCard,
        DholeCard, GreatRaceOfYithCard, QuachilUttausCard, ShadowPharaohCard, HoundOfTindalosCard, BrownJenkinCard, ElderShoggothCard,
        ByatisCard, AbhothCard, DaolothCard, NyogthaCard, TulzschaCard, YgolonacCard,
        AzathothIGOOCard, CthughaCard, MotherHydraCard, YigCard, FatherDagonCard, GhatanotoaIGOOCard,
        BloatedWomanCard, AtlachNachaCard, BokrugCard, GlaakiIGOOCard,
    )

    def parseFaction(s : String) : |[Faction] = factions.%(_.short == s).single

    def parseGameOption(s : String) : |[GameOption] = GameOptions.all.%(_.toString == s).single

    def parseLoyaltyCard(s : String) : |[LoyaltyCard] = loyaltyCards.find(_.productPrefix == s)

    def parseSymbol(s : String) : |[Any] = lookupObject("cws." + s)

    def parseActionConstructor(s : String, n : Int) : |[$[Any] => Any] = lookupClass("cws." + s, n)
}
