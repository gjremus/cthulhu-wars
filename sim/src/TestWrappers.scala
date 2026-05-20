package cws
import hrf.colmat._

/**
 * TestBotGC — enhanced wrapper that forces GC through the full neutral unit test flow:
 * 1. Take loyalty card (specific unit if targetUnit is set)
 * 2. Place unit in South Pacific (force region selection)
 * 3. Awaken Cthulhu
 * 4. Awaken iGOO (if applicable)
 * 5. Force submerge with the neutral unit
 * 6. Unsubmerge into battle
 * 7. Force SBR-triggering actions
 */
object TestBotGC {
    var forceLoyaltyCard : Boolean = true
    var forceSubmerge : Boolean = true
    var forceAwaken : Boolean = true
    var targetUnit : Option[String] = None  // e.g. "Servitor", "Atlach-Nacha", "Bokrug", "Gla'aki"
    var forceIGOOAwaken : Boolean = true
    var forcePlaceSpinneret : Boolean = false  // For Atlach-Nacha
    var forceGiveBokrug : Boolean = false
    var testLog : StringBuilder = new StringBuilder

    def reset() : Unit = {
        forceLoyaltyCard = true
        forceSubmerge = true
        forceAwaken = true
        forceIGOOAwaken = true
        forcePlaceSpinneret = false
        forceGiveBokrug = false
        targetUnit = None
        testLog = new StringBuilder
    }

    def log(msg : String) : Unit = {
        testLog.append("[TestBotGC] " + msg + "\n")
        println("[TestBotGC] " + msg)
    }

    def ask(actions : $[Action], r : Double)(game : Game) : Action = {
        val names = actions./(a => a.getClass.getSimpleName)

        // Force loyalty card acquisition
        if (forceLoyaltyCard) {
            // If targeting a specific unit, look for its NeutralMonstersAction or IGOOAction
            if (targetUnit.isDefined) {
                actions.find(a => a.getClass.getSimpleName.contains("NeutralMonstersAction") &&
                    a.toString.contains(targetUnit.get)).foreach { a =>
                    log("Taking NM card: " + targetUnit.get)
                    return a
                }
                actions.find(a => a.getClass.getSimpleName.contains("IGOOAction") &&
                    a.toString.contains(targetUnit.get)).foreach { a =>
                    log("Taking IGOO card: " + targetUnit.get)
                    return a
                }
            }
            // Generic: take any loyalty card
            actions.find(a => a.getClass.getSimpleName.contains("LoyaltyCardDoomAction")).foreach { a =>
                log("Taking loyalty card (doom)")
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("NeutralMonstersAction")).foreach { a =>
                log("Taking NM card: " + a)
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("IGOOAction")).foreach { a =>
                log("Taking IGOO card: " + a)
                return a
            }
        }

        // Force Cthulhu awakening
        if (forceAwaken) {
            actions.find(a => a.getClass.getSimpleName.contains("AwakenAction") &&
                a.toString.contains("Cthulhu")).foreach { a =>
                log("Awakening Cthulhu")
                return a
            }
        }

        // Force iGOO awakening
        if (forceIGOOAwaken) {
            actions.find(a => a.getClass.getSimpleName.contains("IndependentGOOMainAction")).foreach { a =>
                log("Awakening iGOO: " + a)
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("IndependentGOOAction")).foreach { a =>
                log("Placing iGOO: " + a)
                return a
            }
            // Atlach-Nacha/Bokrug/Glaaki custom awaken
            actions.find(a => a.getClass.getSimpleName.contains("CthughaAwakenMainAction") ||
                a.getClass.getSimpleName.contains("AzathothAwakenMainAction")).foreach { a =>
                log("Custom iGOO awaken: " + a)
                return a
            }
        }

        // Force submerge
        if (forceSubmerge) {
            actions.find(a => a.getClass.getSimpleName.contains("SubmergeMainAction")).foreach { a =>
                log("Submerging")
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("SubmergeAction")).foreach { a =>
                log("Submerge unit: " + a)
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("SubmergeDoneAction")).foreach { a =>
                log("Submerge done")
                return a
            }
            // Unsubmerge
            actions.find(a => a.getClass.getSimpleName.contains("UnsubmergeMainAction")).foreach { a =>
                log("Unsubmerging")
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("UnsubmergeAction")).foreach { a =>
                log("Unsubmerge to: " + a)
                return a
            }
        }

        // Force Place Spinneret (Atlach-Nacha)
        if (forcePlaceSpinneret) {
            actions.find(a => a.getClass.getSimpleName.contains("PlaceSpinneretMainAction")).foreach { a =>
                log("Placing Spinneret")
                return a
            }
        }

        // Force Give Bokrug
        if (forceGiveBokrug) {
            actions.find(a => a.getClass.getSimpleName.contains("GiveBokrugMainAction")).foreach { a =>
                log("Giving Bokrug")
                return a
            }
            actions.find(a => a.getClass.getSimpleName.contains("GiveBokrugAction")).foreach { a =>
                log("Give Bokrug to: " + a)
                return a
            }
        }

        // Force attack (to get into battle with neutral unit)
        actions.find(a => a.getClass.getSimpleName.contains("AttackAction")).foreach { a =>
            if (game.factions.head == GC && game.turn >= 3) {
                log("Forcing attack: " + a)
                return a
            }
        }

        // Servitor: force assign to enemy
        actions.find(a => a.getClass.getSimpleName.contains("ServitorAssignFactionAction")).foreach { a =>
            log("Assigning Servitor: " + a)
            return a
        }

        // Place at South Pacific if available in placement menus
        actions.find(a => a.toString.contains("South Pacific") || a.toString.contains("S Pacific")).foreach { a =>
            if (a.getClass.getSimpleName.contains("SummonAction") ||
                a.getClass.getSimpleName.contains("LoyaltyCardSummonAction") ||
                a.getClass.getSimpleName.contains("ServitorPlaceAction")) {
                log("Placing in South Pacific: " + a)
                return a
            }
        }

        // Fall through to standard bot
        BotGC.ask(actions, r)(game)
    }
}

object TestBotCC {
    var forceLoyaltyCard : Boolean = true
    def ask(actions : $[Action], r : Double)(game : Game) : Action = {
        if (forceLoyaltyCard) {
            actions.find(a => a.getClass.getSimpleName.contains("LoyaltyCardDoomAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("NeutralMonstersAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("IGOOAction")).foreach(a => return a)
        }
        BotCC.ask(actions, r)(game)
    }
}

object TestBotSL {
    var forceLoyaltyCard : Boolean = true
    def ask(actions : $[Action], r : Double)(game : Game) : Action = {
        if (forceLoyaltyCard) {
            actions.find(a => a.getClass.getSimpleName.contains("LoyaltyCardDoomAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("NeutralMonstersAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("IGOOAction")).foreach(a => return a)
        }
        BotSL.ask(actions, r)(game)
    }
}

object TestBotOW {
    var forceLoyaltyCard : Boolean = true
    def ask(actions : $[Action], r : Double)(game : Game) : Action = {
        if (forceLoyaltyCard) {
            actions.find(a => a.getClass.getSimpleName.contains("LoyaltyCardDoomAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("NeutralMonstersAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("IGOOAction")).foreach(a => return a)
        }
        BotOW.ask(actions, r)(game)
    }
}

object TestBotDS {
    var forceLoyaltyCard : Boolean = true
    def ask(actions : $[Action], r : Double)(game : Game) : Action = {
        if (forceLoyaltyCard) {
            actions.find(a => a.getClass.getSimpleName.contains("LoyaltyCardDoomAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("NeutralMonstersAction")).foreach(a => return a)
            actions.find(a => a.getClass.getSimpleName.contains("IGOOAction")).foreach(a => return a)
        }
        BotDS.ask(actions, r)(game)
    }
}
