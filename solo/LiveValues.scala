package cws

import scala.scalajs.js
import scala.scalajs.js.JSON
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

/**
 * Runtime override system for faction values.
 * Fetches live-values.json from designer API and overrides compiled values.
 * This allows Simple Updates to work WITHOUT requiring rebuild/redeploy.
 */
object LiveValues {
    private var overrides = Map[String, Map[String, Any]]()
    private var loaded = false

    /**
     * Fetch live values for all designer factions that have them.
     * Call this on game startup BEFORE any faction objects are instantiated.
     */
    def loadAll(): Unit = {
        val designerFactions = List("CS", "DGT", "FBE", "FM", "PRM", "XSS")

        designerFactions.foreach { acr =>
            val url = s"/designer/api/live/$acr/values"

            Ajax.get(url).onComplete {
                case Success(xhr) =>
                    try {
                        val json = JSON.parse(xhr.responseText)
                        val values = json.asInstanceOf[js.Dynamic].values.asInstanceOf[js.Dictionary[Any]]

                        // Parse the flat key paths back into nested structure
                        // E.g., "units.rows.csun0001.dice" -> Map("Meteorite_combat" -> 0)
                        val parsed = parseValueKeys(acr, values.toMap)
                        overrides = overrides + (acr -> parsed)

                        dom.console.log(s"[LiveValues] Loaded $acr overrides: ${parsed.size} values")
                    } catch {
                        case e: Throwable =>
                            dom.console.warn(s"[LiveValues] Failed to parse $acr values: ${e.getMessage}")
                    }

                case Failure(xhr) =>
                    // Not an error - faction might not have live values yet
                    dom.console.log(s"[LiveValues] No live values for $acr (${xhr.getMessage})")
            }
        }

        loaded = true
    }

    /**
     * Get a value with live override if available.
     * Usage: LiveValues.get("CS", "Meteorite_combat", CSValues.Meteorite_combat)
     */
    def get[T](faction: String, key: String, default: T): T = {
        overrides.get(faction).flatMap(_.get(key)) match {
            case Some(value) => value.asInstanceOf[T]
            case None => default
        }
    }

    /**
     * Check if live values are loaded.
     */
    def isLoaded: Boolean = loaded

    private def parseValueKeys(acr: String, values: Map[String, Any]): Map[String, Any] = {
        // The designer API returns flat keys like:
        // "units.rows.csun0001.cost": 1
        // "units.rows.csun0001.dice": 0
        //
        // We need to map these to FactionValues field names like:
        // "Meteorite_cost", "Meteorite_combat"
        //
        // The mapping is generated into FactionValues.scala from the faction-values JSON files.

        val mapping = FactionValuesMapping.designerMappings.getOrElse(acr, Map.empty[String, String])

        values.flatMap { case (designerPath, value) =>
            mapping.get(designerPath).map { fvKey =>
                fvKey -> value
            }
        }
    }
}
