package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  val colors = List(
    (60d, Color(255, 255, 255)),
    (32d, Color(255, 0, 0)),
    (12d, Color(255, 255, 0)),
    (0d, Color(0, 255, 255)),
    (-15d, Color(0, 0, 255)),
    (-27d, Color(255, 0, 255)),
    (-50d, Color(33, 0, 107)),
    (-60d, Color(0, 0, 0))
  ).toSeq

  /*def generateImageImpl(year: Int, zoom: Int, x: Int, y: Int, data: Iterable[(Location, Double)]): Unit = {
    val image = Interaction.tile(data, colors, zoom, x, y)
    val stringPath = Extraction.fsPath(String.format("/tiles/%i.%i.%i.bmp", year, x, y))
    image.output(stringPath)
  }*/

  test("get first tile") {
    val testTemps = Seq(
      (Location(-50, -50), 50d),
      (Location(-50, 0), 32d),
      (Location(-50, 50), 10d),
      (Location(0, -50), 0d),
      (Location(0, 0), -10d),
      (Location(0, 50), -20d),
      (Location(50, -50), -30d),
      (Location(50, 0), -45d),
      (Location(50, 50), -55d)
    )


    val image = Interaction.tile(testTemps, colors, 0, 0, 0)
    image.output("""C:\Users\mprushin\Desktop\Scala\observatory\firstTile.bmp""")
  }


}
