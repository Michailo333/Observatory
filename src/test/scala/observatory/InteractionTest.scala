package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  val colors1 = List(
    (60d, Color(255, 255, 255)),
    (32d, Color(255, 0, 0)),
    (12d, Color(255, 255, 0)),
    (0d, Color(0, 255, 255)),
    (-15d, Color(0, 0, 255)),
    (-27d, Color(255, 0, 255)),
    (-50d, Color(33, 0, 107)),
    (-60d, Color(0, 0, 0))
  )

  val testTemps1 = Seq(
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
  def generateImageImpl(year: Int, zoom: Int, x: Int, y: Int, data: Iterable[(Location, Double)]): Unit = {
    val image = Interaction.tile(data, colors1, zoom, x, y)
    val stringPath = """C:\Users\mprushin\Desktop\Scala\observatory\tilesGenerationOutput\tile.%d.%d.%d.%d.bmp""".format(year, zoom, x, y)
    image.output(stringPath)
  }

  test("get first tile") {
    Interaction.tile(testTemps1, colors1, 0, 0, 0).output("""C:\Users\mprushin\Desktop\Scala\observatory\firstTile.bmp""")
  }

  test("get first and second tiles"){
    Interaction.tile(testTemps1, colors1, 0, 0, 0).output("""C:\Users\mprushin\Desktop\Scala\observatory\tiles\firstTile.bmp""")

    Interaction.tile(testTemps1, colors1, 1, 0, 0).output("""C:\Users\mprushin\Desktop\Scala\observatory\tiles\secondTile-0-0.bmp""")
    Interaction.tile(testTemps1, colors1, 1, 0, 1).output("""C:\Users\mprushin\Desktop\Scala\observatory\tiles\secondTile-0-1.bmp""")
    Interaction.tile(testTemps1, colors1, 1, 1, 0).output("""C:\Users\mprushin\Desktop\Scala\observatory\tiles\secondTile-1-0.bmp""")
    Interaction.tile(testTemps1, colors1, 1, 1, 1).output("""C:\Users\mprushin\Desktop\Scala\observatory\tiles\secondTile-1-1.bmp""")
  }

  test("generate tiles"){
    Interaction.generateTiles[Seq[(Location, Double)]](Seq((1990, testTemps1)), generateImageImpl)
  }


}
