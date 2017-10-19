package observatory

import java.io.File
import java.nio.file.{Path, Paths}
import java.nio.file.Files

import com.sksamuel.scrimage.Image
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

  val tilesStoragePath = "C:\\Users\\mprushin\\Desktop\\Scala\\observatory\\target\\temperatures\\" //"""C:\Users\mprushin\Desktop\Scala\observatory\target\"""//Paths.get(getClass.getResource("/target/temperatures/").toURI).toString

  def generateImageImpl(year: Int, zoom: Int, x: Int, y: Int, data: Iterable[(Location, Double)]): Unit = {
    val image = Interaction.tile(data, colors1, zoom, x, y)
    val stringPath = tilesStoragePath+"%d/%d/%d-%d.png".format(year, zoom, x, y)
    writeImage(image, stringPath)
  }

  def writeImage(image: Image, path: String): Path ={
    val parentDir = Paths.get(path).getParent
    if (!Files.exists(parentDir)) Files.createDirectories(parentDir)
    image.output(path)
  }

  test("get first tile") {
    Interaction.tile(testTemps1, colors1, 0, 0, 0).output(tilesStoragePath+"firstTile.bmp")
  }

  /*test("get first and second tiles"){
    Interaction.tile(testTemps1, colors1, 0, 0, 0).output(getClass.getResource("/observatory/")+"firstTile-0-0.bmp")

    Interaction.tile(testTemps1, colors1, 1, 0, 0).output(getClass.getResource("/observatory/")+"secondTile-0-0.bmp")
    Interaction.tile(testTemps1, colors1, 1, 0, 1).output(getClass.getResource("/observatory/")+"secondTile-0-1.bmp")
    Interaction.tile(testTemps1, colors1, 1, 1, 0).output(getClass.getResource("/observatory/")+"secondTile-1-0.bmp")
    Interaction.tile(testTemps1, colors1, 1, 1, 1).output(getClass.getResource("/observatory/")+"secondTile-1-1.bmp")
  }*/

  test("generate tiles"){
    Interaction.generateTiles[Seq[(Location, Double)]](Seq((2015, testTemps1)), generateImageImpl)
  }
}
