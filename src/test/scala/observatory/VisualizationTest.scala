package observatory


import java.time.LocalDate
import java.util.Calendar

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers


trait VisualizationTest extends FunSuite with Checkers {

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

  val testTemps2 = Seq(
    (Location(0, -180), 32d),
    //    (Location(0, 180), 32d),
    (Location(90, 0), -15d)
    //    (Location(-90, 0), -15d),
    //    (Location(0, 0), 0d)
  )


  def testVisualiseTester(correctColor: Color, temperature: Double): Unit = {
    val color = Visualization.interpolateColor(colors, temperature)
    assert(color == correctColor)
  }

  test("interpolate color") {
    testVisualiseTester(Color(255, 255, 255), 60d)
    testVisualiseTester(Color(255, 0, 0), 32d)
    testVisualiseTester(Color(255, 255, 0), 12d)
    testVisualiseTester(Color(0, 255, 255), 0d)
    testVisualiseTester(Color(0, 0, 255), -15d)
    testVisualiseTester(Color(255, 0, 255), -27d)
    testVisualiseTester(Color(33, 0, 107), -50d)
    testVisualiseTester(Color(0, 0, 0), -60d)

    testVisualiseTester(Color(0, 0, 0), -70d)
    testVisualiseTester(Color(255, 255, 255), 70d)
    testVisualiseTester(Color(128, 255, 128), 6d)

  }
  test("predict temperature for -90 180"){
    val temp = Visualization.predictTemperature(testTemps2, Location(90, -180))
    assert(!temp.isNaN)
  }


  test("get image for test data") {
    val image = Visualization.visualize(testTemps, colors)
    image.output("""C:\Users\mprushin\Desktop\Scala\observatory\testTempsImage.bmp""")
  }


  /*test("get image for test dat 2") {
    val image = Visualization.visualize(testTemps2, colors)
    image.output("""C:\Users\mprushin\Desktop\Scala\observatory\testTemps2Image.bmp""")
    val computedColor = Color(image.pixel(0, 0).red, image.pixel(0, 0).green, image.pixel(0, 0).blue)
    val shouldBeCloserToColor = Color(255, 0, 0)
    val shouldntBeCloserToColor = Color(0, 0, 255)

    val correctDif = ColorDifference(computedColor, shouldBeCloserToColor)
    val incorrectDif = ColorDifference(computedColor, shouldntBeCloserToColor)

    assert(correctDif < incorrectDif)
  }*/

  private def ColorDifference(color1: Color, color2: Color) = {
    (color2.red - color1.red).abs + (color2.green - color1.green).abs + (color2.blue - color1.blue).abs
  }

  /*test("get image for real data") {
    val temps = Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv").toList
    val result = Extraction.locationYearlyAverageRecords(temps).toList
    val image = Visualization.visualize(result, colors)
    image.output("""C:\Users\mprushin\Desktop\Scala\observatory\realDataImage.bmp""")
  }*/


}
