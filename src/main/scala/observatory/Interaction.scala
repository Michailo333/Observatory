package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{createPixel, interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = Math.pow(2, zoom)
    val rho = Math.PI / 180

    /*val lat = Math.atan(Math.sinh(Math.PI * (1 - 2 * y / n)))
    val lon = (x / (n * 2) - 1) * Math.PI*/

    val lat = Math.toDegrees(Math.atan(Math.sinh(Math.PI * (1 - 2 * y / n))))
    val lon = Math.toDegrees((x / (n * 2) - 1) * Math.PI)

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val x_add = Extraction.sc.parallelize(0 to 255)
    val y_add = Extraction.sc.parallelize(0 to 255)
    val matrix = x_add.cartesian(y_add).map(pair => (pair._1, pair._2, tileLocation(zoom + 8, x * 256 + pair._1, y * 256 + pair._2)))
    val pixelMatrix = matrix.map(loc => (loc._1, loc._2, createPixel(interpolateColor(colors, predictTemperature(temperatures, loc._3)))))
    val array = pixelMatrix.map(pair => (pair._1 + pair._2 * 256, pair._3)).sortBy(pair => pair._1).collect().map(pair => pair._2)

    Image(256, 256, array)

  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    yearlyData.foreach(pair => {
      (0 to 3).flatMap(zoomLevel => generateForYearAndZoomLevel(pair._1, zoomLevel, pair._2))
        .foreach(params => generateImage(params._1, params._2, params._3, params._4, params._5))
    })
  }

  def generateForYearAndZoomLevel[Data](year: Int,
                                        zoomLevel: Int,
                                        data: Data
                                       ): Iterable[(Int, Int, Int, Int, Data)] = {
    val x = 0 until Math.pow(2, zoomLevel).toInt
    val y = 0 until Math.pow(2, zoomLevel).toInt

    x.flatMap(x_val => y.map(y_val => (x_val, y_val))).map(pair => (year, zoomLevel, pair._1, pair._2, data))
  }

}
