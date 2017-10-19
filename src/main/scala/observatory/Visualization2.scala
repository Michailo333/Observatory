package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Visualization.{createPixel, interpolateColor, predictTemperature}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x   X coordinate between 0 and 1
    * @param y   Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             x: Double,
                             y: Double,
                             d00: Double,
                             d01: Double,
                             d10: Double,
                             d11: Double
                           ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param zoom   Zoom level of the tile to visualize
    * @param x      X value of the tile to visualize
    * @param y      Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: (Int, Int) => Double,
                     colors: Iterable[(Double, Color)],
                     zoom: Int,
                     x: Int,
                     y: Int
                   ): Image = {
    val x_add = Extraction.sc.parallelize(0 to 255)
    val y_add = Extraction.sc.parallelize(0 to 255)
    val matrix = x_add.cartesian(y_add).map(pair => (pair._1, pair._2, tileLocation(zoom + 8, x * 256 + pair._1, y * 256 + pair._2)))
    val pixelMatrix = matrix.map(loc => (loc._1, loc._2, createPixel(interpolateColor(colors, calculateValue(grid, loc._3.lon, loc._3.lat)))))
    val array = pixelMatrix.map(pair => (pair._1 + pair._2 * 256, pair._3)).sortBy(pair => pair._1).collect().map(pair => pair._2)

    Image(256, 256, array)
  }

  /**
    * Calculates value by grid and coordinates
    * @param grid - Grid with data
    * @param x - X value
    * @param y - Y value
    * @return Interpolated value for (X, Y) point
    */
  def calculateValue( grid: (Int, Int)=>Double, x: Double, y: Double): Double={
    val x0 = x.floor.toInt
    val x1 = x.ceil.toInt
    val y0 = y.floor.toInt
    val y1 = y.ceil.toInt

    bilinearInterpolation(x, y, grid(x0, y0), grid(x0, y1), grid(x1, y0), grid(x1, y1))
  }

}
