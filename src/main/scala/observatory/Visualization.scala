package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.spark.{SparkConf, SparkContext}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val power: Double = 6

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    if (!temperatures.filter(pair => pair._1 == location).isEmpty) {
      temperatures.filter(pair => pair._1 == location).head._2
    } else {
      temperatures.foldLeft(0d)((acc, pair) => acc + weight(pair._1, location) * pair._2) / temperatures.foldLeft(0d)((acc, pair) => acc + weight(pair._1, location))
    }
  }

  def weight(location: Location, location1: Location): Double = {
    1 / Math.pow(distance(location, location1), power)
  }

  def distance(location: Location, location1: Location): Double = {
    Math.acos(
      Math.sin(location.lat * Math.PI / 180) * Math.sin(location1.lat * Math.PI / 180) +
        Math.cos(location.lat * Math.PI / 180) * Math.cos(location1.lat * Math.PI / 180) *
          Math.cos(location1.lon * Math.PI / 180 - location.lon * Math.PI / 180)
    )
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    if(points.isEmpty || value.isNaN){
      Color(255,255,255)
    }
    else if (!points.filter(p => p._1 == value).isEmpty) {
      points.filter(p => p._1 == value).head._2
    }
    else if (points.forall(p => value > p._1)) {
      points.filter(p => p._1 == points.map(p => p._1).max).head._2
    }
    else if (points.forall(p => value < p._1)) {
      points.filter(p => p._1 == points.map(p => p._1).min).head._2
    }
    else {
      val maxLower = points.filter(p => p._1 < value).maxBy(p => p._1)
      val minGreater = points.filter(p => p._1 > value).minBy(p => p._1)

      val k1 = 1 - (value - maxLower._1) / (minGreater._1 - maxLower._1)
      val k2 = (value - maxLower._1) / (minGreater._1 - maxLower._1)

      val red = (maxLower._2.red * k1 + minGreater._2.red * k2).round.toInt
      val green = (maxLower._2.green * k1 + minGreater._2.green * k2).round.toInt
      val blue = (maxLower._2.blue * k1 + minGreater._2.blue * k2).round.toInt

      Color(red, green, blue)
    }


  }

  def createPixel(color:Color): Pixel={
    Pixel(color.red, color.green, color.blue, 255)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    var lats= Extraction.sc.parallelize(-89 to 90)
    var lons=Extraction.sc.parallelize(-180 until 180)
    var matrix = lats.cartesian(lons).map(pair=>Location(pair._1, pair._2))
    val pixelMatrix = matrix.map(loc=> (loc, createPixel(interpolateColor(colors, predictTemperature(temperatures, loc)))))

    var array: Array[Pixel] = new Array[Pixel](360 * 180)
    pixelMatrix.collect().foreach(pair=>{
      array((pair._1.lon.toInt + 180) + (pair._1.lat.toInt + 89) * 360)=pair._2
    })

    Image(360, 180, array)
  }

}

