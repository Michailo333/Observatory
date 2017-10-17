import com.sksamuel.scrimage.{Pixel, X11Colorlist}
import observatory.{Color, Location, Visualization}

val arr = "010013,,1,1".split(",")
arr.length
val list = List((1, Pixel(1, 2, 3, 4)), (2, Pixel(1, 2, 3, 4)), (3, Pixel(1, 2, 3, 4)), (4, Pixel(1, 2, 3, 4)), (5, Pixel(1, 2, 3, 4)))



val a = new Array[Pixel](100)
a(0) = Pixel(1, 2, 3, 4)
a

val black = Pixel(X11Colorlist.Black.toInt)
black.alpha

val range = (-10 to 10)

val loc1 = Location(90, 0)
val loc2 = Location(0, -180)
val loc3 = Location(0, 0)
val loc4 = Location(-89, 0)
val loc5 = Location(0, 179)


def distance(location1: Location, location2: Location): Double = {
  Math.acos(
    Math.sin(location1.lat * Math.PI / 180) * Math.sin(location2.lat * Math.PI / 180) +
      Math.cos(location1.lat * Math.PI / 180) *
        Math.cos(location2.lat * Math.PI / 180) *
        Math.cos(location2.lon * Math.PI / 180 - location1.lon * Math.PI / 180)
  )
}

distance(loc1, loc3)
distance(loc2, loc3)
distance(loc1, loc2)

distance(loc4, loc3)
distance(loc5, loc3)
distance(loc4, loc5)



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


val testTemps2 = Seq(
  (Location(0, -180), 32d),
  //    (Location(0, 180), 32d),
  (Location(90, 0), -15d)
  //    (Location(-90, 0), -15d),
  //    (Location(0, 0), 0d)
)

Visualization.predictTemperature(testTemps2, Location(90, -180))
