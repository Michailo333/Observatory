package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(stnIdentifier: Int, wbanIdentifier: Int, location: Location) extends Serializable

case class Temperature(stnIdentifier: Int, wbanIdentifier: Int, month: Int, day: Int, temperature: Double) extends Serializable