package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.{SparkConf, SparkContext}


/**
  * 1st milestone: data extraction
  */
object Extraction {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Observatory")
  @transient lazy val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("WARN")

    /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = sc.textFile(fsPath(stationsFile))
    val temperatures = sc.textFile(fsPath(temperaturesFile))

    val stationsRDD = stations
      .filter(line => {
        val arr = line.split(",")
        arr.length == 4 && (arr(0) != "" || arr(1) != "") && arr(2) != "" && arr(3) != ""
      })
      .map(line => {
        val arr = line.split(",")
        Station(
          stnIdentifier = if (arr(0) == "") 0 else arr(0).toInt,
          wbanIdentifier = if (arr(1) == "") 0 else arr(1).toInt,
          location = Location(arr(2).toDouble, arr(3).toDouble)
        )
      }).map(s => ((s.stnIdentifier, s.wbanIdentifier), s))

    val temperaturesRDD = temperatures
      .filter(line => {
        val arr = line.split(",")
        arr.length == 5 && (arr(0) != "" || arr(1) != "") && arr(2) != "" && arr(3) != "" && arr(4) != ""
      })
      .map(line => {
        val arr = line.split(",")
        if (arr.length < 5 || arr(0) == "") line.drop(1)
        Temperature(
          stnIdentifier = if (arr(0) == "") 0 else arr(0).toInt,
          wbanIdentifier = if (arr(1) == "") 0 else arr(1).toInt,
          month = arr(2).toInt,
          day = arr(3).toInt,
          temperature = BigDecimal((arr(4).toDouble - 32) / 1.8).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
        )
      }).map(t => ((t.stnIdentifier, t.wbanIdentifier), t))

    temperaturesRDD.join(stationsRDD).map(pair => {
      (LocalDate.of(year, pair._2._1.month, pair._2._1.day), pair._2._2.location, pair._2._1.temperature)
    }).toLocalIterator.toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    sc.parallelize(records.toSeq).map(pair => (pair._2, pair._3)).groupByKey.map(pair => (pair._1, pair._2.sum / pair._2.count(a => true))).toLocalIterator.toSeq
  }

  /**
    * @param resource Relative URL to file
    * @return Absolute Path to resource.
    */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString


}
