package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {

  test("work on the test data") {
    val result = Extraction.locateTemperatures(2017, "/stations1.csv", "/temperatures1.csv")

    val correctResult = Seq(
      (LocalDate.of(2017, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2017, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2017, 1, 29), Location(37.358, -78.438), 2.0)
    )

    assert(result.toList == correctResult)
  }

  test("average on the test data") {
    val temps = Extraction.locateTemperatures(2017, "/stations1.csv", "/temperatures1.csv")
    val result = Extraction.locationYearlyAverageRecords(temps)
    val correctResult = Seq(
      (Location(37.358, -78.438), 1.0),
      (Location(37.35, -78.433), 27.3)
    )

    assert(result.toList == correctResult)
  }

  test("work on real data") {
    val temps = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val result = Extraction.locationYearlyAverageRecords(temps)
    assert(result.count(pair => true) > 10)
  }

  test("station identifiers") {
    val result = Extraction.locateTemperatures(2000, "/stations2.csv", "/temperatures2.csv")
    val correctResult = Seq(
      (LocalDate.of(2000, 1, 1), Location(1, -1), -12.22),
      (LocalDate.of(2000, 1, 2), Location(2, -2), -12.22),
      (LocalDate.of(2000, 1, 3), Location(3, -3), -12.22)
    )

    assert(result.toList == correctResult)
  }
}