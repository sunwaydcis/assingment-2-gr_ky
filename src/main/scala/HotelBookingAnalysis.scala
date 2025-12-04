import scala.io.Source
import java.io.File
import scala.language.postfixOps
import java.io.BufferedReader
import java.io.FileReader


// 1. Data Model and Utilities

// Case Class for Immutable Data Model
case class Booking(
                    hotelName: String,
                    originCountry: String,
                    destinationCountry: String,
                    destinationCity: String,
                    bookingPrice: Double,
                    discount: Double,
                    profitMargin: Double,
                    noOfDays: Int,
                    rooms: Int
                  ) {
  def pricePerRoomPerDay: Double = {
    val days = if (noOfDays <= 0) 1 else noOfDays
    val roomCount = if (rooms <= 0) 1 else rooms
    bookingPrice / (days * roomCount)
  }
}

// 2. Utilities
object Utils {
  def safeToDouble(str: String): Double =
    try str.toDouble catch { case _: Throwable => 0.0}

  def safeToInt(str: String): Int =
    try str.toInt catch { case _: Throwable => 1 }

  def safeParseDiscount(discountStr: String): Double =
    try discountStr.replace("%", "").trim.toDouble / 100.0
    catch { case _: Throwable => 0.0}
}

// 3. Encapsulation: Data Processor

class HotelDataProcessor(filePath: String) {
  import Utils._

  private lazy val processedData: List[Booking] = loadData()

  def getAllBookings: List[Booking] = processedData

  private def loadData(): List[Booking] = {
    val file = new File(filePath)
    if (!file.exists()) return List.empty

    var reader: BufferedReader = null
    try {
      reader = new BufferedReader(new FileReader(file))
      val lines = Iterator.continually(reader.readLine()).takeWhile(_ != null).drop(1).toList

      lines.flatMap { line =>
        val cols = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)").map(_.trim)
        if (cols.length >= 24) {
          try {
            Some(
              Booking(
                hotelName = cols(16).replaceAll("\"", ""),
                originCountry = cols(6).replaceAll("\"", ""),
                destinationCountry = cols(9).replaceAll("\"", ""),
                destinationCity = cols(10).replaceAll("\"", ""),
                bookingPrice = safeToDouble(cols(20)),
                discount = safeParseDiscount(cols(21)),
                profitMargin = safeToDouble(cols(23).replaceAll("\"", "")),
                noOfDays = safeToInt(cols(13)),
                rooms = safeToInt(cols(15))
              )
            )
          } catch { case _: Throwable => None }
        } else None
      }
    } finally {
      if (reader != null) reader.close()
    }
  }
}

// 4. Scoring Engine

object ScoringEngine {
  def normalize(value: Double, min: Double, max: Double): Double =
    if (max - min == 0) 0.0 else (value - min) / (max - min)
}

//5. Polymorphism: Analysis Traits and Implementations

trait AnalysisReport {
  def analyze(bookings: List[Booking]): Unit
}

//Q1: Country with the highest number of bookings
class CountryAnalysis extends AnalysisReport {
  def analyze(bookings: List[Booking]): Unit = {
    val result = bookings
      .groupBy(_.destinationCountry)
      .view.mapValues(_.size).toMap
      .maxByOption(_._2)

    println("\n[1. Highest Number of Bookings Analysis]")
    result foreach { case (country, count) =>
        println(s" - Country: $country")
        println(s" - Bookings: $count")
    }
  }
}

// Q2: Most economical hotel
class EconomicalAnalysis extends AnalysisReport{
  import ScoringEngine._

  def analyze(bookings: List[Booking]): Unit={

    val hotelGroups =
      bookings.groupBy(b => (b.destinationCountry, b.hotelName, b.destinationCity))

    val hotelAverages = hotelGroups.map { case (key, list) =>
      val avgPrice = list.map(_.pricePerRoomPerDay).sum / list.size
      val avgDisc  = list.map(_.discount).sum / list.size
      val avgProfit= list.map(_.profitMargin).sum / list.size
      key -> (avgPrice, avgDisc, avgProfit)
    }

    val prices = hotelAverages.values.map(_._1)
    val discs = hotelAverages.values.map(_._2)
    val profits = hotelAverages.values.map(_._3)

    val minP = prices.min; val maxP = prices.max
    val minD = discs.min; val maxD = discs.max
    val minM = profits.min; val maxM = profits.max

    val finalScores = hotelAverages.map { case (hotelKey, (p, d, m)) =>
      val priceScore = 1 - normalize(p, minP, maxP)
      val discScore = normalize(d, minD, maxD)
      val profScore = normalize(m, minM, maxM)

      val finalScore = (priceScore + discScore + profScore) / 3
      hotelKey -> finalScore
    }

    val best = finalScores.maxByOption(_._2)

    println("\n[2. Most Economical Hotel Analysis]")
    best.foreach { case ((country, hotel, city), score) =>
      println(s"- Hotel: $hotel ($city, $country)")
      println(f"- Final Score: ${score * 100}%.2f")
    }
  }
}

// Q3: Most profitable hotel
class ProfitAnalysis extends AnalysisReport {
  import ScoringEngine._

  def analyze(bookings: List[Booking]): Unit = {

    val hotelGroups =
      bookings.groupBy(b => (b.destinationCountry, b.hotelName, b.destinationCity))

    val hotelStats = hotelGroups.map { case (key, list) =>
      val visitors = list.size
      val avgProfit = list.map(_.profitMargin).sum / list.size
      key -> (visitors.toDouble, avgProfit)
    }

    val visitorVals = hotelStats.values.map(_._1)
    val profitVals  = hotelStats.values.map(_._2)

    val minV = visitorVals.min; val maxV = visitorVals.max
    val minP = profitVals.min;  val maxP = profitVals.max

    val finalScores = hotelStats.map { case (key, (v,p)) =>
      val visitorScore = normalize(v, minV, maxV)
      val profitScore  = normalize(p, minP, maxP)
      val finalScore = (visitorScore + profitScore) / 2
      key -> finalScore
    }

    val best = finalScores.maxByOption(_._2)

    println("\n[3. Most Profitable Hotel Analysis]")
    best.foreach { case ((country, hotel, city), score) =>
      println(s"- Hotel: $hotel ($city, $country)")
      println(f"- Final Score: ${score * 100}%.2f")
    }
  }
}

// 6. Runner & Main
class AnalysisRunner(analyses: List[AnalysisReport]) {
  def runReport(bookings: List[Booking]):Unit={
    println("---Hotel Booking Analysis Report---")
    analyses.foreach(_.analyze(bookings))
    println("-----------------------------------")
  }
}

object Main {
  def main(args: Array[String]):Unit={
    val processor = new HotelDataProcessor("Hotel_Dataset.csv")
    val bookings = processor.getAllBookings

    val analyses: List[AnalysisReport] = List(
      new CountryAnalysis,
      new EconomicalAnalysis,
      new ProfitAnalysis
    )

    new AnalysisRunner(analyses).runReport(bookings)
  }
}