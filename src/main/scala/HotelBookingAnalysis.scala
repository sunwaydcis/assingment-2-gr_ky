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
  def netCustomerCost: Double = bookingPrice * (1.0 - discount)
  def profitPerBooking: Double = bookingPrice * profitMargin
}

object Utils {
  def safeToDouble(str: String): Double =
    try str.toDouble catch { case _: Throwable => 0.0}
  def safeToInt(str: String): Int =
    try str.toInt catch { case _: Throwable => 1 }
  def safeParseDiscount(discountStr: String): Double = {
    try {
      discountStr.replace("%", "").trim.toDouble / 100.0
    } catch {
      case _: Throwable => 0.0
    }
  }
}

// 2. Encapsulation: Data Processor

class HotelDataProcessor(filePath: String) {
  import Utils._

  private lazy val processedData: List[Booking] = loadData()

  def getAllBookings: List[Booking] = processedData

  private def loadData(): List[Booking] = {
    val file = new File(filePath)
    if (!file.exists()) {
      println(s"CRITICAL ERROR: File not found at path: ${file.getAbsolutePath}")
      return List.empty[Booking]
    }

    var reader: BufferedReader = null
    try {
      reader = new BufferedReader(new FileReader(file))
      val lines = Iterator.continually(reader.readLine()).takeWhile(_ != null).drop(1).toList

      if (lines.isEmpty) return List.empty[Booking]

      lines.flatMap { line =>
        val cols = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)").map(_.trim)
        if (cols.length >= 24) {
          try {
            val hotel = cols(16).replaceAll("\"", "")
            val countryOrigin = cols(6).replaceAll("\"", "")
            val countryDestination = cols(9).replaceAll("\"", "")
            val price = safeToDouble(cols(20))
            val discountRatio = safeParseDiscount(cols(21))
            val profit = safeToDouble(cols(23).replaceAll("\"","").trim)
            val days = safeToInt(cols(13))
            val rooms = safeToInt(cols(15))

            if (countryOrigin.nonEmpty && price > 0)
              Some(Booking(hotel, countryOrigin, countryDestination, price, discountRatio, profit, days, rooms))
            else
              None
          } catch {
            case _: Throwable => None
          }
        } else {
          None
        }
      }
    } catch {
      case e: Exception =>
        println(s"Error during data processing: ${e.getMessage}")
        List.empty[Booking]
    } finally {
      if (reader != null) reader.close()
    }
  }
}

//3. Polymorphism: Analysis Traits and Implementations

case class Q2GlobalBounds(
  priceMin: Double, priceMax: Double,
  discountMin: Double, discountMax: Double,
  profitMin: Double, profitMax: Double
)

case class Q3GlobalBounds(
  visitorsMin: Double, visitorsMax: Double,
  avgProfitMin: Double, avgProfitMax: Double
)

object ScoringEngine {
  def normalize(value: Double, min: Double, max: Double): Double = {
    if (max - min == 0) 0.0
    else (value - min) / (max - min) * 100.0
  }

  //Q2: Calculate global bounds for Price Per Room Per Day, Discount, and Profit Margin
  def calculateQ2Bounds(bookings: List[Booking]) : Q2GlobalBounds = {
    val prices = bookings.map(_.pricePerRoomPerDay)
    val discounts = bookings.map(_.discount)
    val profits = bookings.map(_.profitMargin)

    Q2GlobalBounds(
      priceMin = prices.min, priceMax= prices.max,
      discountMin = discounts.min, discountMax = discounts.max,
      profitMin = profits.min, profitMax = profits.max
    )
  }

  //Q3: Calculate global bounds for Hotel-level Visitor Count and Average Profit Margin
  def calculateQ3Bounds(hotelMetrics: Map[String, (Int, Double)]): Q3GlobalBounds = {
    val visitorCounts = hotelMetrics.values.map(_._1.toDouble)
    val avgProfits = hotelMetrics.values.map(_._2)

    Q3GlobalBounds(
      visitorsMin = visitorCounts.min, visitorsMax = visitorCounts.max,
      avgProfitMin = avgProfits.min, avgProfitMax = avgProfits.max
    )
  }
}

trait AnalysisReport {
  def analyze(bookings: List[Booking]): Unit
}

//Q1: Country with the highest number of bookings
class CountryAnalysis extends AnalysisReport {
  def analyze(bookings: List[Booking]): Unit = {
    val result = bookings
      .groupBy(_.destinationCountry)
      .mapValues(_.size)
      .maxByOption { case (_, count) => count }

    println("\n[1. Highest Number of Bookings Analysis]")
    result match {
      case Some((country, count)) =>
        println(s" - Country: **$country**")
        println(s" - Bookings: **$count**")
      case None => println("- No data available.")
    }
  }
}

// Q2: Most economical hotel
class EconomicalAnalysis extends AnalysisReport{
  import ScoringEngine._

  def analyze(bookings: List[Booking]): Unit={
    if(bookings.isEmpty) return println("-No data available.")

    val bounds = calculateQ2Bounds(bookings)

    val scoredBookings = bookings.map{ b=>
      // 1. Score Price:
      val priceRaw = normalize(b.pricePerRoomPerDay, bounds.priceMin, bounds.priceMax)
      val priceScore = 100.0 - priceRaw

      // 2. Score Discount:
      val discountScore = normalize(b.discount, bounds.discountMin, bounds.discountMax)

      // 3. Score Profit Margin:
      val profitScore = normalize (b.profitMargin, bounds.profitMin, bounds.profitMax)

      // 4. Final Score:
      val averageScore = (priceScore + discountScore + profitScore) / 3.0

      (b.hotelName, averageScore)
    }

    // 5. Final step:
    val hotelScores = scoredBookings
      .groupBy(_._1)
      .mapValues(scores => scores.map(_._2).sum/scores.size)

    val mostEconomical = hotelScores.maxByOption(_._2)

    println("\n[2. Most Economical Hotel Analysis]")
    mostEconomical match{
      case Some((hotel,score)) =>
        println(f"-Hotel: **$hotel**")
        println(f"-Highest Average Score: **$score%.2f** (out of 100)")
      case None => println("-No data available.")
    }
  }
}

// Q3: Most profitable hotel
class ProfitAnalysis extends AnalysisReport{
  import ScoringEngine._

  def analyze(bookings:List[Booking]): Unit={
    if(bookings.isEmpty) return println("-No data available.")

    val hotelMetrics = bookings
      .groupBy(_.hotelName)
      .mapValues{ list =>
        val visitorCount = list.size
        val avgProfitMargin = list.map(_.profitMargin).sum/list.size
        (visitorCount, avgProfitMargin)
      }
      .toMap

    val bounds = ScoringEngine.calculateQ3Bounds(hotelMetrics)

    val finalScores = hotelMetrics.map{ case(hotel,(visitor, avgProfit)) =>
      // 1. Normalize visitors
      val visitorScore = normalize(visitor.toDouble, bounds.visitorsMin, bounds.visitorsMax)

      // 2. Normalize Avg Profit Margin
      val profitScore = normalize(avgProfit, bounds.avgProfitMin, bounds.avgProfitMax)

      // 3. Final Score
      val finalScore = (visitorScore + profitScore) / 2.0

      (hotel, finalScore)
    }
    val mostProfitable = finalScores.maxByOption(_._2)

    println("\n[3. Most Profitable Hotel Analysis]")
    mostProfitable match{
      case Some((hotel, score))=>
        println(f"-Hotel: **$hotel**")
        println(f"-Highest Normalized Score: **$score%.2f** (out of 100)")
      case None => println("-No data available.")
    }
  }
}

class AnalysisRunner(analyses: List[AnalysisReport]) {
  def runReport(bookings: List[Booking]):Unit={
    println("---Hotel Booking Analysis Report---")
    analyses.foreach(report => report.analyze(bookings))

    println("-----------------------------------")
  }
}

object Main {
  def main(args: Array[String]):Unit={
    val filePath = "Hotel_Dataset.csv"

    val dataProcessor = new HotelDataProcessor(filePath)
    val allBookings = dataProcessor.getAllBookings

    if(allBookings.isEmpty){
      println("Analysis failed: Could not load data or data is empty.")
      return
    }

    val fullReportAnalyses: List[AnalysisReport]=List(
      new CountryAnalysis,
      new EconomicalAnalysis,
      new ProfitAnalysis
    )

    val runner = new AnalysisRunner(fullReportAnalyses)
    runner.runReport(allBookings)
  }
}