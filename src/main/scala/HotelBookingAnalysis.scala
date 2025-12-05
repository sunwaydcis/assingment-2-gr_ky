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
    if (!file.exists()) {
      println(s"CRITICAL ERROR: File not found at path: ${file.getAbsolutePath}")
      return List.empty
    }

    var reader: BufferedReader = null
    try {
      reader = new BufferedReader(new FileReader(file))

      val headerLine = reader.readLine()
      val headers = headerLine
        .split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)")
        .map(_.trim.replaceAll("\"", ""))

      /*println("\n==== CSV HEADERS FOUND ====")
      headers.foreach(h => println(s"[$h]"))
      println("=============================\n")*/

      val index = headers.zipWithIndex.toMap
      def col(name: String): Int=
        index.getOrElse(name, throw new RuntimeException(s"Missing column: $name"))

      val lines = Iterator.continually(reader.readLine()).takeWhile(_ != null).toList

      lines.flatMap { line =>
        val cols = line
          .split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)")
          .map(_.trim)

          try {
            val hotel = cols(col("Hotel Name")).replaceAll("\"", "")
            val origin = cols(col("Origin Country")).replaceAll("\"", "")
            val destination = cols(index("Destination Country")).replaceAll("\"", "")
            val city = cols(col("Destination City")).replaceAll("\"", "")
            val price = Utils.safeToDouble(cols(col("Booking Price[SGD]")).replaceAll("\"", ""))
            val discount = Utils.safeParseDiscount(cols(col("Discount")).replaceAll("\"", ""))
            val profit = Utils.safeToDouble(cols(col("Profit Margin")).replaceAll("\"", ""))
            val days = Utils.safeToInt(cols(col("No of Days")).replaceAll("\"", ""))
            val rooms = Utils.safeToInt(cols(col("Rooms")).replaceAll("\"", ""))

            if(hotel.nonEmpty && destination.nonEmpty && price > 0)
              Some(Booking(
                hotelName = hotel,
                originCountry = origin,
                destinationCountry = destination,
                destinationCity = city,
                bookingPrice = price,
                discount = discount,
                profitMargin = profit,
                noOfDays = days,
                rooms = rooms
              ))
            else None
          } catch {
            case e: Throwable =>
              println(s"Row skipped due to error: ${e.getMessage}")
              None
          }
        }
      } catch {
      case e: Exception =>
        println(s"Error loading data: ${e.getMessage}")
        List.empty
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

    if (bookings.isEmpty) {
      println("\n[1. Highest Number of Bookings Analysis]")
      println("- No data available.")
      return
    }

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
    if (bookings.isEmpty) {
      println("No data available.")
      return
    }

    val hotelGroups =
      bookings.groupBy(b => (b.destinationCountry, b.hotelName, b.destinationCity))

    if (hotelGroups.isEmpty){
      println("\n[2. Most Economical Hotel Analysis]")
      println("No valid hotel groups found.")
      return
    }

    val hotelAverages = hotelGroups.map { case (key, list) =>
      val avgPrice = list.map(_.pricePerRoomPerDay).sum / list.size
      val avgDisc  = list.map(_.discount).sum / list.size
      val avgProfit= list.map(_.profitMargin).sum / list.size
      key -> (avgPrice, avgDisc, avgProfit)
    }

    val prices = hotelAverages.values.map(_._1).toList
    val discs = hotelAverages.values.map(_._2).toList
    val profits = hotelAverages.values.map(_._3).toList

    if(prices.isEmpty || discs.isEmpty || profits.isEmpty){
      println("\n[2. Most Economical Hotel Analysis]")
      println("- Insufficient data after processing.")
      return
    }

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

    if (bookings.isEmpty) {
      println("\n[3. Most Profitable Hotel Analysis]")
      println("- No data available.")
      return
    }

    val hotelGroups =
      bookings.groupBy(b => (b.destinationCountry, b.hotelName, b.destinationCity))

    if (hotelGroups.isEmpty) {
      println("\n[3. Most Profitable Hotel Analysis]")
      println("- No valid hotel groups found.")
      return
    }

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