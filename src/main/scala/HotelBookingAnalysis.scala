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
                  bookingPrice: Double,
                  discount: Double,
                  profitMargin: Double
) {
  def netCustomerCost: Double = bookingPrice * (1.0 - discount)
  def profitPerBooking: Double = bookingPrice * profitMargin
}

object Utils {
  def safeToDouble(str: String): Double =
    try str.toDouble catch { case _: Throwable => 0.0}

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

  private val processedData: List[Booking] = loadData()

  def getAllBookings: List[Booking] = processedData

  private def loadData(): List[Booking] = {
    val file = new File(filePath)

    if (!file.exists()) {
      println(s"CRITICAL ERROR: File not found at path: ${file.getAbsolutePath}")
      return List.empty[Booking]
    }

    println(s"SUCCESS: Found file at: ${file.getAbsolutePath}")

    var reader: BufferedReader = null
    try {
      reader = new BufferedReader(new FileReader(file))

      val lines = Iterator.continually(reader.readLine()).takeWhile(_ != null).drop(1).toList

      if (lines.isEmpty) {
        println("CRITICAL ERROR: File is empty or reading failed after finding file.")
        return List.empty[Booking]
      }
      lines.flatMap { line =>
        val cols = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)").map(_.trim)

        if (cols.length >= 24) {
          try {
            val hotel = cols(16).replaceAll("\"", "")
            val country = cols(6).replaceAll("\"", "")
            val price = safeToDouble(cols(20))
            val discountRatio = safeParseDiscount(cols(21))
            val profit = safeToDouble(cols(23))

            if (country.nonEmpty && price > 0)
              Some(Booking(hotel, country, price, discountRatio, profit))
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
        println(s"Error loading data: ${e.getMessage}")
        List.empty[Booking]
    } finally {
      if (reader != null) reader.close()
    }
  }
}

//3. Polymorphism: Analysis Traits and Implementations

trait AnalysisReport {
  def analyze(bookings: List[Booking]): Unit
}

//Q1: Country with the highest number of bookings
class CountryAnalysis extends AnalysisReport {
  def analyze(bookings: List[Booking]): Unit = {
    val result = bookings
      .groupBy(_.originCountry)
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
  def analyze(bookings: List[Booking]):Unit={
    val result = bookings
      .groupBy(_.hotelName)
      .mapValues(list => list.map(_.netCustomerCost).min)
      .minByOption {case(_,minNetCost)=> minNetCost}

    println("\n[2. Most Economical Hotel Analysis]")
    result match{
      case Some((hotel,cost))=>
        println(f"-Hotel: **$hotel**")
        println(f"-Minimum Net Customer Cost: **SGD $cost%.2f**")
      case None => println("-No data available.")
    }
  }
}

// Q3: Most profitable hotel
class ProfitAnalysis extends AnalysisReport{
  def analyze(bookings:List[Booking]): Unit={
    val result = bookings
      .groupBy(_.hotelName)
      .mapValues(hotelBookings => hotelBookings.map(_.profitPerBooking).sum)
      .maxByOption{case(_, totalProfit)=>totalProfit }

    println("\n[3. Most Profitable Hotel Analysis]")
    result match{
      case Some((hotel, profit))=>
        println(f"-Hotel: **$hotel**")
        println(f"-Total Estimated Profit: **SGD $profit%.2f**")
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