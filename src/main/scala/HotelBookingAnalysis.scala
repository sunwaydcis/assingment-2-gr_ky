import scala.io.Source
import java.io.File
import scala.language.postfixOps

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
}

object Utils {
  def safeToDouble(str: String): Double =
    try str.toDouble catch { case _: Throwable => 0.0}

  def safeParseDiscount(discountStr: String): Double = {
    try {
      discountStr.replace("%", "").trim.toDouble / 100
    } catch {
      case _: Throwable => 0.0
    }
  }
}

// 2. Encalsulation: Data Processor

class HotelDataProcessor(filePatch: String) {
  import Utils._

  private val processedData: List[Booking] = loadData()

  def getAllBooking: List[Booking] = processedData

  private def loadData() : List[Booking] = {
    try {
      val lines = Source.fromFile(filePath).getLines().drop(1)
      lines.toList.flatMap { line =>
        val cols = line.split(",(?=([^\"]\"[^\"]\")[^\"]$)").map(_.trim)

        if (cols.length >= 24){
          try {
            val hotel = cols(16).replaceAll("\"", "")
            val country = cols(6).replaceAll("\"", "")
            val price = safeToDouble(cols(20))
            val discountRatio = safeParseDiscount(cols(21))
            val profit = safeToDouble(cols(23))

            if(country.nonEmpty && price > 0)
              Some(Booking(hotel,country,price,discountRatio,profit))
            else
              None
          } catch {
            case _: Throwable => None
          }
        } else {
          None
        }
      }
    }catch {
      case e: Exception =>
        println(s"Error loading data: ${e.getMessage}")
        List.empty[Booking]
    }
  }
}

//3. Polymorphism: Analysis Traits and Implementations

trait AnalysiaReport {
  def analyze(bookings: List[Booking]): Unit
}

//Q1: Country with the highest number of bookings
class CountryAnalysis extends AnalysisReport {
  def analyze(bookings: List[Booking]): Unit = {
    val result = bookings
      .groupBy(_.originCountry)
      .mapValues(_.size)
      .maxByOption { case (_, count) => count }
    
    println("\n[1. Highest Number of Bookings Analysis")
    result match {
      case Some((country, count)) =>
        println(s" - Country: *$country*")
        println(s" - Country: *$count*")
      case None => println("- No data available.")
    }
  }
}