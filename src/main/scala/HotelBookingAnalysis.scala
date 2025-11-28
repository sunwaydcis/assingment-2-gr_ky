import scala.io.{Source, Codec}
import scala.util.{Try, Success, Failure}
import java.io.File
import java.nio.charset.CodingErrorAction

// --- DATA REPRESENTATION ---

/**
 * Case Class to represent a single row of the Hotel Dataset.
 * Utilizing Case Class as per 'Advanced class in Scala' slides for immutable data holding.
 */
case class Booking(
                    bookingId: String,
                    destinationCountry: String,
                    numberOfPeople: Int,
                    hotelName: String,
                    bookingPrice: Double,
                    discount: Double,
                    profitMargin: Double
                  )

// --- DATA LOADER OBJECT ---

object DataLoader:
  /**
   * Reads the CSV file and converts it into a List of Booking objects.
   * Handles the '%' in the discount column and parses numbers safely.
   */
  def loadData(filePath: String): List[Booking] =
    // --- FIX FOR MALFORMED INPUT ERROR ---
    // This tells Scala to read the file using an encoding that supports special characters
    implicit val codec: Codec = Codec("ISO-8859-1")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val bufferedSource = Source.fromFile(filePath)
    try
      val lines = bufferedSource.getLines().toList
      // Drop header row and map the rest
      lines.drop(1).flatMap { line =>
        val cols = line.split(",").map(_.trim)

        // Safety check for column count (dataset has 24 columns)
        if cols.length >= 24 then
          try
            // Parse Discount: Remove '%' and convert to decimal
            val discountStr = cols(21).replace("%", "")
            val discountVal = discountStr.toDouble / 100.0

            Some(Booking(
              bookingId = cols(0),
              destinationCountry = cols(9),
              numberOfPeople = cols(11).toInt,
              hotelName = cols(16),
              bookingPrice = cols(20).toDouble,
              discount = discountVal,
              profitMargin = cols(23).toDouble
            ))
          catch
            case e: Exception =>
              // Skip malformed rows silently or log if needed
              None
        else
          None
      }
    finally
      bufferedSource.close()
    end try
  end loadData
end DataLoader

// --- ANALYZER CLASS ---

/**
 * Class responsible for performing EDA on the bookings list.
 * Demonstrates use of Collection API (groupBy, map, maxBy, minBy).
 */
class HotelAnalyzer(bookings: List[Booking]):

  // Question 1: Country with the highest number of bookings
  def getTopDestinationCountry(): (String, Int) =
    val grouped = bookings.groupBy(_.destinationCountry)
    val counted = grouped.map { (country, list) => (country, list.size) }
    counted.maxBy(_._2)
  end getTopDestinationCountry

  // Question 2a: Most economical Hotel based on Booking Price (Lowest Price)
  def getEconomicalHotelByPrice(): Booking =
    bookings.minBy(_.bookingPrice)
  end getEconomicalHotelByPrice

  // Question 2b: Most economical Hotel based on Discount (Highest Discount)
  def getEconomicalHotelByDiscount(): Booking =
    bookings.maxBy(_.discount)
  end getEconomicalHotelByDiscount

  // Question 2c: Most economical Hotel based on Profit Margin (Lowest Margin is better for customer)
  def getEconomicalHotelByMargin(): Booking =
    bookings.minBy(_.profitMargin)
  end getEconomicalHotelByMargin

  // Question 3: Most profitable hotel considering visitors and margin
  // Logic: Calculate Total Profit ($) for each hotel = Sum of (Booking Price * Profit Margin)
  def getMostProfitableHotel(): (String, Double) =
    val grouped = bookings.groupBy(_.hotelName)

    val hotelProfits = grouped.map { (hotel, bookingList) =>
      val totalProfit = bookingList.map(b => b.bookingPrice * b.profitMargin).sum
      (hotel, totalProfit)
    }

    hotelProfits.maxBy(_._2)
  end getMostProfitableHotel

end HotelAnalyzer

// --- MAIN EXECUTION ---

object HotelApp:
  def main(args: Array[String]): Unit =
    println("--- Hotel Booking Data Analysis ---")

    val filename = "Hotel_Dataset.csv"

    // --- DEBUGGING: FILE PATH CHECK ---
    val file = new File(filename)
    println(s"Current Working Directory: ${System.getProperty("user.dir")}")
    println(s"Looking for file at: ${file.getAbsolutePath}")

    if !file.exists() then
      println("\n[ERROR] File not found!")
      println("Please move 'Hotel_Dataset (1).csv' into the folder printed above.")
    else
      // If file exists, proceed with loading
      val bookings = DataLoader.loadData(filename)

      if bookings.isEmpty then
        println("Error: No data loaded. Please check the file content.")
      else
        val analyzer = new HotelAnalyzer(bookings)

        // 1. Top Destination Country
        val (topCountry, count) = analyzer.getTopDestinationCountry()
        println(s"\n1. Country with highest number of bookings:")
        println(s"   Result: $topCountry ($count bookings)")

        // 2. Economical Options
        println(s"\n2. Hotel offering the most economical option based on:")

        val bestPrice = analyzer.getEconomicalHotelByPrice()
        println(s"   a) Booking Price (Lowest): ${bestPrice.hotelName} (SGD ${bestPrice.bookingPrice})")

        val bestDiscount = analyzer.getEconomicalHotelByDiscount()
        println(s"   b) Discount (Highest): ${bestDiscount.hotelName} (${bestDiscount.discount * 100}%)")

        val bestMargin = analyzer.getEconomicalHotelByMargin()
        println(s"   c) Profit Margin (Lowest): ${bestMargin.hotelName} (${bestMargin.profitMargin})")

        // 3. Most Profitable
        val (profHotel, profitAmount) = analyzer.getMostProfitableHotel()
        println(s"\n3. Most profitable hotel (considering volume and margin):")
        println(s"   Result: $profHotel (Total Profit: SGD ${f"$profitAmount%.2f"})")

      end if
    end if
  end main
end HotelApp