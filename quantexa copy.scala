import io.Source
import scala.util._


object quantexa {
    def main(args: Array[String]) = {
        val symbol = "flightData.csv"

        //Q1
        def total_flights_month(symbol: String, year: Int) : List[(String, Int)] = {
            val lines = Source.fromFile(symbol).getLines.toList
            //We have the lines, we need to filter the list to acquire all the unique flightId's and their date, then count for each month the amount of different unique flights
            
        }

        //Q2
        def frequent_flyers() : Void = {
            
        }

        //Q3
        def number_countries() : Void = {

        }

        //Q4
        def flights_together() : Void = {
            
        }

    }
}

