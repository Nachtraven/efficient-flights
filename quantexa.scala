import io.Source
import scala.util._


object quantexa {
    def main(args: Array[String]) = {

        val fileName = "flightData.csv"
        val dateFormat = "yyyy-MM-dd"
        val dtf = java.time.format.DateTimeFormatter.ofPattern(dateFormat)



        //Make a list from CSV file and drop first denomination line
        def get_all_flights(file: String) : List[String] = {
            Source.fromFile(file).getLines.toList.drop(1)
        }

        //Q1 Find the total number of flights for each month.
        def total_flights_month(file: String) : Map[String, Int] = {
            get_all_flights(file).groupBy(_.split(",")(4).substring(5).dropRight(3)).mapValues(_.size)
        }

        //java.time.LocalDate.parse((_.split(",")(4)), dtf).getMonthValue
        println(total_flights_month(fileName))
    }
}

