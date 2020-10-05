import io.Source
import scala.util._


object quantexa {
    def main(args: Array[String]) = {

        val fileName = "flightData copy.csv"
        val dateFormat = "yyyy-MM-dd"
        //val dtf = java.time.format.DateTimeFormatter.ofPattern(dateFormat)
        //java.time.LocalDate.parse((_.split(",")(4)), dtf).getMonthValue


        //Make a list from CSV file and drop first denomination line
        def get_all_flights(file: String) : List[String] = {
            Source.fromFile(file).getLines.toList.drop(1)
        }

        //Q1 Find the total number of flights for each month.
        def total_flights_month(file: String) : Map[String, Int] = {
            get_all_flights(file).groupBy(_.split(",")(4).substring(5).dropRight(3)).mapValues(_.size)
        }

        //Q2 Find the top x frequent flyers
        def frequent_flyers(file: String, amount: Int) : Seq[(String, Int)] = {
            get_all_flights(file).groupBy(_.split(",")(0)).mapValues(_.size).toSeq.sortWith(_._2 > _._2).toList
        }


        //println(total_flights_month(fileName))
        println(frequent_flyers(fileName, 100))
    }
}

