import io.Source
import scala.util._


object quantexa {
    def main(args: Array[String]) = {

        val flightData = "flightData.csv"
        val passengers = "passengers.csv"
        val dateFormat = "yyyy-MM-dd"

        //Make a list from CSV file and drop first denomination line
        def get_all_csv(file: String) : List[String] = {
            Source.fromFile(file).getLines.toList.drop(1)
        }

        //Q1 Find the total number of flights for each month. This is a method that would not work if different years were used
        def total_flights_month(file: String) : Map[String, Int] = {
            get_all_csv(file).groupBy(_.split(",")(4).substring(5).dropRight(3)).mapValues(_.size)
        }



        //Q2 Find the top x frequent flyers
        def frequent_flyers(file: String, amount: Int) : Map[Int, Int] = {
            get_all_csv(file).groupBy(_.split(",")(0).toInt).mapValues(_.size).toSeq.sortWith(_._2 > _._2).toList.take(amount).toMap
        }

        //Create map of ID->List[ID, VALUE, ...] from a list of the form List[String] where String: "ID, VAL, VAL, ..."
        def get_map(list: List[String]): Map[Int, List[String]] = {
            list.groupBy(_.split(",")(0).toInt)
        }

        //Associate ID's and flights with ID's and names, return List[Flights, ID, NAME] 
        def associate_name_id(passengers: Map[Int, List[String]], flyers: Map[Int, Int]): Unit = {
            println(flyers.map { case (k, v) => (k, (v, passengers.getOrElse(k, ""))) }.values)
        }


        //Q3
        def country_hops(base: String, )

        //Q1
        //println(total_flights_month(flightData))


        //Q2
        //println(frequent_flyers(flightData, 4))
        //println(get_map(get_all_csv(passengers)))
        associate_name_id(get_map(get_all_csv(passengers)), frequent_flyers(flightData, 4))
    }
}

