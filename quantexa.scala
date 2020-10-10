import io.Source
import scala.util._


object quantexa {
    def main(args: Array[String]) = {

        val flightData = "flightData.csv"
        val passengers = "passengers.csv"

        //Make a list from CSV file and drop first denomination line
        def get_all_csv(file: String) : List[String] = {
            Source.fromFile(file).getLines.toList.drop(1)
        }




        //Q1: Find the total number of flights for each month.
        //INPUT: a List of flights
        //OUTPUT: a Map[Month -> Number of flights]
        //This is a method that would not work if different years were used
        //To be able to have inter year data, one would need to parse the dates into a time format and then count them out
        def total_flights_month(flights: List[String]) : Map[String, Int] = {
            flights.groupBy(_.split(",")(4).substring(5).dropRight(3)).mapValues(_.size)
        }




        //Q2 Find the top n = amount frequent flyers
        //INPUT: a List of flights, amount of flyers to take
        //OUTPUT: A map [Flyer ID -> amount of flights]
        def frequent_flyers(flights: List[String], amount: Int) : Map[Int, Int] = {
            flights.groupBy(_.split(",")(0).toInt).mapValues(_.size).toSeq.sortWith(_._2 > _._2).toList.take(amount).toMap
        }

        //Create map of ID->List[ID, VALUE, ...] from a list of the form List[String] where String: "ID, VAL, VAL, ..."
        def get_map(list: List[String]): Map[Int, List[String]] = {
            list.groupBy( x => x.split(",")(0).toInt)
        }

        //Associate ID's and names with ID's and flights, return List[Flights, ID, NAME]
        def associate_name_id(passengers: Map[Int, List[String]], flyers: Map[Int, Int]): Unit = {
            println((flyers.map{ case (k,v) => (k, (v, passengers.get(k))) }))
        }






        //Tail recursion to collect all visited countries by an ID
        def get_visited_countries(sequence: List[List[String]], skip: String, accumulator: List[String]): List[String] ={
            sequence match{
                case Nil => accumulator
                case sequenceHead::sequenceTail => {
                    //If skip is neither the departure or arrival country
                    if (sequenceHead(0) != skip && sequenceHead(1) != skip) {
                        if (!accumulator.contains(sequenceHead(0))){
                            val updatedAccumulator = sequenceHead(0) :: accumulator
                            return get_visited_countries(sequenceTail, skip, updatedAccumulator)
                        }
                        if (!accumulator.contains(sequenceHead(1))){
                           val updatedAccumulator = sequenceHead(1) :: accumulator
                           return get_visited_countries(sequenceTail, skip, updatedAccumulator)
                        }
                        else return get_visited_countries(sequenceTail, skip, accumulator)
                    }
                    else return get_visited_countries(sequenceTail, skip, accumulator)
                }
            }
        }

        def get_loop_len(flights: List[String], skip: String): List[String] = {
            val seqOfCountries = for (flight <- flights) yield {
                flight.split(",").toList.drop(2).take(2)
            }
            get_visited_countries(seqOfCountries, skip, List[String]())
        }

        //Q3 we want the longest cycle length where "uk" cuts the cycle
        //We assume that flight data is organised chronologically, that nobody skips borders with a method other than an airplane
        //TODO: FIX recursive
        def country_hops(flyers: List[String], base: String): Unit = {
            
            var listOfHops = Map[Int, Int]()
            
            for (flights <- flyers.groupBy(x => x.split(",")(0).toInt)) yield {
                listOfHops += (flights._1 -> get_loop_len(flights._2, base).size)
            }
            println(listOfHops.toSeq.filter(_._2 > 3).toList)
        }








        //Q4
        def flights_together(flyers: List[String]): Unit = {
        }




        //Q1
        println(total_flights_month(get_all_csv(flightData)))
        println("---")

        //Q2
        associate_name_id(get_map(get_all_csv(passengers)), frequent_flyers(get_all_csv(flightData), 100))
        println("---")

        //Q3
        country_hops(get_all_csv(flightData), "uk")
        println("---")


        //Q4
        //flights_together(get_all_csv(flightData))
    }
}

