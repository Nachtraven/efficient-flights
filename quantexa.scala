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
        def total_flights_month(flights: List[String]) : List[(String, Int)] = {
            flights.groupBy(_.split(",")(4).substring(5).dropRight(3)).mapValues(_.size).toSeq.sortWith(_._1 < _._1).toList
        }




        //Q2 Find the top n = amount frequent flyers
        //INPUT: a List of flights, amount of flyers to take
        //OUTPUT: A map [Flyer ID -> amount of flights]
        def frequent_flyers(flights: List[String], amount: Int) : Map[Int, Int] = {
            flights.groupBy(_.split(",")(0).toInt).mapValues(_.size).toSeq.sortWith(_._2 > _._2).toList.take(amount).toMap
        }

        //Create map of ID->List[ID, VALUE, ...] from a list of the form List[String] where String: "ID, VAL, VAL, ..."
        def get_map(list: List[String]): Map[Int, List[String]] = {
            list.groupBy(_.split(",")(0).toInt)
        }

        //Associate ID's and names with ID's and flights, return Map[Int, List[String]] where string is flights, ID, name
        def associate_name_id(passengers: Map[Int, List[String]], flyers: Map[Int, Int]): Map[Int, List[String]] = {
            flyers.map{ case (k,v) => (k -> (v.toString :: (passengers.getOrElse(k, List.empty)) )) }
        }






        //Q3 get longest sequence of hops excluding a certain country tag
        //We assume that flight data is organised chronologically, that nobody skips borders with a method other than an airplane
        //INPUT: A list of String[Flight], a "base" country to skip on and a minimum number of hops
        //OUTPUT: a List[(Int, Int)] linking a flyer ID with an amount of hops
        def greatest_number_countries(flyers: List[String], base: String, numberHops: Int): List[(Int, Int)] = {
            //map id -> flights grouped by flyer id
            val flyersMap = flyers.groupBy(_.split(",")(0).toInt)
            country_hops(flyersMap.toList, base, List[(Int, Int)]()).toSeq.filter(_._2 > numberHops).sortWith(_._2 > _._2).toList
        }

        //Get sequence of countries visited by this flier
        //Recursion to collect all visited countries by an ID
        def get_visited_countries(sequence: List[List[String]], skip: String, accumulator: List[String]): List[String] ={
            sequence match{
                case Nil => accumulator
                case sequenceHead::sequenceTail => {
                    //If skip is neither the departure or arrival country and the departure is different to arrival
                    if ((sequenceHead(0) != skip && sequenceHead(1) != skip) && sequenceHead(0) != sequenceHead(1)) {
                        //Check if we've already been here
                        if (!accumulator.contains(sequenceHead(0)) && !accumulator.contains(sequenceHead(1))){
                            val updatedAccumulator = sequenceHead(0) :: sequenceHead(1) :: accumulator
                            get_visited_countries(sequenceTail, skip, updatedAccumulator)
                        }
                        //We've seen one of the two locations before
                        else if (!accumulator.contains(sequenceHead(0))){
                            val updatedAccumulator = sequenceHead(0) :: accumulator
                            get_visited_countries(sequenceTail, skip, updatedAccumulator)
                        }
                        else if (!accumulator.contains(sequenceHead(1))){
                           val updatedAccumulator = sequenceHead(1) :: accumulator
                           get_visited_countries(sequenceTail, skip, updatedAccumulator)
                        }
                        //We've seen both locations
                        else return get_visited_countries(sequenceTail, skip, accumulator)
                    }

                    //If skip is neither the departure or arrival country and the departure is identical to arrival
                    else if((sequenceHead(0) != skip && sequenceHead(1) != skip) && (sequenceHead(0) == sequenceHead(1))){
                        if (!accumulator.contains(sequenceHead(0))){
                            val updatedAccumulator = sequenceHead(0) :: accumulator
                            get_visited_countries(sequenceTail, skip, updatedAccumulator)
                        } 
                        //We've seen this location before
                        else return get_visited_countries(sequenceTail, skip, accumulator)
                    } 
                    else return get_visited_countries(sequenceTail, skip, accumulator)
                }
            }
        }

        //Recursive function to iterate over each flyer ID
        //INPUT: A list of Strings containing (ID, List[Flights]), a "base" country to skip on and an accumulator
        //OUTPUT: a Map[Int, Int] linking flyer ID's with an amount of hops
        def country_hops(flyers: List[(Int, List[String])], base: String, accumulator: List[(Int, Int)]): List[(Int, Int)] = {
            flyers match{
                case Nil => accumulator
                case flyersHead::flyersTail => {
                    //For this flyer, collect in a list the order of hops
                    val seqOfCountries = for (flight <- flyersHead._2) yield {
                        flight.split(",").toList.drop(2).take(2)
                    }
                    val updatedAccumulator = (flyersHead._1, get_visited_countries(seqOfCountries, base, List[String]()).size) :: accumulator
                    return (country_hops(flyersTail, base, updatedAccumulator))
                }
            }
        }

        
        



        //Q4 get the passengers who have been on more than N flights together.
        //INPUT: A list of flights taken by people, atLeastNTimes, a minimum number of times two people must fly together to qualify
        //OUTPUT: A list of Tuples(ID, ID) with the associated amount of times this pair flew together 
        def flights_together(flights: List[String], atLeastNTimes: Int): List[((Int,Int), Int)] = {
            //Get a map of FlightID => sorted FlyerIDs
            val flyersMap = flights.take(50000).groupBy(_.split(",")(1).toInt).map{ 
                case (k,v) => (k, v.map {
                    _.split(",").toList.take(1)
                    }.flatten.toSeq.sortWith(_ < _).toList)
            }
            //Get all the instance pairs of travelers together
            val allPairs = get_all_flight_pairs(flyersMap, List[(Int, Int)]())
            //Count the amount of times each pair occurs
            val countedMap = allPairs.groupBy(pair => (pair._1, pair._2)).mapValues(_.size)
            //Filter so we have at least N flights together and sort by most to least flights a pair has done
            countedMap.toSeq.filter(atLeastNTimes < _._2).sortWith(_._2 > _._2).toList          
        }

        //Recursive function to get all the pairs of people on a flight
        //INPUT: A List of IDs on a flight
        //OUTPUT: A list of pairs (ID, ID) of all people on the flight
        def get_pairs_of_flight(flightList: List[String]): List[(Int, Int)] = {
            flightList match{
                case Nil => List[(Int, Int)]()
                case flightListHead::flightListTail => {
                    flightListTail.map{  x =>  (flightListHead.toInt, x.toInt)  } ::: get_pairs_of_flight(flightListTail)

                }
            }
        }

        //Recursive function to get for each flight the pairs of people on that flight
        //INPUT: flyersMap mapping a flight to a list of flyer ID's, an (initially empty) accumulator
        //OUTPUT: A list of all the pairs of (ID, ID) having traveled together on all flights
        //Duplicates of pairs if multiple travels together
        def get_all_flight_pairs(flyersMap: Map[Int,List[String]], accumulator: List[(Int, Int)]): List[(Int, Int)] = { 
            if(flyersMap.isEmpty) return accumulator
            else return get_all_flight_pairs(flyersMap.tail, accumulator ::: get_pairs_of_flight(flyersMap.head._2))
        }


        




        //Q1
        println(total_flights_month(get_all_csv(flightData)))
        println("---")

        //Q2
        println(associate_name_id(get_map(get_all_csv(passengers)), frequent_flyers(get_all_csv(flightData), 100)))
        println("---")

        //Q3
        println(greatest_number_countries(get_all_csv(flightData), "uk", 3))
        println("---")


        //Q4
        println(flights_together(get_all_csv(flightData), 3).take(100))
    }
}

