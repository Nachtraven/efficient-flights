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

        
        




        def get_pairs_of_flight(flightList: List[String]): List[Seq[Int]] = { //Map[(Int, Int) -> Int] , currentIndex: Int
            /*
            if (flightList.length > 0){
                flightListTail.map{  x =>  Seq(flightListHead.toInt, x.toInt)  } ++ get_pairs_of_flight(flightListTail)
            } else List[Seq(Int, Int)]
            */
            flightList match{
                case Nil => List[Seq[Int]]()
                case flightListHead::flightListTail => {
                    flightListTail.map{  x =>  Seq(flightListHead.toInt, x.toInt)  } ::: get_pairs_of_flight(flightListTail)
                    //get_pairs(flightListTail, pairList)
                    /*
                    val wowmap = flightListTail.map{  x =>  ((flightListHead, x), 1)  }
                    wowmap.map( x => (x._1 = x._1 +1) )
                    (x => (, 1))
                    println(wowmap)
                    get_pairs(flightListTail, pairListUpdated, currentIndex + 1)
                    */
                }
            }
            /*
            var currIndex = 0
            var pariListVar = pairList
            for (flyer <- flightList){
                for (secondFlyer <- flightList.dropLeft(currIndex)){
                    if (pariListVar.getOrElse((flyer, secondFlyer), -1) != -1){
                        //We got it in the list
                        pariListVar = pariListVar.get((flyer, secondFlyer)) 
                    }else 
                }
                currIndex = currIndex + 1
            }
            */
        }


        def get_all_flight_pairs(flyersMap: Map[Int,List[String]], accumulator: List[Seq[Int]]): List[Seq[Int]] = { //
            if(flyersMap.isEmpty) return accumulator
            else return get_all_flight_pairs(flyersMap.tail, accumulator ::: get_pairs_of_flight(flyersMap.head._2))
/*
            get_all_flight_pairs()

            for ((k,v) <- flyersMap){
                println(get_pairs_of_flight(v))
                //pairList = get_pairs(v, pairList, 0)
            }
*/
        }


        //Q4
        def flights_together(flyers: List[String], atLeastNTimes: Int): Unit = {
            //Get a map of FlightID => FlyerIDs
            val flyersMap = flyers.take(5).groupBy(_.split(",")(1).toInt).map{ 
                case (k,v) => (k, v.map {
                    _.split(",").toList.take(1)
                    }.flatten)
            }
            println(flyersMap)
            println("--------")
            
            val allPairs = get_all_flight_pairs(flyersMap, List[Seq[Int]]())
            println(allPairs)
        }




        //Q1
        //println(total_flights_month(get_all_csv(flightData)))
        println("---")

        //Q2
        //println(associate_name_id(get_map(get_all_csv(passengers)), frequent_flyers(get_all_csv(flightData), 100)))
        println("---")

        //Q3
        //println(greatest_number_countries(get_all_csv(flightData), "uk", 3))
        println("---")


        //Q4
        flights_together(get_all_csv(flightData), 5)
    }
}

