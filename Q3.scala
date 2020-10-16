package question

import io.Source
import scala.util._


class Q3 (val flightList: List[String]) {

        //Q3 get longest sequence of hops excluding a certain country tag
        //We assume that nobody skips borders with a method other than an airplane
        //INPUT: A list of String[Flight], a "base" country to skip on and a minimum number of hops
        //OUTPUT: a List[(Int, Int)] linking a flyer ID with an amount of hops
        def greatest_number_countries(flyers: List[String], base: String, numberHops: Int): List[(Int, Int)] = {
            //Create a map associating a flyer id -> flights taken
            val flyersMap = flyers.groupBy(_.split(",")(0).toInt)
            country_hops(flyersMap.toList, base, List[(Int, Int)]())
            //Only take those id with more than n hops and sort by descending
            .toSeq.filter(_._2 > numberHops).sortWith(_._2 > _._2).toList
        }


        //Recursive function to iterate over each flyer ID and return the amount of hops the flyer has done
        //INPUT: A list of Strings containing (ID, List[Flights]), a "base" country to skip on and an accumulator
        //OUTPUT: a List[Int, Int] linking flyer ID's with an amount of hops
        def country_hops(flyers: List[(Int, List[String])], base: String, accumulator: List[(Int, Int)]): List[(Int, Int)] = {
            flyers match{
                //No more flyers to iterate over, return the accumulator
                case Nil => accumulator
                case flyersHead::flyersTail => {
                    //For this flyer, iterate over each flight and get the to/from location
                    val seqOfCountries = for (flight <- flyersHead._2) yield {
                        flight.split(",").toList.drop(2).take(2)
                    }
                    //Update the accumulator with the id of the flyer and the length of the sequence of countries visited
                    val updatedAccumulator = (flyersHead._1, get_visited_countries(seqOfCountries, base, List[String](), 0).size) :: accumulator
                    country_hops(flyersTail, base, updatedAccumulator)
                }
            }
        }

        //Get sequence of countries visited by this flier
        //Recursion to collect all visited countries by an ID
        def get_visited_countries(sequence: List[List[String]], skip: String, accumulator: List[String], highest: Int): List[String] ={
            sequence match{
                case Nil => accumulator
                case sequenceHead::sequenceTail => {


                    //If departure and arrival are different
                    if(sequenceHead(0) != sequenceHead(1)){
                        
                        //If the departure and arrival aren't the location to skip
                        if (sequenceHead(0) != skip && sequenceHead(1) != skip){

                            //Check if we've already been here
                            if (!accumulator.contains(sequenceHead(0)) && !accumulator.contains(sequenceHead(1))){
                                val updatedAccumulator = sequenceHead(0) :: sequenceHead(1) :: accumulator
                                return get_visited_countries(sequenceTail, skip, updatedAccumulator, highest)
                            }

                            //We've seen one of the two locations before
                            else {
                                //check if seen is first
                                if (!accumulator.contains(sequenceHead(0))){
                                    val updatedAccumulator = sequenceHead(0) :: accumulator
                                }
                                //It's the second
                                else {
                                    val updatedAccumulator = sequenceHead(1) :: accumulator
                                }

                                //return get_visited_countries(sequenceTail, skip, updatedAccumulator, highest) //CHECK HIGHEST
                            }
                        }

                        //One of the two locations is a skip
                        else {
                            //Check if it's the departure or arrival
                            val newHighest = accumulator.size //THIS IS BAD
                            //Departure
                            if (sequenceHead(0) == skip){
                                val newHighest = accumulator.size
                                val updatedAccumulator = sequenceHead(1) :: List[String]()
                            }
                            //Arrival
                            if (sequenceHead(1) == skip){
                                val updatedAccumulator = sequenceHead(1) :: accumulator
                            }
                            return get_visited_countries(sequenceTail, skip, List[String](), newHighest)
                        }
                    } 


                    //If departure and arrival are the same.
                    else{
                        //If the pair is a skip
                        if (sequenceHead(1) != skip){
                            //Return a reinitialized list and the current longest list
                            return get_visited_countries(sequenceTail, skip, List[String](), highest)
                        }
                        //If the pair isn't a skip
                        else {
                            //If the pair is the first thing we see, the size will be zero and we must add it
                            if (accumulator.size == 0){
                                val updatedAccumulator = sequenceHead(0) :: accumulator
                                return get_visited_countries(sequenceTail, skip, updatedAccumulator, highest)
                            }
                            //The pair isn't first meaning we got here from another location, drop this pair and move on
                            else{
                                return get_visited_countries(sequenceTail, skip, accumulator, highest)
                            }
                        }
                    }

                }
            }
        }





    //Output result
    println(greatest_number_countries(flightList, "uk", 3))
}