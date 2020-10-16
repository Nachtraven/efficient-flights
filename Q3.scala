package question

import scala.util._


class Q3 (val flightList: List[String]) {

    //Q3 get longest sequence of hops excluding a certain country tag
    //We assume that nobody skips borders with a method other than an airplane

    //INPUT: A list of String[Flight], a "base" country to skip on and a minimum number of hops
    //OUTPUT: a List[(Int, Int)] linking a flyer ID with an amount of hops
    def greatest_number_countries(flyers: List[String], base: String, numberHops: Int): List[(Int, Int)] = {
        //Map id -> flights grouped by flyer id
        val flyersMap = flyers.groupBy(_.split(",")(0).toInt)
        
        country_hops(flyersMap.toList, base, List[(Int, Int)]())
        //Filter the output to take all those with more than n hops and order by descending amount of hops
        .toSeq.filter(_._2 > numberHops).sortWith(_._2 > _._2).toList
    }


    //Recursive function to iterate over each flyer ID and count the amount of different countries visited
    //INPUT: A list of Strings containing (ID, List[Flights]), a "base" country to skip on and an accumulator
    //OUTPUT: a List[(Int, Int)] linking flyer ID's with an amount of hops
    def country_hops(flyers: List[(Int, List[String])], base: String, accumulator: List[(Int, Int)]): List[(Int, Int)] = {
        flyers match{
            case Nil => accumulator
            case flyersHead::flyersTail => {
                //For this flyer, collect in a the order of hops
                val seqOfCountries = for (flight <- flyersHead._2) yield {
                    flight.split(",").toList.drop(2).take(2)
                }
                //Update the accumulator with the current flyer id, longest sequence
                val updatedAccumulator = (flyersHead._1, get_longest_sequence(seqOfCountries, base, List[String](), 0)) :: accumulator
                return (country_hops(flyersTail, base, updatedAccumulator))
            }
        }
    }

    
    //Recursively get the sequence of countries visited by this flyer ID
    //INPUT: A list of lists of countries; List(List(be, uk), List(uk, de)), a country tag to skip on, an accumulator
    //OUTPUT: the length of the longest sequence
    def get_longest_sequence(sequence: List[List[String]], skip: String, accumulator: List[String], longest: Int): Int ={
        sequence match{
            case Nil => {
                if(accumulator.size > longest) return accumulator.size
                else return longest
            }
            
            case sequenceHead::sequenceTail => {
                //If departure and arrival are different
                if(sequenceHead(0) != sequenceHead(1)){
                    
                    //If the departure and arrival aren't the location to skip
                    if (sequenceHead(0) != skip && sequenceHead(1) != skip){

                        //Check if we've already been here
                        if (!accumulator.contains(sequenceHead(0)) && !accumulator.contains(sequenceHead(1))){
                            val updatedAccumulator = sequenceHead(0) :: sequenceHead(1) :: accumulator
                            return get_longest_sequence(sequenceTail, skip, updatedAccumulator, longest)
                        }

                        //We've seen one of the two locations before
                        else {
                            //check if seen is first
                            if (!accumulator.contains(sequenceHead(0))){
                                val updatedAccumulator = sequenceHead(0) :: accumulator
                                return get_longest_sequence(sequenceTail, skip, updatedAccumulator, longest)
                            }
                            //It's the second
                            else {
                                val updatedAccumulator = sequenceHead(1) :: accumulator
                                return get_longest_sequence(sequenceTail, skip, updatedAccumulator, longest)
                            }
                        }
                    }

                    //One of the two locations is a skip
                    else {
                        //Check if it's the departure or arrival

                        //Departure is skip
                        if (sequenceHead(0) == skip){
                            //If up until now the sequence was longer than the previously longest sequence, this is the new longest. 
                            //Set longest to the length of the sequence then reinitialize it
                            if(accumulator.size > longest){
                                return get_longest_sequence(sequenceTail, skip, sequenceHead(1) :: List[String](), accumulator.size) 
                            }
                            //The previous sequence wasn't the longest, reinitialize sequence and move on
                            else {
                                return get_longest_sequence(sequenceTail, skip, sequenceHead(1) :: List[String](), longest) 
                            }
                        }

                        //Arrival is skip
                        if (sequenceHead(1) == skip){
                            //If up until now the sequence was longer than the previously longest sequence, this is the new longest. 
                            //Set longest to the length of the sequence then reinitialize it
                            if(accumulator.size > longest){
                                return get_longest_sequence(sequenceTail, skip, List[String](), accumulator.size) 
                            }
                            //The previous sequence wasn't the longest, reinitialize sequence and move on
                            else {
                                return get_longest_sequence(sequenceTail, skip, List[String](), longest) 
                            }
                        }

                        //If neither are skip, we have a problem
                        println(" ERR ")
                        return get_longest_sequence(sequenceTail, skip, List[String](), 0)
                    }
                }




                //If departure and arrival are the same.
                else {
                    //If the pair is a skip
                    if (sequenceHead(1) != skip){
                        //Return a reinitialized list and the current longest list
                        return get_longest_sequence(sequenceTail, skip, List[String](), longest)
                    }


                    //If the pair isn't a skip
                    else {
                        //If the pair is the first thing we see, the size will be zero and we must add it
                        if (accumulator.size == 0){
                            val updatedAccumulator = sequenceHead(0) :: accumulator
                            return get_longest_sequence(sequenceTail, skip, updatedAccumulator, longest)
                        }
                        //The pair isn't first meaning we got here from another location, drop this pair and move on
                        else{
                            return get_longest_sequence(sequenceTail, skip, accumulator, longest)
                        }
                    }
                }
            }
        }
    }

    




    //Output result
    println("Q3")
    println(greatest_number_countries(flightList, "uk", 3))
}