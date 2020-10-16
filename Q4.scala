package question

import scala.util._

import java.text.SimpleDateFormat
import java.util.Date


class Q4 (val flightList: List[String]) {

    val dateFormat = "yyyy-MM-dd";
    val dtf = new SimpleDateFormat(dateFormat)

    //Q4/Challenge Q: get the passengers who have been on more than N flights together within a date range
    //INPUT: A list of flights taken by people, atLeastNTimes, a minimum number of times two people must fly together to qualify, and from, to dates
    //OUTPUT: A list of Tuples(ID, ID) with the associated amount of times this pair flew together 
    def flights_together_dates(flights: List[String], atLeastNTimes: Int, from: Date, to: Date): List[((Int,Int), Int)] = {
        val flightsWithinRange = get_flights_within_range(flights, from, to)

        //Get a map of FlightID => sorted FlyerIDs
        val flyersMap = flightsWithinRange.groupBy(_.split(",")(1).toInt).map{
            case (k,v) => (k, v.map {
                _.split(",").toList.take(1)
                }.flatten.toSeq.sortWith(_ < _).toList)
        }

        //Get all the pairs of travelers having traveled together for all flights
        val allPairs = get_pairs_all_flights(flyersMap, List[(Int, Int)]())

        //Count the amount of times each pair occurs
        val countedMap = allPairs.groupBy(pair => (pair._1, pair._2)).mapValues(_.size)

        //Filter so we have at least N flights together and sort by most to least flights a pair has done
        countedMap.toSeq.filter(atLeastNTimes < _._2).sortWith(_._2 > _._2).toList          
    }

    //INPUT: A list of flights from and to dates
    //OUTPUT: A list of flights within the date range
    def get_flights_within_range(flights: List[String], from: Date, to: Date): List[String] = {
        flights.map{ x => if (!dtf.parse(x.split(",")(4)).before(from) && !dtf.parse(x.split(",")(4)).after(to)) {
                Some(x)
            } else {
                None
            }
        }.flatten
    }


    //Recursive function to go over each flight and accumulate all pairs for all flights
    //INPUT: flyersMap mapping a flight to a list of flyer ID's, an (initially empty) accumulator
    //OUTPUT: A list of all the pairs of (ID, ID) having traveled together on all flights
    //Duplicates of pairs if multiple travels together
    def get_pairs_all_flights(flyersMap: Map[Int,List[String]], accumulator: List[(Int, Int)]): List[(Int, Int)] = { 
        if(flyersMap.isEmpty) {
            return accumulator
        }
        else {
            return get_pairs_all_flights(flyersMap.tail, accumulator ::: get_pairs_on_flight(flyersMap.head._2))
            }
    }


    //Recursive function to get all the pairs of people on a flight
    //INPUT: A List of IDs on a flight
    //OUTPUT: A list of pairs (ID, ID) of all people on the flight
    def get_pairs_on_flight(flightList: List[String]): List[(Int, Int)] = {
        flightList match{
            case Nil => List[(Int, Int)]()
            case flightListHead::flightListTail => {
                //We have people on the flight left
                flightListTail.map{  
                    //Pair the current element with each of the following elements
                    x =>  (flightListHead.toInt, x.toInt)  
                    //And append this to the list of all pairs
                    } ::: get_pairs_on_flight(flightListTail)

            }
        }
    }

    


    //Output result
    println("Q4/Bonus")
    println(flights_together_dates(flightList, 3, dtf.parse("2017-01-01"), dtf.parse("2017-03-01")))
}