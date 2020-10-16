
import io.Source
import scala.util._
import java.text.SimpleDateFormat
import java.util.Date
import java.time.LocalDate
import question._

object quantexa {
    def main(args: Array[String]) = {

        val flightData = "flightData.csv"
        val passengers = "passengers.csv"
        val dateFormat = "yyyy-MM-dd";
        val dtf = new SimpleDateFormat(dateFormat)

        //Make a list from CSV file and drop first denomination line
        def get_csv(file: String) : List[String] = {
            Source.fromFile(file).getLines.toList.drop(1)
        }




        //Q4 get the passengers who have been on more than N flights together.
        //INPUT: A list of flights taken by people, atLeastNTimes, a minimum number of times two people must fly together to qualify
        //OUTPUT: A list of Tuples(ID, ID) with the associated amount of times this pair flew together 
        def flights_together(flights: List[String], atLeastNTimes: Int): List[((Int,Int), Int)] = {
            //Get a map of FlightID => List(FlyerIDs sorted ascending)
            val flyersMap = flights.groupBy(_.split(",")(1).toInt).map{ 
                case (k,v) => (k, v.map {
                    _.split(",").toList.take(1)
                    }.flatten.toSeq.sortWith(_ < _).toList)
            }
            //Get all the instance pairs of travelers together
            val allPairs = get_all_flight_pairs(flyersMap, Map[(Int, Int), Int]())
            //Count the amount of times each pair occurs
                            //val countedMap = allPairs.groupBy(pair => (pair._1, pair._2)).mapValues(_.size)
            //Filter so we have at least N flights together and sort by most to least flights a pair has done
            allPairs.toSeq.filter(atLeastNTimes < _._2).sortWith(_._2 > _._2).toList    
        }

        //Recursive function to get for each flight the pairs of people on that flight
        //INPUT: flyersMap mapping a flight to a list of flyer ID's, an (initially empty) accumulator
        //OUTPUT: A list of all the pairs of (ID, ID) having traveled together on all flights
        //Duplicates of pairs if multiple travels together
        def get_all_flight_pairs(flyersMap: Map[Int,List[String]], accumulator: Map[(Int, Int), Int]): Map[(Int, Int), Int] = { 
            if(flyersMap.isEmpty){
                return accumulator
            } 
            else {
                //Get all flyer id pairs associated with a flight id
                val flightPairs = get_pairs_of_flight(flyersMap.head._2)
                //Transform these pairs of ids into a map (id, id) -> 1 as they occur once in a flight
                val flightPairMap = flightPairs.map{case x => (x, 1)}
                //Add this map to the accumulator to keep track of the number of times a pair has occured
                val updatedAccumulator = accumulator ++ flightPairMap.map{ case (k,v) => k -> (v + accumulator.getOrElse(k,0)) }
                return get_all_flight_pairs(flyersMap.tail, updatedAccumulator)
            }
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



/*

        //Challenge Q: get the passengers who have been on more than N flights together within a date range
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
            //Get all the instance pairs of travelers together
            val allPairs = get_all_flight_pairs(flyersMap, List[(Int, Int)]())
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

*/



        //Q4 print output to q4 in format ((flyer ID, flyer ID), Number of flights together)
        println(flights_together(get_csv(flightData), 3))

        //Bonus Q print output in format ((flyer ID, flyer ID), Number of flights together)
        //Code duplication due to the creation of two nearly identical functions, but the assignment requires it
        //println(flights_together_dates(get_csv(flightData), 3, dtf.parse("2017-01-01"), dtf.parse("2017-01-10")))

        
        //val ans1 = new Q1(get_csv(flightData))
        //val ans2 = new Q2(get_csv(flightData), get_csv(passengers))
        //val ans3 = new Q3(get_csv(flightData))
    }
}

