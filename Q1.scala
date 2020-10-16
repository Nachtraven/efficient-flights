package question

import scala.util._
import java.time.LocalDate


class Q1 (val flightList: List[String]) {

    //Q1: Find the total number of flights for each month.
    //INPUT: a List of flights
    //OUTPUT: a Map[Month -> Number of flights]
    //Group the list by the 5th element in the line (the month) and map to the number of occurences of this element (the month)
    //This is a method that would not work if different years were used as we don't differentiate between jan 2017 and jan 2018
    def total_flights_month(flights: List[String]) : List[(Int, Int)] = {
        flights.groupBy{
            //Group by the 5th element (month)
            singleFlight => (LocalDate.parse(singleFlight.split(",")(4)).getMonthValue)
            //Map it to the amount of times it occurs
            }.mapValues(_.size).toList
    }

    //Order a list of (Int, Int) by the first element in the tuple in ascending fashion
    def order_list_by_key(list: List[(Int, Int)]) : List[(Int, Int)] = {
        list.toSeq.sortWith(_._1 < _._1).toList
    }


    //Output result
    val output = order_list_by_key(total_flights_month(flightList))
    println("Q1")
    println(output)
}