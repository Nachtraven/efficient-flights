package question

import scala.util._


class Q2 (val flightList: List[String], val passengerList: List[String]) {

    //Q2 Find the IDs of the top n = amount of flyers
    //INPUT: List of flights, amount of flyers to take
    //OUTPUT: A map [Flyer ID -> amount of flights taken]
    def frequent_flyers(flights: List[String], amount: Int) : Map[Int, Int] = {
        flights.groupBy{
            //Take the 1st element, the ID
            _.split(",")(0).toInt
        //Map it to the amount of times it occurs
        }.mapValues(_.size)
        //Sort it in descending order by the second element (amount of flights) and take the first n 
        .toSeq.sortWith(_._2 > _._2).toList.take(amount).toMap
    }

    //Create map of ID->List[ID, VALUE, ...] from a list of the form List[String] where String: "ID, VAL, VAL, ..."
    def get_map(list: List[String]): Map[Int, List[String]] = {
        list.groupBy(_.split(",")(0).toInt)
    }

    //Associate ID's and names with ID's and flights, return Map[Int, List[String]] where string is flights, ID, name
    def associate_name_id(passengers: Map[Int, List[String]], flyers: Map[Int, Int]): Map[Int, List[String]] = {
        flyers.map{ 
            case (k,v) => (k -> (v.toString :: (passengers.getOrElse(k, List.empty)) )) 
        }
    }


    //Output result
    val unformatted = associate_name_id(get_map(passengerList), frequent_flyers(flightList, 100))
    println("Q2")
    //Format result by descending amount of flights taken and print each line individually
    unformatted.values.toList.toSeq.sortWith(_(0) > _(0)).foreach(println(_))
}