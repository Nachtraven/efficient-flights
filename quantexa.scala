import io.Source
import scala.util._
import question._


object quantexa {
    def main(args: Array[String]) = {

        val flightDataFile = "flightData.csv"
        val passengersFile = "passengers.csv"

        //Make a list from CSV file and drop first denomination line
        def get_csv(file: String) : List[String] = {
            Source.fromFile(file).getLines.toList.drop(1)
        }
        val flightData = get_csv(flightDataFile)
        val passengers = get_csv(passengersFile)

        val ans1 = new Q1(flightData)
        val ans2 = new Q2(flightData, passengers)
        val ans3 = new Q3(flightData)
        val ans4 = new Q4(flightData)
    }
}

