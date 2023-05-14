package com.rockthejvm
import scala.io.Source
import scala.util.{Random, Try, Using}
import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object project extends App {

  sealed trait EnergySource
  case object Solar extends EnergySource
  case object Wind extends EnergySource
  case object Hydro extends EnergySource

  case class RenewableEnergySource(sourceType: EnergySource, energyOutput: Double, malfunction: Boolean = false) {
    def monitor: String = s"Monitoring ${sourceType.toString.toLowerCase} power..."
    def control: String = s"Controlling ${sourceType.toString.toLowerCase} power..."
    def generateEnergyData: Double = if (malfunction) 0 else energyOutput
    def checkHealth: Option[String] = if (malfunction || energyOutput < 10) Some(s"Low energy output from ${sourceType.toString.toLowerCase} power") else None
  }

  class RenewableEnergyPlant(sources: List[RenewableEnergySource]) {
    def monitorAll: Unit = sources.map(_.monitor).foreach(m => println(s"$m"))
    def controlAll: Unit = sources.map(_.control).foreach(c => println(s"$c"))
    def collectEnergyData: List[(LocalDateTime, Double)] = sources.map(source => (LocalDateTime.now(), source.generateEnergyData))
    def checkAllSources: Unit = sources.flatMap(_.checkHealth).foreach(alert => println(s"Alert: $alert"))
  }

  val plant = new RenewableEnergyPlant(List(
    RenewableEnergySource(Solar, Random.nextDouble() * 100),
    RenewableEnergySource(Wind, Random.nextDouble() * 200),
    RenewableEnergySource(Hydro, Random.nextDouble() * 300)
  ))

  println("Monitoring all energy sources:")
  plant.monitorAll

  println("\nControlling all energy sources:")
  plant.controlAll

  val energyData = plant.collectEnergyData
  val pw = new PrintWriter(new File("energy_data.txt"))
  energyData.foreach { case (time, data) => pw.write(time.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME) + "," + data + "\n") }
  pw.close()

  val energyDataFile = "energy_data.txt"
  println(s"\nDisplaying energy data from $energyDataFile:")
  Using(Source.fromFile(energyDataFile)) { source =>
    source.getLines().foreach(println)
  }.recover {
    case e: Exception => println(s"Error reading file: $e")
  }

  def calculateStatistics(data: List[Double]): Unit = {
    val sortedData = data.sorted
    val mean = data.sum / data.length
    val median = if (data.length % 2 == 0) (sortedData(data.length / 2 - 1) + sortedData(data.length / 2)) / 2 else
      sortedData(data.length / 2)
    val mode = data.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    val range = sortedData.last - sortedData.head
    val midrange = (sortedData.head + sortedData.last) / 2

    println("\nStatistics:")
    println(s"Mean: $mean")
    println(s"Median: $median")
    println(s"Mode: $mode")
    println(s"Range: $range")
    println(s"Midrange: $midrange")
  }

  calculateStatistics(energyData.map(_._2))

  println("\nChecking all energy sources:")
  plant.checkAllSources

  def filterData(unit: ChronoUnit, amount: Long): Unit = {
    Using(Source.fromFile(energyDataFile)) { source =>
      val lines = source.getLines().toList
      val now = LocalDateTime.now()
      val filteredLines = lines.filter { line =>
        val timestamp = LocalDateTime.parse(line.split(",")(0), DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        unit.between(timestamp, now) <= amount
      }
      println(s"\nFiltered data for the last $amount ${unit.toString.toLowerCase}s:")
      filteredLines.foreach(println)
    }.recover {
      case e: Exception => println(s"Error reading file: $e")
    }
  }

  println("\nFiltering data:")

  def searchData(timestamp: String): Unit = {
    Using(Source.fromFile(energyDataFile)) { source =>
      val lines = source.getLines().toList
      lines.find(line => line.startsWith(timestamp)) match {
        case Some(value) => println(s"\nData found: $value")
        case None => println("No data found for the given timestamp.")
      }
    }.recover {
      case e: Exception => println(s"Error reading file: $e")
    }
  }

  println("\nFiltering data for the last hour:")
  filterData(ChronoUnit.HOURS, 1)

  println("\nFiltering data for the last day:")
  filterData(ChronoUnit.DAYS, 1)

  println("\nFiltering data for the last week:")
  filterData(ChronoUnit.WEEKS, 1)

  println("\nFiltering data for the last month:")
  filterData(ChronoUnit.MONTHS, 1)

  println("\nSearching data:")
  searchData("2023-05-12T12") // replace with the timestamp I want to search for
}


