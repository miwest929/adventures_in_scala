// Experiments in k-means clustering algorithms. More to come!!
import scala.io._

object KMeansDemo extends App {
  class DataItem(var state: String, var income: Int) {
    override def toString = s"$state - $income"
  }

  class KMeansClustering {
    private def computeMean(group: List[DataItem]) = {
      val mean = group.map(_.income).sum / group.length
      new DataItem(s"Cluster$mean", mean)
    }

    private def initialCentroids(k: Int, data: List[DataItem]) = {
      var groups = data.grouped( Math.ceil(data.length / k).toInt )

      groups.map(computeMean _).toList
    }

    private def findClosest(centroids: List[DataItem], row: DataItem) = {
      centroids.minBy(c => Math.abs(row.income - c.income))
    }

    private def recalibrateCentroids(centroids: List[DataItem], assignment: scala.collection.mutable.Map[DataItem, List[DataItem]]) = {
      assignment.values.filter(v => v.length > 0).map(v => computeMean(v)).toList
    }

    private def hasCentroidsMoved(current: List[DataItem], prev: List[DataItem]) = {
      val pairs = current.zip(prev)
      pairs.count(p => p._1.income == p._2.income) != current.length
    }

    def cluster(k: Int, data: List[DataItem]) = {
      var centroids = initialCentroids(k, data)
      var previousCentroids = List[DataItem]()
      var clusterAssignment = scala.collection.mutable.Map[DataItem, List[DataItem]]()

      while (hasCentroidsMoved(centroids, previousCentroids)) {
        clusterAssignment.clear
        centroids.foreach(c => clusterAssignment(c) = List[DataItem]())
        data.foreach(row => clusterAssignment( findClosest(centroids, row) ) +:= row)
        previousCentroids = centroids
        centroids = recalibrateCentroids(centroids, clusterAssignment)
      }

      clusterAssignment
    }
  }

  var stateCapitaData = List[DataItem]()

  def createRecord(row: String) = {
    var parts = row.split(",").filter(d => d.length > 0).toList
    new DataItem(parts(1), parts(2).toInt)
  }
  Source.fromFile("data/percapita.txt").getLines.foreach(l => stateCapitaData +:= createRecord(l))

  val kmeans = new KMeansClustering
  val result = kmeans.cluster(5, stateCapitaData)

  println("------ COMPUTED CLUSTERS -----")
  result.foreach(c => println(c))
}
