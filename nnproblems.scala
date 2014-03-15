import scala.util.Random

object SplitList {
  def split(pivot: Int, list: List[Int]) = {
    def split_recur(pivot: Int, list: List[Int], extractList: List[Int]): Tuple2[List[Int], List[Int]] = {
      if (pivot == 0 || list.isEmpty) (extractList, list)
      else (split_recur(pivot - 1, list.tail, extractList ::: (list.head :: Nil)))
    }

    split_recur(pivot, list, List[Int]())
  }

/*
P15
scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
*/
  def duplicateN(times: Int, list: List[Int]) = {
    def duplicateN_tail(curTimes: Int, list: List[Int], duplicated: List[Int]): List[Int] = {
      if (list.isEmpty) duplicated
      else if (curTimes == 0) duplicateN_tail(curTimes, list.tail, duplicated)
      else duplicateN_tail(curTimes - 1, list, duplicated ::: List(list.head))
    }

    duplicateN_tail(times, list, List())
  }

  // P22
  def range(start: Int, end: Int): List[Int] = {
    def range_recur(curr: Int, list: List[Int]): List[Int] = {
      if (curr > end) list
      else range_recur(curr + 1, list ::: (curr :: Nil))
    }

    range_recur(start, List())
  }

  // P19
  def rotate(count: Int, list: List[Int]) = {
    def rotate_recur(count: Int, list: List[Int]): List[Int] = {
      if (count == 0) list
      else (rotate_recur(count - 1, list.tail ::: (list.head :: Nil)))
    }

    if (count < 0) rotate_recur( list.length + count, list )
    else rotate_recur(count, list)
  }

  // P20
  def removeAt(pos: Int, list: List[Int]) = {
    def removeAt_recur(pos: Int, headList: List[Int], tailList: List[Int]): List[Int] = {
      if (pos == 0) headList ::: tailList.tail
      else (removeAt_recur(pos - 1, headList ::: (tailList.head :: Nil), tailList.tail))
    }

    removeAt_recur(pos, List(), list)
  }

  // P21
  def insertAt(value: Int, pos: Int, list: List[Int]) = {
    def insertAt_recur(pos: Int, headList: List[Int], tailList: List[Int]): List[Int] = {
      if (pos == 0) headList ::: (value :: Nil) ::: tailList
      else (insertAt_recur(pos - 1, headList ::: (tailList.head :: Nil), tailList.tail))
    }

    insertAt_recur(pos, List(), list)
  }

  // P23 (**) Extract a given number of randomly selected elements from a list.
  def randomSelect(count: Int, list: List[Int]) = {
    def randomSelect2(count: Int, randomPick: List[Int]): List[Int] = {
      if (count == 0) randomPick
      else randomSelect2(count - 1, removeAt((Random.nextDouble * list.length).toInt, randomPick))
    }

    randomSelect2(list.length - count, list)
  }

  def same(list1: List[Int], list2: List[Int]): Boolean = {
    if (list1.isEmpty || list2.isEmpty) list1.isEmpty && list2.isEmpty
    else list1.head == list2.head && same(list1.tail, list2.tail)
  }

  def monotonic(list: List[Int]): Boolean = {
    same(list, list.sorted)
  }

  def binToDec(l: List[Int]) = l.reverse.zipWithIndex.map(x => x._1 * (Math.pow(2, x._2))).sum

  // P49 (**) Gray code.
  def gray(n: Int): List[String] = {
    if (n == 1) List("0", "1")
    else {
      var prevGray = gray(n-1)
      prevGray.map(i => "0" + i) ::: prevGray.reverse.map(i => "1" + i)
    }
  }

  def main(args: Array[String]) {
    var monotonicCount = 0
    for (i <- 1 to 100) {
      var r = randomSelect(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

      if (monotonic(r)) monotonicCount += 1
    }

    println("Out of 100 random seq " + monotonicCount + " seq are monotonic.")

//    println( removeAt(2, List(1, 2, 4, 5)) )
//    println( removeAt(3, List(1, 2, 4, 5)) )

    //println( insertAt(3, 4, List(1, 2, 4, 5)) )

    //println( rotate(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)) )
    //println( rotate(-2, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)) )

    //println( range(1, 10) );
    //println( range(5, 9) );
    //println( range(8, 13) );

    //println( duplicateN(2, List(1, 2)) )
    //println( duplicateN(0, List(3, 5, 7, 9, 11)) )

    //println( split(3, List[Int](1, 2, 3, 4)) )
  }
}
