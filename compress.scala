/*
Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
                     List('a, 'b, 'c, 'a, 'd, 'e)
*/
object CompressDuplicates {
  def compress(list: List[Symbol]): List[Symbol] = {
    def compress_recur(list: List[Symbol], lastSym: Any, compressed: List[Symbol]): List[Symbol] = {
      if (list.isEmpty) compressed
      else if (lastSym != Nil && list.head == lastSym) compress_recur(list.tail, lastSym, compressed)
      else compress_recur(list.tail, list.head, compressed ::: (list.head :: Nil))
    }

    compress_recur(list, Nil, List[Symbol]())
  }

  def main(args: Array[String]) {
    println( compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) )
  }
}
