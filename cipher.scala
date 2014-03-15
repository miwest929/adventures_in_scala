import scala.collection.mutable.{Map, HashMap}

object Cipher {
  class CaesarCipher {
    private var caesar_encode = new HashMap[Char, Char]
    private var caesar_decode = new HashMap[Char, Char]

    // Construct caesar cipher
    for ((a, b)  <- (('a' to 'y') zip ('b' to 'z'))) {
      caesar_encode += (a -> b)
      caesar_decode += (b -> a)
    }
    caesar_encode += ('z' -> 'a')
    caesar_decode += ('a' -> 'z')

    def encode(input: String) = {
      input.map(c => caesar_encode(c))
    }

    def decode(input: String) = {
      input.map(c => caesar_decode(c))
    }
  }

  def main(args: Array[String]) {
    var cipher = new CaesarCipher
    var crypted = cipher.encode("hellotherefromthetopoftheworld")
    println(crypted)
    println(cipher.decode(crypted))
  }
}
