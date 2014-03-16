object TextProgram {
  class TextHelper {
    val suffix = "..."

    def ellipse(original: String, maxLength: Int) = {
      //if(original.length <= maxLength) {
      //    return original;
      //}

      original.substring(0, maxLength - suffix.length) + suffix;
    }
  }

  def main(args: Array[String]) {
    val helper = new TextHelper()
    println(helper.ellipse("Hello world!", 10))
  }
}
