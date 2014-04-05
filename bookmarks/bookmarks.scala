import scala.io._
import scala.sys.process._
import scala.language.postfixOps

class Page(var url: String, var title: String, var words: Array[String] = Array[String]()) {
  override def toString = s"$title <=> $url"
}

class BookmarkCrawler {
  private val aTagRegex = "<a href=\"(.*?)\".*>(.*?)</a>"

  var links = Array[Page]()

  def go(path: String) = {
    Source.fromFile(path).getLines.foreach(line => parseLine(line))
  }

  def getPages = links

  private def parseLine(line: String) = {
    val linkRegex = new scala.util.matching.Regex(aTagRegex, "linkUrl", "linkTitle")
    val result = linkRegex.findFirstMatchIn(line.toLowerCase)

    if (result.isDefined) {
      val url = result.get.group("linkUrl")
      val title = result.get.group("linkTitle")

      links +:= new Page(url, title)
    }
  }
}

class HtmlReader {
  def read(url: String) = {
    println(s"Downloading and processing $url")

    val result = s"curl $url" #| "java -jar lib/tika-app-1.5.jar --text" !!

    parse( result.split("\n") )
  }

  private def parse(contents: Array[String]) = contents.flatMap(getWords _)
  private def getWords(line: String) = {
    val delimeters = "[ ().,;]+"

    line.split(delimeters)
  }
}

// Example of the Facade design pattern
class PageRegistry {
  var pages = Array[Page]()
  var html = new HtmlReader

  def register(page: Page) = {
    page.words = html.read(page.url)
    println(page.words.mkString(", "))
    pages +:= page
  }
}

object BookmarksSearchEngine extends App {
  val crawler = new BookmarkCrawler
  val registry = new PageRegistry

  crawler.go("data/bookmarks.html")
  crawler.getPages.foreach(l => registry.register(l))
}
