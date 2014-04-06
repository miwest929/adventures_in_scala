import scala.io._
import scala.sys.process._
import scala.language.postfixOps
import java.sql.{Connection,DriverManager}

class BookmarksDatabase(user: String, password: String, database: String) {
/*
| Tables_in_bookmarks_search_engine |
+-----------------------------------+
| link_words                        |
| links                             |
| urls                              |
| word_locations                    |
| words
*/
  val connection = establishConnection(user, password, database)
  val NO_LAST_INSERT_ID = 0

  var wordExists = scala.collection.mutable.Map[String, Integer]()

  def registerUrl(url: String, words: Array[String]) = {
    println("Storing data in MySQL!!!")

    val urlId = executeInsertQuery("INSERT IGNORE INTO urls (url) VALUES (\"" + url + "\")")

    if (urlId > 0) {
      val wordIds = words.map(createWord _)

      var valuesArray = List[String]()
      for( (wordId, location) <- wordIds.view.zipWithIndex) valuesArray +:= s"($urlId, $wordId, $location)"
      var values = valuesArray.mkString(",")
      executeInsertQuery(s"INSERT INTO word_locations (url_id, word_id, location) VALUES $values")
      //for((wordId, location) <- wordIds.view.zipWithIndex) executeInsertQuery(s"INSERT INTO word_locations (url_id, word_id, location) VALUES ($urlId, $wordId, $location)")
    }
  }

  def close = connection.close

  def doesWordExist(word: String) = {
    wordExists.exists(_._1 == word)
  }

  def doesUrlExist(url: String) = {
    var result = executeQuery("SELECT COUNT(*) cnt FROM urls WHERE url = \"" + url + "\"")
    var count = 0
    while (result.next) { count = result.getString("cnt").toInt }

    if (count > 0) true else false
  }

   // Use INSERT IGNORE so duplicate words aren't persisted
   private def createWord(word: String) = {
    if (doesWordExist(word)) {
      wordExists(word)
    } else {
      var wordId = executeInsertQuery("INSERT IGNORE INTO words (word) VALUES (\"" + word + "\")")
      wordExists(word) = wordId

      wordId
    }
  }

  private def executeInsertQuery(query: String) = {
    executeUpdate(query)

    // Get the last inserted id
    var result = executeQuery("SELECT LAST_INSERT_ID()")
    var lastId = NO_LAST_INSERT_ID
    while (result.next) {
      lastId = result.getString("LAST_INSERT_ID()").toInt
    }

    lastId
  }

  private def executeUpdate(query: String) = {
    try {
      val statement = connection.createStatement
      statement.executeUpdate(query)
    } catch {
      case e:com.mysql.jdbc.exceptions.jdbc4.MySQLSyntaxErrorException => println(s"Syntax Error in Query: $query")
    }
  }

  private def executeQuery(query: String) = {
    val statement = connection.createStatement
    statement.executeQuery(query)
  }

  private def establishConnection(user: String, password: String, database: String): Connection = {
    DriverManager.getConnection(s"jdbc:mysql://localhost/$database", user, password)
  }
}

class Page(var url: String, var title: String) {
  //var words = Array[String]()
  //var wordFrequency = scala.collection.mutable.Map[String, Integer]() 

  //def setWords(newWords: Array[String]) = {
    //words = newWords

    //wordFrequency = scala.collection.mutable.Map[String, Integer]()
    //words.foreach(w => countWord(w))
  //}
/*
  private def countWord(word: String) = {
    try {
      wordFrequency(word) += 1
    } catch {
      case e:java.util.NoSuchElementException => wordFrequency(word) = 1
    }
  }
*/
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

// This class can be parallelized when necessary
class HtmlReader {
  def read(url: String) = {
    println(s"Downloading and processing $url")

    println("Apache Tika is extracting text from URL!")

    val result = s"curl $url" #| "java -jar -Xms512m -Xmx1024m lib/tika-app-1.5.jar --text" !!

    println("Post-processing text extracted from Tika!!")

    parse( result.split("\n") )
  }

  private def parse(contents: Array[String]) = contents.flatMap(getWords _)
  private def getWords(line: String) = {
    val delimeters = "[ ().,;\\?\"']+"

    var words = line.split(delimeters).map(_ trim).filter(_ != "")

    words
  }
}

// Example of the Facade design pattern
class PageRegistry {
  val db = new BookmarksDatabase("root", "", "bookmarks_search_engine")
  //var pages = Array[Page]()
  val html = new HtmlReader

  def register(page: Page) = {
    //page.setWords( html.read(page.url) )
    if (!db.doesUrlExist(page.url)) {
      var words = html.read(page.url)
      db.registerUrl(page.url, words)
      //println(page.words.mkString(", "))
      //println(page.wordFrequency.mkString(": "))

  //    pages +:= page
    }
  }

  def computeImportantWords = {
//    val wordCounts = pages.foreach(p => p.tallyWords)
  }
}

object BookmarksSearchEngine extends App {
  val crawler = new BookmarkCrawler
  val registry = new PageRegistry

  crawler.go("data/bookmarks.html")

  // This action can be parallelizable
  crawler.getPages.foreach(l => registry.register(l))
  registry.db.close
}
