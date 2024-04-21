import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
  /**
   * @param pages a list of RankedWebPage objects to be searched
   * @param query a list of search terms to be counted in those pages
   * @return a list of the number of times any of the terms appeared in each page in the same order as given
   */
  def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
    pages.map { page =>
      val textWords = page.text.toLowerCase.split("\\W+").toList
      val textCount = query.map { term =>
        textWords.count(_.contains(term.toLowerCase))
      }
      textCount.sum.toDouble
    }
  }

  /**
   * @param pages a list of RankedWebPage objects to be searched
   * @param query a list of search terms to be counted in those pages
   * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
   */
  def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
    // TODO: implement this method and remove this stub
    // List of page sizes of each page in pages
    val pageSizes = pages.map(page => page.text.toLowerCase.split("\\W+").toList.length)
    // Combine the terms count and the page size then normalize
    count(pages, query).zip(pageSizes).map((cnt, total) => cnt/total)
  }

  /**
   * @param pages a list of RankedWebPage objects to be searched
   * @param query a list of search terms to be counted in those pages
   * @return      a list of the TF-IDF score for each page in the same order given
   */
  def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
    // TF values using prev method
    val TFValues = tf(pages, query)
    // Number of documents in the corpus 
    val N = pages.size
    // List of D values
    val DValues = pages.map { page =>
      val textWords = page.text.toLowerCase.split("\\W+").toList
      (for word <- query yield if textWords.contains(word) then 1 else 0).sum.toDouble
    }
    // Formula in Appendix C
    val IDFValues = DValues.map(D => log(N/(D+1)))
    // TFIDF calculation
    TFValues.zip(IDFValues).map((tf, idf) => tf/idf)
  }
}
