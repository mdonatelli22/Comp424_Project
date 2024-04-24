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
    // List of page sizes of each page in pages
    val pageSizes = pages.par.map(page => page.text.toLowerCase.split("\\W+").toList.length)
    // Combine the terms count and the page size then normalize
    count(pages, query).zip(pageSizes).map((cnt, total) => cnt/total)
  }

  /**
   * @param pages a list of RankedWebPage objects to be searched
   * @param query a list of search terms to be counted in those pages
   * @return      a list of the TF-IDF score for each page in the same order given
   */
  def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
    // Number of pages. useful later ...
    val N = pages.length
    // Computing TFIDF value for each page
    pages.map { page =>
      // All words on the page
      val pageWords = page.text.toLowerCase.split("\\W+").toList
      // TFIDF value for each term
      val tfidf = query.map { term =>
        val wordCount = pageWords.count(_.contains(term.toLowerCase))
        val TF = wordCount / pageWords.length
        val D = (for p <- pages yield if page.text.toLowerCase.split("\\W+").toList.contains(term.toLowerCase) then 1 else 0).sum.toDouble
        val IDF = log(N/D)
        TF/IDF
      }
      tfidf.sum // Summing the TFIDF values together for the page
    }
  }
}
