import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {
  /**
   * @param pages A map of page.id to page for some number of WebPage objects
   * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
   */
  def equal(pages: Map[String, WebPage]): Map[String, Double] = {
    pages.map({case (pageId, _) => pageId -> 1.0})
  }

  /**
   * @param pages A map of page.id to page for some number of WebPage objects
   * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
   */
  def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
    pages.map { case (pageId, _) =>
      val numPagesLinking = pages.values.count(_.links.contains(pageId))
      pageId -> numPagesLinking.toDouble
    }
  }

  def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
    val randNum = new scala.util.Random
    val pageIds = pages.keys.toList
    val numOfPages = pageIds.size

    //following appendix B formulas
    val countVisits = (1 to 10000).par.foldLeft(Map[String,Int]().withDefaultValue(0)) { (counts,_) =>
      val lastPage = (1 to 100).foldLeft(pageIds(randNum.nextInt(numOfPages))) { (currPage, _) =>
        if(randNum.nextDouble() < 0.85 && pages(currPage).links.nonEmpty){
          pages(currPage).links(randNum.nextInt(pages(currPage).links.size))
        }else{
          pageIds(randNum.nextInt(numOfPages))
        }

      }
      counts.updated(lastPage, counts(lastPage) + 1)
    }
    val totalWalks = 10000 + 100
    countVisits.map { case (pageId, count) =>
      pageId -> ((count.toDouble + 1) / totalWalks + numOfPages)

    }

  }
}