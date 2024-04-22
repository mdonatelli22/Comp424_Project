import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Sorting

@main def main(): Unit = {
  /* rankingMethod and termMatchingMethod let's you select which method
     To use for ranking ad term frequencies.
     It's useful to try different combinations for the report */
  println("Input the ranking method and termMatching method to use (e.g: 2 1)")
  println("Ranking: 0 -> equal, 1 -> indegree, 2 -> pagerank")
  println("Search: 0 -> count, 1 -> tf, 2 -> tfidf")
  println("Press 'Enter' for default")
  var rankingMethod = 0;
  var termMatchingMethod = 0;
  val input = readLine().split(" ").toList
  if input.length > 1 then {
    rankingMethod  = input.head.toInt 
    termMatchingMethod = input.tail.head.toInt
  }

  println("=============================================================")
  println("   _____          __      __      __.__.__  .__  .__ ")
  println("  /  _  \\   _____|  | __ /  \\    /  \\__|  | |  | |__| ____  ")
  println(" /  /_\\  \\ /  ___/  |/ / \\   \\/\\/   /  |  | |  | |  |/ __ \\")
  println("/    |    \\___ \\|     <   \\        /|  |  |_|  |_|  \\  ___/ ")
  println("\\____|__  /____  >__|_ \\   \\__/\\  / |__|____/____/__|\\___  >")
  println("        \\/     \\/     \\/        \\/                       \\/")
  println("=============================================================")

  // Load WebPage.id -> WebPage map to better handle graph
  val pages: Map[String, WebPage] = mapWebPages(loadWebPages()) // completed for you

  // TODO: Measure the importance of each page using one of the functions in PageRank
  val rankedPages: List[RankedWebPage] = rankingMethod match
    case 0 => PageRank.equal(pages).map { case (pageId, weight) =>
                val page = pages(pageId)
                new RankedWebPage(page, weight)
              }.toList
    case 1 => PageRank.indegree(pages).map { case (pageId, weight) =>
                val page = pages(pageId)
                new RankedWebPage(page, weight)
              }.toList
    case 2 => PageRank.pagerank(pages).map { case (pageId, weight) =>
                val page = pages(pageId)
                new RankedWebPage(page, weight)
              }.toList
    case _ => PageRank.equal(pages).map { case (pageId, weight) =>
                val page = pages(pageId)
                new RankedWebPage(page, weight)
              }.toList

  // Get user input then perform search until ":quit" is entered
  var query: String = ""
  var terms: List[String] = List()
  while {
    // get search query from the user
    query = readLine("search> ")
    // split the users search query into separate terms based on spaces
    terms = query.trim.split(' ').toList
    // this is the last line in the expression i.e. the condition of our while loop
    terms != List(":quit")
  } do {
    // TODO: Measure the textual match of each page to these terms using one of the functions in PageSearch
    val searchedPages: List[SearchedWebPage] = termMatchingMethod match
      case 0 => PageSearch.count(rankedPages, terms).zip(rankedPages).map { case (score, page) =>
                  new SearchedWebPage(page, score)
                } // call PageSearch.???? here
      case 1 => PageSearch.tf(rankedPages, terms).zip(rankedPages).map { case (score, page) =>
                  new SearchedWebPage(page, score)
                }
      case 2 => PageSearch.tfidf(rankedPages, terms).zip(rankedPages).map { case (score, page) =>
                  new SearchedWebPage(page, score)
                }
      case _ => PageSearch.count(rankedPages, terms).zip(rankedPages).map { case (score, page) =>
                  new SearchedWebPage(page, score)
                }


    // normalize the ranges for weight and textmatch on these pages
    val pageArray = SearchedWebPageNormalize.normalize(searchedPages).toArray
    // sort this array based on the chosen averaging scheme i.e.
    //    (ArithmeticOrdering || GeometricOrdering || HarmonicOrdering)
    Sorting.quickSort(pageArray)(ArithmeticOrdering) // TODO: change this from name ordering to something else!!!
    // Print the top ranked pages in descending order
    for p <- pageArray.reverse.slice(0, 10) do println(f"${p.name}%-15s  ${p.url}")

    // print a divider to make reading the results easier
    println("=============================================================")
  }
}

// Load a List of WebPage objects from the packaged prolandwiki.csv file
def loadWebPages(): List[WebPage] = {
  // create an input stream to the proglangwiki.csv
  val fh = Source.fromInputStream(
    getClass.getClassLoader.getResourceAsStream("proglangwiki.csv"))
  // load all pages from the file line by line
  val pages = (for line <- fh.getLines yield {
    val id::name::url::text::links = line.split(",").toList // warning, but will work
    new WebPage(id, name, url, text, links)
  }).toList
  fh.close
  pages
}

// Convert a List[WebPage] to a Map[String, WebPage]
def mapWebPages(pages: List[WebPage]): Map[String, WebPage] = (for page <- pages yield (page.id, page)).toMap