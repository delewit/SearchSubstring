package search_substrings

import scala.io.Source._
import scala.io.StdIn._


class SearchSubstring(text : String){

  val textLength = this.text.length

  def searchForSubstring(substring : String) : List[Int] = {
    val substringLength = substring.length
    if(substring.length > textLength) Nil
    else{
      val partitions = for (k <- 0 to textLength - substringLength + 1)
          yield { this.text.slice(k, k + substringLength) }
      if(!partitions.contains(substring)) Nil 
      else{
        val possibleSolutions = (0 to partitions.length - 1).zip(partitions)
        val possibleSolutions_ = possibleSolutions.filter(x => x._2 == substring)
        possibleSolutions_.foldRight(List[Int]()) {(x, y) => x._1 :: y} 
      } }
  }

}  // End of the SearchSubstring class.



object SearchSubstring extends App{


  def apply(s : String) : SearchSubstring = {
    val searchObject = new SearchSubstring(s)
    searchObject
  }


  val openFile = fromFile("Scala.txt")  // An Iterator object.
  val readFile = openFile.getLines    // An Iterator[String] object.


  def getSentences(it : Iterator[String], sentences : String = "") : String = {
    if(!it.hasNext) sentences
    else getSentences(it, sentences ++ it.next) }


  print("\nWhat piece of text do you want to search for in \"Scala.txt\"? ")

  val pattern = readLine

  val sentences = getSentences(readFile)

  openFile.close

  val searchObject = SearchSubstring(sentences)

  val searchResults = searchObject.searchForSubstring(pattern)
  
  if(searchResults.isEmpty)
    println("Your pattern doesn't match any part of the text.")
  else {
    println(pattern ++ " can be found at the following position(s) in \"Scala.txt\".")
    println(searchResults.mkString(", ")) }

  println("")


}  // End of the SearchSubstring singleton object.



