package forcomp

import scala.annotation.tailrec

object Anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences =
    w.withFilter(_.isLetter).map(_.toLower).groupBy(c => c).map {
      case (c, l) => c -> l.length
    }.toList.sortBy(_._1)

  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(w => wordOccurrences(w))

  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() =>
      List(List())
    case head :: tail => {
      for {
        count <- 0 to head._2
        other <- combinations(tail)
      } yield ((head._1, count) :: other).filter(_._2 > 0)
    }.toList
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    @tailrec
    def subtract(xs: Map[Char, Int], y: Occurrences): Occurrences = y match {
      case Nil =>
        xs.toList
      case head :: tail =>
        val (char, count) = head
        if (count > xs(char)) throw new Exception()
        subtract(xs + (char -> (xs(char) - count)), tail)
    }

    subtract(x.toMap withDefaultValue 0, y).filter(_._2 > 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = ???
}
