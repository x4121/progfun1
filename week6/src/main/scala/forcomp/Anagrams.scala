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

    subtract(x.toMap withDefaultValue 0, y).filter(_._2 > 0).sortBy(_._1)
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def anagram(occurrences: Occurrences, sentence: Sentence): List[Sentence] = occurrences match {
      case List() =>
        List(sentence)
      case _ =>
        (for {
          combination <- combinations(occurrences)
          word <- dictionaryByOccurrences.get(combination).getOrElse(List())
        } yield anagram(subtract(occurrences, combination), sentence :+ word)).flatten
    }

    anagram(sentenceOccurrences(sentence), List.empty[Word])
  }
}
