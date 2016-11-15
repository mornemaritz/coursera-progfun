import forcomp.Anagrams.{Occurrences, Sentence, Word}
import forcomp._

import scala.None

// ("b",2)
for{
  rest <- List(List())
  num <- 0 to 2
} yield if (num == 0) rest else ("b", num) :: rest
// 0 -> List(List())
// 1 -> List(("b",1))
// 2 -> List(("b",2))
// -> List(List(), List(("b",1)), List(("b",2)))

// ("a",2)
for{
  rest <- List(List(), List(("b",1)), List(("b",2)))
  num <- 0 to 2
} yield if (num == 0) rest else ("a", num) :: rest
// 0 -> List(List(), List(("b",1)), List(("b",2)))
// 1 -> List(List(("a",1)), List(("a",1),("b",1)), List(("a",1),("b",2)))
// 2 -> List(List(("a",2)), List(("a",2),("b",1)), List(("a",1),("b",2)))
// -> List(List(), List(("b",1)), List(("b",2)),List(("a",1)), List(("a",1),("b",1)), List(("a",1),("b",2)),List(("a",2)), List(("a",2),("b",1)), List(("a",1),("b",2)))

val combinations = Anagrams.combinations(
  Anagrams.sentenceOccurrences(List("Heather"))
)

def loop(acc: List[Word], combs: List[Occurrences]): List[Word] = {
  if (combs.isEmpty)
    acc
  else
  (for {
    comb <- combs
    if comb.nonEmpty
  } yield (comb, getWords(comb)) match {
      case (c, Some(words)) => words.flatMap(w => loop(acc :+ w, combs.map(x => Anagrams.subtract(x,c))))
      case (_, None) => List.empty
  }).flatten
}

def getWords(occ: Occurrences): Option[List[Word]] = {
  Anagrams.dictionaryByOccurrences.get(occ)
}

loop(List.empty, combinations)

val combs = Anagrams.combinations(Anagrams.wordOccurrences("abc"))

val rem = combs.map(o => Anagrams.subtract(o,Anagrams.wordOccurrences("a"))).distinct





