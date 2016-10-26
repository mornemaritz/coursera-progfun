import forcomp._

val words: List[String] = loadDictionary filter(word => word forall (chr => chr.isLetter))
val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "NMO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
)

val charCode: Map[Char,Char] =
  for((digit,string) <- mnem; ltr <- string)
    yield ltr -> digit

def wordCode(word: String): String = word.toUpperCase map charCode

val wordsForNum: Map[String,Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] ={
  if(number.isEmpty) Set(List())
  else {
    for {
      numberSplit <- 1 to number.length
      words <- wordsForNum(number take numberSplit)
      rest <- encode(number drop numberSplit)
    } yield words :: rest
  }.toSet
}

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")
