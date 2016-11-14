package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: Roberto Carlos"){
    assert(sentenceOccurrences(List("Roberto", "Carlos")) === List(('a',1), ('b',1), ('c',1), ('e',1), ('l',1), ('o',3), ('r',3), ('s',1), ('t',1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r = lad") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: aardvark - mark = advar"){
    val aardvark = List(('a', 3), ('d', 1), ('k', 1), ('r', 2), ('v', 1))
    val mark = List(('a', 1), ('k', 1), ('m', 1), ('r', 1))
    val advar = List(('a', 2), ('d', 1), ('r', 1), ('v', 1))
    assert(subtract(aardvark,mark)===advar)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence: Yes man"){
    val sentence = List("Yes", "man")
    val anas = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: Lukas Rytz"){
    val sentence = List("Lukas", "Rytz")
    val anagrams = List(
      List("Ku", "Salz", "try"),
      List("Katz", "surly"),
      List("Salz", "try", "Ku"),
      List("Ku", "try", "Salz"),
      List("Salz", "Ku", "try"),
      List("try", "Ku", "Salz"),
      List("surly", "Katz"),
      List("try", "Salz", "Ku")
    )

    assert(sentenceAnagrams(sentence).toSet == anagrams.toSet)
  }

  test("sentence anagrams: I love you"){
    val sentence = List("I love you")
    val anagrams = List(
      List("Lev", "you", "Io"),
      List("Io", "you", "Lev"),
      List("Io", "Lev", "you"),
      List("you", "Io", "Lev"),
      List("you", "olive"),
      List("you", "Lev", "Io"),
      List("Lev", "Io", "you"),
      List("olive", "you")
    )

    assert(sentenceAnagrams(sentence).toSet == anagrams.toSet)
  }

  test("sentence anagrams: Yell Xerxes"){
    val sentence = List("Yell Xerxes")
    val anagrams = List(
      List("yell", "Xerxes"),
      List("sex", "yell", "Rex"),
      List("Lyle", "sex", "Rex"),
      List("Xerxes", "yell"),
      List("Lyle", "Xerxes"),
      List("yell", "sex", "Rex"),
      List("Lyle", "Rex", "sex"),
      List("Rex", "yell", "sex"),
      List("Rex", "sex", "yell"),
      List("sex", "Rex", "yell"),
      List("yell", "Rex", "sex"),
      List("sex", "Lyle", "Rex"),
      List("Rex", "Lyle", "sex"),
      List("sex", "Rex", "Lyle"),
      List("Rex", "sex", "Lyle"),
      List("Xerxes", "Lyle")
    )

    assert(sentenceAnagrams(sentence).toSet == anagrams.toSet)
  }

  test("sentence anagrams: Heather"){
    val sentence = List("Heather")
    val anagrams = List(
      List("re", "the", "ha"),
      List("he", "hat", "re"),
      List("three", "ha"),
      List("hare", "the"),
      List("her", "Thea"),
      List("at", "he", "her"),
      List("her", "et", "ha"),
      List("re", "ah", "the"),
      List("he", "he", "art"),
      List("hear", "the"),
      List("et", "her", "ha"),
      List("he", "re", "hat"),
      List("hat", "re", "he"),
      List("heather"),
      List("he", "heart"),
      List("re", "ha", "the"),
      List("here", "hat"),
      List("he", "at", "her"),
      List("he", "hater"),
      List("ah", "three"),
      List("her", "heat"),
      List("he", "earth"),
      List("there", "ha"),
      List("ha", "re", "the"),
      List("re", "heath"),
      List("et", "ha", "her"),
      List("ah", "et", "her"),
      List("ah", "her", "et"),
      List("re", "the", "ah"),
      List("he", "he", "rat"),
      List("rat", "he", "he"),
      List("ha", "ether"),
      List("ha", "her", "et"),
      List("he", "tar", "he"),
      List("he", "rat", "he"),
      List("at", "her", "he"),
      List("ha", "there"),
      List("ether", "ha"),
      List("art", "he", "he"),
      List("ah", "re", "the"),
      List("tar", "he", "he"),
      List("hate", "her"),
      List("the", "hear"),
      List("he", "art", "he"),
      List("earth", "he"),
      List("Rhea", "the"),
      List("three", "ah"),
      List("et", "ah", "her"),
      List("her", "et", "ah"),
      List("the", "hare"),
      List("the", "re", "ah"),
      List("re", "hat", "he"),
      List("her", "at", "he"),
      List("hat", "he", "re"),
      List("he", "he", "tar"),
      List("ha", "the", "re"),
      List("the", "Hera"),
      List("ah", "the", "re"),
      List("et", "her", "ah"),
      List("ha", "three"),
      List("heat", "her"),
      List("hater", "he"),
      List("the", "ha", "re"),
      List("he", "her", "at"),
      List("heart", "he"),
      List("there", "ah"),
      List("ah", "there"),
      List("ah", "ether"),
      List("Hera", "the"),
      List("her", "he", "at"),
      List("hat", "here"),
      List("ha", "et", "her"),
      List("the", "ah", "re"),
      List("her", "ah", "et"),
      List("ether", "ah"),
      List("the", "Rhea"),
      List("her", "hate"),
      List("re", "he", "hat"),
      List("heath", "re"),
      List("Thea", "her"),
      List("her", "ha", "et"),
      List("the", "re", "ha")
    )

    assert(sentenceAnagrams(sentence).toSet == anagrams.toSet)
  }
}
