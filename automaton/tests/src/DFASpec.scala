package automaton.tests

import automaton._
import utest._

object DFASpec extends TestSuite {
  val simpleDFA: DFA[Int, Char] = DFA(
    initialState = 1,
    transition = Map(
      (1, 'a') -> 2,
      (1, 'b') -> 3,
      (2, 'a') -> 1,
      (2, 'b') -> 3
    ),
    finalStates = Set(3))

  val simpleNFA: NFA[Int, Char] = NFA(
    initialState = 0,
    transition = Map(
      (0, None) -> Set(1, 7),
      (1, None) -> Set(2, 4),
      (2, Some('a')) -> Set(3),
      (3, None) -> Set(6),
      (4, Some('b')) -> Set(5),
      (5, None) -> Set(6),
      (6, None) -> Set(7),
      (7, Some('a')) -> Set(8),
      (8, Some('b')) -> Set(9)
    ),
    finalStates = Set(9)
  )
  val tests: Tests = Tests {
    test("simpleDFA can accept b as the last word."){
      simpleDFA.accepts("aaaaaaaaaaaaab")
    }
    test("nfa generated dfa"){

      simpleNFA.cleanEpsilonTransition.transition.foreach(println)
      simpleNFA.accepts("aab")
//      nfaGeneratedDFA.accepts("aab")
    }
  }
}