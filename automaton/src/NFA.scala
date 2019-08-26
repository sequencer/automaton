package automaton

import scala.annotation.tailrec

/**
 * @tparam State a finite set of states, [[chisel3.util.Enum]] in this DSL
 * @tparam Symbol a finite set of input symbols,
 * @param transition transition functions,
 *                   state, Some(symbol) -> state
 *                   state, None -> state means epsilon transition
 * @param initialState the initial state of NFA
 * @param finalStates functions to judge if a state is final state
 *
 * @example
 *
 * */
case class NFA[State, Symbol](initialState: State,
                              transition: Map[(State, Option[Symbol]), Set[State]],
                              finalStates: Set[State]) {
  def alphabet: Set[Symbol] = transition.keys.map(_._2).filter(_.isDefined).map(_.get).toSet
  def states: Set[State] = Set(initialState) ++ transition.keys.map(_._1).toSet ++ transition.values.flatten.toSet ++ finalStates

  /**
   * @param states is a set of state, represent a epsilon closure.
   * @param word is a word in alphabet.
   * @return a set of states which can be transited by word.
   * */
  def move(states: Set[State], word: Option[Symbol]): Set[State] = {
    states.flatMap(s => transition.getOrElse((s, word), Set[State]()))
  }

  /**
   * transform state by input words with [[transition]]
   * @param states current input states of NFA
   * @param words the remain word to be processed, the first word in words will be consumed,
   *              use generated state and remain words for iteration
   * */
  def run(states: Set[State], words: Seq[Symbol]): Set[State] = {
    words.toList match {
      case word :: wordTail => run(move(states, Some(word)), wordTail)
      case Nil => states
    }
  }

  /**
   * @param words is a list of Symbol, send to NFA in sequence,
   *              find if it can be accepted by this [[NFA]]
   * */
  def accepts(words: Seq[Symbol]): Boolean = run(Set(initialState), words).subsetOf(finalStates)

  /**
   * return a epsilon closure of a input state.
   * */
  def epsilonClosure(state: State): Set[State] = epsilonClosure(Set(state))

  /**
   * helper function to generate all the epsilonClosure
   * states is a set of state in a same epsilon closure
   * each return will generate the next sets of epsilon closure
   * util generated states is same as input states
   * */
  @tailrec
  private def epsilonClosure(states: Set[State]): Set[State] = {
    val newStates: Set[State] = move(states, None) ++ states
    if(newStates == states) {
      states
    } else {
      epsilonClosure(newStates)
    }
  }

}