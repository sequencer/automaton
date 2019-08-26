package automaton

/**
 * @tparam State a finite set of states, [[chisel3.util.Enum]] in this DSL
 * @tparam Symbol a finite set of input symbols,
 *
 * @param transition transition functions, (state, symbol) -> state
 * @param initialState the initial state of NFA
 * @param finalStates functions to judge if a state is final state
 *
 * */
case class DFA[State, Symbol](initialState: State,
                              transition: Map[(State, Symbol), State],
                              finalStates: Set[State]) {
  def move(state: State, word: Symbol): State = transition(state, word)

  /**
   * transform state by input words with [[transition]]
   * @param state current input state of DFA
   * @param words the remain word to be processed, the first word in words will be consumed,
   *              use generated state and remain words for iteration
   * */
  def run(state: State, words: List[Symbol]): State = {
    words match {
      case word :: wordTail => run(move(state, word), wordTail)
      case Nil => state
    }
  }

  /**
   * @param words is a list of Symbol, send to DFA in sequence,
   *              find if it can be accepted by this [[DFA]]
   * */
  def accepts(words: List[Symbol]): Boolean = {
    finalStates.contains(run(initialState, words))
  }
}

object DFA {
  /**
   * generate DFA by NFA,
   * notice the type of DFA State is Set[State],
   * because DFA state is a epsilon closure in NFA
   * */
  def apply[State, Symbol](nfa: NFA[State, Symbol]): DFA[Set[State], Symbol] = DFA(
    nfa.epsilonClosure(nfa.initialState),
    nfa.transition.map {
      case ((state, Some(word)), _) => ((nfa.epsilonClosure(state), word), nfa.move(nfa.epsilonClosure(state), Some(word)))
    },
    nfa.finalStates.map(nfa.epsilonClosure)
  )
}