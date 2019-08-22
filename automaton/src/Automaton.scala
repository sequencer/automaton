package automaton

import set._

object DFA {
  def apply[State, Sym](nfa: NFA[State, Sym]): DFA[Set[State], Sym] = {
    val move: (Set[State], Sym) => Set[State] = (s: Set[State], w: Sym) => nfa.epsClosure(nfa.move(s, Some(w)))
    val initialState: Set[State] = nfa.epsClosure(Set(nfa.initialState))
    val isFinal: Set[State] => Boolean = { s: Set[State] =>
      s exists nfa.isFinal
    }
    DFA(move, initialState, isFinal)
  }

  def dfaNfaEquiv[State, Sym](nfa: NFA[State, Sym], word: List[Sym]): Boolean = {
    nfa.accepts(word) == DFA(nfa).accepts(word)
  }
}

case class DFA[State, Sym](move: (State, Sym) => State, initialState: State, isFinal: State => Boolean) {
  def run(state: State, word: List[Sym]): State = {
    word match {
      case Nil => state
      case w :: ws => run(move(state, w), ws)
    }
  }

  def accepts(word: List[Sym]): Boolean = {
    isFinal(run(initialState, word))
  }
}

case class NFA[State, Sym](validStates: Set[State], move: (State, Option[Sym]) => Set[State], initialState: State, isFinal: State => Boolean) {
  def move(states: Set[State], w: Option[Sym]): Set[State] = {
    states match {
      case (x + xs) => move(x, w) ++ move(xs, w)
      case _ => Set()
    }
  }
  def run(states: Set[State], word: List[Sym]): Set[State] = {
    word match {
      case Nil => epsClosure(states)
      case w :: ws =>
        val newStates: Set[State] = move(states, Some(w))
        run(epsClosure(newStates), ws)
    }
  }

  def epsClosure(states: Set[State]): Set[State] = {
    val m: Set[State] = move(states, None[Sym])
    val newStates: Set[State] = states ++ m

    if (states same newStates) {
      states
    } else {
      epsClosure(newStates)
    }
  }

  def epsClosed(states: Set[State]): Boolean = {
    states same epsClosure(states)
  }

  def accepts(word: List[Sym]): Boolean = {
    val start: Set[State] = epsClosure(Set(initialState))
    run(start, word) exists isFinal
  }

}

object NFASpecs {
  def moveValid[S, W](nfa: NFA[S, W], states: Set[S], w: Option[W]): Boolean = {
    nfa.move(states, w) subsetOf nfa.validStates
  }
  def epsClosureIdem[S, W](nfa: NFA[S, W], states: Set[S]): Boolean = {
    nfa.epsClosure(states) same nfa.epsClosure(nfa.epsClosure(states))
  }
  def epsClosureIdem2[S, W](nfa: NFA[S, W], states: Set[S]): Boolean = {
    nfa.epsClosure(states) == nfa.epsClosure(nfa.epsClosure(states))
  }
}
