package automaton

case class MemDFA[State, Sym](dfa: DFA[State, Sym], cache: Map[(State, Sym), State]) {
  def move(s: State, w: Sym): (State, MemDFA[State, Sym]) = {
    cache.get((s, w)) match {
      case Some(t) => (t, this)
      case None =>
        val t: State = dfa.move(s, w)
        (t, MemDFA(dfa, cache + ((s, w) -> t)))
    }
  }
  def run(state: State, word: List[Sym]): (State, MemDFA[State, Sym]) = {
    word match {
      case Nil => (state, this)
      case w :: ws =>
        val (t, mem) = move(state, w)
        mem.run(t, ws)
    }
  }
  def accepts(word: List[Sym]): (Boolean, MemDFA[State, Sym]) = {
    val (s, mem) = run(dfa.initialState, word)
    (dfa.isFinal(s), mem)
  }
}

object Specs {
  def equiv[State, Sym](dfa: DFA[State, Sym], mem: MemDFA[State, Sym], word: List[Sym]): Boolean = {
    dfa.accepts(word) == mem.accepts(word)._1
  }
}
