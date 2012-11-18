package jp.seraphr.monad

case class State[_S, +_E](run: _S => (_E, _S))

/**
 *
 */
object State {

  class StateMonad[_S] extends Monad[({type _State[_E] = State[_S, _E]})#_State] {
    override def unit[_E](aElement: _E): State[_S, _E] = State(s => (aElement, s))

    override def bind[_E1, _E2](aMonad: State[_S, _E1])(aFunc: _E1 => State[_S, _E2]): State[_S, _E2] = {
      val State(tPreviousFunc) = aMonad

      State(s0 => {
        val (e1, s1) = tPreviousFunc(s0)
        aFunc(e1).run(s1)
      })
    }
  }

}
