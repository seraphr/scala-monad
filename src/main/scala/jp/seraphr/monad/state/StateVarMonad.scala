package jp.seraphr.monad.state

import jp.seraphr.monad.State.StateMonad
import jp.seraphr.monad.{Monad, State, Var}

/**
 *
 */
class StateVar[_V] {
  type _StateVar[_E] = State[Var[_V], _E]

  /**
   * StateMonadを用いて実装したVar用Monadインスタンス
   *
   * @return
   */
  implicit val StateVarMonad: Monad[_StateVar] = new StateMonad[Var[_V]]

  def get: _StateVar[_V] = State[Var[_V], _V](v => (v.value, v))

  def getF = (_: Any) => get

  def set(aElement: _V): _StateVar[Unit] = State[Var[_V], Unit](v => ((), Var(aElement)))

  def setF(aElement: _V) = (_: Any) => set(aElement)
}
