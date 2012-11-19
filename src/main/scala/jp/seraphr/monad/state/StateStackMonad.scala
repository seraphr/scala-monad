package jp.seraphr.monad.state

import jp.seraphr.monad.{Monad, State}
import jp.seraphr.monad.State.StateMonad

/**
 *
 */
class StateStack[_V] {
  type _StateStack[_E] = State[List[_V], _E]

  implicit val StateStackMonad: Monad[_StateStack] = new StateMonad[List[_V]]

  def pop: _StateStack[_V] = State[List[_V], _V](xs => (xs.head, xs.tail))

  def popF = (_: Any) => pop

  def push(aElement: _V): _StateStack[Unit] = State[List[_V], Unit](xs => ((), aElement :: xs))

  def pushF(aElement: _V) = (_: Any) => push(aElement)

  def isEmpty: _StateStack[Boolean] = State[List[_V], Boolean](xs => (xs.isEmpty, xs))

  def isEmptyF = (_: Any) => isEmpty
}
