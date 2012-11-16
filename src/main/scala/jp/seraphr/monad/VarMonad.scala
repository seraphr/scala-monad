package jp.seraphr.monad

/**
 * 一つの値を格納するコンテナ型
 *
 * @param value
 * @tparam _E
 */
case class Var[+_E](value: _E)

/**
 * Var用Monadインスタンス
 */
object VarMonad extends Monad[Var] {
  override def unit[_E](aElement: _E): Var[_E] = Var(aElement)

  override def bind[_E1, _E2](aMonad: Var[_E1])(aFunc: _E1 => Var[_E2]) = aFunc(aMonad.value)
}
