package jp.seraphr.monad

sealed trait MyOption[+_E]

case class Some[_E](value: _E) extends MyOption[_E]

case object None extends MyOption[Nothing]

object MyOption {

  /**
   * MyOption用Monadインスタンス
   */
  implicit object OptionMonad extends Monad[MyOption] {
    override def unit[_E](aElement: _E): MyOption[_E] = if (aElement == null) None else Some(aElement)

    override def bind[_E1, _E2](aMonad: MyOption[_E1])(aFunc: _E1 => MyOption[_E2]) = aMonad match {
      case Some(tVal) => aFunc(tVal)
      case None => None
    }
  }

}