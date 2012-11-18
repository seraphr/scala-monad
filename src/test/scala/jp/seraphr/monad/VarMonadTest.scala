package jp.seraphr.monad

import org.scalatest.FunSuite

/**
 *
 */
class VarMonadTest extends FunSuite with TestUtil {

  import jp.seraphr.monad.Monad._
  import jp.seraphr.monad.Var._

  test("unit returns Var") {
    val tVar = unit("hoge")
    typeIs[Var[String]](tVar)
    assert(tVar === Var("hoge"))
  }

  test("bind returns new Var") {
    val tVar = unit("hoge")
    val tLength = tVar.bind(a => Var(a.length))
    typeIs[Var[Int]](tLength)
    assert(tLength === Var("hoge".length))
  }

  test("Var can be used in for") {
    import jp.seraphr.monad.Monad._
    import jp.seraphr.monad.Var._

    val tVariable = unit(10)
    val tVariable2 = unit(20)

    val t200 = for {
      a <- tVariable
      b <- tVariable2
    } yield a * b

    typeIs[Var[Int]](t200)
    assert(t200.value === 200)
  }
}
