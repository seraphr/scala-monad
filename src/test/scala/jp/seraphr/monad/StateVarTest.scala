package jp.seraphr.monad

import jp.seraphr.monad.state.StateVar
import org.scalatest.FunSuite

/**
 *
 */
class StateVarTest extends FunSuite with TestUtil {

  import jp.seraphr.monad.Monad._

  test("unit returns State") {
    object IntStateVar extends StateVar[Int]
    import IntStateVar._

    val tUnit = unit(100)
    typeIs[State[Var[Int], Int]](tUnit)
    assert(tUnit.run(Var(1)) ===(100, Var(1)))
  }

  test("setF set value") {
    object IntStateVar extends StateVar[Int]
    import IntStateVar._

    val tSet100 = unit(0).bind(setF(100))
    assert(tSet100.run(Var(1)) ===((), Var(100)))
  }

  test("getF get value") {
    object IntStateVar extends StateVar[Int]
    import IntStateVar._

    val tGet100 = unit(0).bind(setF(100)).bind(getF)
    val tAssert = tGet100.map(a => {
      assert(a === 100);
      a
    })

    tAssert.run(Var(1))
  }

  test("set and get can be use in for") {
    object IntStateVar extends StateVar[Int]
    import IntStateVar._

    val tSetGet = for {
      _ <- set(321)
      v <- get
      _ <- set(111)
      v2 <- get
    } yield v + v2

    assert(tSetGet.run(Var(0)) ===(432, Var(111)))
  }

  test("new function add can be created") {
    object IntStateVar extends StateVar[Int]
    import IntStateVar._

    def add(aArg: Int) = for {
      v <- get
      _ <- set(v + aArg)
    } yield ()

    val tAdd20 = for {
      _ <- add(20)
      v <- get
    } yield v

    assert(tAdd20.run(Var(100)) ===(120, Var(120)))
  }
}
