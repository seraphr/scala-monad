package jp.seraphr.monad

import org.scalatest.FunSuite
import state.StateStack

/**
 *
 */
class StateStackTest extends FunSuite with TestUtil {

  import jp.seraphr.monad.Monad._

  test("unit returns State") {
    object StringStateStack extends StateStack[String]
    import StringStateStack._

    val tUnit = unit("hogehoge")
    typeIs[State[List[String], String]](tUnit)
    assert(tUnit.run(List()) === ("hogehoge", List()))
  }

  test("pushF push value") {
    object StringStateStack extends StateStack[String]
    import StringStateStack._

    val tPushHoge = unit("fuga").bind(pushF("hoge"))
    assert(tPushHoge.run(List()) === ((), List("hoge")))
  }

  test("popF pop value") {
    object StringStateStack extends StateStack[String]
    import StringStateStack._

    val tPopFuga = push("fuga").bind(popF)
    assert(tPopFuga.run(List()) === ("fuga", List()))
  }

  test("isEmptyF check if the StackIsEmpty or else") {
    object StringStateStack extends StateStack[String]
    import StringStateStack._

    val tIsEmpty = push("fuga").bind(popF).bind(isEmptyF)
    assert(tIsEmpty.run(List()) === (true, List()))
    assert(tIsEmpty.run(List("hoge")) === (false, List("hoge")))
  }

  test("functions can be used in for") {
    object StringStateStack extends StateStack[String]
    import StringStateStack._

    val tHogeFugaPiyo = for {
      _ <- push("fuga")
      _ <- push("hoge")
      _ <- push("piyo")
      tPiyo <- pop
      tHoge <- pop
      tFuga <- pop
    } yield tHoge + tFuga + tPiyo

    assert(tHogeFugaPiyo.run(List()) === ("hogefugapiyo", List()))
  }

  test("new function add can be created") {
    object IntStateStack extends StateStack[Int]
    import IntStateStack._

    def add = for {
      left <- pop
      right <- pop
      _ <- push(left + right)
    } yield ()

    val tAdd = for {
      _ <- push(10)
      _ <- push(20)
      _ <- push(30)
      _ <- add
      _ <- add
      v <- pop
    } yield v

    assert(tAdd.run(List()) === (60, List()))
  }
}
