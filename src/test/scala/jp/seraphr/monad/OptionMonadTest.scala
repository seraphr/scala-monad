package jp.seraphr.monad

import org.scalatest.FunSuite
import jp.seraphr.monad.Monad._
import jp.seraphr.monad.MyOption._

/**
 *
 */
class OptionMonadTest extends FunSuite with TestUtil {
  test("unit returns Some") {
    assert(unit(11) === Some(11))
  }

  test("unit(null) returns None") {
    assert(unit(null) === None)
  }

  test( """"Some bind f" returns Some""") {
    val tHogehoge = unit("hogehoge")
    val tLength = tHogehoge.bind(a => unit(a.length))

    assert(tLength === Some("hogehoge".length))
  }

  test( """"None bind f" returns None """) {
    val tNone = unit(null)
    val tPiyo = tNone.bind(_ => unit("piyo"))

    assert(tPiyo === None)
  }

  test("MyOption can be used in for") {
    val tHoge = unit("hoge")
    val tPiyo = unit("piyo")
    val tHogePiyo = for {
      a <- tHoge
      b <- tPiyo
    } yield a + b

    assert(tHogePiyo === Some("hogepiyo"))
  }

  test("can use for with None") {
    val tNone = for {
      a <- unit("hoge")
      b <- unit(null)
      c <- unit("piyo")
    } yield a + b + c

    assert(tNone === None)
  }

}
