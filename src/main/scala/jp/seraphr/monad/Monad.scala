package jp.seraphr.monad

/**
 * 型クラスMonad
 *
 * @tparam _M Monadのインスタンスを持つ（型クラスMonadに属する）型
 */
trait Monad[_M[_]] {
  def unit[_E](aElement: _E): _M[_E]
  def bind[_E1, _E2](aMonad: _M[_E1])(aFunc: _E1 => _M[_E2]): _M[_E2]
}

/**
 * Monadの各メソッドを呼び出したりするオブジェクト
 */
object Monad {
  /**
   * aElementを格納する_M型のオブジェクトを生成する
   *
   * @param aElement コンテナに格納する要素
   * @tparam _E Monadの要素型
   * @tparam _M Monadに属するコンテナ型
   * @return aElementを格納する_M型のオブジェクト
   */
  def unit[_E, _M[_] : Monad](aElement: _E) = implicitly[Monad[_M]].unit(aElement)

  /**
   * Monadに属する型のオブジェクトに対して、bindの呼び出しを行う
   *
   * @param aMonad Monadに属する型のオブジェクト
   * @param aFunc bindの引数
   * @tparam _E1 現在コンテナに格納されている型
   * @tparam _E2 変換後にコンテナに格納されている型
   * @tparam _M Monadに属するコンテナ型
   * @return bind適用後の_M型のオブジェクト
   */
  def bind[_E1, _E2, _M[_] : Monad](aMonad: _M[_E1])(aFunc: _E1 => _M[_E2]) = implicitly[Monad[_M]].bind(aMonad)(aFunc)

  /**
   * Monadに属する型から、MonadImpl型への暗黙型変換
   *
   * @param aContainer
   * @tparam _E
   * @tparam _M
   * @return
   */
  implicit def MonadToImpl[_E, _M[_] : Monad](aContainer: _M[_E]) = new MonadImpl(aContainer, implicitly[Monad[_M]])
}

/**
 * モナドに属する型のオブジェクトに bind(flatMap)/map/foreachのメソッドを追加するためのラッパクラス
 *
 * @param aContainer
 * @param aMonad
 * @tparam _E
 * @tparam _M
 */
class MonadImpl[_E, _M[_]](aContainer: _M[_E], aMonad: Monad[_M]) {
  private implicit val mContainer = aContainer;
  private implicit val mMonad = aMonad;

  def bind[_E2](aFunc: _E => _M[_E2]) = Monad.bind(mContainer)(aFunc)
  def map[_E2](aFunc: _E => _E2) = this.bind(c => Monad.unit(aFunc(c)))

  def flatMap[_E2](aFunc: _E => _M[_E2]) = bind(aFunc)
  def foreach(aFunc: _E => Unit): Unit = this.bind(c => {
    aFunc(c); Monad.unit(c)
  })

  def con = aContainer

  override def toString = aContainer.toString
}