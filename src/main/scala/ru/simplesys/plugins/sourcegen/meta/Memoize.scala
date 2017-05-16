package ru.simplesys.plugins
package sourcegen
package meta

class Memoize1[-T, +R](f: T => R) extends (T => R) {
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R = {
    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x)
      vals + ((x, y))
      y
    }
  }
}

object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)
}

class Memoize2[-T1, -T2, +R](f: (T1, T2) => R) extends ((T1, T2) => R) {
  import scala.collection.mutable
  private[this] val vals = mutable.Map.empty[(T1, T2), R]

  def apply(x: T1, y: T2): R = {
    if (vals.contains(x, y)) {
      vals(x, y)
    }
    else {
      val z = f(x, y)
      vals + ((x -> y, z))
      z
    }
  }
}

object Memoize2 {
  def apply[T1, T2,  R](f: (T1, T2) => R) = new Memoize2(f)
}