package feh.dsl.swing2

import scala.collection.mutable
import feh.util._

trait Var[T] {
  def get: T
  def set(t: T)
  def affect(f: T => T)

  def onChange(f: T => Unit): Var.Unregister
}

object Var{
  type Unregister = () => Unit

  def apply[T](initial: T): Var[T]        = new Impl[T](initial)
  def readOnly[T](value:  T): Var[T]      = new ReadOnly[T](value)
  def static[T](value:  T): Var[T]        = readOnly(value)
  def proxy[T](get: => T, set: T => Unit) = new Proxy[T](get, set)

  trait ImplCommon[T]{
    self: Var[T] =>

    protected val onUpdate = mutable.HashSet.empty[T => Unit]
    protected def updated(t: T) = onUpdate.foreach(_(t))

    def onChange(f: T => Unit): Var.Unregister = {
      onUpdate += f
      () => onUpdate -= f
    }

  }

  class Impl[T](initial: T) extends Var[T] with ImplCommon[T]{
    private var v = initial

    def get = synchronized(v)

    def set(t: T) = {
      synchronized(v = t)
      updated(t)
    }

    def affect(f: (T) => T): Unit = {
      val x = synchronized(f(v) $$ (v = _))
      updated(x)
    }

    def atomically[R](f: T => R): R = synchronized(f(v))

  }

  class ReadOnly[T](value: T) extends Var[T]{
    def get: T = value

    def set(t: T): Unit = {}
    def affect(f: (T) => T): Unit = {}

    def onChange(f: T => Unit): Unregister = () => {}
  }

  class Proxy[T](_get: => T, _set: T => Unit) extends Var[T] with ImplCommon[T]{
    def get = synchronized(_get)
    def set(t: T) = synchronized{
      _set(t)
      updated(t)
    }

    def affect(f: (T) => T): Unit = {
      val x = synchronized(f(_get) $$ _set)
      updated(x)
    }
  }

}
