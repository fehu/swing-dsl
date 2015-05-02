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

  def apply[T](initial: T): Var[T]    = new Impl[T](initial)
  def readOnly[T](value:  T): Var[T]  = new ReadOnly[T](value)
  def static[T](value:  T): Var[T]    = readOnly(value)

  class Impl[T](initial: T) extends Var[T]{
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

    protected val onUpdate = mutable.HashSet.empty[T => Unit]
    protected def updated(t: T) = onUpdate.foreach(_(t))

    def onChange(f: T => Unit): Var.Unregister = {
      onUpdate += f
      () => onUpdate -= f
    }
  }

  class ReadOnly[T](value: T) extends Var[T]{
    def get: T = value

    def set(t: T): Unit = {}
    def affect(f: (T) => T): Unit = {}

    def onChange(f: T => Unit): Unregister = () => {}
  }


}
