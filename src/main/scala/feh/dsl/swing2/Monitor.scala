package feh.dsl.swing2

import javax.swing.table.DefaultTableModel

import scala.swing._
import scala.swing.event.ButtonClicked


trait Monitoring[T, C <: Component]{
  def component: C
  def variable: Var[T]
}

trait Controlling[T, C <: Component] extends Monitoring[T, C]{
  var onUserChange: T => Unit = t => {}
}

trait Monitor[T, C <: Component] {
  def build(_c: => C, v: Var[T]): Monitoring[T, C]
}

object Monitor{
  def apply[T, C <: Component](c: => C, v: Var[T])
                              (implicit m: Monitor[T, C]): Monitoring[T, C] = m.build(c, v)
  def apply[T, C <: Component](v: Var[T], c: => C)
                              (implicit m: Monitor[T, C]): Monitoring[T, C] = m.build(c, v)

  def custom[T, C <: Component](v: Var[T], _c: => C)
                               (initial: C => Unit)
                               (onVarChange: C => T => Unit): Monitoring[T, C] =
    new Monitoring[T, C]{
      lazy val component: C = {
        val c = _c
        initial(c)
        v.onChange(onVarChange(c))
        c
      }

      def variable: Var[T] = v
    }

  def forTable[T](v: Var[T], c: => Table)
                 (columns: Seq[String], initial: T, prepareData: T => Array[Array[AnyRef]])
                 (afterModelUpd: Table => Unit = _ => {}): Monitoring[T, Table] = {

    def mkModel(t: T) = new DefaultTableModel(prepareData(t), columns.toArray[AnyRef]){
      override def isCellEditable(row: Int, column: Int) = false
    }

    Monitor.custom(v, c){
      c =>      c.model = mkModel(initial)
                afterModelUpd(c)
    }{
      c => t => c.model = mkModel(t)
                afterModelUpd(c)
    }
  }

  implicit def monitorForLabel[T]: Monitor[T, Label] = new Monitor[T, Label]{
    def build(c: => Label, v: Var[T]): Monitoring[T, Label] = buildTextMonitor(c, v)
  }

  implicit object ProgressControl extends Monitor[Int, ProgressBar]{
    override def build(_c: => ProgressBar, v: Var[Int]): Monitoring[Int, ProgressBar] = new Monitoring[Int, ProgressBar] {
      lazy val component: ProgressBar = {
        val c = _c
        v.onChange(c.value = _)
        c
      }
      def variable: Var[Int] = v
    }
  }


  private def buildTextMonitor[T, C <: Component {def text: String; def text_=(s: String)}](_c: => C, v: Var[T]) = new Monitoring[T, C] {
    lazy val component: C = {
      val c = _c
      v.onChange(v => c.text = v.toString)
      c
    }
    def variable: Var[T] = v
  }
}


trait Control[T, C <: Component] extends Monitor[T, C]{
  override def build(c: => C, v: Var[T]): Controlling[T, C]
}

object Control{
  def apply[T, C <: Component](c: => C, v: Var[T])
                              (implicit m: Control[T, C]): Controlling[T, C] = m.build(c, v)
  def apply[T, C <: Component](v: Var[T], c: => C)
                              (implicit m: Control[T, C]): Controlling[T, C] = m.build(c, v)

  def custom[T, C <: Component](v: Var[T], _c: => C)
                               (initial: C => Unit)
                               (onVarChange: C => T => Unit)
                               (listenToUserChange: C => Unit): Controlling[T, C] =
    new Controlling[T, C] {
      lazy val component: C = {
        val c = _c
        initial(c)
        v.onChange(onVarChange(c))
        listenToUserChange(c)
        c
      }

      def variable: Var[T] = ???
    }

  implicit object CheckboxControlForBoolean extends Control[Boolean, CheckBox]{
    override def build(_c: => CheckBox, v: Var[Boolean]): Controlling[Boolean, CheckBox] = new Controlling[Boolean, CheckBox]{
      def variable = v
      lazy val component = {
        val c = _c
        c.selected = v.get
        v.onChange(c.selected = _)
        c.reactions += {
          case ButtonClicked(_) =>
            val sel = c.selected
            v set sel
            onUserChange(sel)
        }
        c
      }

    }
  }
}