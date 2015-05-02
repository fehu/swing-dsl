package feh.dsl.swing2

import scala.swing.{ProgressBar, TextComponent, Label, Component}


trait Monitoring[T, C <: Component]{
  def component: C
  def variable: Var[T]
}

trait Controlling[T, C <: Component] extends Monitoring[T, C]

trait Monitor[T, C <: Component] {
  def build(c: C, v: Var[T]): Monitoring[T, C]
}

object Monitor{
  def apply[T, C <: Component](c: C, v: Var[T])
                              (implicit m: Monitor[T, C]): Monitoring[T, C] = m.build(c, v)
  def apply[T, C <: Component](v: Var[T], c: C)
                              (implicit m: Monitor[T, C]): Monitoring[T, C] = m.build(c, v)

  implicit def monitorForLabel[T]: Monitor[T, Label] = new Monitor[T, Label]{
    def build(c: Label, v: Var[T]): Monitoring[T, Label] = buildTextMonitor(c, v)
  }

  implicit object ProgressControl extends Monitor[Int, ProgressBar]{
    override def build(c: ProgressBar, v: Var[Int]): Monitoring[Int, ProgressBar] = new Monitoring[Int, ProgressBar] {
      lazy val component: ProgressBar = {
        v.onChange(c.value = _)
        c
      }
      def variable: Var[Int] = v
    }
  }


  private def buildTextMonitor[T, C <: Component {def text: String; def text_=(s: String)}](c: C, v: Var[T]) = new Monitoring[T, C] {
    def component: C = {
      v.onChange(v => c.text = v.toString)
      c
    }
    def variable: Var[T] = v
  }
}


trait Control[T, C <: Component] extends Monitor[T, C]{
  override def build(c: C, v: Var[T]): Controlling[T, C]
}

object Control{
  def apply[T, C <: Component](c: C, v: Var[T])
                              (implicit m: Control[T, C]): Controlling[T, C] = m.build(c, v)

}