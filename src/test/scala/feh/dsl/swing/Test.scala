package feh.dsl.swing


class Test extends layout.Grid9 with form.DSL{

  val l1 = label("Label 1")

  def l1_ = dslFormBuilderIsPlacable(l1 -> "label")

  def p1 = place(l1_) in theCenter

  layout(
    p1
  )

}
