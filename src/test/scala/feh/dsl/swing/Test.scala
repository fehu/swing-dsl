package feh.dsl.swing

import feh.dsl.swing.layout.LayoutBuilder


object Test extends layout.Grid9 with form.DSL with App{

  implicit def builder: LayoutBuilder[Placing] = ???

  val l = layout(
    place( label("Label 1") -> noId ) in theCenter,
    place( monitorFor("Text 1").text -> "txt1" ) in theWest
  )

  println(l)
  println(l.components.all)

  val f = frame(l -> noId)
}
