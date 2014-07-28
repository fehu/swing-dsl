package feh.dsl.swing


object Test extends layout.Grid9 with form.DSL with App{

  def l = layout(
    place(label("Label 1") -> noId) in theCenter,
    place( monitorFor("text 1").text -> "text1" ) in theWest
  )

  println(l)
}
