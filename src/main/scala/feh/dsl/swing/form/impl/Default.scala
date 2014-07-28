package feh.dsl.swing.form.impl

import feh.util._
import feh.dsl.swing.form.{TextCreationDSLHelper, FormCreationDSL}
import FormCreationDSL._
import scala.swing.{TextField, Label}
import scala.concurrent.{ExecutionContext, Future}
import feh.dsl.swing.layout.LayoutDSL.{BuildMeta, Placable}
import scala.xml.NodeSeq

trait Default extends FormCreationDSL with TextCreationDSLHelper with TextFormCreationDefaults{
  type FormConfig = cfg.type
  protected val formsCfg = cfg

  def updateForms(): Unit = ???

  implicit def dslFormBuilderIsPlacable[T](p: (DSLFormBuilder[T], String)): Placable[T] = new Placable[T] {
    def meta = p._1.meta
    def id = Option(p._2)
  }


  val monitorFor = new MonitorCreation[FormConfig] {
    type Chooser[T] = MonitorComponentChooser[T, FormConfig]
    type MChooser[K, V] = MapMonitorComponentChooser[K, V, FormConfig]
  }
  val action = new ActionCreation[FormConfig]
  val controlFor = new ControlCreation[FormConfig]

  def html[T](t: => T)(build: T => NodeSeq)(implicit chooser:  (=> T) => MonitorComponentChooser[T, FormConfig]): FormConfig#Label[T] =
    label(t).extractSync(t => surroundHtml(<html>{build(t)}</html>).toString())

  def label[T](text: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T, FormConfig]): FormConfig#Label[T] =
    monitorFor(text).label.static(set = true)

}


trait TextFormCreationDefaults extends DefaultTextFormCreationDSLBuilders{
  // // // // // // // // // // // // // // // // //
  // // // // // // // configs  // // // // // // //
  // // // // // // // // // // // // // // // // //

  object cfg extends FormCreationDSL.FormCreationDSLBuilderConfig
    with TextFormsConfig
  {

  }

  trait TextFormsConfig extends FormCreationDSLBuilderConfig{
    type Label[T] = DSLLabelBuilder[T]
    type Text[T] = DSLTextBuilder[T]
//    type TextArea = this.type
//    type TextComponent = this.type
  }

  // // // // // // // // // // // // // // // // //
  // // // // // // // choosers // // // // // // //
  // // // // // // // // // // // // // // // // //

  implicit def monitorComponentChooser[T](v: => T) = new MonitorComponentChooser[T, cfg.type]{
    type Config = cfg.type
    protected val formsCfg = cfg

    def text: Config#Label[T] = DSLLabelBuilder(v.lifted, static = false)

    /** unlike [[text]], [[label]] content loads only at form creation */
    def label: Config#Label[T] = DSLLabelBuilder(v.lifted, static = true)



    def textField: Config#Text[T] = DSLTextBuilder(v.lifted, static = false)

    def bigText: Config#TextComponent[T] = ???

    def textArea: Config#TextArea[T] = ???
  }
}

object Default{
  def extractStringSync[T] = Left((_: T).toString)

}