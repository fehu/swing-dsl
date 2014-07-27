package feh.dsl.swing.form.impl

import feh.util._
import feh.dsl.swing.form.{TextCreationDSLHelper, FormCreationDSL}
import FormCreationDSL._
import scala.swing.{TextField, Label}
import scala.concurrent.{ExecutionContext, Future}
import feh.dsl.swing.layout.LayoutDSL.{BuildMeta, Placable}

trait Default extends FormCreationDSL with TextCreationDSLHelper with DefaultTextFormCreationDSLBuilders{
  protected val formsCfg = cfg

  def updateForms(): Unit = ???

  implicit def dslFormBuilderIsPlacable[T](p: (DSLFormBuilder[T], String)): Placable[T] = new Placable[T] {
    def meta = p._1.meta
    def id = Option(p._2)
  }

}


trait DefaultTextFormCreationDSLBuilders{
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
  // // // // // // // builders // // // // // // //
  // // // // // // // // // // // // // // // // //

  case class DSLLabelBuilder[T] protected[form] (
       get: () => T,
       static: Boolean,
       extractString: Either[(T) => String, ((T) => Future[String], ExecutionContext)] = Default.extractStringSync[T],
       effects: List[DSLLabelBuilder[T]#Form => Unit] = Nil)
    extends DSLFormBuilder[T] with DSLFutureStringExtraction[T, String]
  {
    type Form = Label with UpdateInterface
    protected type Builder = DSLLabelBuilder[T]
    def formName = "Label"

    def static(set: Boolean): Builder = copy(static = set)
    def affect(effects: (Comp => Unit)*): Builder = copy(effects = this.effects ++ effects)

    def build: Form = new Label with UpdateInterface{
      if(static) extractAndThen(get(), text = _)
      def updateForm() = if(!static) { extractAndThen(get(), text = _) }

      effects.foreach(_(this))
    }

  }

  case class DSLTextBuilder[T] protected[form] (get: () => T,
                                                static: Boolean,
                                                extractString: Either[(T) => String, ((T) => Future[String], ExecutionContext)],
                                                effects: List[DSLTextBuilder[T]#Form => Unit] = Nil)
    extends DSLFormBuilder[T] with DSLFutureStringExtraction[T, String]
  {
    type Form = TextField with UpdateInterface
    protected type Builder = DSLTextBuilder[T]
    def formName: String = "TextField"

    def static(set: Boolean): Builder = copy(static = set)
    def affect(effects: (Comp => Unit)*): Builder = copy(effects = this.effects ++ effects)

    def build: Form = new TextField() with UpdateInterface{
      if(static) extractAndThen(get(), text = _)
      def updateForm() = if(!static) { extractAndThen(get(), text = _) }

      effects.foreach(_(this))
    }
  }


  // // // // // // // // // // // // // // // // //
  // // // // // // // choosers // // // // // // //
  // // // // // // // // // // // // // // // // //

  implicit def monitorComponentChooser[T](v: => T): MonitorComponentChooser[T] = new MonitorComponentChooser[T]{
    protected val formsCfg = cfg

    def text: formsCfg.Label[T] = DSLLabelBuilder(v.lifted, static = false)

    /** unlike [[text]], [[label]] content loads only at form creation */
    def label: formsCfg.Label[T] = DSLLabelBuilder(v.lifted, static = true)



    def textField: formsCfg.Text[T] = ???

    def bigText: formsCfg.TextComponent[T] = ???

    def textArea: formsCfg.TextArea[T] = ???
  }
}

object Default{

  def extractStringSync[T] = Left((_: T).toString)

}