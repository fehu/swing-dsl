package feh.dsl.swing.form

import scala.swing.Component
import scala.collection.immutable.NumericRange
import scala.reflect.ClassTag
import scala.concurrent.{ExecutionContext, Future}
import feh.util._
import feh.dsl.swing.layout.LayoutDSL.{BuildMetaType, BuildMeta}


trait FormCreationMeta {
  type Constraints

  case object FormBuildMetaTpe extends BuildMetaType("Form")

//    implicit class MetaWrapper(meta: BuildMeta){
//      def copy(c: Component = meta.component, layout: List[Constraints => Unit] = meta.layout) = BuildMeta(c, layout: _*)
//      def addLayout(effects: (Constraints => Unit)*) = copy(layout = meta.layout ++ effects)
//    }

}

trait FormCreationDSLBuilders extends FormCreationMeta{

  trait AbstractDSLBuilder{
    type Comp <: Component

    protected type Builder <: AbstractDSLBuilder // self type

    def build: Comp

    def meta: BuildMeta

    def affect(effects: (Comp => Unit)*): Builder
//    @deprecated("should not be here, it has to do only with LayoutDSL")
//    def layout(effects: (Constraints => Unit)*): Builder
  }

  trait UpdateInterface{
    def updateForm()
  }


  trait DSLFormBuilder[T] extends AbstractDSLBuilder{
    builder =>

    type Form <: Component with UpdateInterface
//    type Params
    def formName: String

    def static: Boolean
    def static(set: Boolean): DSLFormBuilder[T]

    type Comp = Form

    def meta = BuildMeta(FormBuildMetaTpe, formName, build)
  }

  trait DSLFutureStringExtraction[T, Inner] extends DSLFormBuilder[T]{
    def get: () => T
    def extractString: Either[T => Inner, (T => Future[Inner], ExecutionContext)]
    def extractAndThen(from: T, f: Inner => Unit) = extractString
      .left.map{ extr => f(extr(get())) }
      .right.map{ case (extr, c) => extr(get()).map(f)(c) }
  }


  trait FormCreationDSLBuilderConfig{
    type Label[T] <: DSLFormBuilder[T]
    type Text[T] <: DSLFormBuilder[T]
    type TextArea[T] <: DSLFormBuilder[T]
    type TextComponent[T] <: DSLFormBuilder[T]
    type Spinner[T] <: DSLFormBuilder[T]
    type Slider[T] <: DSLFormBuilder[T]

    type List[T] <: DSLFormBuilder[T]
    type ComboBox[T] <: DSLFormBuilder[T]

    type Button <: DSLFormBuilder[Unit]
    type ToggleButton <: DSLFormBuilder[Unit]
  }
}

trait FormCreationChoosers extends FormCreationDSLBuilders{

  trait DSLComponentChooser{
    protected val formsCfg: FormCreationDSLBuilderConfig
  }

  trait MonitorComponentChooser[T, Config <: FormCreationDSLBuilderConfig] extends DSLComponentChooser{
    protected val formsCfg: Config

    def text: formsCfg.Label[T]
    /** unlike [[text]], [[label]] content loads only at form creation
      */
    def label: Config#Label[T]
    def textField: Config#Text[T]
    def textArea: Config#TextArea[T]
    def bigText: Config#TextComponent[T]

    def asText = text
    def asLabel = text
    def asTextField = textField
    def asTextArea = textArea
  }

  trait MapMonitorComponentChooser[K, V, Config <: FormCreationDSLBuilderConfig] extends MonitorComponentChooser[Map[K, V], Config]{
    def list(implicit order: Ordering[K]): Config#List[Map[K, V]]

    def asList(implicit order: Ordering[K]) = list
  }

  trait ControlComponentChooser[T, Config <: FormCreationDSLBuilderConfig] extends DSLComponentChooser{
    def textForm(implicit tBuilder: String => T): Config#Text[T]
    def textArea(implicit tBuilder: String => T): Config#TextArea[T]
  }

  trait NumericControlComponentChooser[N, Config <: FormCreationDSLBuilderConfig] extends ControlComponentChooser[N, Config]{
    implicit def num: Numeric[N]

    def spinner: Config#Spinner[N]
    def slider(range: NumericRange[N]): Config#Slider[N]
  }

  trait SeqControlComponentChooser[T, Config <: FormCreationDSLBuilderConfig] extends DSLComponentChooser{
    implicit def ctag: ClassTag[T]

    def dropDownList(set: T => Unit): Config#ComboBox[T]
  }

  trait ToggleComponentChooser[Config <: FormCreationDSLBuilderConfig] extends DSLComponentChooser{
    def toggle(label: String): Config#ToggleButton
  }

  trait TriggerComponentChooser[Config <: FormCreationDSLBuilderConfig] extends DSLComponentChooser{
    def button(label: String): Config#Button
  }
}