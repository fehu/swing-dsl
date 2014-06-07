package feh.dsl.swing.form

import scala.swing.Component
import scala.collection.immutable.NumericRange
import scala.reflect.ClassTag
import scala.concurrent.{ExecutionContext, Future}
import feh.util._


trait FormCreationMeta {
  type Constraints

  abstract class BuildMetaType(val name: String) {
    override def toString = name
  }
  case object FormBuildMetaTpe extends BuildMetaType("Form")

  sealed trait BuildMeta{
    def tpe: BuildMetaType
    
    def buildComponent: Component
    def component: String

    def name = s"$tpe:$component"
    override def toString: String = s"BuildMeta($name)"
  }

  object BuildMeta{
    def apply(`type`: BuildMetaType, name: String, build: => Component): BuildMeta = new BuildMeta{
      def tpe = `type`
      def buildComponent = build
      def component = name
    }

//    implicit class MetaWrapper(meta: BuildMeta){
//      def copy(c: Component = meta.component, layout: List[Constraints => Unit] = meta.layout) = BuildMeta(c, layout: _*)
//      def addLayout(effects: (Constraints => Unit)*) = copy(layout = meta.layout ++ effects)
//    }

    def build(`type`: BuildMetaType, name: String, build: => Component) = apply(`type`, name, build)

    def unapply(meta: BuildMeta): Option[(BuildMetaType, String, Lifted[Component])] =
      Some(meta.tpe, meta.component, meta.buildComponent.lift)
  }
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
    type Params
    def formName: String

    def static: Boolean
    def static(set: Boolean): Builder

    type Comp = Form

    def meta = BuildMeta(FormBuildMetaTpe, formName, build)

    override def affect(effects: (Comp => Unit)*): Builder
  }

  trait DSLFutureStringExtraction[T, Inner] extends DSLFormBuilder[T]{
    def get: () => T
    def extractString: Either[T => Inner, (T => Future[Inner], ExecutionContext)]
    def extractAndThen(from: T, f: Inner => Unit) = extractString
      .left.map{ extr => f(extr(get())) }
      .right.map{ case (extr, c) => extr(get()).map(f)(c) }
  }


  trait FormCreationDSLBuilderConfig{
    type Label[T] <: DSLFormBuilder[T] {
      def extract(f: T => String): Label[T]
    }
    type Text[T] <: DSLFormBuilder[T] {
      def extract(f: T => String): Label[T]
    }
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

  trait MonitorComponentChooser[T] extends DSLComponentChooser{
    def text: formsCfg.Label[T]
    /** unlike [[text]], [[label]] content loads only at form creation
      */
    def label: formsCfg.Label[T]
    def textField: formsCfg.Text[T]
    def textArea: formsCfg.TextArea[T]
    def bigText: formsCfg.TextComponent[T]

    def asText = text
    def asLabel = text
    def asTextField = textField
    def asTextArea = textArea
  }

  trait MapMonitorComponentChooser[K, V] extends MonitorComponentChooser[Map[K, V]]{
    def list(implicit order: Ordering[K]): formsCfg.List[Map[K, V]]

    def asList(implicit order: Ordering[K]) = list
  }

  trait ControlComponentChooser[T] extends DSLComponentChooser{
    def textForm(implicit tBuilder: String => T): formsCfg.Text[T]
    def textArea(implicit tBuilder: String => T): formsCfg.TextArea[T]
  }

  trait NumericControlComponentChooser[N] extends ControlComponentChooser[N]{
    implicit def num: Numeric[N]

    def spinner: formsCfg.Spinner[N]
    def slider(range: NumericRange[N]): formsCfg.Slider[N]
  }

  trait SeqControlComponentChooser[T] extends DSLComponentChooser{
    implicit def ctag: ClassTag[T]

    def dropDownList(set: T => Unit): formsCfg.ComboBox[T]
  }

  trait ToggleComponentChooser extends DSLComponentChooser{
    def toggle(label: String): formsCfg.ToggleButton
  }

  trait TriggerComponentChooser extends DSLComponentChooser{
    def button(label: String): formsCfg.Button
  }
}