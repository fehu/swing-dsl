package feh.dsl.swing

import scala.swing.event.Event
import scala.swing.Component

trait AbstractGUI extends SwingFrameAppCreation with SwingAppBuildingEnvironment{

  trait GuiFrame extends SwingFrame with LayoutDSL with LayoutBuilder

  object Description{
    sealed trait Conf

    /**
     * General idea is using it to define APIs for elements
     */
    class Elem[Builder <: AbstractDSLBuilder](defaultBuilder: Builder) extends Conf{
      private var _builder = defaultBuilder
      def builder: Builder = _builder
      def reconfigure(f: (Builder => Builder)*) = {
        _builder = Function.chain(f)(builder)
        this
      }
      def has[X](depend: X) = new Elem(builder) with Has[X]{ val dependency = depend }
    }

    trait EventHandler{
      self: Conf =>
      type HandleEvent[E <: Event] = E => Unit
    }
    trait HandlesEvent[E <: Event] extends EventHandler{
      self: Conf =>
      def handle: HandleEvent[E]
    }
    trait Handles2Events[E1 <: Event, E2 <: Event] extends EventHandler{
      self: Conf =>
      def handle1: HandleEvent[E1]
      def handle2: HandleEvent[E2]
    }

    trait Needs[X] {
      self: Conf =>
      def dependency: X
    }

    trait Has[X] extends Needs[X]{ self: Conf => }
    
    object Elem{
      def apply[Builder <: AbstractDSLBuilder](b: Builder) = new Elem[Builder](b)
      def apply[Builder <: AbstractDSLBuilder, E <: Event](b: Builder, event: E => Unit) =
        new Elem[Builder](b) with HandlesEvent[E]{ def handle = event }
    }

    implicit def builderToElem[Builder <: AbstractDSLBuilder](b: Builder): Elem[Builder] = Elem(b)
		implicit def elemToBuilder[Builder <: AbstractDSLBuilder](el: Elem[Builder]): Builder = el.builder
		implicit def elemToComponent(el: Elem[_ <: AbstractDSLBuilder]): Component = el.builder.component
		implicit def formElemToBuildMeta[B <: DSLFormBuilder[_]](el: Elem[B]): BuildMeta = el.builder.formMeta

    implicit def panelElemToBuildMeta(el: Elem[PanelBuilder]): BuildMeta = el.builder.meta
    implicit def boxPanelElemToBuildMeta(el: Elem[BoxPanelBuilder]): BuildMeta = el.builder.meta
    implicit def elemToGridBagMeta(el: Elem[GridBagBuilder]): BuildMeta = el.builder

		
    type AbstractBuilder = AbstractDSLBuilder
    type LabelBuilder[T] = DSLLabelBuilder[T]
    type ButtonBuilder = DSLButtonBuilder
    type KeyedListBuilder[K, V] = DSLKeyedListBuilder[K, V]
    type PanelBuilder = DSLPanelBuilder
    type GridBagBuilder = GridBagMeta
    type BoxPanelBuilder = DSLBoxPanelBuilder
  }
}