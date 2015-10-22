package feh.dsl.swing

import javax.swing.event.TableModelListener
import javax.swing.table.{AbstractTableModel, TableModel}
import feh.dsl.swing.swing.Spinner
import scala.swing._
import scala.swing.event.{EditDone, SelectionChanged, ValueChanged, ButtonClicked}
import scala.swing.Label
import java.awt.Color
import scala.collection.mutable
import feh.util._
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.concurrent.{ExecutionContext, Future}
import javax.swing._
import scala.reflect.ClassTag


object FormCreation extends FormCreation{
  type Constraints = GridBagPanel#Constraints

  object DSL extends FormCreationDSL{
    def updateForms(): Unit = ???
  }
}
import FormCreation._

trait FormCreation {

  protected trait FormCreationDSL{
    def monitorFor[K, V](get: => Map[K, V])(implicit chooser:  (=> Map[K, V]) => MapMonitorComponentChooser[K, V]) = chooser(get)
    def monitorFor[T](get: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T]) = chooser(get)
    def monitorForSeq[T](get: => Seq[T])(implicit chooser:  (=> Seq[T]) => SeqMonitorComponentChooser[T]) = chooser(get)
    def controlFor[T](get: => T)(set: T => Unit)(implicit chooser: (=> T, T => Unit) => ControlComponentChooser[T]) = chooser(get, set)
    def controlGivenDomain[T](get: => T)(set: T => Unit)(implicit chooser: (=> T, T => Unit) => WithDomainControlComponentChooser[T]) = chooser(get, set)
    def controlForSeq[T](get: => Seq[T], static: Boolean = false)(implicit chooser: (=> Seq[T], Boolean) => SeqControlComponentChooser[T]) = chooser(get, static)
    def controlForNumeric[N](get: => N)(set: N => Unit) // todo: sould it exist?
                                      (implicit chooser: (=> N, N => Unit) => NumericControlComponentChooser[N], num: Numeric[N]) = chooser(get, set)   
    def controlForOrdered[T](get: => T)(set: T => Unit) // todo: sould it exist?
                                      (implicit chooser: (=> T, T => Unit) => OrderedControlComponentChooser[T], ordering: Ordering[T]) = chooser(get, set)
    def triggerFor(action: => Unit)(implicit chooser: (=> Unit) => TriggerComponentChooser) = chooser(action)
    def toggleFor(on: => Unit, off: => Unit)(implicit chooser: (=> Unit, => Unit) => ToggleComponentChooser) = chooser(on, off)

    def updateForms()
  }



  implicit def monitorComponentChooser[T](get: => T): MonitorComponentChooser[T] = new MonitorComponentChooser(get)
  implicit def mapMonitorComponentChooser[K, V](get: => Map[K, V]): MapMonitorComponentChooser[K, V] = new MapMonitorComponentChooser(get)
  implicit def seqMonitorComponentChooser[T](get: => Seq[T]): SeqMonitorComponentChooser[T] = new SeqMonitorComponentChooser(get)
  implicit def controlComponentChooser[T](get: => T, set: T => Unit) = new ControlComponentChooser(get, set)
  implicit def seqControlComponentChooser[T: ClassTag](get: => Seq[T], static: Boolean) = new SeqControlComponentChooser(get.lifted, static)
  implicit def withDomainControlComponentChooser[T](get: => T, set: T => Unit) = new WithDomainControlComponentChooser(get, set)
  implicit def numericControlComponentChooser[N: Numeric](get: => N, set: N => Unit): NumericControlComponentChooser[N] = new NumericControlComponentChooser(get, set)
  implicit def orderedControlComponentChooser[N: Numeric](get: => N, set: N => Unit): OrderedControlComponentChooser[N] = new OrderedControlComponentChooser(get, set)
  implicit def triggerComponentChooser(action: => Unit): TriggerComponentChooser = new TriggerComponentChooser(action)
  implicit def toggleComponentChooser(on: => Unit, off: => Unit): ToggleComponentChooser = new ToggleComponentChooser(on, off)



//  protected implicit def buildForm[T](builder: DSLFormBuilder[T]): Component = builder.build()
  implicit def buildMapMeta: DSLKeyedListBuilder[_, _] => BuildMeta = _.formMeta
  implicit def buildDSLFormBuilderMeta: DSLFormBuilder[_] => BuildMeta = _.formMeta

  implicit def abstractDSLBuilderToComponent: AbstractDSLBuilder => Component = _.component

  implicit def componentToMeta(c: Component): BuildMeta = BuildMeta(c)



  protected class MonitorComponentChooser[T](_get: => T){
    def get = _get

    implicit def tBuilder: String => T = _ => null.asInstanceOf[T]

    def text = DSLLabelBuilder(get.lifted, static = false)
    def label = DSLLabelBuilder(get.lifted, static = true)
    def textField = DSLTextFormBuilder(get.lifted, null)      .affect(_.editable = false)
    def textArea = DSLTextAreaBuilder(get.lifted)             .affect(_.editable = false)
    def bigText = DSLTextComponentBuilder(get.lifted)         .affect(_.editable = false)
  }

  protected class MapMonitorComponentChooser[K, V](_get: => Map[K, V]) extends MonitorComponentChooser[Map[K, V]](_get){
    def list(implicit order: Ordering[K]) = DSLKeyedListBuilder(() => get, order)

  }

  protected class SeqMonitorComponentChooser[T](get: => Seq[T]) extends MonitorComponentChooser[Seq[T]](get){
    def table(columnClasses: (String, Class[_])*)(implicit evidence: T <:< Product) =
      DSLTableBuilder[T with Product](get.lifted.asInstanceOf[() => Seq[T with Product]], DSLTableBuilder.DSLTableModel(columnClasses))
  }

  protected class ControlComponentChooser[T](_get: => T, val set: T => Unit){
    def get: T = _get

    def textForm(implicit tBuilder: String => T): DSLTextFormBuilder[T] = new DSLTextFormBuilder(get.lifted, set)
    def textForm(verifier: String => Boolean)(implicit tBuilder: String => T): DSLTextFormBuilder[T] =
      new DSLTextFormBuilder(get.lifted, set, verify = verifier)
    def textArea = new DSLTextAreaBuilder(get.lifted) // todo: set

  }

  protected class OrderedControlComponentChooser[T: Ordering](_get: => T, override val set: T => Unit) extends ControlComponentChooser(_get, set){
    def spinner(model: SpinnerModel) = new DSLOrderedSpinnerBuilder(() => get, set, model)
  }

  protected class NumericControlComponentChooser[N: Numeric](_get: => N, override val set: N => Unit) extends ControlComponentChooser(_get, set){
    def slider(min: N, max: N, step: N, labelPos: DSLSliderBuilder.LabelPosition.type => DSLSliderBuilder.LabelPosition = null) =
      new DSLSliderBuilderByRange(() => get, set, min, max, step, Option(labelPos).map(_(DSLSliderBuilder.LabelPosition)))

    def spinner(model: SpinnerNumberModel = new SpinnerNumberModel) = new DSLNumericSpinnerBuilder(() => get, set, model)
  }

  protected class WithDomainControlComponentChooser[T](get: => T, set: T => Unit) {
    def slider(domain: Seq[T], labelPos: DSLSliderBuilder.LabelPosition.type => DSLSliderBuilder.LabelPosition = null) =
      new DSLSliderBuilderBySeq(get.lifted, set, domain, Option(labelPos).map(_(DSLSliderBuilder.LabelPosition)))
  }

  protected class ToggleComponentChooser(on: => Unit, off: => Unit){
    def toggle(label: String) = new DSLToggleButtonBuilder(on.lifted, off.lifted, label)
  }

  protected class TriggerComponentChooser(action: => Unit){
    def button(label: String) = new DSLButtonBuilder(() => action, label)
  }

  protected class SeqControlComponentChooser[T: ClassTag](get: () => Seq[T], static: Boolean){
    def dropDownList(set: T => Unit) = new DSLComboBoxBuilder(get, static, set)
  }



  object BuildMeta{
    def apply(_component: Component, _layout: (Constraints => Unit)*): BuildMeta = new BuildMeta{
      lazy val component: Component = _component
      lazy val `type` = _component.getClass.getName
      lazy val layout: List[(FormCreation.Constraints) => Unit] = _layout.toList
    }

    implicit class MetaWrapper(meta: BuildMeta){
      def copy(c: Component = meta.component, layout: List[Constraints => Unit] = meta.layout) = BuildMeta(c, layout: _*)
      def addLayout(effects: (Constraints => Unit)*) = copy(layout = meta.layout ++ effects)
    }

    def build(_component: Component): BuildMeta = apply(_component)

    def unapply(meta: BuildMeta): Option[(Component, List[Constraints => Unit])] = Option(meta.component).map(_ -> meta.layout)
  }

  trait BuildMeta{
    def component: Component
    def layout: List[Constraints => Unit]
    def `type`: String

    override def toString: String = s"BuildMeta($component, ${layout.length} layout changes)"
  }



  protected trait AbstractDSLBuilder{
    type Comp <: Component

    def component: Component

    def affect(effects: (Comp => Unit)*): AbstractDSLBuilder
    def layout(effects: (Constraints => Unit)*): AbstractDSLBuilder
  }

  protected trait DSLFormBuilder[+T] extends AbstractDSLBuilder{
    builder =>

    type Form <: Component with UpdateInterface
    type Comp = Form
    def `type`: String

    case class FormBuildMeta(form: Form, layout: List[Constraints => Unit]) extends BuildMeta{
      def component: Component = form
      def `type`: String = builder.`type`
    }

    protected implicit def toFormMeta(p: (Form, List[Constraints => Unit])): FormBuildMeta = FormBuildMeta(p._1, p._2)

    def formMeta: FormBuildMeta

    def component = formMeta.component

    override def affect(effects: (Form => Unit)*): DSLFormBuilder[T]
    override def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T]

  }


  protected trait DSLFutureStringExtraction[T, Inner] extends DSLFormBuilder[T]{
    protected[FormCreation] def get: () => T
    protected[FormCreation] def extractString: Either[T => Inner, (T => Future[Inner], ExecutionContext)]
    def extractAndThen(from: T, f: Inner => Unit) = extractString
      .left.map{ extr => f(extr(get())) }
      .right.map{ case (extr, c) => extr(get()).map(f)(c) }
  }

  protected[swing] trait UpdateInterface{
    self: Component =>

    def updateForm()

    def lockForm()    = this.enabled = false
    def unlockForm()  = this.enabled = true
  }

  object DefaultToString{
    def nullsafe[T]: T=> String = t => Option(t).map(_.toString) getOrElse ""
  }



  protected case class DSLTextComponentBuilder[T] protected[swing] (protected[FormCreation] val get: () => T,
        protected[FormCreation] val effects: List[DSLTextComponentBuilder[T]#Form => Unit] = Nil,
        protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
        protected[FormCreation] val extractString: Either[T => String, (T => Future[String], ExecutionContext)] = Left(DefaultToString.nullsafe[T]))
    extends DSLFormBuilder[T] with DSLFutureStringExtraction[T, String]
  {
    type Form = TextComponent with UpdateInterface
    def `type` = "TextComponent"

    def form = new TextComponent with UpdateInterface{
      def updateForm(){ extractAndThen(get(), text = _) }
    }

    lazy val formMeta: FormBuildMeta = form -> layout
    override def affect(eff: (DSLTextComponentBuilder[T]#Form => Unit) *) = copy(effects = effects ++ eff)
    override def layout(effects: (FormCreation.Constraints => Unit) *) = copy(layout = layout ++ effects)
    def extract(f: T => String) = copy(extractString = Left(f))
    def extractFuture(f: T => Future[String])(implicit c: ExecutionContext) = copy(extractString = Right(f -> c))
  }



  protected case class DSLLabelBuilder[T] protected[swing] (protected[FormCreation] val get: () => T,
         protected[FormCreation] val static: Boolean,
         protected[FormCreation] val effects: List[DSLLabelBuilder[T]#Form => Unit] = Nil,
         protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
         protected[FormCreation] val color: Color = Color.black,
         protected[FormCreation] val extractString: Either[T => String, (T => Future[String], ExecutionContext)] = Left(DefaultToString.nullsafe[T]))
    extends DSLFormBuilder[T] with DSLFutureStringExtraction[T, String]
  {
    type Form = Label with UpdateInterface

    def `type` = "Label"

    def form = new Label with UpdateInterface{
      foreground = color
      if(static) extractAndThen(get(), text = _)
      def updateForm() = if(!static) { extractAndThen(get(), text = _) }

      effects.foreach(_(this))
    }

    lazy val formMeta: FormBuildMeta = form -> layout

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ::: effects.toList)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)

    def color(c: Color): DSLLabelBuilder[T]  = copy(color = c)
    def withColor(c: Color): DSLLabelBuilder[T] = color(c)
    def extract(f: T => String): DSLLabelBuilder[T] = copy(extractString = Left(f))
    def extractFuture(f: T => Future[String])(implicit c: ExecutionContext) = copy(extractString = Right(f -> c))
  }



  protected case class DSLTextFormBuilder[T](protected[FormCreation] val get: () => T,
                                             protected[FormCreation] val set: T => Unit,
                                             protected[FormCreation] val effects: List[DSLTextFormBuilder[_]#Form => Unit] = Nil,
                                             protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
                                             protected[FormCreation] val static: Boolean = false,
                                             protected[FormCreation] val verify: String => Boolean = _ => true,
                                             protected[FormCreation] val extractString: T => String = DefaultToString.nullsafe[T])
                                            (implicit tBuilder: String => T) extends DSLFormBuilder[T]{
                                             // todo: set value
    type Form = TextField with UpdateInterface
    def `type` = "Text"

    def form = new TextField(if(static) extractString(get()) else "") with UpdateInterface{
      def updateForm(): Unit = if(!static){
        text = extractString(get())
      }

      shouldYieldFocus = verify

      effects.foreach(_(this))

      Option(set).foreach{ s =>
        listenTo(this)
        reactions += {
          case EditDone(_) => s(text)
        }
      }
    }

    lazy val formMeta: FormBuildMeta = form -> layout

    def swapStatic = copy(static = !static)

    def affect(eff: (Form => Unit)*): DSLTextFormBuilder[T] = copy(effects = effects ++ eff)
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T] = copy(layout = layout ++ effects)
    def extract(f: T => String) = copy(extractString = f)
  }



  protected case class DSLTextAreaBuilder[T](protected[FormCreation] val get: () => T,
                                             protected[FormCreation] val effects: List[DSLTextAreaBuilder[_]#Form => Unit] = Nil,
                                             protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
                                             protected[FormCreation] val extractString: T => String = DefaultToString.nullsafe[T]) extends DSLFormBuilder[T]{
                                            // todo: set value
    type Form = TextArea with UpdateInterface
    def `type` = "TextArea"

    def form = new TextArea with UpdateInterface{
      def updateForm(){
        text = extractString(get())
      }
      effects foreach (_(this))
    }
    lazy val formMeta: FormBuildMeta = form -> layout

    def affect(eff: (Form => Unit)*): DSLTextAreaBuilder[T] = copy(effects = effects ++ eff)
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[T] = copy(layout = layout ++ effects)
  }



  protected case class DSLComboBoxBuilder[T : ClassTag](protected[FormCreation] val get: () => Seq[T],
                                                        protected[FormCreation] val static: Boolean,
                                                        protected[FormCreation] val selected: T => Unit,
                                                        protected[FormCreation] val effects: List[DSLComboBoxBuilder[T]#Form => Unit] = Nil,
                                                        protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
                                                        protected[FormCreation] val renderer: ListView.Renderer[T] = null, // if no renderer is defined `extractString` is used to build one
                                                        protected[FormCreation] val extractString: T => String = DefaultToString.nullsafe[T]) extends DSLFormBuilder[T]{
    builder =>

    type Form = ComboBox[T] with UpdateInterface
    def `type` = "ComboBox"

    def form = new ComboBox[T](if(static) get() else Nil) with UpdateInterface{
      private var last: Seq[T] = null
//      lazy val underlying = new JComboBox[T]() with SuperMixin
//      override lazy val peer = underlying.asInstanceOf[JComboBox[Any]]

      def updateForm() = if(!static) {
        val g = get()
        if(last != g){
          last = g
          peer.asInstanceOf[JComboBox[T]].setModel(new DefaultComboBoxModel[T](g.asInstanceOf[Seq[AnyRef with T]].toArray))
        }
      }

      Option(builder.renderer)
        .map(renderer = _)
        .getOrElse { renderer = ListView.Renderer(extractString) }

      listenTo(selection)
      reactions += {
        case SelectionChanged(_) => selected(selection.item)
      }
    }
    lazy val formMeta: FormBuildMeta = form -> layout

    def withRenderer(vr: ListView.Renderer[T]) = copy(renderer = vr)
    def withStringExtractor(extr: T => String) = copy(extractString = extr)

    def affect(eff: (Form => Unit)*): DSLComboBoxBuilder[T] = copy(effects = effects ++ eff)
    def layout(effects: (Constraints => Unit)*): DSLComboBoxBuilder[T] = copy(layout = layout ++ effects)
  }


  protected trait DSLSpinnerBuilder[T] extends DSLFormBuilder[T]{
    type Form = Spinner[T] with UpdateInterface
    def `type` = "Spinner"

    protected[FormCreation] def get: () => T
    protected[FormCreation] def set: T => Unit
    protected[FormCreation] def model: SpinnerModel
    protected[FormCreation] def effects: List[DSLSpinnerBuilder[T]#Form => Unit]
    protected[FormCreation] val layout: List[Constraints => Unit]


    def setValue(v: T)      = affectModel(_.setValue(v))

    lazy val form: Form = new Spinner[T](model) with UpdateInterface{
      spinner =>

      def updateForm(): Unit = value = get()

      effects.foreach(_(spinner))

      listenTo(spinner)
      reactions += {
        case e@ValueChanged(`spinner`) => set(spinner.value) // todo: secure ??
      }

    }

    lazy val formMeta: FormBuildMeta = form -> layout

    def affectModel(effect: SpinnerModel => Unit) = { effect(model); this}

  }

  protected case class DSLOrderedSpinnerBuilder[T: Ordering](
              protected[FormCreation] val get: () => T,
              protected[FormCreation] val set: T => Unit,
              protected[FormCreation] val model: SpinnerModel,
              protected[FormCreation] val effects: List[DSLSpinnerBuilder[T]#Form => Unit] = Nil,
              protected[FormCreation] val layout: List[Constraints => Unit] = Nil
                                                       )
    extends DSLSpinnerBuilder[T]
  {
    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }

  protected case class DSLNumericSpinnerBuilder[N: Numeric](
              protected[FormCreation] val get: () => N,
              protected[FormCreation] val set: N => Unit,
              protected[FormCreation] val model: SpinnerNumberModel,
              protected[FormCreation] val effects: List[DSLSpinnerBuilder[N]#Form => Unit] = Nil,
              protected[FormCreation] val layout: List[Constraints => Unit] = Nil
              )
    extends DSLSpinnerBuilder[N]
  {
    import Ordered._

    def maxValue(max: N)  = affectNumericModel(_.setMaximum(max))
    def minValue(min: N)  = affectNumericModel(_.setMinimum(min))
    def step(step: N)     = affectNumericModel(_.setStepSize(step.asInstanceOf[Number]))

    def affectNumericModel(effect: SpinnerNumberModel => Unit) = { effect(model); this}

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }


  protected trait DSLSliderBuilder[T] extends DSLFormBuilder[T] {
    builder =>

    type Form = DSLSliderBuilder.SliderExt
    def `type` = "SliderExt"

    protected[FormCreation] def get: () => T
    protected[FormCreation] def set: T => Unit
    protected[FormCreation] def labelPos: Option[DSLSliderBuilder.LabelPosition]
    protected[FormCreation] def effects: List[DSLSliderBuilder[T]#Form => Unit]
    protected[FormCreation] def layout: List[Constraints => Unit]

    val slider: Slider with UpdateInterface

    lazy val form: Form = new DSLSliderBuilder.SliderExt(slider, labelPos.map(_ => valLabel)){
      layout ++= (labelPos match{
        case None                                       => Seq(slider   -> sliderConstraints)
        case Some(DSLSliderBuilder.LabelPosition.Left)  => Seq(valLabel -> labelConstraints,
                                                               slider   -> sliderConstraints)
        case Some(DSLSliderBuilder.LabelPosition.Right) => Seq(slider   -> sliderConstraints,
                                                               valLabel -> labelConstraints)
      })

      effects.foreach(_(this))
    }

    lazy val valLabel = DSLLabelBuilder(get, static = false).form

    // todo: this is for floating
    lazy val formMeta: FormBuildMeta = form -> layout

    override def affect(effects: (Form => Unit)*): DSLSliderBuilder[T]

    def vertical = affect(_.slider.orientation = Orientation.Vertical)
    def horizontal = affect(_.slider.orientation = Orientation.Horizontal)
    def showLabels = affect(_.slider.paintLabels = true)
    def labels(step: T, build: T => String): DSLSliderBuilder[T] = ???
    def labels(map: Map[Int, Label]): DSLSliderBuilder[T] = affect(_.slider.labels = map).showLabels
    //    def defaultLabels(step: N): DSLSliderBuilder[N] = defaultLabels(divideStep(step)) todo
    def defaultLabels(step: Int): DSLSliderBuilder[T] =
      affect(sl => sl.slider.peer setLabelTable sl.slider.peer.createStandardLabels(step)) // todo
      .showLabels

    protected abstract class SliderUpd extends Slider with UpdateInterface{
      slider =>

      def sliderMin = Int.MinValue

      def toSliderVal(n: T): Int
      def fromSliderVal(v: Int): T

      def stepsCount: Int

      min = sliderMin
      max = sliderMin + stepsCount - 1

      def updateForm(): Unit = { value = toSliderVal(get()) }

      listenTo(slider)
      reactions += {
        case e@ValueChanged(`slider`) if !slider.adjusting =>
          set(fromSliderVal(slider.value))
          if(labelPos.isDefined) valLabel.updateForm()
      }
    }

  }

  protected case class DSLSliderBuilderByRange[N](protected[FormCreation] val get: () => N,
                                                  protected[FormCreation] val set: N => Unit,
                                                  protected[FormCreation] val min: N,
                                                  protected[FormCreation] val max: N,
                                                  protected[FormCreation] val step: N,
                                                  protected[FormCreation] val labelPos: Option[DSLSliderBuilder.LabelPosition],
                                                  protected[FormCreation] val effects: List[DSLSliderBuilder[N]#Form => Unit] = Nil,
                                                  protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
                                                 (implicit val num: Numeric[N])
    extends DSLSliderBuilder[N]
  {
    builder =>

    import num._

    lazy val slider: Slider with UpdateInterface = new SliderUpd {
      lazy val stepsCount = divide(num.minus(builder.max, builder.min), builder.step).toInt() + 1

      def toSliderVal(n: N) = sliderMin + divide(num.minus(n, builder.min), builder.step).toInt()
      def fromSliderVal(v: Int) = num.plus(builder.min, num.times(num.fromInt(v - sliderMin), builder.step))

      def divide(n1: N, n2: N) = num match {
        case num: Integral[N]   => num.quot(n1, n2).ensuring(num.rem(n1, n2) == 0)
        case num: Fractional[N] => num.div(n1, n2)
      }
    }

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }

  protected case class DSLSliderBuilderBySeq[T](protected[FormCreation] val get: () => T,
                                                protected[FormCreation] val set: T => Unit,
                                                protected[FormCreation] val domain: Seq[T],
                                                protected[FormCreation] val labelPos: Option[DSLSliderBuilder.LabelPosition],
                                                protected[FormCreation] val effects: List[DSLSliderBuilder[T]#Form => Unit] = Nil,
                                                protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLSliderBuilder[T]
  {

    lazy val slider: Slider with UpdateInterface = new SliderUpd{
      lazy val values    = Stream.from(Int.MinValue).zip(domain).toMap
      lazy val valuesInv = values.map(_.swap)

      lazy val stepsCount = domain.size

      def toSliderVal(n: T) = valuesInv(n)
      def fromSliderVal(v: Int) = values(v)
    }

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }

  object DSLSliderBuilder{

    type LabelPosition = LabelPosition.Value
    object LabelPosition extends Enumeration{
      val Left, Right = Value
    }

    abstract class SliderExt(val slider: Slider with UpdateInterface,
                             val labelOpt: Option[Label with UpdateInterface]) extends GridBagPanel with UpdateInterface{
      def sliderConstraints = new Constraints() .$$ {_.fill = Fill.Horizontal}
                                                .$$ {_.weightx = 1}
      def labelConstraints  = new Constraints() .$$ {_.weightx = 1}

      def updateForm(): Unit = {
        slider.updateForm()
        labelOpt.foreach(_.updateForm())
      }

      override def lockForm(): Unit ={
        slider.lockForm()
        labelOpt.foreach(_.lockForm())
      }
      override def unlockForm(): Unit = {
        slider.unlockForm()
        labelOpt.foreach(_.unlockForm())
      }
    }


  }


  protected case class DSLKeyedListBuilder[K, V](protected[FormCreation] val get: () => Map[K, V],
                                                 protected[FormCreation] implicit val order: Ordering[K],
                                                 protected[FormCreation] val effects: List[DSLKeyedListBuilder[K, V]#Form => Unit] = Nil,
                                                 protected[FormCreation] val layout: List[Constraints => Unit] = Nil,
                                                 protected[FormCreation] val renderer: ListView.Renderer[(K, V)] = null)
    extends DSLFormBuilder[Map[K, V]]
  {
    builder =>

    type Form = ListView[(K, V)] with UpdateInterface
    def `type` = "ListView"

    lazy val form: ListView[(K, V)] with UpdateInterface = new ListView[(K, V)](Nil) with UpdateInterface{
      listView =>

      Option(builder.renderer).foreach(listView.renderer = _)

      val mapCache = mutable.HashMap.apply(get().toSeq: _*)

      def updateForm(): Unit = {
        val newMap = get()
        val addDiff = newMap.keySet -- mapCache.keySet
        val rmDiff = mapCache.keySet -- newMap.keySet
        val empt = !rmDiff.isEmpty || !addDiff.isEmpty

        mapCache --= rmDiff
        mapCache ++= addDiff.map(k => k -> newMap(k))

        if(empt)
          listView.listData = mapCache.toSeq.sortBy(_._1)
      }
    }

    lazy val formMeta: FormBuildMeta = form -> layout

    def affect(effects: (Form=> Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)

    def render(vr: ListView.Renderer[(K, V)]) = copy(renderer = new ListView.Renderer[(K, V)]{
      def componentFor(list: ListView[_ <: (K, V)], isSelected: Boolean, focused: Boolean, a: (K, V), index: Int): Component =
        vr.componentFor(list, isSelected, focused, a, index)
    })

    def renderKeys(r: ListView.Renderer[(K, V)]) = copy(renderer = r)
  }



  object DSLTableBuilder{
    case class DSLTableModel(columnClasses: Seq[(String, Class[_])]) extends TableModel{

      def getColumnClass(columnIndex: Int) = columnClasses(columnIndex)._2
      def getColumnCount = columnClasses.size
      def getColumnName(columnIndex: Int) = columnClasses(columnIndex)._1

      def isCellEditable(rowIndex: Int, columnIndex: Int) = ???
      def getValueAt(rowIndex: Int, columnIndex: Int) = ???
      def setValueAt(aValue: scala.Any, rowIndex: Int, columnIndex: Int) = ???
      def getRowCount = ???
      def addTableModelListener(l: TableModelListener) = ???
      def removeTableModelListener(l: TableModelListener) = ???
    }
  }


  protected case class DSLTableBuilder[T <: Product](protected[FormCreation] val get: () => Seq[T],
//                                                     protected[FormCreation] implicit val order: Ordering[T],
                                                     protected[FormCreation] val model: DSLTableBuilder.DSLTableModel,
                                                     protected[FormCreation] val effects: List[DSLTableBuilder[T]#Form => Unit] = Nil,
                                                     protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[Seq[T]]
  {
    builder =>

    type Form = Table with UpdateInterface

    def `type` = "Table"

    private var lastGet: Seq[T] = null

    lazy val form = new Table() with UpdateInterface{
      var rows: Seq[T] = Nil

      model = new AbstractTableModel {

        def getRowCount = rows.size

        def getColumnCount = builder.model.getColumnCount
        override def getColumnName(columnIndex: Int) = builder.model.getColumnName(columnIndex)
        override def getColumnClass(columnIndex: Int) = builder.model.getColumnClass(columnIndex)

        def getValueAt(rowIndex: Int, columnIndex: Int) =
          rows(rowIndex).productIterator.toSeq(columnIndex).asInstanceOf[AnyRef]

        override def setValueAt(value: Any, row: Int, col: Int) {
          fireTableCellUpdated(row, col)
        }

      }

      def updateForm(): Unit = {
        rows = get()

        if(lastGet == rows) return
        lastGet = rows

        for(i <- 0 until model.getRowCount; j <- 0 until model.getColumnCount) updateCell(i, j)
      }
    }

    lazy val formMeta: FormBuildMeta = form -> layout

    override def affect(effects: (Form => Unit) *) = copy(effects = this.effects ++ effects)

    override def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)
  }



  protected case class DSLButtonBuilder(protected[FormCreation] val action: () => Unit,
                                        protected[FormCreation] val label: String,
                                        protected[FormCreation] val effects: List[DSLButtonBuilder#Form => Unit] = Nil,
                                        protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[Unit]
  {
    builder =>

    type Form = Button with UpdateInterface
    def `type` = "Button"

    lazy val formMeta: FormBuildMeta =  new Button() with UpdateInterface{
      button =>

      text = label
      reactions += {
        case c@ButtonClicked(`button`) => builder.action()
      }
      listenTo(button)
      effects.foreach(_(this))
      def updateForm(): Unit = {}
    } -> layout

    def affect(effects: ((Form) => Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
  }



  protected case class DSLToggleButtonBuilder(protected[FormCreation] val on: () => Unit,
                                              protected[FormCreation] val off: () => Unit,
                                              protected[FormCreation] val label: String,
                                              protected[FormCreation] val effects: List[DSLToggleButtonBuilder#Form => Unit] = Nil,
                                              protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[Unit]
  {
    type Form = ToggleButton with UpdateInterface
    def `type` = "ToggleButton"

    lazy val formMeta: FormBuildMeta =  new ToggleButton(label) with UpdateInterface{
      toggle =>

      def updateForm(): Unit = {
        text = label
      }

      listenTo(toggle)
      reactions += {
        case ButtonClicked(`toggle`) => if(toggle.selected) on() else off()
      }
      effects.foreach(_(this))
    } -> layout

    def affect(effects: (Form => Unit) *): DSLFormBuilder[Unit] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[Unit] = ???
  }



  class DSLFormBuilderOps[B <: AbstractDSLBuilder](val builder: B){

    def sizes(min: Dimension = null,
              max: Dimension = null,
              preferred: Dimension = null): B =
    {
      def helper(dim: Dimension, f: (builder.Comp, Dimension) => Unit): Option[builder.Comp => Unit] = Option(dim) map (d => f(_, d))

      val h =
        helper(min, _.minimumSize = _) ::
        helper(max, _.maximumSize = _) ::
        helper(preferred, _.preferredSize = _) :: Nil
      builder.affect(h.flatten: _*).asInstanceOf[B]
    }

    def fillHorizontally: B = builder.layout(_.fill = Fill.Horizontal).asInstanceOf[B]
    def fillBoth: B = builder.layout(_.fill = Fill.Both).asInstanceOf[B]

    def height(i: Int) = builder.layout(_.gridheight = i).asInstanceOf[B]
    def width(i: Int) = builder.layout(_.gridwidth = i).asInstanceOf[B]

    def xWeight(w: Double): B = builder.layout(_.weightx = w).asInstanceOf[B]
    def yWeight(w: Double): B = builder.layout(_.weighty = w).asInstanceOf[B]
    def maxXWeight: B = xWeight(1)
    def minXWeight: B = xWeight(0)
    def maxYWeight: B = yWeight(1)
    def minYWeight: B = yWeight(0)
    def halfXWeight: B = xWeight(.5)
    def halfYWeight: B = yWeight(.5)

    def insets(default: Int = 0)(top: Int = default, left: Int = default, bottom: Int = default, right: Int = default): B =
      builder.layout(_.insets = new Insets(top, left, bottom, right)).asInstanceOf[B]

    def anchor(f: Anchor.type => Anchor.Value): B = builder.layout(_.anchor = f(Anchor)).asInstanceOf[B]
  }

  implicit class ListDSLFormBuilderOps[K, V](builder: DSLKeyedListBuilder[K, V]) extends DSLFormBuilderOps[DSLKeyedListBuilder[K, V]](builder)
  implicit class TableDSLBuilderOps[T <: Product](builder: DSLTableBuilder[T]) extends DSLFormBuilderOps[DSLTableBuilder[T]](builder)
  implicit class ButtonDSLFormBuilderOps(builder: DSLButtonBuilder) extends DSLFormBuilderOps[DSLButtonBuilder](builder)
  implicit class LabelDSLFormBuilderOps[T](builder: DSLLabelBuilder[T]) extends DSLFormBuilderOps[DSLLabelBuilder[T]](builder)
  implicit class SliderDSLFormBuilderOps[N: Numeric](builder: DSLSliderBuilder[N]) extends DSLFormBuilderOps[DSLSliderBuilder[N]](builder)
  implicit class ComboBoxDSLFormBuilderOps[T: ClassTag](builder: DSLComboBoxBuilder[T]) extends DSLFormBuilderOps[DSLComboBoxBuilder[T]](builder)
}
