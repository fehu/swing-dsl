package feh.dsl.swing

import javax.swing.event.TableModelListener
import javax.swing.table.{AbstractTableModel, TableModel}

import scala.swing._
import scala.swing.event.{SelectionChanged, ValueChanged, ButtonClicked}
import scala.swing.Label
import java.awt.Color
import concurrent.duration.FiniteDuration
import scala.collection.mutable
import scala.collection.immutable.NumericRange
import feh.util._
import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.xml.NodeSeq
import scala.concurrent.{ExecutionContext, Future}
import javax.swing._
import scala.reflect.ClassTag


object FormCreation{
  type Constraints = GridBagPanel#Constraints
}
import FormCreation._

trait FormCreation {

  protected trait FormCreationDSL{
    protected def monitorFor[K, V](get: => Map[K, V])(implicit chooser:  (=> Map[K, V]) => MapMonitorComponentChooser[K, V]) = chooser(get)
    protected def monitorFor[T](get: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T]) = chooser(get)
    protected def monitorForSeq[T](get: => Seq[T])(implicit chooser:  (=> Seq[T]) => SeqMonitorComponentChooser[T]) = chooser(get)
    protected def controlFor[T](get: => T)(set: T => Unit)(implicit chooser: (=> T, T => Unit) => ControlComponentChooser[T]) = chooser(get, set)
    protected def controlForSeq[T](get: => Seq[T], static: Boolean = false)(implicit chooser: (=> Seq[T], Boolean) => SeqControlComponentChooser[T]) = chooser(get, static)
    protected def numericControlFor[N](get: => N)(set: N => Unit) // todo: sould it exist?
                                      (implicit chooser: (=> N, N => Unit) => NumericControlComponentChooser[N], num: Numeric[N]) = chooser(get, set)
    protected def triggerFor(action: => Unit)(implicit chooser: (=> Unit) => TriggerComponentChooser) = chooser(action)
    protected def toggleFor(on: => Unit, off: => Unit)(implicit chooser: (=> Unit, => Unit) => ToggleComponentChooser) = chooser(on, off)

    def updateForms()
  }

  implicit def monitorComponentChooser[T](get: => T): MonitorComponentChooser[T] = new MonitorComponentChooser(get)
  implicit def mapMonitorComponentChooser[K, V](get: => Map[K, V]): MapMonitorComponentChooser[K, V] = new MapMonitorComponentChooser(get)
  implicit def seqMonitorComponentChooser[T](get: => Seq[T]): SeqMonitorComponentChooser[T] = new SeqMonitorComponentChooser(get)
  implicit def controlComponentChooser[T](get: => T, set: T => Unit) = new ControlComponentChooser(get, set)
  implicit def seqControlComponentChooser[T: ClassTag](get: => Seq[T], static: Boolean) = new SeqControlComponentChooser(get.lifted, static)
  implicit def numericControlComponentChooser[N: Numeric](get: => N, set: N => Unit): NumericControlComponentChooser[N] = new NumericControlComponentChooser(get, set)
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

    def asText = text
    def asLabel = text
    def asTextField = textField
    def asTextArea = textArea
  }

  protected class MapMonitorComponentChooser[K, V](_get: => Map[K, V]) extends MonitorComponentChooser[Map[K, V]](_get){
    def list(implicit order: Ordering[K]) = DSLKeyedListBuilder(() => get, order)

    def asList(implicit order: Ordering[K]) = list
  }

  protected class SeqMonitorComponentChooser[T](get: => Seq[T]) extends MonitorComponentChooser[Seq[T]](get){
    def table(columnClasses: (String, Class[_])*)(implicit evidence: T <:< Product) =
      DSLTableBuilder[T with Product](get.lifted.asInstanceOf[() => Seq[T with Product]], DSLTableBuilder.DSLTableModel(columnClasses))
  }

  protected class ControlComponentChooser[T](_get: => T, val set: T => Unit){
    def get: T = _get

    def textForm(implicit tBuilder: String => T) = new DSLTextFormBuilder(get.lifted, set)
    def textArea = new DSLTextAreaBuilder(get.lifted) // todo: set

  }

  protected class NumericControlComponentChooser[N: Numeric](_get: => N, override val set: N => Unit) extends ControlComponentChooser(_get, set){
    def spinner = new DSLSpinnerBuilder(get) 
    def slider(range: NumericRange[N]) = new DSLSliderBuilder(() => get, set, range)
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
  protected trait DSLFormBuilder[T] extends AbstractDSLBuilder{
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

  protected trait UpdateInterface{
    def updateForm()
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
                                             protected[FormCreation] val extractString: T => String = DefaultToString.nullsafe[T])
                                            (implicit tBuilder: String => T) extends DSLFormBuilder[T]{
                                             // todo: set value
    type Form = TextField with UpdateInterface
    def `type` = "Text"

    def form = new TextField(if(static) extractString(get()) else "") with UpdateInterface{
      def updateForm(): Unit = if(!static){
        text = extractString(get())
      }
      effects.foreach(_(this))

      Option(set).foreach{ s =>
        listenTo(this)
        reactions += {
          case ValueChanged(_) => s(text)
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
        if(last != null && last != g){
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

    def affect(eff: (Form => Unit)*): DSLComboBoxBuilder[T] = copy(effects = effects ++ eff)
    def layout(effects: (Constraints => Unit)*): DSLComboBoxBuilder[T] = copy(layout = layout ++ effects)
  }

  protected class DSLSpinnerBuilder[N: Numeric](get: => N) extends DSLFormBuilder[N]{
    type Form = Null with UpdateInterface//JSpinner // todo
    def `type` = ???
    lazy val formMeta: FormBuildMeta = ???

    def affect(effects: (Form => Unit)*): DSLSpinnerBuilder[N] = ???
    def layout(effects: (Constraints => Unit)*): DSLFormBuilder[N] = ???
  }
  
  protected case class DSLSliderBuilder[N: Numeric](protected[FormCreation] val get: () => N,
                                                    protected[FormCreation] val set: N => Unit,
                                                    protected[FormCreation] val range: NumericRange[N],
                                                    protected[FormCreation] val effects: List[DSLSliderBuilder[N]#Form => Unit] = Nil,
                                                    protected[FormCreation] val layout: List[Constraints => Unit] = Nil)
    extends DSLFormBuilder[N]
  {
    type Form = Slider with UpdateInterface
    def `type` = "Slider"
    val num = implicitly[Numeric[N]]
    import num._

    lazy val form: Slider with UpdateInterface = new Slider with UpdateInterface{
      slider =>

//      val n = range.length

      def parseInt(i: Int) = fromInt(i) * range.step
      def toInt(n: N) = n match{
        case d: Double => (d / range.step.toDouble()).toInt
      }

      min = 0
      max = toInt(range.max)

      effects.foreach(_(slider))

      def updateForm(): Unit = { value = toInt(get()) }

      listenTo(slider)
      reactions += {
        case e@ValueChanged(`slider`) if !slider.adjusting =>
          set(parseInt(value))
      }

    }

    // todo: this is for floating
    lazy val formMeta: FormBuildMeta = form -> layout

    private def divideStep = (_: N) match{
      case d: Double =>
        if (d < range.step.toDouble) 1
        else (d / range.step.toDouble).toInt
    }
    
    def vertical = affect(_.orientation = Orientation.Vertical)
    def horizontal = affect(_.orientation = Orientation.Horizontal)
    def showLabels = affect(_.paintLabels = true)
    def labels(step: N, build: N => String): DSLSliderBuilder[N] = ???
    def labels(map: Map[Int, Label]): DSLSliderBuilder[N] = affect(_.labels = map).showLabels
    def defaultLabels(step: N): DSLSliderBuilder[N] = defaultLabels(divideStep(step))
    def defaultLabels(step: Int): DSLSliderBuilder[N] =
      affect(sl => sl.peer setLabelTable sl.peer.createStandardLabels(step))
        .showLabels

    def affect(effects: (Form => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)
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

    def render(vr: ListView.Renderer[V]) = copy(renderer = new ListView.Renderer[(K, V)]{
      def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: (K, V), index: Int): Component =
        vr.componentFor(list, isSelected, focused, a._2, index)
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

    def affect(effects: ((Form) => Unit) *): DSLFormBuilder[Unit] = copy(effects = this.effects ++ effects)
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
