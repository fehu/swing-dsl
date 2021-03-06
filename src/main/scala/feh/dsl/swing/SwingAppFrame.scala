package feh.dsl.swing

import java.util.UUID
import javax.swing.Box.Filler
import javax.swing.JPanel

import feh.dsl.swing.FormCreation.Constraints
import feh.dsl.swing.util.AwtUtils
import feh.util._

import scala.collection.mutable
import scala.swing.ScrollPane.BarPolicy
import scala.swing.Swing._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.xml.NodeSeq

trait AppFrameControl{
  def start()
  def stop()
}

trait SwingAppBuildingEnvironment extends SwingFrameAppCreation{
  
  trait SwingFrame extends Frame with AppFrameControl {
    self: LayoutDSL with LayoutBuilder =>

    override def closeOperation(): Unit = {
      stop()
      super.closeOperation()
    }
  }

}


//object SwingFrameAppCreation extends SwingFrameAppCreation
trait SwingFrameAppCreation extends FormCreation{
  trait LayoutBuilder{
    dsl: LayoutDSL =>
    
    /**
     * builds layout within itself
     */
    def buildLayout()
  }
  
  trait LayoutDSL extends FormCreationDSL{
    val layout: List[AbstractLayoutSetting]

      val componentAccess = new RegistringComponentAccess

    // upper lever settings
    protected def split(orientation: scala.swing.Orientation.type => scala.swing.Orientation.Value)(leftOrDown: LayoutElem, rightOrUp: LayoutElem) =
      SplitLayout(orientation(scala.swing.Orientation), leftOrDown, rightOrUp)
    @deprecated protected def set(s: UpperLevelLayoutGlobalSetting): AbstractLayoutSetting = s
    protected def panel = new PanelChooser
    protected def tabs(placement: scala.swing.Alignment.type => scala.swing.Alignment.Value,
                       layoutPolicy: TabbedPane.Layout.type => TabbedPane.Layout.Value,
                       static: Boolean = true)
                      (tabs: => Seq[LayoutElem]) =
      TabbedLayout(placement(scala.swing.Alignment), layoutPolicy(TabbedPane.Layout), static, () => tabs)
    protected def scrollable[M <% BuildMeta](vert: BarPolicy.Value = BarPolicy.AsNeeded,
                                             hor: BarPolicy.Value = BarPolicy.AsNeeded)
                                            (content: M, id: String) = DSLScrollPaneBuilder(vert, hor, content, id)
    protected def boxed[M <% BuildMeta](content: M, id: String) = BoxedBuilder(content, id)

    // normal settings
    protected def place[M <% BuildMeta](what: M, id: String): DSLPlacing = DSLPlacing(what, id)
    protected def place[T](builder: DSLFormBuilder[T], id: String): DSLPlacing = DSLPlacing(builder.formMeta, id)
    protected def place(builder: DSLPanelBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
    protected def place(builder: DSLScrollPaneBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
    protected def place(builder: BoxedBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
    protected def place(meta: GridBagMeta, id: String): DSLPlacing = DSLPlacing(meta, id)
    protected def make(s: LayoutGlobalSetting) = s

    protected def noId: String = null

    protected def html[T](t: => T)(build: T => NodeSeq): DSLLabelBuilder[T] = label(t)
      .extract(t => surroundHtml(<html>{build(t)}</html>).toString)
    protected def html(html: NodeSeq): DSLLabelBuilder[NodeSeq] = label(surroundHtml(html))
    protected def label[T](text: => T): DSLLabelBuilder[T] = new DSLLabelBuilder[T](() => text, static = true)


    protected case class DSLPlacing(what: BuildMeta, id: String){
      def at(pos: DSLAbsolutePosition): LayoutElem = LayoutElem(what, id, pos).register
      def in(pos: DSLAbsolutePosition): LayoutElem = at(pos)
      def to(rel: DSLRelativePositionDirection): DSLRelativelyOf = DSLRelativelyOf(what, id, rel)
      def on(rel: DSLRelativePositionDirection): DSLRelativelyOf = to(rel)

      def transform(f: BuildMeta => BuildMeta): DSLPlacing = copy(f(what))
    }

    protected case class DSLRelativelyOf(what: BuildMeta, id: String, dir: DSLRelativePositionDirection){
      def of(rel: String): LayoutElem = LayoutElem(what, id, DSLRelativePosition(dir, rel)).register
      def of[C <% Component](c: C) = LayoutElem(what, id, DSLRelativePosition(dir, componentAccess.id(c))).register
      def from(rel: String): LayoutElem = of(rel)
      def from[C <% Component](c: C) = of(c)
    }

    protected implicit class AbstractLayoutSettingWrapper(list: List[AbstractLayoutSetting]){
      def and(other: AbstractLayoutSetting) = list :+ other
    }

    protected implicit class LayoutSettingListWrapper(list: List[LayoutSetting]){
      def and(other: LayoutSetting) = list :+ other
    }
    protected implicit def layoutSettingToListWrapper(s: AbstractLayoutSetting): List[AbstractLayoutSetting] = s :: Nil

    protected[SwingFrameAppCreation] implicit class LayoutElemRegister(elem: LayoutElem){
      def register = {
//        println(s"$elem is registered!")
        componentAccess.register(elem)
        elem
      }
    }

    protected implicit def componentIdPairToUnplacedLayoutElem[C <% Component](p: (C, String)): UnplacedLayoutElem = {
      val el = LayoutElem.unplaced(BuildMeta(p._1), p._2)
      el.register
      el
    }

    protected implicit def dslFormBuilderIdPairToUnplacedLayoutElem[B <: DSLFormBuilder[_]](p: (B, String)): UnplacedLayoutElem = {
      val el = LayoutElem.unplaced(p._1.component, p._2)
      el.register
      el
    }

    protected implicit class ComponentToUnplacedLayoutElemWithoutRegisteringWrapper[C <% Component](c: C){
      def withoutRegistering = LayoutElem.unplaced(BuildMeta(c), null)
    }

    implicit class ExplicitToComponentWrapper[C <% Component](c: C){
      def toComponent: Component = c
    }

    implicit class WithoutIdWrapper[C <% Component](c: C){
      def withoutId: UnplacedLayoutElem = c -> noId
    }

    implicit class WithoutIdSeqWrapper[C <% Component](c: Seq[C]){
      def withoutIds: Seq[UnplacedLayoutElem] = c.map(k => (k, noId): UnplacedLayoutElem)
    }

    class RegistringComponentAccess extends ComponentAccess{
      private val componentsMap = mutable.HashMap.empty[String, LayoutElem]
      private val delayedMap = mutable.HashMap.empty[String, LayoutElem]

      protected[LayoutDSL] def register(elem: LayoutElem){
        elem.meta match{
          case _ if componentsMap.contains(elem.id) =>
            if(elem == componentsMap(elem.id)) sys.error(s"reusing id ${elem.id}")
          // else just do not re-register
          case _ if delayedMap.contains(elem.id) =>
            delayedMap -= elem.id
            register(elem)
          case gb: GridBagMeta => delayedMap += elem.id -> elem // should be called again on panel creation
          case _ => componentsMap += elem.id -> elem

        }
      }

      def get(id: String) = componentsMap.get(id).map(_.meta.component)
      def getLayoutOf(id: String) = componentsMap orElse delayedMap lift id
      def getId[C <% Component](c: C) = {
        val comp = c: Component
        componentsMap.find(_._2.meta.component == comp).map(_._1)
      }
      def all: Seq[LayoutElem] = componentsMap.values.toSeq
    }
    private def surroundHtml(in: NodeSeq) = in match{
      case <html>{_*}</html> => in
      case _ => <html>{in}</html>
    }
  }

  implicit class TryUpdateInterface(comp: Component){
    def tryUpdate() = rec(_.updateForm()) (comp)
    def tryLock()   = rec(_.lockForm())   (comp)
    def tryUnlock() = rec(_.unlockForm()) (comp)

    private def rec(f: UpdateInterface => Unit)(c: Component): Unit = c match {
      case upd: UpdateInterface => f(upd)
      case c: Container => c.contents.foreach(rec(f))
      case _ =>
    }
  }

  protected class PanelChooser{
    private def unplaced[E <% UnplacedLayoutElem](e: Seq[E]) = e.toList.map(x => x: UnplacedLayoutElem)

    def flow[E <% UnplacedLayoutElem](alignment: FlowPanel.Alignment.type => FlowPanel.Alignment.Value)(elems: E*) =
      DSLFlowPanelBuilder(alignment(FlowPanel.Alignment), unplaced(elems))
    def box[E <% UnplacedLayoutElem](alignment: Orientation.type => Orientation.Value)(elems: E*) =
      DSLBoxPanelBuilder(alignment(Orientation), unplaced(elems))
    def grid[E <% UnplacedLayoutElem](rows: Int, cols: Int)(elems: E*) =
      DSLGridPanelBuilder(rows -> cols, unplaced(elems))
    def gridBag(settings: LayoutSetting*)(implicit gbBuilder: GridBagComponentBuilder) = GridBagMeta(settings.toList)
    def apply[E <% UnplacedLayoutElem](el: E) = DSLSimplePanelBuilder(el)
  }

  sealed trait AbstractLayoutSetting
  trait UpperLevelLayoutSetting extends AbstractLayoutSetting{
    def and(that: UpperLevelLayoutSetting) = this :: that :: Nil
  }
  trait UpperLevelLayoutGlobalSetting extends UpperLevelLayoutSetting

  trait LayoutSetting extends AbstractLayoutSetting{
    def and(that: LayoutSetting) = this :: that :: Nil
    def and(that: List[LayoutSetting]) = this :: that
  }
  trait LayoutGlobalSetting extends LayoutSetting

  // // //// // //// // //// // //// // //  Upper Level Layout Settings  // // //// // //// // //// // //// // //

  object SplitLayout{
    def meta(o: Orientation.Value, l: LayoutElem, r: LayoutElem, la: List[Constraints => Unit]) = {
      if(l.meta.component.isInstanceOf[UpdateInterface] || r.meta.component.isInstanceOf[UpdateInterface])
        new SplitPane(o, l.meta.component, r.meta.component) with UpdateInterface{
          def updateForm() = {
            l.meta.component match{ case uf: UpdateInterface => uf.updateForm() }
            r.meta.component match{ case uf: UpdateInterface => uf.updateForm() }
          }
        }
      else new SplitPane(o, l.meta.component, r.meta.component)
    } |> (BuildMeta(_, la: _*))

  }

  case class SplitLayout(orientation: scala.swing.Orientation.Value,
                         leftOrDown: LayoutElem,
                         rightOrUp: LayoutElem,
                         layout: List[Constraints => Unit] = Nil,
                         effects: List[SplitPane => Unit] = Nil
                          )
                         extends LayoutSetting with AbstractDSLBuilder
  {
    type Comp = SplitPane
    def component = new SplitPane(orientation, leftOrDown.meta.component, rightOrUp.meta.component) with UpdateInterface{
      val updateHooks = List(leftComponent, rightComponent).collect{ case upd: UpdateInterface => upd.updateForm.lifted }
      def updateForm() =  updateHooks.foreach(_())
    }

    def affect(effects: (SplitLayout#Comp => Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)
  }

  case class Title(title: String) extends UpperLevelLayoutGlobalSetting
  case class Size(size: (Int, Int)) extends UpperLevelLayoutGlobalSetting

  case class TabbedLayout(placement: scala.swing.Alignment.Value,
                          layoutPolicy: TabbedPane.Layout.Value,
                          static: Boolean,
                          elems: () => Seq[LayoutElem],
                          layout: List[Constraints => Unit] = Nil,
                          effects: List[TabbedLayout#Comp => Unit] = Nil
                           )
    extends LayoutSetting with AbstractDSLBuilder
  {
    type Comp = TabbedPane with UpdateInterface

    def component = new TabbedPane() with UpdateInterface{
      tabLayoutPolicy = layoutPolicy
      tabPlacement = placement

      protected lazy val pagesCache = mutable.HashMap.empty[LayoutElem, (Page, () => Unit)].withDefault{
        elem =>
          val c = elem.meta.component
          new Page(elem.id, c) -> (c match {
            case upd: UpdateInterface => () => upd.updateForm()
            case _ => () =>
          })
      }

      def updateForm() = {
        if(!static) {
          pages.clear()
          pages ++= elems().map(pagesCache.apply _ andThen(_._1))
          pagesCache.foreach(_._2._2())
        }
        pages.foreach(_.content.tryUpdate())
      }

      if(static) pages ++= elems().map(pagesCache.apply _ andThen(_._1))

      effects.foreach(_(this))

      override def lockForm(): Unit   = {
        super.lockForm()
        pages.foreach(_.content.tryLock())
      }
      override def unlockForm(): Unit = {
        super.unlockForm()
        pages.foreach(_.content.tryUnlock())
      }
    }

    def meta: BuildMeta = BuildMeta(component, layout: _*)

    def affect(effects: (TabbedLayout#Comp => Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)
  }

  implicit def tabbedLayoutMeta(tl: TabbedLayout) = tl.meta

  // // //// // //// // //// // //// // //  Layout Settings  // // //// // //// // //// // //// // //

  class LayoutElem private (val meta: BuildMeta, val id: String, val pos: DSLPosition) extends LayoutSetting{
    override def toString: String = s"LayoutElem($id, $pos, $meta)"
    def copy(meta: BuildMeta = this.meta, id: String = this.id, pos: DSLPosition = this.pos) = LayoutElem(meta, id, pos)
  }
  object LayoutElem{
    def apply(meta: BuildMeta, id: String, pos: DSLPosition): LayoutElem =
      new LayoutElem(meta, getOrRandom(id, meta), pos)
    def unplaced(c: BuildMeta, id: String): UnplacedLayoutElem = new LayoutElem(c, getOrRandom(id, c), null) with UnplacedLayoutElem
    def unapply(el: LayoutElem): Option[(BuildMeta, String, DSLPosition)] = Some(el.meta, el.id, el.pos)

    private def getOrRandom(id: String, meta: BuildMeta) = Option(id) getOrElse randomId(meta)
    def randomId = (_: BuildMeta) match{
      case meta if meta.component == null => meta.`type` + "-" + UUID.randomUUID()
      case meta => meta.`type` + "#" + meta.component.hashCode()
    }
  }

  trait UnplacedLayoutElem extends LayoutElem{
    override val pos = Undefined
  }

  case class Scrollable(vert: BarPolicy.Value = BarPolicy.AsNeeded,
                        hor: BarPolicy.Value = BarPolicy.AsNeeded) extends LayoutGlobalSetting

  case class DSLScrollPaneBuilder(vert: BarPolicy.Value,
                            hor: BarPolicy.Value,
                            protected val content: BuildMeta,
                            id: String,
                            protected val effects: List[DSLScrollPaneBuilder#Comp => Unit] = Nil,
                            protected val layout: List[Constraints => Unit] = Nil)
    extends LayoutSetting with AbstractDSLBuilder
  {
    type Comp = ScrollPane
    def component = {
      val c =
        if(content.component.isInstanceOf[UpdateInterface]) new ScrollPane(content.component) with UpdateInterface{
          def updateForm(): Unit = content.component.asInstanceOf[UpdateInterface].updateForm()
        }
        else new ScrollPane(content.component)
      c.horizontalScrollBarPolicy = hor
      c.verticalScrollBarPolicy = vert
      effects.foreach(_(c))
      c
    }

    def affect(effects: (Comp => Unit)*) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit)*) = copy(layout = layout ++ effects)

    def meta = BuildMeta(component, content.layout ++ layout: _*)
  }

  case class BoxedBuilder(protected val content: BuildMeta,
                       id: String)
    extends LayoutSetting
  {
    def component =
      if(content.component.isInstanceOf[UpdateInterface]) new BoxPanel(Orientation.NoOrientation) with UpdateInterface{
        contents += content.component
        def updateForm(): Unit = content.component.asInstanceOf[UpdateInterface].updateForm()
      }
      else new BoxPanel(Orientation.NoOrientation){}

    def meta = BuildMeta(component, content.layout: _*)
  }

  case class GridBagComponentBuilder(build: GridBagMeta => GridBagPanel)

  object GridBagMeta{
    implicit def toComponent(meta: GridBagMeta) = meta.panel
  }
  case class GridBagMeta(settings: List[LayoutSetting],
                         layout: List[(Constraints) => Unit] = Nil,
                         effects: List[GridBagPanel => Unit] = Nil
                          )
                        (implicit gbBuilder: GridBagComponentBuilder) extends BuildMeta with DSLPanelBuilder {
    type Panel = GridBagPanel
    def `type` = "GridBag"
    def panelName = `type`
    
    def layout(effects: (Constraints => Unit)*): GridBagMeta = copy(layout = layout ++ effects)

    def affect(effects: (Panel => Unit) *) = copy(effects = this.effects ++ effects)
    def elems = ???
    def panel = gbBuilder.build(this) $$ {p => effects.foreach(_(p))}
    override def meta = this

    def append(el: LayoutElem*) = copy(settings ++ el)
    def prepend(el: LayoutElem*) = copy(el.toList ::: settings)

    override def toString: String = s"GridBagMeta($settings, $layout)"


  }

  implicit class TabsBuilderOps(builder: TabbedLayout) extends DSLFormBuilderOps[TabbedLayout](builder)
  implicit class ScrollBuilderOps(builder: DSLScrollPaneBuilder) extends DSLFormBuilderOps[DSLScrollPaneBuilder](builder)
  implicit class PanelBuilderOps(builder: DSLPanelBuilder) extends DSLFormBuilderOps[DSLPanelBuilder](builder)
  implicit class GridBagMetaOps(meta: GridBagMeta) extends DSLFormBuilderOps[GridBagMeta](meta)
  implicit class FlowPanelBuilderOps(builder: DSLFlowPanelBuilder) extends DSLFormBuilderOps[DSLFlowPanelBuilder](builder)
  implicit class BoxPanelBuilderOps(builder: DSLBoxPanelBuilder) extends DSLFormBuilderOps[DSLBoxPanelBuilder](builder)
  implicit class GridPanelBuilderOps(builder: DSLGridPanelBuilder) extends DSLFormBuilderOps[DSLGridPanelBuilder](builder)

  implicit def buildFlowPanelMeta: DSLFlowPanelBuilder => BuildMeta = _.meta
  implicit def buildBoxPanelMeta: DSLBoxPanelBuilder => BuildMeta = _.meta
  implicit def buildGridPanelMeta: DSLGridPanelBuilder => BuildMeta = _.meta

  trait DSLPanelBuilder extends LayoutSetting with AbstractDSLBuilder{
    type Panel <: scala.swing.Panel
    type Comp = Panel
    def panelName: String
    def elems: List[LayoutSetting]
    def panel: Panel
    def meta = BuildMeta(panel, layout: _*)
    def component = panel

    def id: String = s"$panelName: ${UUID.randomUUID().toString}"

    def append(el: LayoutElem*): DSLPanelBuilder
    def prepend(el: LayoutElem*): DSLPanelBuilder

    def affect(effects: (Panel => Unit)*): DSLPanelBuilder
    def layout(effects: (Constraints => Unit)*): DSLPanelBuilder
    protected def layout: List[Constraints => Unit]
  }

  case class DSLFlowPanelBuilder(alignment: FlowPanel.Alignment.Value,
                             elems: List[LayoutElem],
                             panelName: String = "FlowPanel",
                             protected val effects: List[DSLFlowPanelBuilder#Panel => Unit] = Nil,
                             protected val layout: List[Constraints => Unit] = Nil) extends DSLPanelBuilder
  {

    type Panel = FlowPanel
    def panel = {
      val p = new FlowPanel(alignment)(elems.map(_.meta.component): _*)
      effects.foreach(_(p))
      p
    }
    def prepend(el: LayoutElem*) = copy(elems = el.toList ++ elems)
    def append(el: LayoutElem*) = copy(elems = elems ++ el)

    def affect(effects: (DSLFlowPanelBuilder#Panel => Unit) *): DSLPanelBuilder = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)
  }

  case class DSLBoxPanelBuilder(alignment: Orientation.Value,
                            elems: List[LayoutElem],
                            indent: Option[Int] = Some(10),
                            panelName: String = "BoxPanel", 
                            glueElems: Boolean = true,
                            protected val effects: List[DSLBoxPanelBuilder#Panel => Unit] = Nil,
                            protected val layout: List[Constraints => Unit] = Nil) extends DSLPanelBuilder
  {
    type Panel = BoxPanel

    def indent(i: Option[Int]): DSLBoxPanelBuilder = copy(indent = i)
    def doNotGlue = copy(glueElems = false)

    def prepend(el: LayoutElem*): DSLBoxPanelBuilder = copy(elems = el.toList ++ elems)
    def append(el: LayoutElem*): DSLBoxPanelBuilder = copy(elems = elems ++ el)
    def appendGlue = copy(elems = elems :+ LayoutElem.unplaced(BuildMeta(createGlue), null))
    def prependGlue = copy(elems = LayoutElem.unplaced(BuildMeta(createGlue), null) +: elems)
    def appendStrut(i: Int) = copy(elems = elems :+ LayoutElem.unplaced(BuildMeta(createStrut(i)), null))
    def prependStrut(i: Int) = copy(elems = LayoutElem.unplaced(BuildMeta(createStrut(i)), null) +: elems)
    def rigid(dim: Dimension) = copy(elems = elems :+ LayoutElem.unplaced(BuildMeta(Swing.RigidBox(dim)), null))

    def panel = {
      val p = new BoxPanel(alignment)
      val contents =
        if(glueElems)
          Y[List[Component], List[Component]](
            rec => {
              case Nil => Nil
              case x :: Nil => List(x)
              case x :: y :: tail if y.peer.isInstanceOf[Filler] => x :: y :: rec(tail)
              case x :: y :: tail if x.peer.isInstanceOf[Filler] && y.peer.isInstanceOf[Filler] => x :: createGlue :: y :: rec(tail)
              case x :: y :: tail => x :: createGlue :: y :: rec(y :: tail)
            }
          )(elems.map(_.meta.component))
        else elems.map(_.meta.component)

      def createRigid(i: Int) = RigidBox(i -> i)

      p.contents ++= indent.map{
        i => createRigid(i) :: contents ::: List(createRigid(i))
      } getOrElse contents

      effects.foreach(_(p))
      p
    }
    def affect(effects: (DSLBoxPanelBuilder#Panel => Unit) *): DSLBoxPanelBuilder = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *): DSLBoxPanelBuilder = copy(layout = layout ++ effects)

    protected def createGlue = alignment match {
      case Orientation.Horizontal => Swing.HGlue
      case Orientation.Vertical => Swing.VGlue
    }

    protected def createStrut(i: Int) = alignment match{
      case Orientation.Horizontal => Swing.HStrut(i)
      case Orientation.Vertical => Swing.VStrut(i)
    }
  }

  case class DSLGridPanelBuilder(size: (Int, Int),
                                 elems: List[LayoutElem],
                                 indent: Option[Int] = Some(10),
                                 panelName: String = "GridPanel",
                                 protected val effects: List[DSLGridPanelBuilder#Panel => Unit] = Nil,
                                 protected val layout: List[Constraints => Unit] = Nil) extends DSLPanelBuilder{
    type Panel = GridPanel
    def panel = new GridPanel(size._1, size._2){
      contents ++= elems.map(_.meta.component)
      effects.foreach(_(this))
    }

    def prepend(el: LayoutElem*) = copy(elems = el.toList ++ elems)
    def append(el: LayoutElem*) = copy(elems = elems ++ el)

    def affect(effects: (DSLGridPanelBuilder#Panel => Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = layout ++ effects)
  }

  case class DSLSimplePanelBuilder(elem: LayoutElem,
                                   panelName: String = "Panel",
                                   protected val effects: List[DSLGridPanelBuilder#Panel => Unit] = Nil,
                                   protected val layout: List[Constraints => Unit] = Nil) extends DSLPanelBuilder{
    type Panel = scala.swing.Panel
    def elems = List(elem)

    def panel = new scala.swing.Panel{
      peer.add(elem.meta.component.peer)
    }

    def append(el: LayoutElem*) = ???
    def prepend(el: LayoutElem*) = ???

    def affect(effects: (Panel => Unit) *) = copy(effects = this.effects ++ effects)
    def layout(effects: (Constraints => Unit) *) = copy(layout = this.layout ++ effects)
  }

  // // //// // //// // //// // //// // //  Positions  // // //// // //// // //// // //// // //

  sealed trait DSLPosition{
    def isRelative: Boolean
    final def isAbsolute = !isRelative

    def isDefined: Boolean
  }
  case object Undefined extends DSLPosition{ def isRelative = false; final def isDefined = false }
  trait DSLAbsolutePosition extends DSLPosition{ final def isRelative = false ; final def isDefined = true}
  case class DSLRelativePosition(dir: DSLRelativePositionDirection, relTo: String) extends DSLPosition{
    final def isRelative = true ; final def isDefined = true
  }
  trait DSLRelativePositionDirection

  trait ComponentAccess{
    def apply(id: String): Component = get(id).get
    def get(id: String): Option[Component]

    def layoutOf(id: String): LayoutElem = getLayoutOf(id).get
    def getLayoutOf(id: String): Option[LayoutElem]

    def id[C <% Component](c: C): String = getId(c).get
    def getId[C <% Component](c: C): Option[String]
    
    def all: Seq[LayoutElem]

    def collect[R](f: PartialFunction[LayoutElem, R]): Seq[R] = all.collect(f)

    def updatable = collect{
      case LayoutElem(BuildMeta(c, _), _, _) if c.isInstanceOf[UpdateInterface] => c.asInstanceOf[UpdateInterface]
    }
  }

  trait ComponentAccessBuilder{
    def build(layout: List[AbstractLayoutSetting]):ComponentAccess
  }

  // // //// // //// // //// // //// // //  Impl  // // //// // //// // //// // //// // //

  trait Layout9PositionsDSL extends LayoutDSL{

    case object Center extends DSLAbsolutePosition
    case object North extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object South extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object West extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object East extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object NorthWest extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object NorthEast extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object SouthWest extends DSLAbsolutePosition with DSLRelativePositionDirection
    case object SouthEast extends DSLAbsolutePosition with DSLRelativePositionDirection
    def theCenter = Center
    def theNorth = North
    def theSouth = South
    def theWest = West
    def theEast = East
    def theNorthWest = NorthWest
    def theNorthEast = NorthEast
    def theSouthWest = SouthWest
    def theSouthEast = SouthEast

    def Left  = West
    def Right = East
    def Up    = North
    def Down  = South
    def theLeft = Left
    def theRight = Right
    def Top = Up
    def theTop = Up
    def Bottom = Down
    def theBottom = Down
  }


  protected def collectDsl[R](l: List[AbstractLayoutSetting], func: PartialFunction[AbstractLayoutSetting, R]): List[R] = l match{
    case Nil => Nil
    case setting :: tail => func.lift(setting).map(_ :: collectDsl(tail, func)) getOrElse collectDsl(tail, func)
  }

  protected def collectFirstDsl[R](l: List[AbstractLayoutSetting], func: PartialFunction[AbstractLayoutSetting, R]): Option[R] = l match{
    case Nil => None
    case setting :: tail => func.lift(setting) orElse collectFirstDsl(tail, func)
  }

  trait LayoutDSLDefaultImpl extends LayoutDSL{
    protected lazy val updatingComponents = componentAccess.updatable

    def updateForms(): Unit = updatingComponents.foreach(_.updateForm())
  }

  trait Frame9PositionsLayoutBuilder extends LayoutBuilder with GridBagLayoutBuilder{
    frame: Frame =>

    /**
     * builds layout within `frame`
     */
    def buildLayout(): Unit = {
      collectFirstDsl(layout, { case Title(title) => frame.title = title })
      collectFirstDsl(layout, { case Size(size) => frame.size = size })

      /*
      frame.contents = layout match{
        case LayoutElem(BuildMeta(c, _), _, _) :: Nil => new GridPanel(1, 1){
          contents += c
        }
        case _ => gridBag(layout)
      }
       */
      frame.contents = gridBag(layout)
      frame.pack()
    }


  }
  trait GridBagLayoutBuilder extends LayoutBuilder with AwtUtils with Layout9PositionsDSL with LayoutDSLDefaultImpl{

    implicit object gbBuilder extends GridBagComponentBuilder(
      meta =>
        gridBag(meta.settings, forceGB = true).asInstanceOf[GridBagPanel]
    )

    object SettingsPrinter extends PrintIndents{
      private def print(s: List[AbstractLayoutSetting])(implicit p: Param): Unit = s match {
        case GridBagMeta(st, _, _) :: tail =>
          printlni("GridBagMeta(")
          nextDepth{
            print(st)
          }
          printlni(if(tail.isEmpty) ")" else "),")
        case LayoutElem(BuildMeta(c, _), id, pos) :: tail =>
          printlni("LayoutElem(")
          nextDepth{
            printlni(s"id=$id")
            printlni(s"pos=$pos")
            printlni(s"component=$c")
          }
          printlni(if(tail.isEmpty) ")" else "),")
        case elem => printlni(elem.toString())
      }

      def asString(s: List[AbstractLayoutSetting]): String = {
        implicit val p = newBuilder(4)
        print(s)
        p.mkString
      }
    }

    def gridBag(settings: List[AbstractLayoutSetting],
                forceGB: Boolean = false,
                forceGBPersistent: Boolean = false): Component = {

//      println("building gridBag: ")
//      println(SettingsPrinter.asString(settings))
//      println("===")

      def panel(elems: List[LayoutElem], scroll: Option[Scrollable]) = { // l: List[LayoutSetting]
        val center = elems.collect{ case e@LayoutElem(_, _, Center) => e} -> Orientation.Horizontal

        val north = elems.collect{ case e@LayoutElem(_, _, North) => e}   -> Orientation.Horizontal
        val south = elems.collect{ case e@LayoutElem(_, _, South) => e}   -> Orientation.Horizontal
        val west = elems.collect{ case e@LayoutElem(_, _, West) => e}     -> Orientation.Vertical
        val east = elems.collect{ case e@LayoutElem(_, _, East) => e}     -> Orientation.Vertical

        val northWest = elems.collect{ case e@LayoutElem(_, _, NorthWest) => e} -> Orientation.Horizontal
        val northEast = elems.collect{ case e@LayoutElem(_, _, NorthEast) => e} -> Orientation.Vertical
        val southWest = elems.collect{ case e@LayoutElem(_, _, SouthWest) => e} -> Orientation.Vertical
        val southEast = elems.collect{ case e@LayoutElem(_, _, SouthEast) => e} -> Orientation.Horizontal

        def n(list: List[List[LayoutElem]]) = if((false /: list)((acc, e) => acc || e.nonEmpty)) 1 else 0

        val x1 = west._1 :: northWest._1 :: southWest._1 :: Nil
        val x2 = center._1 :: north._1 :: south._1 :: Nil
        val x3 = east._1 :: northEast._1 :: southEast._1 :: Nil
        val (nx1, nx2, nx3) = (n(x1), n(x2), n(x3))
        val nx = nx1 + nx2 + nx3

        val y1 = north._1 :: northWest._1 :: northEast._1 :: Nil
        val y2 = center._1 :: west._1 :: east._1 :: Nil
        val y3 = south._1 :: southWest._1 :: southEast._1 :: Nil
        val (ny1, ny2, ny3) = (n(y1), n(y2), n(y3))
        val ny = ny1 + ny2 + ny3


        def all = northWest :: north :: northEast :: west :: center :: east :: southWest :: south :: southEast :: Nil

        val p = new GridBagPanel{
          panel =>

          def createContent(c: (Seq[LayoutElem], Orientation.Value)): Option[BuildMeta] = {
            val (elems, orientation) = c
            if(elems.size == 0) None // do nothing
            else if(elems.size == 1) elems.head.meta match{
              case GridBagMeta(s, l, _) =>
                val meta = BuildMeta(gridBag(s, if(forceGBPersistent) forceGB else false, forceGBPersistent), l: _*)
                LayoutElem(meta, elems.head.id, elems.head.pos).register
                Some(meta)
              case m => Some(m)
            }
            else Some(BuildMeta(new BoxPanel(orientation){ contents ++= elems.map(_.meta.component) }/*, todo: take meta from one of components ?? */))
          }

          def applyLayout(constraints: Constraints, buildConstraints: Seq[Constraints => Unit]) = {
            buildConstraints.foreach(_(constraints))
            constraints
          }

          def putContents(c: (Seq[LayoutElem], Orientation.Value))(x: Int, y: Int, buildConstraints: (Constraints => Unit)*) {
            putContents(c, applyLayout(x -> y, buildConstraints))
          }
          def putContents(c: (Seq[LayoutElem], Orientation.Value), constraints: Constraints) {
            val metaOpt = createContent(c)
            metaOpt.foreach{ meta => panel.layout += meta.component -> applyLayout(constraints, meta.layout) }
          }

          if(nx1 == 1 && ny1 == 1) putContents(northWest)   (0, 0)
          if(nx2 == 1 && ny1 == 1) putContents(north)       (1, 0)
          if(nx3 == 1 && ny1 == 1) putContents(northEast)   (2, 0)
          if(nx1 == 1 && ny2 == 1) putContents(west)        (0, 1)
          if(nx2 == 1 && ny2 == 1) putContents(center)      (1, 1)
          if(nx3 == 1 && ny2 == 1) putContents(east)        (2, 1)
          if(nx1 == 1 && ny3 == 1) putContents(southWest)   (0, 2)
          if(nx2 == 1 && ny3 == 1) putContents(south)       (1, 2)
          if(nx3 == 1 && ny3 == 1) putContents(southEast)   (2, 2)
        }

        scroll.map{
          case Scrollable(v, h) => new ScrollPane(p){
            verticalScrollBarPolicy = v
            horizontalScrollBarPolicy = h
          }
        } getOrElse p
      }

      // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

      val scrollable = settings.collectFirst{ // search scrollable on top level
        case sc: Scrollable => sc
      }
      val (absolute, tmp) = settings.collect{ // search elements on top level
        case el: LayoutElem => el
      }.partition(_.pos.isAbsolute)
      val (relative, undef) = tmp.partition(_.pos.isRelative)
      val groupedRelative = relative.groupBy(_.pos.asInstanceOf[DSLRelativePosition].relTo)

      def relativeToAbsolute: DSLPosition => DSLAbsolutePosition = {
        case DSLRelativePosition(dir, _) => dir match {
          case abs: DSLAbsolutePosition => abs
        }
      }

      val absoluteMap = absolute.map(le => le.id -> le).toMap
      def center(id: String) = LayoutElem(componentAccess.layoutOf(id).meta, id, Center)

      val panelsMap = groupedRelative.map{
        case (centerId, elems) => centerId -> panel(center(centerId) :: elems.map(e => e.copy(pos = relativeToAbsolute(e.pos))), None) // todo
      }.toMap
      val unReferenced = (absoluteMap -- panelsMap.keys).map(_._2)
      val builderComponents = settings.collect{
        case b: AbstractDSLBuilder => b.component
      }
      // todo: all this is very shady
      val finalElems =
        panelsMap.map{
          case (id, p) => LayoutElem(p, id, componentAccess.layoutOf(id).pos)
        }.toSeq ++ unReferenced
      val finalComponents = finalElems.map(_.meta.component) ++ builderComponents
      val res =
        if(finalComponents.length == 1 && finalComponents.head.isInstanceOf[GridBagPanel] && scrollable.isEmpty)
          finalComponents.head
        else if(forceGB) panel(finalElems.toList, None)
        else if(finalComponents.length == 1) finalComponents.head
        else panel(finalElems.toList, scrollable)
      res
    }
  }
}

object SwingSurroundingFrameAppCreation extends SwingSurroundingFrameAppCreation
@deprecated
trait SwingSurroundingFrameAppCreation extends SwingFrameAppCreation{

  trait SurroundingLayoutBuilder extends LayoutBuilder{
    dsl: SurroundingLayoutDSL =>
  }

  trait SurroundingLayoutDSL extends LayoutDSL with FormCreationDSL{

    private val awtToSwingComponentCache = mutable.HashMap.empty[java.awt.Component, Component]
    implicit def awtToSwingComponent(c: java.awt.Component): Component =
      awtToSwingComponentCache.getOrElse(c, {
        val p = new Panel { // let's see, let's see ...
          peer.add(new JPanel{
            add(c)
            /*
                          override def paintComponent(g: Graphics) {
                            c.paint(g)
                            super.paintComponent(g)
                          }
            */
          })
        }
        awtToSwingComponentCache += c -> p
        p
      })
  }
}