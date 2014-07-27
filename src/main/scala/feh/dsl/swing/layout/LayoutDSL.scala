package feh.dsl.swing.layout

import scala.swing._
import feh.dsl.swing.form.FormCreationDSL._
import feh.util._
import scala.Some


object LayoutDSL{

  sealed trait AbstractLayoutSetting

  trait FrameSetting extends AbstractLayoutSetting

  trait LayoutSetting extends AbstractLayoutSetting{
    def and(that: LayoutSetting) = this :: that :: Nil
    def and(that: List[LayoutSetting]) = this :: that

    def &(that: LayoutSetting) = and(that)
    def &(that: List[LayoutSetting]) = and(that)
  }

  implicit class LayoutSettingListWrapper(list: List[LayoutSetting]){
    def and(other: LayoutSetting) = list :+ other
    def &(other: LayoutSetting) = and(other)
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


  // // //// // //// // //// // //// // //  Layout Settings  // // //// // //// // //// // //// // //

  sealed trait BuildMeta{
    def tpe: BuildMetaType

    def buildComponent: Component
    def component: String

    def name = s"$tpe:$component"
    override def toString: String = s"BuildMeta($name)"
  }

  abstract class BuildMetaType(val name: String) {
    override def toString = name
  }

  object BuildMeta{
    def apply(`type`: BuildMetaType, name: String, build: => Component): BuildMeta = new BuildMeta{
      def tpe = `type`
      def buildComponent = build
      def component = name
    }
    def build(`type`: BuildMetaType, name: String, build: => Component) = apply(`type`, name, build)

    def unapply(meta: BuildMeta): Option[(BuildMetaType, String, Lifted[Component])] =
      Some(meta.tpe, meta.component, meta.buildComponent.lift)
  }

  case class LElem[+P <: DSLPlacing](elem: LayoutElem) extends LayoutElem(elem.meta, elem.id, elem.pos)

  class LayoutElem protected[layout] (val meta: BuildMeta, val id: String, val pos: DSLPosition) extends LayoutSetting{
    override def toString: String = s"LayoutElem($id, $pos, $meta)"
    def copy(meta: BuildMeta = this.meta, id: String = this.id, pos: DSLPosition = this.pos) = LayoutElem(meta, id, pos)

    def wrap[P <: DSLPlacing] = LElem[P](this)
  }
  object LayoutElem{
    def apply(meta: BuildMeta, id: String, pos: DSLPosition): LayoutElem =
      new LayoutElem(meta, getOrRandom(id, meta), pos)
    def unplaced(c: BuildMeta, id: String): UnplacedLayoutElem = new LayoutElem(c, getOrRandom(id, c), null) with UnplacedLayoutElem
    def unapply(el: LayoutElem): Option[(BuildMeta, String, DSLPosition)] = Some(el.meta, el.id, el.pos)

    private def getOrRandom(id: String, meta: BuildMeta) = Option(id) getOrElse randomId(meta)
    def randomId(meta: BuildMeta) = meta.name + "#" + meta.component.hashCode()
  }

  trait UnplacedLayoutElem extends LayoutElem{
    override val pos = Undefined
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  case class LayoutMeta(build: BuildMeta, layout: List[Constraints => Unit])

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DSLPlacing{
    type Abs <: DSLAbsolutePosition
    type Dir <: DSLRelativePositionDirection

    def at(pos: Abs): LayoutElem
    def in(pos: Abs): LayoutElem
    def to(rel: Dir): DSLRelativelyOf
    def on(rel: Dir): DSLRelativelyOf
  }

  trait DSLRelativelyOf{
    def of(rel: String): LayoutElem
    def of(meta: BuildMeta): LayoutElem

    def from(rel: String): LayoutElem
    def from(meta: BuildMeta): LayoutElem
  }
  
  trait Placable[+T]{
    def meta: BuildMeta
    def id: Option[String]
  }

  trait Layout{
    def placings: Seq[LayoutElem]
    def components: ComponentAccess
  }
}

import LayoutDSL._

trait LayoutDSL{
  type L <: Layout
  type Placing <: DSLPlacing

  def place[P](p: Placable[P]): Placing

  def layout(pl: LElem[Placing]*): L
}

trait LayoutBuilder[-L <: Layout, +R <: Component]{
  def build(layout: L): R
}

/*

trait LayoutDSL extends FormCreationDSL{
  protected val componentAccess: RegistringComponentAccess


  def place[M <% BuildMeta](what: M, id: String): DSLPlacing = DSLPlacing(what, id)
  def place[T](builder: DSLFormBuilder[T], id: String): DSLPlacing = DSLPlacing(builder.formMeta, id)
  def place(builder: DSLPanelBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
  def place(builder: DSLScrollPaneBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
  def place(builder: BoxedBuilder): DSLPlacing = DSLPlacing(builder.meta, builder.id)
  def place(meta: GridBagMeta, id: String): DSLPlacing = DSLPlacing(meta, id)

  def noId: String = null

  protected implicit class LayoutElemRegister(elem: LayoutElem){
    def register = elem $${
      componentAccess.register _
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
}

trait LayoutBuilder{
//  dsl: LayoutDSL =>

  val layout: List[AbstractLayoutSetting]
  protected val componentAccess: RegistringComponentAccess

  /**
   * builds [[layout]]
   */
  def buildLayout()
}

//object SwingFrameAppCreation extends SwingFrameAppCreation
trait SwingFrameAppCreation extends FormCreationDSL{

  trait LayoutDSL extends FormCreationDSL{

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
  }

  
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

  case class Title(title: String) extends FrameSetting
  case class Size(size: (Int, Int)) extends FrameSetting



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

  implicit class ScrollBuilderOps(builder: DSLScrollPaneBuilder) extends DSLFormBuilderOps[DSLScrollPaneBuilder](builder)
  implicit class PanelBuilderOps(builder: DSLPanelBuilder) extends DSLFormBuilderOps[DSLPanelBuilder](builder)
  implicit class GridBagMetaOps(meta: GridBagMeta) extends DSLFormBuilderOps[GridBagMeta](meta)
  implicit class FlowPanelBuilderOps(builder: DSLFlowPanelBuilder) extends DSLFormBuilderOps[DSLFlowPanelBuilder](builder)
  implicit class BoxPanelBuilderOps(builder: DSLBoxPanelBuilder) extends DSLFormBuilderOps[DSLBoxPanelBuilder](builder)
  implicit class GridPanelBuilderOps(builder: DSLGridPanelBuilder) extends DSLFormBuilderOps[DSLGridPanelBuilder](builder)

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
}*/
