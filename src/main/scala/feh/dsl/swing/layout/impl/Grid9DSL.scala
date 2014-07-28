package feh.dsl.swing.layout.impl

import feh.dsl.swing.layout.{RegistringComponentAccess, ComponentAccess, LayoutDSL}
import feh.dsl.swing.layout.LayoutDSL._

object Grid9DSL {


  class Placing[P](placable: Placable[P])(implicit compReg: RegistringComponentAccess)
    extends GenericPlacing(placable)
  {
    type Placing = Grid9DSL.Placing[P]
    type Abs = Grid9DSLPositions#Absolute
    type Dir = Grid9DSLPositions#RelativeDirection
  }

  case class Layout(placings: Seq[LayoutElem], components: ComponentAccess) extends LayoutDSL.Layout
}

trait Grid9DSLPositions{
  trait Absolute extends DSLAbsolutePosition
  trait RelativeDirection extends DSLRelativePositionDirection

  case object Center extends Absolute
  case object North extends Absolute with RelativeDirection
  case object South extends Absolute with RelativeDirection
  case object West extends Absolute with RelativeDirection
  case object East extends Absolute with RelativeDirection
  case object NorthWest extends Absolute with RelativeDirection
  case object NorthEast extends Absolute with RelativeDirection
  case object SouthWest extends Absolute with RelativeDirection
  case object SouthEast extends Absolute with RelativeDirection
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

/**
 * ex Layout9PositionsDSL
 */
trait Grid9DSL extends LayoutDSL with Grid9DSLPositions with DSLComponentRegister{
  type L = Grid9DSL.Layout
  type Placing = Grid9DSL.Placing[_]

  def layout(pl: LElem[Placing]*): L = Grid9DSL.Layout(pl.map(_.elem), regComp)
  def place[P](p: Placable[P]): Placing = new Grid9DSL.Placing(p)(regComp)

  /** element won't be registered */
  def noId = null
}