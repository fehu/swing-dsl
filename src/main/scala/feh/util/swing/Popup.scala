package feh.util.swing

import javax.swing.PopupFactory
import scala.swing.Component

object Popup extends PopupFactory{
  def apply(owner: Component, contents: Component, x: Int, y: Int) = getPopup(owner.peer, contents.peer, x, y)
}

