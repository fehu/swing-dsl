package feh.dsl.swing

import scala.swing.Frame

trait AppFrameControl{
  def start()
  def stop()
}

/*
trait SwingAppBuildingEnvironment extends SwingFrameAppCreation{
  trait SwingAppFrame extends Frame with AppFrameControl {
    self: LayoutDSL with LayoutBuilder =>

    override def closeOperation(): Unit = {
      stop()
      super.closeOperation()
    }
  }

}
*/
