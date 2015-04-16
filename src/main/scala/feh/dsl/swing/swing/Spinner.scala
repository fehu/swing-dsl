package feh.dsl.swing.swing

import javax.swing.{SpinnerModel, JSpinner, JComponent}

import scala.swing.Component

class Spinner[T](val model: SpinnerModel) extends Component{
  override lazy val peer: JSpinner = new JSpinner(model)

  def value = peer.getValue.asInstanceOf[T]
  def value_=(v: T) { peer.setValue(v) }

}
