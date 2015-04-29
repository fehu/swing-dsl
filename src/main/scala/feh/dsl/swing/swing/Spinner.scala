package feh.dsl.swing.swing

import javax.swing.{SpinnerModel, JSpinner, JComponent}

import scala.swing.event.ValueChanged
import scala.swing.{Publisher, Component}

class Spinner[T](val model: SpinnerModel) extends Component with Publisher{
  override lazy val peer: JSpinner = new JSpinner(model)

  def value = peer.getValue.asInstanceOf[T]
  def value_=(v: T) { peer.setValue(v) }

  peer.addChangeListener(new javax.swing.event.ChangeListener {
    def stateChanged(e: javax.swing.event.ChangeEvent) {
      publish(new ValueChanged(Spinner.this))
    }
  })
}
