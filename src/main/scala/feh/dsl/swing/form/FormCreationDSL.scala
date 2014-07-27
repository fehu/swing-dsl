package feh.dsl.swing.form

import scala.swing.GridBagPanel
import scala.xml.NodeSeq


object FormCreationDSL extends FormCreationChoosers{
  type Constraints = GridBagPanel#Constraints

  class MonitorCreation{
    def apply[T](get: => T)
                (implicit chooser:  (=> T) => MonitorComponentChooser[T]) = chooser(get)

    def apply[K, V](get: => Map[K, V])
                   (implicit chooser:  (=> Map[K, V]) => MapMonitorComponentChooser[K, V]) = chooser(get)
  }

  class ControlCreation{
    def apply[T](get: => T)(set: T => Unit)
                (implicit chooser: (=> T, T => Unit) => ControlComponentChooser[T]) = chooser(get, set)

    /** 
     * 
     * @param get
     * @param static <code>true</code>: load the sequence from @get on form creation and do not reload them
     *               <code>false</code>: load the sequence from @get on every [[feh.dsl.swing.SwingFrameAppCreation#LayoutBuilder.buildLayout]]
     * @param chooser
     * @tparam T
     * @return
     */
    def seq[T](get: => Seq[T], static: Boolean = false)
              (implicit chooser: (=> Seq[T], Boolean) => SeqControlComponentChooser[T]) = chooser(get, static)
    
    def num[N](get: => N)(set: N => Unit)
              (implicit chooser: (=> N, N => Unit) => NumericControlComponentChooser[N], num: Numeric[N]) = chooser(get, set)
  }

  class ActionCreation{
    def trigger(action: => Unit)(implicit chooser: (=> Unit) => TriggerComponentChooser) = chooser(action)
    def toggle(on: => Unit, off: => Unit)(implicit chooser: (=> Unit, => Unit) => ToggleComponentChooser) = chooser(on, off)
  }

}
import FormCreationDSL._

  trait FormCreationDSL{
    val monitorFor = new MonitorCreation
    val controlFor = new ControlCreation
    val action = new ActionCreation

    def updateForms()
}


trait TextCreationDSLHelper extends DSLComponentChooser{
  self: FormCreationDSL =>

  def html[T](t: => T)(build: T => NodeSeq)(implicit chooser:  (=> T) => MonitorComponentChooser[T]): formsCfg.Label[T] =
    label(t).extract(t => surroundHtml(<html>{build(t)}</html>).toString())
  def html(html: NodeSeq)(implicit chooser:  (=> NodeSeq) => MonitorComponentChooser[NodeSeq]): formsCfg.Label[NodeSeq] =
    label(surroundHtml(html))
  def label[T](text: => T)(implicit chooser:  (=> T) => MonitorComponentChooser[T]): formsCfg.Label[T] =
    monitorFor(text).label.static(set = true).asInstanceOf

  private def surroundHtml(in: NodeSeq) = in match{
    case <html>{_*}</html> => in
    case _ => <html>{in}</html>
  }

}