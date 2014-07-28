package feh.dsl.swing.form

import scala.swing.GridBagPanel
import scala.xml.NodeSeq


object FormCreationDSL extends FormCreationChoosers{
  type Constraints = GridBagPanel#Constraints

  abstract class MonitorCreation[Config <: FormCreationDSLBuilderConfig]{
    type Chooser[T] <: MonitorComponentChooser[T, Config]
    type MChooser[K, V] <: MapMonitorComponentChooser[K, V, Config]

    def apply[T](get: => T)
                (implicit chooser:  (=> T) => Chooser[T]): Chooser[T] = chooser(get)

    def apply[K, V](get: => Map[K, V])
                   (implicit chooser:  (=> Map[K, V]) => MChooser[K, V]): MChooser[K, V] = chooser(get)
  }

  class ControlCreation[Config <: FormCreationDSLBuilderConfig]{
    def apply[T](get: => T)(set: T => Unit)
                (implicit chooser: (=> T, T => Unit) => ControlComponentChooser[T, Config]) = chooser(get, set)

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
              (implicit chooser: (=> Seq[T], Boolean) => SeqControlComponentChooser[T, Config]) = chooser(get, static)
    
    def num[N](get: => N)(set: N => Unit)
              (implicit chooser: (=> N, N => Unit) => NumericControlComponentChooser[N, Config], num: Numeric[N]) = chooser(get, set)
  }

  class ActionCreation[Config <: FormCreationDSLBuilderConfig]{
    def trigger(action: => Unit)
               (implicit chooser: (=> Unit) => TriggerComponentChooser[Config]) = chooser(action)
    def toggle(on: => Unit, off: => Unit)
              (implicit chooser: (=> Unit, => Unit) => ToggleComponentChooser[Config]) = chooser(on, off)
  }

}
import FormCreationDSL._

  trait FormCreationDSL{
    type FormConfig <: FormCreationDSLBuilderConfig

    val monitorFor: MonitorCreation[FormConfig]
    val controlFor: ControlCreation[FormConfig]
    val action: ActionCreation[FormConfig]

    def updateForms()
}


trait TextCreationDSLHelper extends DSLComponentChooser{
  self: FormCreationDSL =>

  type FormConfig <: FormCreationDSLBuilderConfig

  def html[T](t: => T)(build: T => NodeSeq)
             (implicit chooser:  (=> T) => MonitorComponentChooser[T, FormConfig]): formsCfg.Label[T]
  def label[T](text: => T)
              (implicit chooser:  (=> T) => MonitorComponentChooser[T, FormConfig]): formsCfg.Label[T]

  def html(html: NodeSeq)(implicit chooser:  (=> NodeSeq) => MonitorComponentChooser[NodeSeq, FormConfig]): formsCfg.Label[NodeSeq] =
    label(surroundHtml(html))


  protected def surroundHtml(in: NodeSeq) = in match{
    case <html>{_*}</html> => in
    case _ => <html>{in}</html>
  }

}