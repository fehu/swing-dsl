package feh.dsl.swing.container

import feh.dsl.swing.form.FormCreationDSL._
import scala.swing.ScrollPane.BarPolicy
import feh.dsl.swing.layout.LayoutDSL.{BuildMeta, LayoutSetting, UnplacedLayoutElem, LayoutElem}
import scala.swing.{Orientation, FlowPanel}

object ContainerCreationDSL{
  trait ContainerDSLBuilderConfig{
    type ScrollPane <: AbstractDSLBuilder
    type SplitPane <: AbstractDSLBuilder
    type Panel <: AbstractDSLBuilder
    type BoxPanel <: AbstractDSLBuilder
    type FlowPanel <: AbstractDSLBuilder
    type GridPanel <: AbstractDSLBuilder
    type GridBagPanel <: AbstractDSLBuilder
  }

  trait DSLContainerCfgProvider{
    protected val containerCfg: ContainerDSLBuilderConfig
  }

  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //  //

  trait PanelChooser extends DSLContainerCfgProvider{
    def flow[E <% UnplacedLayoutElem](alignment: FlowPanel.Alignment.type => FlowPanel.Alignment.Value)
                                     (elems: E*): containerCfg.FlowPanel
    def box[E <% UnplacedLayoutElem](alignment: Orientation.type => Orientation.Value)
                                    (elems: E*): containerCfg.BoxPanel
    def grid[E <% UnplacedLayoutElem](rows: Int, cols: Int)
                                     (elems: E*): containerCfg.GridPanel
    def gridBag(settings: LayoutSetting*): containerCfg.GridBagPanel
    def apply[E <% UnplacedLayoutElem](el: E): containerCfg.Panel
  }

}

import ContainerCreationDSL._

trait ContainerCreationDSL extends DSLContainerCfgProvider{
  def split(orientation: scala.swing.Orientation.type => scala.swing.Orientation.Value)
           (leftOrDown: LayoutElem, rightOrUp: LayoutElem): containerCfg.SplitPane

  def scrollable[M <% BuildMeta](vert: BarPolicy.Value = BarPolicy.AsNeeded,
                                 hor: BarPolicy.Value = BarPolicy.AsNeeded)
                                (content: M, id: String): containerCfg.ScrollPane
  /** Puts @content in a simple box container
    *
    * @param content
    * @param id
    * @tparam M
    * @return
    */
  def boxed[M <% BuildMeta](content: M, id: String): containerCfg.BoxPanel

  def panel: PanelChooser

}
