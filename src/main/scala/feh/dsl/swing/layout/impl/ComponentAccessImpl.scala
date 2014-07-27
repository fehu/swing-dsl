package feh.dsl.swing.layout.impl

import feh.dsl.swing.layout.RegistringComponentAccess
import feh.dsl.swing.layout.LayoutDSL.{BuildMeta, LayoutElem}
import scala.collection.mutable

class ComponentAccessImpl extends RegistringComponentAccess{
  protected lazy val reg = mutable.HashMap.empty[String, LayoutElem]

  def register(elem: LayoutElem): Unit = Option(elem.id) foreach (reg += _ -> elem)
  def remove(elem: LayoutElem): Unit = Option(elem.id) foreach reg.remove

  def get(id: String): Option[BuildMeta] = getLayoutOf(id).map(_.meta)
  def id(meta: BuildMeta): Option[String] = reg.values.find(_.meta == meta).map(_.id)

  def all: Seq[LayoutElem] = reg.values.toList

  def getLayoutOf(id: String): Option[LayoutElem] = reg.get(id)
}

trait DSLComponentRegister{
  protected lazy val regComp: RegistringComponentAccess = new ComponentAccessImpl
}
