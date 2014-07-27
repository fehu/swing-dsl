package feh.dsl.swing.layout

import scala.swing.Component
import feh.dsl.swing.layout.LayoutDSL.{BuildMeta, AbstractLayoutSetting, LayoutElem}

trait ComponentAccess{
  def apply(id: String): BuildMeta = get(id).get
  def get(id: String): Option[BuildMeta]

  def layoutOf(id: String): LayoutElem = getLayoutOf(id).get
  def getLayoutOf(id: String): Option[LayoutElem]

  def all: Seq[LayoutElem]

  def id(meta: BuildMeta): Option[String]

  def collect[R](f: PartialFunction[LayoutElem, R]): Seq[R] = all.collect(f)

//  def updatable = collect{
//    case LayoutElem(BuildMeta(c, _), _, _) if c.isInstanceOf[UpdateInterface] => c.asInstanceOf[UpdateInterface]
//  }
}

trait RegistringComponentAccess extends ComponentAccess{
  def register(elem: LayoutElem)
  def remove(elem: LayoutElem)
}

trait ComponentAccessBuilder{
  def build(layout: List[AbstractLayoutSetting]):ComponentAccess
}
