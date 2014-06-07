package feh.dsl.swing

import scala.swing.Component
import feh.dsl.swing.LayoutDSL.{AbstractLayoutSetting, LayoutElem}
import feh.dsl.swing.form.FormCreationDSL._

trait ComponentAccess{
  def apply(id: String): Component = get(id).get
  def get(id: String): Option[Component]

  def layoutOf(id: String): LayoutElem = getLayoutOf(id).get
  def getLayoutOf(id: String): Option[LayoutElem]

  def id[C <% Component](c: C): String = getId(c).get
  def getId[C <% Component](c: C): Option[String]

  def all: Seq[LayoutElem]

  def collect[R](f: PartialFunction[LayoutElem, R]): Seq[R] = all.collect(f)

//  def updatable = collect{
//    case LayoutElem(BuildMeta(c, _), _, _) if c.isInstanceOf[UpdateInterface] => c.asInstanceOf[UpdateInterface]
//  }
}

trait RegistringComponentAccess extends ComponentAccess{
  def register(elem: LayoutElem)
}

trait ComponentAccessBuilder{
  def build(layout: List[AbstractLayoutSetting]):ComponentAccess
}
