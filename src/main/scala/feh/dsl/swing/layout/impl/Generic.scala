package feh.dsl.swing.layout.impl

import feh.util._
import feh.dsl.swing.layout.LayoutDSL._
import feh.dsl.swing.layout.{LayoutDSL, RegistringComponentAccess}
import scala.swing.Component

abstract class GenericPlacing[P](val placable: Placable[P])(implicit compReg: RegistringComponentAccess)
  extends LayoutDSL.DSLPlacing
{
  type Placing <: DSLPlacing

  def at(pos: Abs): LElem[Placing] = bld(pos)
  def in(pos: Abs): LElem[Placing] = bld(pos)

  def to(dir: Dir): DSLRelativelyOf = rel(dir)
  def on(dir: Dir): DSLRelativelyOf = rel(dir)

  private def rel(dir: Dir) = new DSLRelativelyOf{
    def of(rel: String): LElem[Placing] = bld(DSLRelativePosition(dir, rel))
    def from(rel: String): LElem[Placing] = of(rel)

    def of(meta: BuildMeta): LElem[Placing] = compReg.id(meta)
      .map(rel => bld(DSLRelativePosition(dir, rel)))
      .getOrThrow(s"$meta is not registered")
    def from(meta: BuildMeta): LElem[Placing] = of(meta)
  }

  private def bld(pos: DSLPosition) = LayoutElem(placable.meta, placable.id.orNull, pos)
    .$$(compReg.register _).pipe(_.wrap[Placing])

}
