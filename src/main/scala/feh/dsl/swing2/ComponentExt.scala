package feh.dsl.swing2

import scala.swing.Component

object ComponentExt {
  implicit class ComponentWrapper(c: Component){
    def lock()    = c.enabled = false
    def unlock()  = c.enabled = true
  }
}
