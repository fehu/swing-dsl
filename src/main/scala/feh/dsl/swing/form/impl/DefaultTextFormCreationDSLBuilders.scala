package feh.dsl.swing.form.impl

import feh.util._
import feh.dsl.swing.form.FormCreationDSL
import FormCreationDSL._
import scala.swing.{TextField, Label}
import scala.concurrent.{ExecutionContext, Future}

abstract class GenericDSLBuilder[T, GConfg] extends DSLFormBuilder[T]
{
  type Builder <: GenericDSLBuilder[T, GConfg]

  val get: () => T
  val static: Boolean
  val conf: GConfg
  val effects: List[Form => Unit]

  protected def copy(get: () => T = this.get,
                     static: Boolean = this.static,
                     conf: GConfg = this.conf,
                     effects: List[Form => Unit] = this.effects): Builder

  def static(set: Boolean) = copy(static = set)
  def affect(effects: (Comp => Unit)*) = copy(effects = this.effects ++ effects.asInstanceOf[Seq[Form => Unit]])
  def configure(transforms: (GConfg => GConfg)*) = copy(conf = Function.chain(transforms) apply this.conf)

  protected def build_init(form: Form, set: T => Unit) = {
    if(static) set(get())
    effects.foreach(_(form))
  }

  protected def build_update(set: T => Unit) = if(!static) set(get())
}

trait GenericStringDSLBuilder [T, GConf]
  extends GenericDSLBuilder[T, GConf] with DSLFutureStringExtraction[T, String]
{
  val get: () => T

  protected def build_init_str(form: Form, set: String => Unit): Unit = {
    if(static) extractAndThen(get(), set)
    effects.foreach(_(form))
  }

  protected def build_update_str(set: String => Unit): Unit = if(!static) extractAndThen(get(), set)
}

trait DefaultTextFormCreationDSLBuilders {

  case class LabelConfig[T](
    extractString: Either[(T) => String, ((T) => Future[String], ExecutionContext)] = Default.extractStringSync[T]
                          )

  case class TextConfig[T](
    extractString: Either[(T) => String, ((T) => Future[String], ExecutionContext)] = Default.extractStringSync[T]
                             )


  case class DSLLabelBuilder[T] protected[form](get: () => T,
                                                static: Boolean,
                                                conf: LabelConfig[T] = LabelConfig[T](),
                                                effects: List[DSLLabelBuilder[T]#Form => Unit] = Nil)
    extends GenericStringDSLBuilder[T, LabelConfig[T]]
  {
    type Builder = DSLLabelBuilder[T]
    type Form = Label with UpdateInterface
    def formName = "Label"

    def build: Form = new Label with UpdateInterface{
      build_init_str(this, text = _)
      def updateForm() = build_update_str(text = _)
    }

    def extractString = conf.extractString
    def extractSync(fsync: T => String) = configure(_.copy(extractString = Left(fsync)))

    protected def copy(get: () => T = this.get, static: Boolean = this.static, conf: LabelConfig[T] = this.conf,
                       effects: List[Form => Unit] = this.effects) = DSLLabelBuilder(get, static, conf, effects)
  }

  case class DSLTextBuilder[T] protected[form](get: () => T,
                                               static: Boolean,
                                               conf: TextConfig[T] = TextConfig[T](),
                                               effects: List[DSLTextBuilder[T]#Form => Unit] = Nil)
    extends GenericStringDSLBuilder[T, TextConfig[T]]
  {
    type Builder = DSLTextBuilder[T]
    type Form = TextField with UpdateInterface
    def formName: String = "TextField"

    def build: Form = new TextField with UpdateInterface{
      build_init_str(this, text = _)
      def updateForm() = build_update_str(text = _)
    }

    def extractString = conf.extractString
    def extractSync(fsync: T => String) = configure(_.copy(extractString = Left(fsync)))

    protected def copy(get: () => T = this.get, static: Boolean = this.static, conf: TextConfig[T] = this.conf,
                       effects: List[Form => Unit] = this.effects) = DSLTextBuilder(get, static, conf, effects)
  }
}
