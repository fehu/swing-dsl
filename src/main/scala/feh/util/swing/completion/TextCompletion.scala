package feh.util.swing.completion

import scala.swing.{Component, EditorPane}
import scala.swing.event._
import feh.util._
import feh.util.swing.completion.CompletionEditorPane.{CompletionPopup, ListeningKeysPressed, AbstractCompletionEditorPane}
import javax.swing.{Popup => JPopup}
import feh.util.swing.{Keys, Popup}
import scala.collection.mutable
import scala.swing.event.KeyPressed
import scala.Some
import scala.swing.event.MouseClicked

trait TextCompletion {
  def complete(context: String, pos: Int, verbosity: Option[Int]): List[String] 
}

object CompletionEditorPane{
  trait AbstractCompletionEditorPane extends EditorPane{
    type Verbosity = Option[Int]

    def completion: TextCompletion

    protected def complete(verbosity: Option[Int], action: List[String] => Unit){
      completion.complete(text, caret.dot, verbosity) match {
        case Nil =>
        case list => action(list.distinct)
      }
    }

  }

  trait ListeningKeysPressed{
    pane: AbstractCompletionEditorPane =>

    listenTo(keys)
    reactions += {
      case kp: KeyPressed if completionKeys.contains(kp.key, kp.modifiers) =>
        val (v, act) = completionKeys(kp.key, kp.modifiers)
        complete(v, l => act(l))
    }

    protected val completionKeys = mutable.HashMap.empty[(Key.Value, Key.Modifiers), (Verbosity, List[String] => Unit)]

    def completeOnKeyPress(c: Map[(Key.Value, Key.Modifiers), Verbosity])(action: (List[String], Verbosity) => Unit) =
      completionKeys ++= c.mapValues(v => v -> ((l: List[String]) => action(l, v))).toSeq
  }

  trait CompletionPopup{
    pane: AbstractCompletionEditorPane =>

    def popup(position: Int, canComplete: List[String], verbosity: Verbosity)
    def completing: Boolean
  }

  object CompletionPopup{
    trait CompletionPopupBase extends CompletionPopup with ListeningKeysPressed{
      pane: AbstractCompletionEditorPane =>

      pane.reactions += {
        case ev if clearPopupsPF.isDefinedAt(ev) =>
          println("clear!")
          if(clearPopupsPF(ev)) clearPopup()
      }

      private var currentVerbosity: Option[Int] = None
      def completing = currentPopup.isDefined

      def popupYOffset: Int = 3

      private var currentPopup: Option[JPopup] = None
      protected def showPopup(p: JPopup) = synchronized{ // todo ?? synchronized ??
        currentPopup.foreach(_.hide())
        currentPopup = Some(p $$ {_.show()})
      }
      protected def popup = synchronized{ currentPopup }
      protected def clearPopup() = synchronized{
        currentPopup.foreach(_.hide())
        currentPopup = None
      }

      def popupContents(canComplete: List[String]): Component
      
      def popup(position: Int, canComplete: List[String], verbosity: Verbosity){
        currentVerbosity = verbosity
        val charRect = pane.peer.getUI.modelToView(pane.peer, position)
        val popupPosX = charRect.x + pane.locationOnScreen.x
        val popupPosY = charRect.y + charRect.height + popupYOffset + pane.locationOnScreen.y
        showPopup(Popup(pane, popupContents(canComplete), popupPosX, popupPosY))
      }

      listenTo(pane.mouse.clicks)
      def clearPopupsPF: PartialFunction[Event, Boolean] = {
        case KeyPressed(_, k, mod, _) if completionKeys.keySet.contains(k, mod) => false
        case _: KeyPressed => true
        case _: MouseClicked => true
      }
    }
    @deprecated("temporary")
    trait Label extends CompletionPopupBase{
      pane: AbstractCompletionEditorPane =>

      def popupContents(canComplete: List[String]) = {
        def labelHtml =
          <html>
            <ul>
              {canComplete.map(s => <li>{s}</li>)}
            </ul>
          </html>
        new scala.swing.Label(labelHtml.toString())
      }
    }
  }
}


object ScalaCompletionEditorPane{
  protected val continueCompletionKeys = Keys.alphanumericSet + Key.Period 

//  case MouseClicked(_, p, mod, n, _) => true
//  case KeyPressed(_, k, 0, _) =>
//  if(completing && continueCompletionKeys.contains(k)){
//    complete(currentVerbosity, popup(caret.dot, _, currentVerbosity))
//    false
//  }}
}

class CompletionEditorPane(val completion: TextCompletion)
  extends AbstractCompletionEditorPane with CompletionPopup.Label with ListeningKeysPressed
//  with CtrlSpaceCompletion with CtrlShiftSpaceCompletion
{
  private val CtrlShiftModifier = Key.Modifier.Control + Key.Modifier.Shift
  completeOnKeyPress(Map(
    (Key.Space, Key.Modifier.Control) -> ctrlSpaceVerbosity,
    (Key.Space, CtrlShiftModifier) -> ctrlShiftSpaceVerbosity
  )){
    case (l, v) => popup(caret.dot, l, v)
  }

  def ctrlShiftSpaceVerbosity = Some(10)
  def ctrlSpaceVerbosity = None
}
