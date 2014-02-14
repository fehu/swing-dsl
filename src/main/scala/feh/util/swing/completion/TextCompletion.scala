package feh.util.swing.completion

import scala.swing.{ScrollPane, ListView, Component, EditorPane}
import scala.swing.event._
import feh.util._
import feh.util.swing.completion.CompletionListEditorPane.{CompletionPopup, ListeningKeysPressed, AbstractCompletionEditorPane}
import javax.swing.{Popup => JPopup}
import feh.util.swing.{Keys, Popup}
import scala.collection.mutable
import scala.swing.event.KeyPressed
import scala.swing.Swing._
import scala.swing.event.MouseClicked
import scala.swing.ScrollPane.BarPolicy
import scala.swing.ListView.IntervalMode
import javax.swing.text.{SimpleAttributeSet, Utilities}
import feh.util.swing.completion.CompletionListEditorPane.CompletionPopup.ScalaCompletion

trait TextCompletion {
  def complete(context: String, pos: Int, verbosity: Option[Int]): List[String] 
}

object CompletionListEditorPane{
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
          if(clearPopupsPF.lift(ev) getOrElse false) clearPopup()
      }

      protected var currentVerbosity: Option[Int] = None
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

    trait ScrollingList extends CompletionPopupBase{
      pane: AbstractCompletionEditorPane =>

      def maxCompletionListHeight = 400

      def onCompletionSelection(sel: String, pos: Int) = {
        val drop = pane.text.charAt(pos-1) match {
          case '.' | ' ' => 0
          case _ => pos - Utilities.getWordStart(pane.peer, pos)
        }
        pane.peer.getDocument $$ (_.remove(pos-drop, drop)) $$ (_.insertString(pos-drop, sel, new SimpleAttributeSet()))
        clearPopup()
      }

      protected val caretPosState = new ScopedState(-1)


      override def popup(position: Int, canComplete: List[String], verbosity: Verbosity) =
        caretPosState.doWith(position){ super.popup(position, canComplete, verbosity) }

      def popupContents(canComplete: List[String]) = new ScrollPane(
          new ListView(canComplete){
            lv =>

            lv.selection.intervalMode = IntervalMode.Single

            listenTo(lv.selection)

            val pos = caretPosState.get.ensuring(_ != -1)
            reactions += {
              case ListSelectionChanged(`lv`, rng, live) =>
                onCompletionSelection(lv.listData(rng.head), pos)
            }
          }
        ){
          horizontalScrollBarPolicy = BarPolicy.Never
          maximumSize = 1000 -> maxCompletionListHeight
        }
    }

    trait ScalaCompletion extends CompletionPopupBase{
      pane: AbstractCompletionEditorPane =>

      protected val continueCompletionKeys = Keys.alphanumericSet + Key.Period + Key.BackSpace

      def completionPopupExtraPF: PartialFunction[Event, Boolean] = {
        case MouseClicked(_, p, mod, n, _) => true
        case KeyPressed(_, k, 0, _) =>
          if(completing && continueCompletionKeys.contains(k)){
            complete(currentVerbosity, popup(caret.dot, _, currentVerbosity))
            false
          }
          else true
      }
      
      override def clearPopupsPF = completionPopupExtraPF orElse super.clearPopupsPF
    }
  }
}


class CompletionListEditorPane(val completion: TextCompletion)
  extends AbstractCompletionEditorPane with CompletionPopup.ScrollingList with ListeningKeysPressed with ScalaCompletion
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
