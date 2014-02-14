package feh.util.swing


object Keys {
  import scala.swing.event.Key._

  lazy val letters = A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: M :: N :: O :: P :: Q :: R :: S :: T ::
    U :: V :: W :: X :: Y :: Z :: Nil

  lazy val numbers = Key0 :: Key1 :: Key2 :: Key3 :: Key4 :: Key5 :: Key6 :: Key7 :: Key8 :: Key9 :: Key0 :: Nil

  lazy val alphanumeric = letters ::: numbers
  lazy val alphanumericSet = alphanumeric.toSet
}
