final case class Box[A](value: A)

object Box {
  implicit def boxPrintable[A: Printable]: Printable[Box[A]] =
    Printable[A].contramap(_.value)

  implicit def boxCodec[A: Codec]: Codec[Box[A]] =
    Codec[A].imap(Box(_), _.value)
}
