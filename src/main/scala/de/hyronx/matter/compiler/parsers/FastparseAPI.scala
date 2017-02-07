package de.hyronx.matter.compiler.parsers

import fastparse.core._
import fastparse.utils._

import de.hyronx.matter.compiler.tokens._

implicit object TokenSetHelper extends ElemSetHelper[Token] {
  def toInt(a: Token): Int = 0
  def ordering = Ordering.by[Token, Token](x ⇒ x)
  def toLowerCase(in: Token) = in
  val allValues = Char.MinValue to Char.MaxValue
}

object TokenReprOps extends ReprOps[Token, List[Token]] {
  def apply(input: List[Token], i: Int) = input(i)
  def slice(input: List[Token], start: Int, end: Int) = input.slice(start, end)
  def length(input: List[Token]) = input.length

  def fromArray(input: Array[Token]): List[Token] = input.toList
  def fromSeq(input: Seq[Token]): List[Token] = input.toList
  def fromSingle(input: Token): List[Token] = List(input)
  def toArray(input: List[Token]): Array[Token] = input.toArray
  def flatten(input: Seq[Token]): List[Token] = input.toList

  def prettyPrint(input: List[Token]): String = input
  def literalize(input: List[Token]): String = Utils.literalize(input)
  def errorMessage(input: ParserInput[Token, List[Token]], expected: List[Token], idx: Int): String = {
    val locationCode = {
      val first = input.slice(idx - 20, idx)
      val last = input.slice(idx, idx + 20)
      val emptyString = ""
      val lastSnippet: String = last.lines.toSeq.headOption.getOrElse(emptyString)
      val firstSnippet: String = first.reverse.lines.toSeq.headOption.getOrElse(emptyString).reverse

      prettyPrint(firstSnippet) + prettyPrint(lastSnippet) + "\n" + (" " * firstSnippet.length) + "^"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
    //TODO Probably we could avoid code duplication by creating only method `locationCode`
    //TODO but it reduces the abstraction
  }

  def prettyIndex(input: ParserInput[Token, List[Token]], index: Int): String = {
    input match {
      case IndexedParserInput(data) ⇒
        var line = 1
        var col = 1
        var i = 0
        while (i < index) {
          if (data(i) == '\n') {
            col = 1
            line += 1
          } else {
            col += 1
          }
          i += 1
        }
        s"$line:$col"
      case _ ⇒ String.valueOf(index)
    }
  }

}

class FastparseAPI[+T](self: Parser[T, Token, List[Token]])(implicit repr: ReprOps[Token, List[Token]])
    extends ParserApi[T, Token, List[Token]] {
  type Elem = Token
  type Repr = List[Token]

  def log(msg: String = self.toString)(implicit output: Logger) = Logged(self, msg, output.f)

  def opaque(msg: String = self.toString) = Opaque(self, msg)

  def rep[R](implicit ev: Repeater[T, R]): Parser[R, Elem, Repr] =
    Repeat(self, 0, Int.MaxValue, Pass[Elem, Repr])
  def rep[R](min: Int = 0, sep: Parser[_, Elem, Repr] = Pass[Elem, Repr],
    max: Int = Int.MaxValue, exactly: Int = -1)(implicit ev: Repeater[T, R]): Parser[R, Elem, Repr] = {
    if (exactly < 0)
      Repeat(self, min, max, sep)
    else
      Repeat(self, exactly, exactly, sep)
  }

  def |[V >: T](p: Parser[V, Elem, Repr]): Parser[V, Elem, Repr] =
    Either[V, Elem, Repr](Either.flatten(Vector(self, p)): _*)

  def ~[V, R](p: Parser[V, Elem, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, Elem, Repr] =
    Sequence.flatten(Sequence(self, p, cut = false).asInstanceOf[Sequence[R, R, R, Elem, Repr]])
  def ~/[V, R](p: Parser[V, Elem, Repr])(implicit ev: Sequencer[T, V, R]): Parser[R, Elem, Repr] =
    Sequence.flatten(Sequence(self, p, cut = true).asInstanceOf[Sequence[R, R, R, Elem, Repr]])

  def ?[R](implicit ev: Optioner[T, R]): Parser[R, Elem, Repr] = Optional(self)

  def unary_! : Parser[Unit, Elem, Repr] = Not(self)

  def ~/ : Parser[T, Elem, Repr] = Cut[T, Elem, Repr](self)

  def ! : Parser[Repr, Elem, Repr] = Capturing(self)

  def map[V](f: T ⇒ V): Parser[V, Elem, Repr] = Mapper(self, f)

  def flatMap[V](f: T ⇒ Parser[V, Elem, Repr]): Parser[V, Elem, Repr] = FlatMapped(self, f)

  def filter(predicate: T ⇒ Boolean): Parser[T, Elem, Repr] = Filtered(self, predicate)
}
