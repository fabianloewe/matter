package de.hyronx.matter.compiler.parsers

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast._

trait BaseParser {
  // Whitespace definition
  val ws = " "
  // New line
  val nl = "\n"
  val ignoreWhitespaces = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(ws.rep(1))
  }
  import fastparse.all._

  class Indentation(val indent: Int = 0) {
    val same = P(nl.rep(1) ~ ws.rep(exactly = indent).!).log() map { x ⇒
      println(s"Same indent: ${x.length}")
      this
    }
    val deeper = P(nl ~ ws.rep(exactly = indent + 2).!).log() map { x ⇒
      println(s"Deeper indent: ${x.length}")
      Indentation(x.length)
    }
    def upper(repeat: Int = 1) = P(nl ~ ws.rep(max = indent - 2).!).log() map { x ⇒
      println(s"Upper indent: ${x.length}")
      Indentation(x.length)
    }
  }
  object Indentation {
    def apply(indent: Int) = new Indentation(indent)
    def apply(indent: Indentation) = new Indentation(indent.indent + 2)
    def apply(parent: ContainerTree, indent: Int = 0): Indentation = {
      if (parent.parent != parent) {
        Indentation(parent.parent, indent + 2)
      } else {
        new Indentation(indent)
      }
    }

    val count: P[Indentation] = {
      P("\n" ~ ws.rep.!).map { x ⇒ Indentation(x.length) }
    }
  }
  def deeper(indent: Option[Indentation], parent: ContainerTree) = {
    indent.getOrElse(Indentation(parent)).deeper map { x ⇒
      println(s"Deeper indent: ${x.indent}")
      x
    }
  }

  def same(indent: Option[Indentation], parent: ContainerTree) = {
    indent.getOrElse(Indentation(parent)).same map { x ⇒
      println(s"Same indent: ${x.indent}")
      x
    }
  }

  val letter = P(lowercase | uppercase)
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val digit = P(CharIn('0' to '9'))

  val variable = P(lowercase ~ (lowercase | digit | "-").rep).!
  val identifier = P(uppercase ~ (letter | digit | "-").rep).!
  val literal = P("\"" ~/ CharsWhile(_ != "\"").!)
  val number = P(digit.rep(1)).!.map(_.toInt)

  val scopedIdentifier: P[Identifier] = {
    P(identifier.!.rep(min = 1, sep = ".")) map { seq ⇒
      Identifier(seq.last, seq.dropRight(1).toList)
    }
  }
}
