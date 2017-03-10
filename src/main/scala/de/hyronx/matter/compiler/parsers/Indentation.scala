package de.hyronx.matter.compiler.parsers;

import fastparse.all._

import de.hyronx.matter.compiler.ast._;

class Indentation(val indent: Int = 0) {
  import Indentation._

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
  def apply(parent: MatterTypeTree, indent: Int = 0): Indentation = {
    if (parent.parent != parent) {
      Indentation(parent.parent, indent + 2)
    } else {
      new Indentation(indent)
    }
  }

  // Whitespace definition
  val ws = " "
  // New line
  val nl = "\n"

  val count: P[Indentation] = {
    P("\n" ~ ws.rep.!).map { x ⇒ Indentation(x.length) }
  }
}
