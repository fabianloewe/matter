package de.hyronx.matter.compiler.parsers;

import fastparse.all._

class Indentation(val indent: Int = 0) {
  import Indentation._

  val same = P(nl.rep(1) ~ ws.rep(exactly = indent).!) map { x ⇒
    println(s"Same indent: ${x.length}")
    this
  }
  val deeper = P(nl ~ ws.rep(exactly = indent + 2).!) map { x ⇒
    println(s"Deeper indent: ${x.length}")
    Indentation(x.length)
  }
  def upper(repeat: Int = 1) = P(nl ~ ws.rep(max = indent - 2).!) map { x ⇒
    println(s"Upper indent: ${x.length}")
    Indentation(x.length)
  }
}

object Indentation {
  def apply(indent: Int) = new Indentation(indent)
  def apply(indent: Indentation) = new Indentation(indent.indent + 2)

  // Whitespace definition
  val ws = " "
  // New line
  val nl = "\n"

  val count: P[Indentation] = {
    P(nl ~ ws.rep.!).map { x ⇒ Indentation(x.length) }
  }
}
