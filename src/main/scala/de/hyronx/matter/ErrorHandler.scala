package de.hyronx.matter

object ErrorHandler {
  // TODO: Define top-level error type
  def apply(error: Any, in: String, file: Option[String] = None) = {
    val errorSign = s"[${Console.RED}error${Console.WHITE}]"
    val output =
      s"$errorSign\n$errorSign ${Console.UNDERLINED}During${Console.RESET}: $in\n" +
        (file match {
          case Some(problematicFile) ⇒ s"$errorSign ${Console.UNDERLINED}In${Console.RESET}: $problematicFile\n"
          case None                  ⇒ ""
        }) +
        s"$errorSign ${Console.UNDERLINED}With${Console.RESET}: " +
        (error match {
          case s: String ⇒ s
          case e: Exception ⇒ e.toString +
            s"\n$errorSign ${Console.UNDERLINED}At${Console.RESET}: " +
            e.getStackTrace.mkString(s"\n$errorSign  at ")
          case other ⇒ other.toString()
        })

    println(output)
  }
}
