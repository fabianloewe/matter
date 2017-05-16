package de.hyronx.matter

object ErrorHandler {
  // TODO: Define top-level error type
  def apply(error: Any, in: String, file: Option[String] = None) = {
    val errorSign = s"[${Console.RED}error${Console.WHITE}]"
    val output =
      s"$errorSign ${Console.UNDERLINED}During${Console.RESET}: $in\n" +
        (file match {
          case Some(problematicFile) ⇒ s"$errorSign ${Console.UNDERLINED}In${Console.RESET}: $problematicFile\n"
          case None                  ⇒ ""
        }) +
        s"$errorSign ${Console.UNDERLINED}With${Console.RESET}: $error"

    println(output)
  }
}
