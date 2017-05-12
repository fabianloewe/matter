package de.hyronx.matter

object ErrorHandler {
  // TODO: Define top-level error type
  def apply(error: Any, in: String, file: String) = {
    println()
    println(s"[${Console.RED}error${Console.WHITE}] $in in file: ${Console.CYAN}$file${Console.WHITE}")
    println(s"[${Console.RED}error${Console.WHITE}] $error")
    println()
  }
}
