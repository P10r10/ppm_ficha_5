package Ficha5

object Main {
  def main(args: Array[String]): Unit = {
    val t: Turma = Turma("id22", List((11, "Antonio", RegimeOPT.Ordinario, Some(8),
      Some(20)), (12, "Jose", RegimeOPT.TrabEstud, Some(13), None)))
      println("Workers-students: " + t.trabs())
    println()
  }
}