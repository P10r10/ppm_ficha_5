package Ficha5

import Ficha5.RegimeOPT.RegimeOPT
import Ficha5.Turma._

case class Turma(id: String, alunos: Alunos) {
  def trabs() = Turma.trabs2(this)
  def searchStudent() = Turma.searchStudent(this)
}

object Turma {
  type Nome = String
  type Numero = Int
  type NT = Option[Float]
  type NP = Option[Float]
  type Regime = RegimeOPT
  type Aluno = (Numero, Nome, Regime, NT, NP)
  type Alunos = List[Aluno]

  //rec. standard
  def trabs1(t: Turma): Alunos = {
    def aux(lst: Alunos): Alunos = lst match {
      case Nil => Nil
      case (x :: xs) => if (x._3 == RegimeOPT.TrabEstud) x :: aux(xs) else aux(xs)
    }

    aux(t.alunos)
  }

  //rec. implícita
  def trabs2(t: Turma): Alunos = t.alunos filter (x => x._3 == RegimeOPT.TrabEstud)

  //prof
  def searchStudent(numero: Numero, alunos: Alunos):Option[Aluno] = {
    alunos.foldRight(None:Option[Aluno])((h,t) => if(h._1 == numero) Some(h) else t)
  }

//    alunos.foldRight(None:Option[Aluno])  ((h, t) = if(h._1 == numero) Some(h) else t)

  //rec. standard
//  def searchStudent(turma: Turma, num: Int):Option[Aluno] = {
//    def search(al: Turma.Alunos, num: Int):Aluno = al match {
//      case Nil => None
//      case h :: t => if (h._1 == num) h :: searchStudent(t, num)
//    }
//  }

}
