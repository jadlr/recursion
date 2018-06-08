package io.trsc.recursion

import fastparse.all._
import matryoshka._
import matryoshka.implicits._
import matryoshka.data.Fix
import scalaz.Scalaz._
import scalaz._

object Calculator extends App {

  sealed trait Calculus[A]
  case class Value[A](v: Int) extends Calculus[A]
  case class Plus[A](x: A, y: A) extends Calculus[A]
  case class Minus[A](x: A, y: A) extends Calculus[A]

  implicit val exprTraverse: Traverse[Calculus] = new Traverse[Calculus] {
    def traverseImpl[G[_] : Applicative, A, B](fa: Calculus[A])(f: A ⇒ G[B]): G[Calculus[B]] =  fa match {
      case Value(query) ⇒ Applicative[G].point(Value(query))
      case Plus(x, y)   ⇒ (f(x) ⊛ f(y)) { (x, y) ⇒ Plus(x, y) }
      case Minus(x, y)  ⇒ (f(x) ⊛ f(y)) { (x, y) ⇒ Minus(x, y) }
    }
  }

  def value(i: Int): Fix[Calculus] = Value[Fix[Calculus]](i).embed
  def plus(x: Fix[Calculus], y: Fix[Calculus]): Fix[Calculus] = Plus[Fix[Calculus]](x, y).embed
  def minus(x: Fix[Calculus], y: Fix[Calculus]): Fix[Calculus] = Minus[Fix[Calculus]](x, y).embed


  val number: P[Fix[Calculus]] = P( CharIn('0'to'9').rep(1).!.map(str ⇒ value(str.toInt)) )
  val addSub: P[Fix[Calculus]] = P( number ~ (CharIn("+-").! ~/ number).rep ).map { case (x, y) ⇒
    y.foldRight(x) {
      case (("-", subtrahend), acc) ⇒ minus(acc, subtrahend)
      case (("+", summand), acc)    ⇒ plus(acc, summand)
    }
  }
  val expr: P[Fix[Calculus]]   = P( addSub ~ End )

  val calculus: Algebra[Calculus, Int] = {
    case Value(i)    ⇒ i
    case Plus(x, y)  ⇒ x + y
    case Minus(x, y) ⇒ x - y
  }

  def calc(string: String): Option[Int] =
    expr.parse(string) match {
      case Parsed.Success(input, _) ⇒ input.cata(calculus).some
      case Parsed.Failure(_, _, _)  ⇒ none
    }

  println(calc("1+2+7-11+3"))
}
