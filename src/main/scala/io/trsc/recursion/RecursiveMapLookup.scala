package io.trsc.recursion

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import scalaz.Scalaz._
import scalaz._

object RecursiveMapLookup extends App {

  sealed trait Expr[A]
  case class Leaf[A](s: String) extends Expr[A]
  case class Lookup[A](m: Map[String, A]) extends Expr[A]

  implicit val exprTraverse: Traverse[Expr] = new Traverse[Expr] {
    def traverseImpl[G[_] : Applicative, A, B](fa: Expr[A])(f: A ⇒ G[B]): G[Expr[B]] =  fa match {
      case Leaf(s)   ⇒ Applicative[G].point(Leaf(s))
      case Lookup(m) ⇒ m.traverse(a ⇒ f(a)).map(Lookup(_))
    }
  }

  def schema[T](implicit T: Corecursive.Aux[T, Expr]): T =
    Lookup[T](Map(
      "a" → Leaf[T]("val_a").embed,
      "b" → Leaf[T]("val_b").embed,
      "c" → Lookup[T](Map(
        "a1" → Leaf[T]("val_a1").embed,
        "b1" → Lookup[T](Map(
          "a2" → Leaf[T]("val_a2").embed
        )).embed
      )).embed
    )).embed


  val find: Algebra[Expr, List[String] ⇒ Option[String]] = {
    case Lookup(m) ⇒ {
      case Nil    ⇒ None
      case h :: t ⇒ m.get(h).flatMap(f ⇒ f(t))
    }
    case Leaf(s) ⇒ {
      case Nil ⇒ s.some
      case _   ⇒ none
    }
  }

  val extract: List[String] ⇒ Option[String] = schema[Fix[Expr]].cata(find)

  println(extract(List("a")))
  println(extract(List("c", "a1")))
  println(extract(List("c", "b1", "a2")))
  println(extract(List("c", "b1", "a2", "xxx")))
  println(extract(List("c", "xxx", "a2")))
}
