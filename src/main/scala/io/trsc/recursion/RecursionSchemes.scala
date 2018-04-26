package io.trsc.recursion

import matryoshka.data.Fix
import matryoshka.implicits._
import matryoshka.{Algebra, _}
import scalaz.Scalaz._
import scalaz._

object RecursionSchemes extends App {

  sealed trait JsonExpression
  case class JsonMatch(query: String) extends JsonExpression
  case class JsonTerm(query: String) extends JsonExpression
  case class JsonOr(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonAnd(subQueries: List[JsonExpression]) extends JsonExpression


  sealed trait Expression[A]
  case class Noop[A](err: String) extends Expression[A]
  case class Fulltext[A](query: String) extends Expression[A]
  case class Exact[A](query: String) extends Expression[A]
  case class And[A](subQueries: List[A]) extends Expression[A]
  case class Or[A](subQueries: List[A]) extends Expression[A]

  object Expression {
    implicit val expressionTraverse: Traverse[Expression] = new Traverse[Expression] {
      def traverseImpl[G[_] : Applicative, A, B](fa: Expression[A])(f: A ⇒ G[B]): G[Expression[B]] = fa match {
        case Fulltext(query)  ⇒ Applicative[G].point(Fulltext(query))
        case Exact(query)     ⇒ Applicative[G].point(Exact(query))
        case And(expressions) ⇒ expressions.traverse(f).map(And(_))
        case Or(expressions)  ⇒ expressions.traverse(f).map(Or(_))
        case Noop(err)        ⇒ Applicative[G].point(Noop(err))
      }
    }
  }

  // Anamorphism - a simple unfold - e.g. from a JSON representation to the transitional format
  val coalgebra: Coalgebra[Expression, JsonExpression] = {
    case JsonMatch(query)     ⇒ Fulltext(query)
    case JsonTerm(query)      ⇒ Exact(query)
    case JsonAnd(expressions) ⇒ And(expressions)
    case JsonOr(expressions)  ⇒ Or(expressions)
  }

  val in: JsonExpression = JsonAnd(JsonMatch("name=albert") :: JsonTerm("age=63") :: Nil)


  val expression: Fix[Expression] = in.ana[Fix[Expression]](coalgebra)

  // Fix(And(List(Fix(Fulltext(name=albert)), Fix(Exact(age=63)))))
  println(expression)

  // Catamorphism - a simple fold - e.g. from the transitional format to String
  val algebra: Algebra[Expression, String] = {
    case Noop(_)          ⇒ ""
    case Fulltext(query)  ⇒ s"match: $query"
    case Exact(query)     ⇒ s"term: $query"
    case And(expressions) ⇒ s"(${expressions.mkString(" and ")})"
    case Or(expressions)  ⇒ s"(${expressions.mkString(" or ")})"
  }

  // (match: name=albert and term: age=63)
  println(expression.cata(algebra))

  // Hylomorphism - the combination of an anamorphism and a catamorphism

  // (match: name=albert and term: age=63)
  println(in.hylo(algebra, coalgebra))


  // Apomorphism - unfold with control over when to terminate
  val gcoalgebra: GCoalgebra[Fix[Expression] \/ ?, Expression, JsonExpression] = {
    case JsonMatch(query)     ⇒ Fulltext(query)
    case JsonTerm(query)      ⇒ Exact(query)
    // Doesn't make too much sense here, but you can terminate early by returning a left with an arbitrary `Fix[Expression]`
    case JsonAnd(expressions) ⇒ And(expressions.map {
      case JsonOr(_)  ⇒ Noop[Fix[Expression]]("too deep").embed.left
      case JsonAnd(_) ⇒ Noop[Fix[Expression]]("too deep").embed.left
      case otherwise  ⇒ otherwise.right
    })
    case JsonOr(expressions)  ⇒ Or(expressions.map {
      case JsonOr(_)  ⇒ Noop[Fix[Expression]]("too deep").embed.left
      case JsonAnd(_) ⇒ Noop[Fix[Expression]]("too deep").embed.left
      case otherwise  ⇒ otherwise.right
    })
  }

  val in2: JsonExpression = JsonAnd(
    JsonMatch("name=albert") ::
    JsonTerm("age=63") ::
    JsonOr(JsonMatch("nickname=smarty") :: JsonMatch("nickname=dumbo") :: Nil) ::
    Nil)

  // Fix(And(List(Fix(Fulltext(name=albert)), Fix(Exact(age=63)), Fix(Noop(too deep)))))
  println(in2.apo[Fix[Expression]](gcoalgebra))


  def size[F[_]: Foldable]: Algebra[F, Int] = _.foldRight(1)(_ + _)

  // Paramorphism - fold with access to the structure that generated a value
  val galgebra: GAlgebra[(Fix[Expression], ?), Expression, String] = {
    case Noop(_)          ⇒ ""
    case Fulltext(query)  ⇒ s"match: $query"
    case Exact(query)     ⇒ s"term: $query"
    case And(expressions) ⇒ s"(${expressions.map {
      case (previous, str) ⇒
        // previous has type `Fix[Expression]` and contains everything that was used to calculate `str`
        val nodeCount = previous.cata(size)
        s"($str [calculated using $nodeCount node(s)])"
    }.mkString(" and ")})"
    case Or(expressions)  ⇒ s"(${expressions.map {
      case (previous, str) ⇒
        val nodeCount = previous.cata(size)
        s"($str [calculated using $nodeCount node(s)])"
    }.mkString(" or ")})"
  }

  // ((match: name=albert [calculated using 1 node(s)]) and (term: age=63 [calculated using 1 node(s)]) and (((match: nickname=smarty [calculated using 1 node(s)]) or (match: nickname=dumbo [calculated using 1 node(s)])) [calculated using 3 node(s)]))
  println(in2.ana[Fix[Expression]](coalgebra).para(galgebra))

  // Histomorphism - fold with access to the structure that generated a value and also all previously generated values
  val cofreeGalgebra: GAlgebra[Cofree[Expression, ?], Expression, String] = {
    case Noop(_)          ⇒ ""
    case Fulltext(query)  ⇒ s"match: $query"
    case Exact(query)     ⇒ s"term: $query"
    case And(expressions) ⇒ s"(${expressions.map {
      // In case of a leaf `t` will just be that leaf
      // In case of a branch `t` is that branch (e.g. Or) that contains a `Cofree`
      // that contains all values needed to calculate the value in the tail and the calculated value in the head
      case Cofree(h, t) ⇒ h
    }.mkString(" and ")})"
    case Or(expressions) ⇒ s"(${expressions.map {
      case Cofree(h, t) ⇒ h
    }.mkString(" and ")})"
  }

  println(in2.ana[Fix[Expression]](coalgebra).histo(cofreeGalgebra))

  // Dynamorphism - the combination of an anamorphism and a histomorphism
  println(in2.dyna(cofreeGalgebra, coalgebra))

  // Futumorphism - unfold with the ability to emit multiple steps at once
  val freeGcoalgebra: GCoalgebra[Free[Expression, ?], Expression, JsonExpression] = {
    case JsonMatch(query)     ⇒ Fulltext(query)
    case JsonTerm(query)      ⇒ Exact(query)
    // Free.roll to emit multiple steps
    // Free.point to continue regular recursion
    case JsonAnd(expressions) ⇒ And(expressions.map {
      case JsonMatch("complex") ⇒ Free.roll(Or[Free[Expression, JsonExpression]](
        Free.pure[Expression, JsonExpression](JsonMatch("splitup1")) ::
        Free.pure[Expression, JsonExpression](JsonMatch("splitup2")) ::
        Nil
      ))
      case otherwise ⇒ Free.pure[Expression, JsonExpression](otherwise) }
    )
    case JsonOr(expressions)  ⇒ Or(expressions.map {
      case JsonMatch("complex") ⇒ Free.roll(Or[Free[Expression, JsonExpression]](
        Free.pure[Expression, JsonExpression](JsonMatch("splitup1")) ::
        Free.pure[Expression, JsonExpression](JsonMatch("splitup2")) ::
        Nil
      ))
      case otherwise ⇒ Free.pure[Expression, JsonExpression](otherwise) }
    )
  }

  val in3: JsonExpression = JsonAnd(
    JsonMatch("name=albert") ::
      JsonTerm("age=63") ::
      JsonOr(JsonMatch("complex") :: JsonMatch("nickname=dumbo") :: Nil) ::
      Nil)

  // Fix(And(List(Fix(Fulltext(name=albert)), Fix(Exact(age=63)), Fix(Or(List(Fix(Or(List(Fix(Fulltext(splitup1)), Fix(Fulltext(splitup2))))), Fix(Fulltext(nickname=dumbo))))))))
  println(in3.futu[Fix[Expression]](freeGcoalgebra))

  // TODO cover all elgot algebra versions
}
