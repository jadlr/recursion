package io.trsc.recursion

import matryoshka.data.Fix
import matryoshka.implicits._
import matryoshka._
import scalaz.Scalaz._
import scalaz._
object AccumulateErrorsInCoalgebra extends App {

  sealed trait JsonExpression
  case class JsonTerm(query: String) extends JsonExpression
  case class JsonOr(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonAnd(subQueries: List[JsonExpression]) extends JsonExpression


  sealed trait Expression[A]
  case class Exact[A](query: String) extends Expression[A]
  case class And[A](subQueries: List[A]) extends Expression[A]
  case class Or[A](subQueries: List[A]) extends Expression[A]

  object Expression {
    implicit val expressionTraverse: Traverse[Expression] = new Traverse[Expression] {
      def traverseImpl[G[_] : Applicative, A, B](fa: Expression[A])(f: A ⇒ G[B]): G[Expression[B]] = fa match {
        case Exact(query)     ⇒ Applicative[G].point(Exact(query))
        case And(expressions) ⇒ expressions.traverse(f).map(And(_))
        case Or(expressions)  ⇒ expressions.traverse(f).map(Or(_))
      }
    }
  }

  val coalg: CoalgebraM[State[List[String], ?], Expression, JsonExpression] = {
    case JsonTerm(query) if query.startsWith("err") ⇒ State { err ⇒ ("invalid query" :: err, Exact(query)) }
    case JsonTerm(query) if query.startsWith("bug") ⇒ State { err ⇒ ("something went wrong" :: err, Exact(query)) }
    case JsonTerm(query)                            ⇒ State { err ⇒ (err, Exact(query)) }
    case JsonAnd(expressions)                       ⇒ State { err ⇒ (err, And(expressions)) }
    case JsonOr(expressions)                        ⇒ State { err ⇒ (err, Or(expressions)) }
  }

  val input: JsonExpression = JsonAnd(
    JsonTerm("name=albert") ::
      JsonTerm("age=63") ::
      JsonOr(
        JsonTerm("job=cool") ::
          JsonTerm("error") ::
          JsonAnd(
            JsonTerm("house=small") ::
            JsonTerm("bugtest=gree") :: Nil
          ) :: Nil
      ) :: Nil
  )

  val output = input.anaM[Fix[Expression]](coalg)

  val result: (List[String], Fix[Expression]) = output.run(Nil)
  println(result)


}
