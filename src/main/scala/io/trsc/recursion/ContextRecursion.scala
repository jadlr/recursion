package io.trsc.recursion

import matryoshka.implicits._
import matryoshka.patterns.{CoEnv, EnvT}
import matryoshka._
import scalaz.Scalaz._
import scalaz._

object ContextRecursion extends App {

  // Non-recursive data type that is the base for usage with recursion schemes
  sealed trait Expression[A]
  case class Query[A](query: String) extends Expression[A]
  case class And[A](subQueries: List[A]) extends Expression[A]
  case class Or[A](subQueries: List[A]) extends Expression[A]
  case class Not[A](subQueries: List[A]) extends Expression[A]
  case class Filter[A](subQueries: List[A]) extends Expression[A]


  // Needed to flip the context when recursing
  implicit val exprTraverse: Traverse[Expression] = new Traverse[Expression] {
    def traverseImpl[G[_] : Applicative, A, B](fa: Expression[A])(f: A ⇒ G[B]): G[Expression[B]] =  fa match {
      case Query(query)       ⇒ Applicative[G].point(Query(query))
      case And(subQueries)    ⇒ subQueries.traverse(f).map(And(_))
      case Or(subQueries)     ⇒ subQueries.traverse(f).map(Or(_))
      case Not(subQueries)    ⇒ subQueries.traverse(f).map(Not(_))
      case Filter(subQueries) ⇒ subQueries.traverse(f).map(Filter(_))
    }
  }

  sealed trait JsonExpression
  case class JsonQuery(query: String) extends JsonExpression
  case class JsonAnd(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonOr(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonNot(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonFilter(subQueries: List[JsonExpression]) extends JsonExpression

  sealed trait EsExpression
  case class EsQuery(query: String) extends EsExpression
  case class EsAnd(subQueries: List[EsExpression]) extends EsExpression
  case class EsOr(subQueries: List[EsExpression]) extends EsExpression
  case class EsNot(subQueries: List[EsExpression]) extends EsExpression
  case class EsFilter(subQueries: List[EsExpression]) extends EsExpression

  val coalgJson: Coalgebra[CoEnv[String, Expression, ?], JsonExpression] = {
    case JsonQuery(str) if str.startsWith("err") ⇒ CoEnv(-\/(s"field error: [$str]"))
    case JsonQuery(str)                          ⇒ CoEnv(\/-(Query(str)))
    case JsonAnd(expressions)                    ⇒ CoEnv(\/-(And(expressions)))
    case JsonOr(expressions)                     ⇒ CoEnv(\/-(Or(expressions)))
    case JsonNot(expressions)                    ⇒ CoEnv(\/-(Not(expressions)))
    case JsonFilter(expressions)                 ⇒ CoEnv(\/-(Filter(expressions)))
  }

  val algErrors: Algebra[CoEnv[String, Expression, ?], List[String]] = {
    case CoEnv(-\/(err))          ⇒ err :: Nil
    case CoEnv(\/-(Query(_)))     ⇒ Nil
    case CoEnv(\/-(And(errs)))    ⇒ errs.flatten
    case CoEnv(\/-(Or(errs)))     ⇒ errs.flatten
    case CoEnv(\/-(Not(errs)))    ⇒ errs.flatten
    case CoEnv(\/-(Filter(errs))) ⇒ errs.flatten
  }

  val algEs: Algebra[CoEnv[String, Expression, ?], Option[EsExpression]] = {
    case CoEnv(-\/(_))            ⇒ none
    case CoEnv(\/-(Query(str)))   ⇒ EsQuery(str).some
    case CoEnv(\/-(And(errs)))    ⇒ errs.sequence.map(EsAnd)
    case CoEnv(\/-(Or(errs)))     ⇒ errs.sequence.map(EsOr)
    case CoEnv(\/-(Not(errs)))    ⇒ errs.sequence.map(EsNot)
    case CoEnv(\/-(Filter(errs))) ⇒ errs.sequence.map(EsFilter)
  }

  val coalgJson2: Coalgebra[EnvT[List[String], Expression, ?], JsonExpression] = {
    case JsonQuery(str) if str.startsWith("err") ⇒ EnvT((s"field error: [$str]" :: Nil, Query("one")))
    case JsonQuery(str)                          ⇒ EnvT((Nil, Query(str)))
    case JsonAnd(expressions)                    ⇒ EnvT((Nil, And(expressions)))
    case JsonOr(expressions)                     ⇒ EnvT((Nil, Or(expressions)))
    case JsonNot(expressions)                    ⇒ EnvT((Nil, Not(expressions)))
    case JsonFilter(expressions)                 ⇒ EnvT((Nil, Filter(expressions)))
  }

  val alg2: Algebra[EnvT[List[String], Expression, ?], ValidationNel[String, EsExpression]] = {
    case EnvT((errs, Query(str))) ⇒
      if (errs.nonEmpty)
        NonEmptyList(errs.head, errs.tail:_*).failure
      else
        EsQuery(str).successNel
    case EnvT((errs, And(expressions)))    ⇒ expressions.sequence.bimap(_ :::> errs.toIList, EsAnd)
    case EnvT((errs, Or(expressions)))     ⇒ expressions.sequence.bimap(_ :::> errs.toIList, EsOr)
    case EnvT((errs, Not(expressions)))    ⇒ expressions.sequence.bimap(_ :::> errs.toIList, EsNot)
    case EnvT((errs, Filter(expressions))) ⇒ expressions.sequence.bimap(_ :::> errs.toIList, EsFilter)
  }

  // Example:
  def badExpression: JsonExpression =
    JsonAnd(List(
      JsonQuery("name=Albert"),
      JsonQuery("errage=102"),
      JsonOr(List(
        JsonQuery("size=XXL"),
        JsonFilter(List(
          JsonQuery("errcolor=red")
        )),
        JsonAnd(List(
          JsonQuery("errram=512mb"),
          JsonQuery("cpu=4ghz")
        ))
      ))
    ))

  def goodExpression: JsonExpression =
    JsonAnd(List(
      JsonQuery("name=Albert"),
      JsonQuery("age=102"),
      JsonOr(List(
        JsonQuery("size=XXL"),
        JsonFilter(List(
          JsonQuery("color=red")
        )),
        JsonAnd(List(
          JsonQuery("ram=512mb"),
          JsonQuery("cpu=4ghz")
        ))
      ))
    ))

  val zipped = AlgebraZip[CoEnv[String, Expression, ?]].zip(algErrors, algEs)

  val (errors, _): (List[String], Option[EsExpression]) = badExpression.hylo(zipped, coalgJson)
  println(errors.mkString(", "))

  val (_, es): (List[String], Option[EsExpression]) = goodExpression.hylo(zipped, coalgJson)
  println(es)

  // EnvT solution
  val badResult = badExpression.hylo(alg2, coalgJson2)
  println(badResult)

  val goodResult = goodExpression.hylo(alg2, coalgJson2)
  println(goodResult)

}
