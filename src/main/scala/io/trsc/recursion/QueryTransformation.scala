package io.trsc.recursion

import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import scalaz.Scalaz._
import scalaz._

import scala.concurrent.Future

object QueryTransformation extends App {


  /**
    * SOURCE:
    * {
    *   "or": [
    *     { "query": "expr" },
    *     {
    *       "and": [
    *         { "query": "expr2" },
    *         { "query": "expr3" }
    *       ]
    *     },
    *     {
    *       "not: [
    *         { "query": "expr4" }
    *       ]
    *     }
    *   ]
    * }
    */
  sealed trait JsonExpression
  case class JsonQuery(query: String) extends JsonExpression
  case class JsonAnd(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonOr(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonNot(subQueries: List[JsonExpression]) extends JsonExpression
  case class JsonFilter(subQueries: List[JsonExpression]) extends JsonExpression

  /**
    * TARGET:
    * {
    *   "bool": {
    *     "should": [
    *       { "query": "expr" },
    *       {
    *         "bool": {
    *           "must": [
    *             { "query": "expr2" },
    *             { "query": "expr3" }
    *           ],
    *           "must_not": [
    *             { "query": "expr4" }
    *           ]
    *         }
    *       }
    *     ]
    *   }
    * }
    */
  sealed trait EsExpression
  case class EsQuery(query: String) extends EsExpression
  case class BoolExpression(must: Option[List[EsExpression]] = None,
                            should: Option[List[EsExpression]] = None,
                            mustNot: Option[List[EsExpression]] = None,
                            filter: Option[List[EsExpression]] = None) extends EsExpression


  // Non-recursive data type that is the base for usage with recursion schemes
  sealed trait Expression[A]
  case class Query[A](query: String) extends Expression[A]
  case class And[A](subQueries: List[A]) extends Expression[A]
  case class Or[A](subQueries: List[A]) extends Expression[A]
  case class Not[A](subQueries: List[A]) extends Expression[A]
  case class Filter[A](subQueries: List[A]) extends Expression[A]

  implicit def showExpression[A]: Show[Expression[A]] = new Show[Expression[A]] {
    override def shows(f: Expression[A]): String = f match {
      case Query(query)       ⇒ query
      case And(subQueries)    ⇒ s"(${subQueries.mkString(" and ")})"
      case Or(subQueries)     ⇒ s"(${subQueries.mkString(" or ")})"
      case Not(subQueries)    ⇒ s"(not [${subQueries.mkString(",")}])"
      case Filter(subQueries) ⇒ s"(filter [${subQueries.mkString(" and ")}])"
    }
  }

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


  // Needed to run algebras at the same time
  implicit def AlgebraMZip[M[_]: Monad, F[_]: Functor]: Zip[AlgebraM[M, F, ?]] =
    new Zip[AlgebraM[M, F, ?]] {
      // def zip[X, Y](a: => B[X], b: => B[Y]): B[(X, Y)]
      def zip[A, B](a: ⇒ F[A] ⇒ M[A], b: ⇒ F[B] ⇒ M[B]): F[(A, B)] ⇒ M[(A, B)] =
        node ⇒ a(node.map(_._1)).flatMap(_a ⇒ b(node.map(_._2)).map((_a, _)))
    }

  // Input format JSON: JsonExpression => Expression[JsonExpression]
  val fromJson: Coalgebra[Expression, JsonExpression] = {
    case JsonQuery(query) ⇒ Query(query)
    case JsonAnd(query) ⇒ And(query)
    case JsonOr(query) ⇒ Or(query)
    case JsonNot(query) ⇒ Not(query)
    case JsonFilter(query) ⇒ Filter(query)
  }

  // Input format String
  val fromString: CoalgebraM[NonEmptyList[String] \/ ?, Expression, String] = { string ⇒
    // Imagine some parboiled2 or fastparse action here
    Query(string).right[NonEmptyList[String]]
  }

  // Output format Elasticsearch: Expression[EsExpression] => NonEmptyList[String] \/ EsExpression
  val toElasticsearch: AlgebraM[NonEmptyList[String] \/ ?, Expression, EsExpression] = {
    case Query(query)       ⇒ EsQuery(query).right[NonEmptyList[String]]
    case And(subQueries)    ⇒ mergeSubqueries(subQueries).map(exprs ⇒ BoolExpression(must = exprs.some))
    case Or(subQueries)     ⇒ mergeSubqueries(subQueries).map(exprs ⇒ BoolExpression(should = exprs.some))
    case Not(subQueries)    ⇒ mergeSubqueries(subQueries).map(exprs ⇒ BoolExpression(mustNot = exprs.some))
    case Filter(subQueries) ⇒ mergeSubqueries(subQueries).map(exprs ⇒ BoolExpression(filter = exprs.some))
  }

  private def mergeSubqueries(queries: List[EsExpression]): NonEmptyList[String] \/ List[EsExpression] = {
    val (bools, exprs) = queries.foldLeft[(List[BoolExpression], List[EsExpression])]((Nil, Nil)) {
      case ((bs, qs), bool @ BoolExpression(_, _, _, _)) ⇒ (bool :: bs, qs)
      case ((bs, qs), q) ⇒ (bs, q :: qs)
    }

    bools.foldLeft(BoolExpression().right[NonEmptyList[String]]) { case (acc, current) ⇒
      acc.flatMap(b ⇒ mergeBool(b, current))
    }.map { bool ⇒
      if ((bool.must <+> bool.should <+> bool.mustNot <+> bool.filter).isDefined)
        bool :: exprs
      else
        exprs
    }
  }

  private def mergeBool(a: BoolExpression, b: BoolExpression): NonEmptyList[String] \/ BoolExpression = {
    val validMust =
      if (a.must.isDefined && b.must.isDefined) "Too many `and` queries on the same level".failureNel[Option[List[EsExpression]]]
      else (a.must <+> b.must).successNel[String]

    val validShould =
      if (a.should.isDefined && b.should.isDefined) "Too many `or` queries on the same level".failureNel[Option[List[EsExpression]]]
      else (a.should <+> b.should).successNel[String]

    val validMustNot =
      if (a.mustNot.isDefined && b.mustNot.isDefined) "Too many `not` queries on the same level".failureNel[Option[List[EsExpression]]]
      else (a.mustNot <+> b.mustNot).successNel[String]

    val validFilter =
      if (a.filter.isDefined && b.filter.isDefined) "Too many `filter` queries on the same level".failureNel[Option[List[EsExpression]]]
      else (a.filter <+> b.filter).successNel[String]

    val valid = (validMust |@| validShould |@| validMustNot |@| validFilter) { (a, o, n, f) ⇒
      BoolExpression(a, o, n, f)
    }

    valid.disjunction
  }

  // Output format String: Expression[String] => String
  val toStr: Algebra[Expression, String] = {
    case Query(query)       ⇒ query
    case And(subQueries)    ⇒ s"(${subQueries.mkString(" and ")})"
    case Or(subQueries)     ⇒ s"(${subQueries.mkString(" or ")})"
    case Not(subQueries)    ⇒ s"(not [${subQueries.mkString(",")}])"
    case Filter(subQueries) ⇒ s"(filter [${subQueries.mkString(" and ")}])"
  }

  // Example:
  def someExpression: JsonExpression =
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

  // Use corecursion to lift our input format into the parameterized data type
  val lifted = someExpression.ana[Fix[Expression]](fromJson)

  // Zip both out formats together to run them at the same time
  val zipped = AlgebraMZip[NonEmptyList[String] \/ ?, Expression].zip(toElasticsearch, toStr.map(_.point[\/[NonEmptyList[String], ?]]))

  // Actually run the conversion
  val result: NonEmptyList[String] \/ (EsExpression, String) = lifted.cataM(zipped)

  result.foreach { case (es, string) ⇒
    println("Elasticsearch output:")
    println(es)
    println("_________________________________________________________")
    println("String output:")
    println(string)
  }

  // Can also just run a single conversion from JSON to Elasticsearch
  val r: NonEmptyList[String] \/ EsExpression = someExpression.ana[Fix[Expression]](fromJson).cataM(toElasticsearch)

  // Or from JSON to string
  val t: Free.Trampoline[String] = someExpression.hyloM(toStr.map(a ⇒ Trampoline.delay(a)), fromJson.map(b ⇒ Trampoline.delay(b)))


  val stackSafeEs = toElasticsearch.map(a ⇒ EitherT.fromDisjunction[Free.Trampoline](a))
  val stackSafeStr = fromString.map(a ⇒ EitherT.fromDisjunction[Free.Trampoline](a))

  val stackSafeResult = "age=37".hyloM(stackSafeEs, stackSafeStr).run.run


  // Or from string to Elasticsearch
  val parsedFromString = "age=37".hyloM(toElasticsearch, fromString)

  println(parsedFromString)


  val tree = lifted.cata(toTree).drawTree

  val hght = lifted.cata(height)

  println(tree)
  println(hght)

}

