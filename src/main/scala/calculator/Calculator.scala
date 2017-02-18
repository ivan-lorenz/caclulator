package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (k,v) =>
      if (isCyclicDependency(k, v(), namedExpressions))
        k -> Var(Double.NaN)
      else
        k -> Var(eval(v(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => references.get(name).map(e => eval(e(), references)).getOrElse(Double.NaN)
      case Plus(e1, e2) => eval(e1, references) + eval(e2, references)
      case Minus(e1, e2) => eval(e1, references) - eval(e2, references)
      case Times(e1, e2) => eval(e1, references) * eval(e2, references)
      case Divide(e1, e2) => eval(e1, references) / eval(e2, references)
      case _ => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

  private def isCyclicDependency(name: String, expr: Expr, references: Map[String, Signal[Expr]]): Boolean = {
    expr match {
      case Ref(n) => isReferencingName(name, getReferenceExpr(n, references))
      case Plus(e1, e2) => isCyclicDependency(name, e1, references) || isCyclicDependency(name, e2, references)
      case Minus(e1, e2) => isCyclicDependency(name, e1, references) || isCyclicDependency(name, e2, references)
      case Times(e1, e2) => isCyclicDependency(name, e1, references) || isCyclicDependency(name, e2, references)
      case Divide(e1, e2) => isCyclicDependency(name, e1, references) || isCyclicDependency(name, e2, references)
      case _ => false
    }
  }

  private def isReferencingName(name: String, expr: Expr): Boolean = {
    expr match {
      case Ref(n) => n == name
      case Plus(e1, e2) => isReferencingName(name, e1) || isReferencingName(name, e2)
      case Minus(e1, e2) => isReferencingName(name, e1) || isReferencingName(name, e2)
      case Times(e1, e2) => isReferencingName(name, e1) || isReferencingName(name, e2)
      case Divide(e1, e2) => isReferencingName(name, e1) || isReferencingName(name, e2)
      case _ => false
    }
  }
}
