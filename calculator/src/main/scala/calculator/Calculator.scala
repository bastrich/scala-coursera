package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions
      .map {
        case (name, exprSignal) => (name, Var(eval(exprSignal(), namedExpressions)))
      }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def help(expr: Expr, usedReferences: Set[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) => {
          if (usedReferences.contains(name)) {
            Double.NaN
          } else {
            help(references.getOrElse(name, Var(Literal(Double.NaN)))(), usedReferences + name)
          }
        }
        case Plus(a, b) => help(a, usedReferences) + help(b, usedReferences)
        case Minus(a, b) => help(a, usedReferences) - help(b, usedReferences)
        case Times(a, b) => help(a, usedReferences) * help(b, usedReferences)
        case Divide(a, b) => help(a, usedReferences) / help(b, usedReferences)
      }
    }

    help(expr, Set())
  }

  /** Get the Expr for a referenced variables.
   * If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
