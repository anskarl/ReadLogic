/*
 *    _____              _  __               _
 *   | __  | ___  ___  _| ||  |    ___  ___ |_| ___
 *   |    -|| -_|| .'|| . ||  |__ | . || . || ||  _|
 *   |__|__||___||__,||___||_____||___||_  ||_||___|
 *                                   |___|
 *   ReadLogic
 *
 *   Copyright 2015 Anastasios Skarlatidis
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package readlogic.prolog

sealed trait Term extends Serializable {

  type SymbolType

  val symbol: SymbolType

  /**
   * Determine whether this term is ground (i.e., does not contain any variable) or not.
   *
   * @return true if it is ground, false otherwise.
   */
  def isGround = false

  def isVariable = false

  def isConstant = false

  def isFunction = false

  def isList = false

  /**
   * Gives the textual representation of this term.
   */
  def toText: String

}

case class Variable(override val symbol: String) extends Term with Serializable {

  override type SymbolType = String

  override def isVariable = true

  override def toText = symbol

  override def toString = "Variable(" + symbol + ")"

  override lazy val hashCode = symbol.##

  override def equals(obj: scala.Any) = obj match {
    case other: Variable => this.symbol == other.symbol
    case _ => false
  }
}

sealed trait Constant extends Term with Serializable {

  override def isGround = true

  override def isConstant = true

  override def toText = symbol.toString

  override def toString = "Constant(" + symbol + ")"

}

object Constant {

  def apply(symbol: Boolean) = BooleanConstant(symbol)

  def apply(symbol: Long) = LongConstant(symbol)

  def apply(symbol: Double) = DoubleConstant(symbol)

  def apply(symbol: String) = StringConstant(symbol)
}

case class BooleanConstant(override val symbol: Boolean) extends Constant with Serializable {
  override type SymbolType = Boolean

  override lazy val hashCode = symbol.##

  override def toString: String = "BooleanConstant("+symbol+")"

  override def equals(obj: scala.Any) = obj match {
    case o: BooleanConstant => symbol == o.symbol
    case _ => false
  }
}

case class LongConstant(override val symbol: Long) extends Constant with Serializable {
  override type SymbolType = Long

  override lazy val hashCode = symbol.##

  override def toString: String = "LongConstant("+symbol+")"

  override def equals(obj: scala.Any) = obj match {
    case c: LongConstant => this.symbol == c.symbol
    case _ => false
  }
}

case class StringConstant(override val symbol: String) extends Constant with Serializable {
  override type SymbolType = String

  override lazy val hashCode = symbol.##

  override def toString: String = "StringConstant("+symbol+")"

  override def equals(obj: scala.Any) = obj match {
    case o: StringConstant => this.symbol == o.symbol
    case _ => false
  }
}

case class DoubleConstant(override val symbol: Double) extends Constant with Serializable {
  override type SymbolType = Double

  override lazy val hashCode = symbol.##


  override def toString: String = "DoubleConstant("+symbol+")"

  override def equals(obj: scala.Any) = obj match{
    case o: DoubleConstant => this.symbol == o.symbol
    case _ => false
  }
}

case class TermFunction(override val symbol: String, terms: List[_ <: Term], values: List[_ <: Term] = Nil) extends Term with Serializable {

  override type SymbolType = String

  lazy val signature = AtomSignature(symbol, arity)

  lazy val variables: Set[Variable] = collectVariablesLists(List(terms, values))

  lazy val constants: Set[Constant] = collectConstantsLists(List(terms, values))

  lazy val functions: Set[TermFunction] = collectFunctionsLists(List(terms, values))



  /**
   * Determine whether this function is ground or not.
   *
   * @return true if it is ground, false otherwise.
   */
  override def isGround = variables.isEmpty

  override def isFunction = true

  def isMultivalued = values.nonEmpty

  override def toText = {
    val strTerms =
      if (terms.isEmpty) "()"
      else "(" + terms.map(_.toText).reduceLeft(_ + ", " + _) + ")"

    values.size match {
      case 0 => symbol + strTerms
      case 1 => symbol + strTerms + "=" + values.head.toText
      case _ => symbol + strTerms + "=(" + values.map(_.toText).reduceLeft(_ + ", " + _) + ")"
    }
  }

  def arity = terms.size + values.size

  override lazy val hashCode = {
    var code = symbol.##
    for (term <- terms) code ^= term.##
    for (value <- values) code ^= value.##
    code
  }

  override def equals(obj: scala.Any) = obj match {
    case other: TermFunction =>
      other.## == this.## &&
        other.arity == this.arity &&
        other.symbol == this.symbol &&
        other.terms == this.terms &&
        other.values == this.values
    case _ => false
  }
}

case class TermList(terms: List[_ <: Term]) extends Term with Serializable {

  override type SymbolType = String

  override val symbol: String = "{L}"

  lazy val variables: Set[Variable] = collectVariables(terms)

  lazy val constants: Set[Constant] = collectConstants(terms)

  lazy val functions: Set[TermFunction] = collectFunctions(terms)

  override def isList: Boolean = true

  /**
   * Gives the textual representation of this term.
   */
  override def toText =
    if (terms.nonEmpty) "[" + terms.map(_.toText).reduceLeft(_ + ", " + _) + "]"
    else "[]"

  /**
   * Determine whether this term is ground (i.e., does not contain any variable) or not.
   *
   * @return true if it is ground, false otherwise.
   */
  override def isGround = variables.isEmpty

  override lazy val hashCode = {
    var code = 120684 // "{L}".## = 120684
    for (term <- terms) code ^= term.##
    code
  }

  override def equals(obj: scala.Any) = obj match {
    case other: TermList => (other.## == this.##) && (this.terms == other.terms)
    case _ => false
  }
}

object TermList {

  implicit def long2Constant(x: Long): LongConstant = LongConstant(x)

  implicit def string2Term(x: String) = extractTerm(x)
    //if(x.matches("[A-Z].*")) Variable(x) else StringConstant(x)

  implicit def double2Constant(x: Double): DoubleConstant = DoubleConstant(x)

  /*implicit def args2Terms(args: String*): Term = args.size match {
    case 0 => sys.error("Cannot accept an empty string as a term")
    case 1 => extractTerm(args(0))
    case _ => TermList(extractTerms(args.toList))
  }*/

  def apply[T <:Term](terms: T* ): TermList = TermList(terms.toList)

  //def apply(args: String*): TermList = TermList(extractTerms(args.toList))

}

