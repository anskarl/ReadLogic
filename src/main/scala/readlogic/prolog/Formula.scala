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

sealed trait Formula extends Serializable {


  lazy val variables: Set[Variable] = subFormulas.foldRight(Set[Variable]())((a: Formula, b) => a.variables ++ b)

  lazy val constants: Set[Constant] = subFormulas.foldRight(Set[Constant]())((a: Formula, b) => a.constants ++ b)

  lazy val functions: Set[TermFunction] = subFormulas.foldRight(Set[TermFunction]())((a: Formula, b) => a.functions ++ b)

  /**
   * Gives the sub-formulas that this formula contains
   */
  def subFormulas: Seq[Formula]

  /**
   * The textual representation of this formula
   */
  def toText: String

  /**
   * @return the number of AtomicFormulas
   */
  def countAtoms: Int = subFormulas.foldRight(1)((current, rest) => current.countAtoms + rest)

  def isUnit: Boolean

}

sealed trait DefiniteClauseConstruct extends Formula with Serializable


sealed case class Atom(symbol: String, args: List[_ <: Term]) extends Formula with DefiniteClauseConstruct with Serializable {

  lazy val signature = AtomSignature(symbol, args.size)

  def arity = args.size

  override def isUnit: Boolean = true

  override def countAtoms = 1

  /**
   * Gives the sub-formulas that this formula contains
   */
  override def subFormulas: Seq[Formula] = Seq()

  /**
   * All variables of this atom
   */
  override lazy val variables: Set[Variable] = collectVariables(args)

  /**
   * All constants of this atom
   */
  override lazy val constants: Set[Constant] = collectConstants(args)

  /**
   * All functions of this atom
   */
  override lazy val functions: Set[TermFunction] = collectFunctions(args)

  def isGround = variables.isEmpty

  override def toText: String = {
    if (args.nonEmpty)
      symbol + "(" + args.map(_.toText).reduceLeft((left, right) => left + ", " + right) + ")"
    else
      symbol
  }

  override def toString: String = {
    if (args.nonEmpty)
      symbol + "(" + args.map(_.toString).reduceLeft((left, right) => left + ", " + right) + ")"
    else symbol
  }

}

sealed trait LogicalConnective extends Formula with DefiniteClauseConstruct

sealed case class Conjunction(left: Formula, right: Formula) extends LogicalConnective with DefiniteClauseConstruct with Serializable {

  require(left ne null, "The left part of a conjunction cannot be empty")
  require(right ne null, "The right part of a conjunction cannot be empty")

  override def isUnit: Boolean = false

  /**
   * Gives the sub-formulas that this formula contains
   */
  override def subFormulas: Seq[Formula] = Seq(left, right)

  /**
   * The textual representation of this formula
   */
  override def toText: String = left.toText + ", " + right.toText

}

/*sealed case class Disjunction(left: Formula, right: Formula) extends LogicalConnective {

  require(left ne null, "The left part of a disjunction cannot be null")
  require(right ne null, "The right part of a disjunction cannot be null")

  override def isUnit: Boolean = false

  /**
   * Gives the sub-formulas that this formula contains
   */
  override def subFormulas: Seq[Formula] = Seq(left, right)

  /**
   * The textual representation of this formula
   */
  override def toText: String = left.toText + " ; " + right.toText

}*/

sealed case class Negation(formula: Formula) extends LogicalConnective with DefiniteClauseConstruct with Serializable {

  require(formula ne null, "The specified formula cannot be null")

  /**
   * Gives the sub-formulas that this formula contains
   */
  override def subFormulas: Seq[Formula] = Seq(formula)

  /**
   * The textual representation of this formula
   */
  override def toText = "not("+formula.toText+")"

  override def isUnit: Boolean = formula.isUnit

  override def countAtoms = 1

}

sealed case class Rule(head: Atom, body: DefiniteClauseConstruct) extends Formula with Serializable {

  require(head ne null, "The head of a rule cannot be null (headless rules are not supported)")
  require(body ne null, "The body of a rule cannot be null (unit clauses are not supported)")

  override lazy val variables: Set[Variable] = body.subFormulas.foldRight(head.variables)((a: Formula, b) => a.variables ++ b)

  override lazy val constants: Set[Constant] = body.subFormulas.foldRight(head.constants)((a: Formula, b) => a.constants ++ b)

  override lazy val functions: Set[TermFunction] = body.subFormulas.foldRight(head.functions)((a: Formula, b) => a.functions ++ b)


  /**
   * Gives the sub-formulas that this formula contains
   */
  override def subFormulas: Seq[Formula] = Seq(head, body)

  /**
   * The textual representation of this formula
   */
  override def toText: String = head.toText + " :- " + body.toText +"."

  override def isUnit: Boolean = false

}
