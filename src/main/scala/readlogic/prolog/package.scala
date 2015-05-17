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

package readlogic

import scala.annotation.tailrec

package object prolog {

  //--------------------------------------------------------------------------------------------------------------------
  //--- Regular expressions
  //--------------------------------------------------------------------------------------------------------------------

  /**
   * Regular expression for matching symbols (e.g., predicate names) that are starting with lower case letters.
   */
  private[prolog] lazy val lowerCasePattern = """([a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r

  /**
   * Regular expression for matching symbols (e.g., variable names) that are starting with upper case letters.
   */
  private[prolog] lazy val upperCasePattern = """([A-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)""".r

  private[prolog] lazy val singleQuotedPattern = """'((-?)([a-zA-Z0-9]|_[a-zA-Z0-9])*)'""".r

  private[prolog] lazy val singleQuotedPatternU = """('(-?)[A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*')""".r

  private[prolog] lazy val singleQuotedPatternL = """'((-?)[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)'""".r

  private[prolog] lazy val doubleQuotedPattern = """\"((-?)([a-zA-Z0-9]|_[a-zA-Z0-9])*)\"""".r

  private[prolog] lazy val doubleQuotedPatternU = """(\"-?[A-Z0-9]([a-zA-Z0-9]|_[a-zA-Z0-9])*\")""".r

  private[prolog] lazy val doubleQuotedPatternL = """\"((-?)[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*)\"""".r

  /**
   * Regular expression for matching logical functions (e.g., foo(), foo(bar), foo(x)=bar and foo(bar)=(x,y)).
   */
  private[prolog] lazy val functionPattern = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*\\(.*\\)""".r

  /**
   * Regular expression for matching integer numbers.
   */
  private[prolog] lazy val integerPattern = """(-?(\d+))""".r

  /**
   * Regular expression for matching floating point numbers.
   */
  private[prolog] lazy val floatingPointPattern = """(-?(\d+)(\.\d+))""".r

  /**
   * Regular expression for matching PROLOG-like negations (either using the symbol 'not' or '\+')
   */
  private[prolog] lazy val negationSymbolPattern = """(not)|(\\\+)""".r


  /**
   * Regular expression for matching infix arithmetic operators, e.g., +, %, *, etc.
   */
  private[prolog] lazy val infixArithOpsPattern = """[+-\\*/%]""".r

  /**
    * Regular expression for matching infix relational operators, e.g., <, =:=, >=, etc.
    */
  private[prolog] lazy val infixRelOps = """(=[:\\]=)|([=]{0,1}<)|(>[=]{0,1})""".r

  private lazy val _negationPattern1 = """(not|\\\+)([ ]+)\((.*)""".r

  private lazy val _negationPattern2 = """(not|\\\+)([ ]+)(\S*.*)""".r

  private lazy val _listHTPattern = """([ ]*\|[ ]*)""".r


  /**
   * Logical Expression
   */
  sealed trait LogicalExpression
  trait FormulaExpression extends LogicalExpression

  /**
   * Expression for including KB files
   * @param filename path to KB file
   */
  case class IncludeFileExpression(filename: String) extends LogicalExpression


  /**
   * Collects all variables from a list of terms
   *
   * @param terms input list of terms
   * @return The resulting set of variables found in the given list of terms, or an empty set if none is found.
   */
  @inline
  def collectVariables(terms: List[_ <: Term]): Set[Variable] =
    terms.foldRight(Set[Variable]())((a, b) => a match {
      case v: Variable => Set(v) ++ b
      case l: TermList => l.variables ++ b
      case f: TermFunction => f.variables ++ b
      case _ => b
    })


  /**
   * Collects all variables from a given list of term lists.
   *
   * @param termLists input list of term list
   * @return The resulting set of variables found in the given lists of terms, or an empty set if none is found.
   */
  def collectVariablesLists(termLists: List[List[_ <: Term]]): Set[Variable] = {
    /*
     * Recursively collect all variables from list of term list
     *
     * @param terms list of term list
     * @param variables the current set of variables found from previous runs
     * @return the resulting set of variables
     */
    @tailrec
    def variablesRec(terms: List[List[_ <: Term]], variables: Set[Variable]): Set[Variable] = {
      if(terms.nonEmpty)
        variablesRec(terms.tail, collectVariables(terms.head) ++ variables)
      else variables
    }

    // Start collecting the variables recursively. Initially the set of variables is empty.
    variablesRec(termLists, Set.empty)
  }

  @inline
  def collectConstants(terms: List[_ <: Term]): Set[Constant] = {
   terms.foldRight(Set[Constant]())((a, rest) => a match {
     case c: Constant => Set(c) ++ rest
     case l: TermList => l.constants ++ rest
     case f: TermFunction => f.constants ++ rest
     case _ => rest
   })
 }

  def collectConstantsLists(termLists: List[List[_ <: Term]]): Set[Constant] = {
    @tailrec
    def constantsRec(terms: List[List[_ <: Term]], constants: Set[Constant]): Set[Constant] = {
      if(terms.nonEmpty)
        constantsRec(terms.tail, collectConstants(terms.head) ++ constants)
      else constants
    }

    // Start collecting the constants recursively. Initially the set of constants is empty.
    constantsRec(termLists, Set.empty)
  }


  @inline
  def collectFunctions(terms: List[_ <: Term]): Set[TermFunction] =
    terms.foldRight(Set[TermFunction]())((t, rest) => t match {
      case l: TermList => l.functions ++ rest
      case f: TermFunction => Set(f) ++ f.functions ++ rest
      case _ => rest
    })

  def collectFunctionsLists(termLists: List[List[_ <: Term]]): Set[TermFunction] = {
    @tailrec
    def functionsRec(terms: List[List[_ <: Term]], functions: Set[TermFunction]): Set[TermFunction] = {
      if(terms.nonEmpty)
        functionsRec(terms.tail, collectFunctions(terms.head) ++ functions)
      else functions
    }

    // Start collecting the functions recursively. Initially the set of functions is empty.
    functionsRec(termLists, Set.empty)
  }


  def reformat(sentence: String, multiline: Boolean = false): String = {

    def cleanup(body: String) =
      body
        .stripMargin
        .split(",[ ]*\n")
        .filterNot(_.trim.isEmpty)
        .map(_.trim.stripSuffix("."))
        .map(_listHTPattern.replaceAllIn(_, ", "))
        .map(_negationPattern1.replaceAllIn(_, "not($3"))
        .map(_negationPattern2.replaceAllIn(_, "not($3)"))

    def rewrite(body: String) =
      if (multiline)
        cleanup(body).reduceLeft((left, right) => "\n\t" + left + ",\n\t" + right)
      else
        cleanup(body).reduceLeft((left, right) => left + ", " + right)

    val parts = sentence.split(":-")

    parts.length match {
      case 1 => rewrite(sentence) + "."
      case 2 => rewrite(parts(0).trim) + " :- " + rewrite(parts(1)) + "."
      case _ => throw new IllegalArgumentException("The sentence '" + sentence + "' is invalid.")
    }

  }


  /**
   * Utility function that processes the given list of arguments (i.e., the arguments of Predicate(arg1, arg2, arg3) is
   * a list List(arg1, arg2, arg3)), in order to return a list of logical terms (e.g., constants, variables, lists and
   * functions).
   *
   * @param arguments list of arguments
   * @return the extracted list of terms
   */
  def extractTerms(arguments: List[Any]): List[_ <: Term] =
    for (arg <- arguments) yield extractTerm(arg)

  /**
   * Utility function that process a single argument, in order to extract a logical term (e.g., constant, variable, list
   * and function).
   *
   * @param argument the argument symbol to process as a logical term
   * @return the extracted logical term
   */
  def extractTerm(argument: Any) = argument match {

    case singleQuotedPatternU(c, _*) => StringConstant(c)

    case singleQuotedPatternL(c, _*) => StringConstant(c)

    case doubleQuotedPatternU(c, _*) => StringConstant(c)

    case doubleQuotedPatternL(c, _*) => StringConstant(c)

    case lowerCasePattern(c, _*) =>
      if(c == "true") Constant(symbol = true)
      else if(c == "false") Constant(symbol = false)
      else Constant(c)

    case floatingPointPattern(n, _*)  => Constant(n.toDouble)

    case integerPattern(n, _*) => Constant(n.toLong)

    case upperCasePattern(v, _*) => Variable(v)

    case function: TermFunction  => function

    case list: TermList => list

    case _ => sys.error("Cannot parse term symbol '" + argument + "'")
  }

}
