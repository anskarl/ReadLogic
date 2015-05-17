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

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

class PrologParser extends JavaTokenParsers with RegexParsers {

  /**
   * Pattern of white space symbols, any matching string will be ignored by the parser. More specific the following will
   * be ignored by the parser:
   *
   * <ul>
   * <li> Any whitespace character '\s', i.e., [ \t\n\x0B\f\r]. </li>
   * <li> Single line comments: anything that is right of '//' or '%' until the end of line.</li>
   * <li> Multiple line comments: anything that is right of '/*' until '*/'. </li>
   * </ul>
   */
  override val whiteSpace = """(\s|//.*\n|%.*\n|%.*\n|(/\*(?:.|[\n\r])*?\*/))+""".r

  //--------------------------------------------------------------------------------------------------------------------
  //--- Formula parsers
  //--------------------------------------------------------------------------------------------------------------------

  def rules: Parser[List[Rule]] = rep(rule)

  /**
   * Parser combinator for typical PROLOG-like rules. A rule has the following general form:
   *
   * {{{
   *   headPredicate(term_1, term_2, ..., term_N) :-
   *      [not] bodyPredicate1(p1_term_1, ..., p1_term_I),
   *      [not] bodyPredicate2(p2_term_1, ..., p2_term_J),
   *      ...,
   *      [not] bodyPredicateM(pM_term_1, ..., pM_term_K).
   * }}}
   *
   * <p> The rule is composed of two parts, the head and the body (separated by the symbol ":-"). The definition of a
   * rule must end with a full stop letter ("."). The head predicate (left side of ":-" symbol) cannot be negated, while
   * the predicates in the body of the rule (right side of ":-" symbol) may optionally negated (using the symbol "not").
   * The predicates in the body are comma separated (indicating a logical conjunction).
   * </p>
   *
   * @return a parser combinator for rule definitions
   */
  def rule: Parser[Rule] = atomicFormula ~ ":-" ~ ruleBodyConstruct ~ "." ^^ {
    case head ~ ":-" ~ body ~ "." => Rule(head, body)
  }

  def ruleBodyConstruct: Parser[DefiniteClauseConstruct] = conjunction | atomicFormula | negation

  /**
   * An atomic formula (i.e., a single predicate) starts with a lowercase letter, it may be followed by a parenthesis
   * and finally it is followed by a dot symbol. The parenthesis can be composed of logical terms (e.g., variables,
   * constants and functions).
   *
   * @return a parser combinator for atomic formulas
   */
  def typicalAtom: Parser[Atom] =
    lowerCasePattern ~ opt("(" ~ repsep(logicalTerm, ",") ~ ")") ^^ {
      case symbol ~ None => Atom(symbol, Nil)
      case symbol ~ Some("(" ~ arguments ~ ")") => Atom(symbol, extractTerms(arguments))
    }

  def atomicFormula = dynamicAtomInfix | typicalAtom

  /**
   * Parser combinator for logical conjunction between atoms, negated atoms, other conjunctions or negated conjunctions.
   *
   * @return a parser combinator for a conjunction
   */
  def conjunction: Parser[DefiniteClauseConstruct] = definiteClauseBodyNoOp * conjunctionOpDefinite

  def definiteClauseBodyNoOp: Parser[DefiniteClauseConstruct] = negation | atomicFormula

  def conjunctionOpDefinite: Parser[(DefiniteClauseConstruct, DefiniteClauseConstruct) => DefiniteClauseConstruct] = {
    "," ^^^ {
      (a: DefiniteClauseConstruct, b: DefiniteClauseConstruct) => Conjunction(a, b)
    }
  }

  /**
   * Parser combinator for logical negation of an atom or other logical literals. Consider for example the following:
   *
   * {{{
   *   not foo(term1,..., termN)
   *   not (foo(term1,..., termN))
   *   not (foo(term1,..., termN), bar(term1,..., termK))
   *
   *   \+ foo(term1,..., termN)
   *   \+ (foo(term1,..., termN))
   *   \+ (foo(term1,..., termN), bar(term1,..., termK))
   * }}}
   *
   * @return a parser combinator for a negation
   */
  def negation: Parser[Negation] = negatedAtom | negatedAtomParenthesis | negatedLiteralParenthesis

  /**
   * Negated atom, without parenthesis:
   *
   * {{{
   *   not atom(...)
   *   \+ atom(...)
   * }}}
   *
   * @return a parser combinator for a negation of a single atom
   */
  def negatedAtom: Parser[Negation] = negationSymbolPattern ~ atomicFormula ^^ {
    case negationSymbol ~ atom => Negation(atom)
  }

  /**
   * Negated atom, with parenthesis:
   *
   * {{{
   *   not (atom(...))
   *   \+ (atom(...))
   * }}}
   *
   * @return a parser combinator for a negation of a single atom
   */
  def negatedAtomParenthesis: Parser[Negation] = negationSymbolPattern ~ "(" ~ atomicFormula ~ ")" ^^ {
    case negationSymbol ~ "(" ~ atom ~ ")" => Negation(atom)
  }

  /**
   * Negation of literals (parenthesis is required):
   *
   * {{{
   *   not (atom1(...), atom2(...), ..., atomN(...))
   *   \+ (atom1(...), atom2(...), ..., atomN(...))
   * }}}
   *
   * Note that the atoms may optionally negated.
   *
   * @return a parser combinator for a negation of a single atom
   */
  def negatedLiteralParenthesis: Parser[Negation] = negationSymbolPattern ~ "(" ~ ruleBodyConstruct ~ ")" ^^ {
    case negationSymbol ~ "(" ~ formulaSymbol ~ ")" => Negation(formulaSymbol)
  }

  //--------------------------------------------------------------------------------------------------------------------
  //--- Term parses
  //--------------------------------------------------------------------------------------------------------------------

  /**
   * Parses terms that represent a logical function. A logical functions can have one of the following forms:
   *
   * <ul>
   * <li>Simple, i.e., {{{f() or f(x,y).}}} </li>
   * <li>With value, i.e., {{{f()=x or f(x,y)=z}}}</li>
   * <li>With multiple values, i.e., {{{f()=(a,b) or f(x,y)=(a,b)}}}</li>
   * </ul>
   *
   * @return a parser combinator for a term representing a logical function
   */
  def termFunction: Parser[TermFunction] = termFunctionMultiValued | termFunctionValued | termFunctionSimple | anonymousTermFunction


  /**
   * Parses a simple logical function. For example the following terms are logical functions:
   *
   * {{{
   *   foo()
   *   foo(bar, X)
   * }}}
   *
   * @return a parser combinator for a typical function
   */
  def termFunctionSimple: Parser[TermFunction] =
    lowerCasePattern ~ "(" ~ opt(repsep(logicalTerm, ",")) ~ ")" ^^ {
      // when there aren't any arguments inside the logical function (e.g., foo())
      case symbol ~ "(" ~ None ~ ")" =>
        TermFunction(symbol, Nil, Nil)
      // when the logical function contains arguments (e.g., foo(bar))
      case symbol ~ "(" ~ Some(arguments) ~ ")" =>
        TermFunction(symbol, extractTerms(arguments), Nil)

    }


  def anonymousTermFunction: Parser[TermFunction] = "(" ~ repsep(logicalTerm, ",") ~ ")" ^^ {
    case "(" ~ arguments ~ ")" =>
      TermFunction("", extractTerms(arguments), Nil)
  }

  /**
   * Parses a logical function which is associated with some other term. For example the following terms
   * are logical functions with some values:
   *
   * {{{
   *   foo() = bar
   *   foo(X) = bar
   * }}}
   *
   * @return a parser combinator for a valued function
   */
  def termFunctionValued: Parser[TermFunction] =
    lowerCasePattern ~ "(" ~ opt(repsep(logicalTerm, ",")) ~ ")" ~ "=" ~ logicalTerm ^^ {
      // when there aren't any arguments inside the logical function and is associated with some value (e.g., foo()=x)
      case symbol ~ "(" ~ None ~ ")" ~ "=" ~ value =>
        TermFunction(symbol, Nil, List(extractTerm(value)))
      // when the logical function contains arguments and is associated with some value (e.g., foo(bar)=x)
      case symbol ~ "(" ~ Some(arguments) ~ ")" ~ "=" ~ value =>
        TermFunction(symbol, extractTerms(arguments), List(extractTerm(value)))
    }

  /**
   * Parses a multiple values logical function. For example the following terms are logical functions that are
   * associated with multiple values:
   *
   * {{{
   *   foo() = (bar, X)
   *   foo(X) = (bar, Y)
   * }}}
   *
   * @return a parser combinator for a multiple valued function
   */
  def termFunctionMultiValued: Parser[TermFunction] =
    lowerCasePattern ~ "(" ~ opt(repsep(logicalTerm, ",")) ~ ")" ~ "=" ~
      "(" ~ repsep(logicalTerm, ",") ~ ")" ^^ {
      // when there aren't any arguments inside the multivalued logical function (e.g., foo()=(x,y))
      case symbol ~ "(" ~ None ~ ")" ~ "=" ~ "(" ~ values ~ ")" =>
        TermFunction(symbol, Nil, extractTerms(values))
      // when the multivalued logical function contains arguments (e.g., foo(bar)=(x,y))
      case symbol ~ "(" ~ Some(arguments) ~ ")" ~ "=" ~ "(" ~ values ~ ")" =>
        TermFunction(symbol, extractTerms(arguments), extractTerms(values))
    }

  /**
   * A logical term can be one of the following:
   *
   * <ul>
   * <li> A constant symbol, i.e., a number or a string that begins with lowercase letters or it is given inside double quotes. </li>
   * <li> A variable symbol (begins with uppercase letter). </li>
   * <li> A list of terms (given inside square brackets). </li>
   * <li> A logical function (e.g., foo(bar) or foo(x)=bar). </li>
   * </ul>
   *
   * @return a parser combinator for logical terms
   */
  def logicalTerm = termFunction | termList | lowerCasePattern | upperCasePattern | floatingPointPattern | integerPattern | singleQuotedPattern | doubleQuotedPattern


  /**
   * Parses a list of terms (expressed with square brackets), for example:
   *
   * <ul>
   * <li> A list of terms: {{{[term1, term2, ..., termN]}}} </li>
   * <li> Separated by the symbol "|": {{{[head | tail]}}} </li>
   * </ul>
   *
   * @return a parser combinator for list of terms
   */
  def termList = termListHT | termListSimple


  /**
   * Parses a list of terms of the following form:
   * {{{[term1, term2, ..., termN]}}}
   *
   * @see [[PrologParser.termList]]
   * @return a parser combinator for list of terms
   */
  def termListSimple: Parser[TermList] =
    "[" ~ opt(repsep(logicalTerm, ",")) ~ "]" ^^ {
      case "[" ~ Some(terms) ~ "]" => TermList(extractTerms(terms))
      case "[" ~ None ~ "]" => TermList(Nil)
    }

  /**
   * Parses a list of terms separated by the symbol "|". The left part is the head of the list (e.g., a single
   * constant or variable) and the right part is the rest of the list.
   *
   * <br/><br/>
   *
   * For example:
   * {{{
   *    [Head | Tail]
   *    [1,2,3 | [4,5,6]] is the same with [1,2,3,4,5,6]
   *    [H | [a,b,c]] is the same with [H, a, b, c]
   *    [H | [T1,T2,T3]] is the same with [H, T1, T2, T3]
   * }}}
   *
   * @return a parser combinator for list of terms
   */
  def termListHT: Parser[TermList] =
    "[" ~ repsep(logicalTerm, ",") ~ "|" ~ (upperCasePattern | termList) ~ "]" ^^ {
      case "[" ~ headList ~ "|" ~ tailList ~ "]" =>
        tailList match {
          case upperCasePattern(v, _*) => TermList(extractTerms(headList) ::: Variable(v) :: Nil)
          case list: TermList => TermList(extractTerms(headList) ::: list.terms)
        }
    }

  //--------------------------------------------------------------------------------------------------------------------
  //--- Infix relational operators (translated to dynamic atoms)
  //--------------------------------------------------------------------------------------------------------------------

  private def relOpConstruct = lowerCasePattern | upperCasePattern | functionPattern

  def dynamicAtomInfix: Parser[Atom] = relOpConstruct ~ infixRelOps ~ relOpConstruct ^^ {
    case left ~ operator ~ right =>
      val symbol = operator match {
        case "=:=" => "equals"
        case """=\=""" => "not_equals"
        case "<" => "lessThan"
        case ">" => "greaterThan"
        case "=<" => "lessThanEq"
        case ">=" => "greaterThanEq"
        case _ => sys.error(s"Unknown infix operator '$operator' (possible bug?)")
      }

      parseAtom(s"$symbol($left, $right)")
  }


  //--------------------------------------------------------------------------------------------------------------------
  //--- Infix arithmetic operators (translated to dynamic functions)
  //--------------------------------------------------------------------------------------------------------------------

  private def simpleTerm = termFunction | lowerCasePattern | upperCasePattern | floatingPointPattern | integerPattern

  def dynamicFunctionInfix: Parser[TermFunction] = simpleTerm ~ infixArithOpsPattern ~ simpleTerm ^^ {
    case leftTerm ~ operator ~ rightTerm =>
      val symbol = operator match {
        case "+" => "plus"
        case "-" => "minus"
        case "*" => "product"
        case "/" => "divide"
        case "%" => "modulo"
        case _ => sys.error(s"Unknown infix operator '$operator' (possible bug?)")
      }

      parseFunction(s"$symbol($leftTerm, $rightTerm)")
  }


  //--------------------------------------------------------------------------------------------------------------------
  //--- Public API with utility functions for parsing
  //--------------------------------------------------------------------------------------------------------------------

  def parseTermList(src: String): TermList = parse(termList, src) match {
    case Success(termList, _) => termList
    case x => sys.error(s"Cannot parse the following expression as a list of terms: '$x'")
  }

  def parseFunction(src: String): TermFunction = parse(termFunction, src) match {
    case Success(funcExpr, _) => funcExpr
    case x => sys.error(s"Cannot parse the following expression as a function: '$x'")
  }

  def parseAtom(src: String): Atom = parse(atomicFormula, src) match {
    case Success(expr, _) => expr
    case x => sys.error(s"Cannot parse the following expression as an Atomic Formula: '$x'")
  }

  def parseRule(src: String): Rule = parse(rule, src) match {
    case Success(ruleExpr, _) => ruleExpr
    case x => sys.error(s"Cannot parse the following expression as a rule: '$x'")
  }

}