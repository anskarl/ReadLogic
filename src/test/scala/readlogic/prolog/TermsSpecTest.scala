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

import org.scalatest.{FunSpec, Matchers}

class TermsSpecTest extends FunSpec with Matchers {

  val parser = new PrologParser()

  /**
   * Test Constants
   */
  describe("The instance of Constant(foo)") {
    val resultingTerm = Constant("foo")

    it("has type String") {
      assert(resultingTerm.symbol.isInstanceOf[String])
    }

    it("has symbol: foo") {
      resultingTerm.symbol should be("foo")
    }

    it("is equal with another instance of Constant(foo)") {
      resultingTerm should equal(Constant("foo"))
    }

    it("is not equal with Constant(bar)") {
      resultingTerm should not equal Constant("bar")
    }

    it("prints as: foo") {
      resultingTerm.toText should be("foo")
    }

  }

  describe("The instance of Constant(10)") {
    val resultingTerm = Constant(10)

    it("has type Long") {
      assert(resultingTerm.symbol.isInstanceOf[Long])
    }

    it("has symbol: 10") {
      resultingTerm.symbol should be(10)
    }

    it("is equal with another instance of Constant(10)") {
      resultingTerm should equal(Constant(10))
    }

    it("is not equal with Constant(11)") {
      resultingTerm should not equal Constant(11)
    }

    it("prints as: 10") {
      resultingTerm.toText should be("10")
    }

  }

  describe("The instance of Constant(10.3)") {
    val resultingTerm = Constant(10.3)

    it("has type Double") {
      assert(resultingTerm.symbol.isInstanceOf[Double])
    }

    it("has symbol: 10.3") {
      resultingTerm.symbol should be(10.3)
    }

    it("is equal with another instance of Constant(10.3)") {
      resultingTerm should equal(Constant(10.3))
    }

    it("is not equal with Constant(11.5)") {
      resultingTerm should not equal Constant(11.5)
    }

    it("prints as: 10.3") {
      resultingTerm.toText should be(10.3.toString)
    }

  }

  describe("The sentence: 10.3") {
    val resultingTerm = extractTerm("10.3")


    it("is parsed as an instance of DoubleConstant") {
      assert(resultingTerm.isInstanceOf[DoubleConstant])
    }

    it("has symbol: 10.3") {
      resultingTerm.symbol should be(10.3)
    }

    it("is equal with another instance of Constant(10.3)") {
      resultingTerm should equal(Constant(10.3))
    }

    it("is not equal with Constant(11.5)") {
      resultingTerm should not equal Constant(11.5)
    }

    it("prints as: 10.3") {
      resultingTerm.toText should be(10.3.toString)
    }

  }


  describe("The sentence: -10.3") {
    val resultingTerm = extractTerm("-10.3")


    it("is an instance of DoubleConstant") {
      assert(resultingTerm.isInstanceOf[DoubleConstant])
    }

    it("has symbol: -10.3") {
      resultingTerm.symbol should be(-10.3)
    }

    it("is equal with another instance of Constant(-10.3)") {
      resultingTerm should equal(Constant(-10.3))
    }

    it("is not equal with Constant(11.5)") {
      resultingTerm should not equal Constant(11.5)
    }

    it("prints as: -10.3") {
      resultingTerm.toText should be((-10.3).toString)
    }

  }

  describe("The instance of Constant(true)") {
    val resultingTerm = Constant(true)

    it("has type Boolean") {
      assert(resultingTerm.symbol.isInstanceOf[Boolean])
    }

    it("has symbol: true") {
      resultingTerm.symbol should be(true)
    }

    it("is equal with another instance of Constant(10.3)") {
      resultingTerm should equal(Constant(true))
    }

    it("is not equal with Constant(false)") {
      resultingTerm should not equal Constant(false)
    }

    it("prints as: true") {
      resultingTerm.toText should be("true")
    }

  }

  describe("The sentence: true") {
    val resultingTerm = extractTerm("true")

    it("is an instance of BooleanConstant") {
      assert(resultingTerm.isInstanceOf[BooleanConstant])
    }

    it("has symbol: true") {
      resultingTerm.symbol shouldEqual (true)
    }

    it("is equal with another instance of Constant(10.3)") {
      resultingTerm should equal(Constant(true))
    }

    it("is not equal with Constant(false)") {
      resultingTerm should not equal Constant(false)
    }

    it("prints as: true") {
      resultingTerm.toText should be("true")
    }

  }


  /**
   * Test Variables
   */
  describe("The instance of Variable(X) is a variable, which") {
    val resultingTerm = Variable("X")

    it("has symbol: X") {
      resultingTerm.symbol should be("X")
    }

    it("is equal with another instance of Variable(X)") {
      resultingTerm should equal(Variable("X"))
    }

    it("is not equal with Variable(Y)") {
      resultingTerm should not equal Variable("Y")
    }

    it("is not equal with Constant(foo)") {
      resultingTerm should not equal Constant("foo")
    }

    it("prints as: X") {
      resultingTerm.toText should be("X")
    }

  }

  /**
   * Test parse Variables
   */
  describe("The sentence of: X") {

    val resultingTerm = extractTerm("X")

    it("is an instance of Variable") {
      assert(resultingTerm.isInstanceOf[Variable])
    }

    it("has symbol: X") {
      resultingTerm.symbol should be("X")
    }

    it("is equal with another instance of Variable(X)") {
      resultingTerm should equal(Variable("X"))
    }

    it("is not equal with Variable(Y)") {
      resultingTerm should not equal Variable("Y")
    }

    it("is not equal with Constant(foo)") {
      resultingTerm should not equal Constant("foo")
    }

    it("prints as: X") {
      resultingTerm.toText should be("X")
    }

  }

  /**
   * Test: parse single quoted constants
   */
  describe("The quoted sentence: 'X'") {

    val resultingTerm = extractTerm("'X'")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol 'X'") {
      resultingTerm.symbol should be("'X'")
    }

    it("is equal with another instance of Constant('X')") {
      resultingTerm should equal(Constant("'X'"))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as 'X'") {
      resultingTerm.toText should be("'X'")
    }

  }

  describe("The quoted sentence: 'x'") {

    val resultingTerm = extractTerm("'x'")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol: x (without quotes)") {
      resultingTerm.symbol should be("x")
    }

    it("is equal with another instance of Constant(x)") {
      resultingTerm should equal(Constant("x"))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as: x (without quotes)") {
      resultingTerm.toText should be("x")
    }

  }

  /**
   * Test: parse single quoted constants
   */
  describe("The quoted sentence: '010101'") {

    val resultingTerm = extractTerm("'010101'")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol '010101'") {
      resultingTerm.symbol should be("'010101'")
    }

    it("is equal with another instance of Constant('010101')") {
      resultingTerm should equal(Constant("'010101'"))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as '010101'") {
      resultingTerm.toText should be("'010101'")
    }

  }

  /**
   * Test: parse double quoted constants
   */
  describe("The quoted sentence: \"X\"") {

    val resultingTerm = extractTerm("\"X\"")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol \"X\"") {
      resultingTerm.symbol should be("\"X\"")
    }

    it("is equal with another instance of Constant(\"X\")") {
      resultingTerm should equal(Constant("\"X\""))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as: \"X\"") {
      resultingTerm.toText should be("\"X\"")
    }

  }

  describe("The quoted sentence: \"x\"") {

    val resultingTerm = extractTerm("\"x\"")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol: x (without double quotes)") {
      resultingTerm.symbol should be("x")
    }

    it("is equal with another instance of Constant(X)") {
      resultingTerm should equal(Constant("x"))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as: x (without double quotes)") {
      resultingTerm.toText should be("x")
    }

  }

  /**
   * Test: parse single quoted constants
   */
  describe("The quoted sentence: \"010101\"") {

    val resultingTerm = extractTerm("\"010101\"")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol \"010101\"") {
      resultingTerm.symbol should be("\"010101\"")
    }

    it("is equal with another instance of Constant(\"010101\")") {
      resultingTerm should equal(Constant("\"010101\""))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as \"010101\"") {
      resultingTerm.toText should be("\"010101\"")
    }

  }

  /**
   * Test: parse single quoted constants
   */
  describe("The quoted sentence: \"-10101\"") {

    val resultingTerm = extractTerm("\"-10101\"")

    it("is an instance of constant") {
      assert(resultingTerm.isInstanceOf[StringConstant])
    }

    it("has symbol \"-10101\"") {
      resultingTerm.symbol should be("\"-10101\"")
    }

    it("is equal with another instance of Constant(\"-10101\")") {
      resultingTerm should equal(Constant("\"-10101\""))
    }

    it("is not equal with Constant(Y)") {
      resultingTerm should not equal Constant("Y")
    }

    it("prints as \"-10101\"") {
      resultingTerm.toText should be("\"-10101\"")
    }

  }


  /**
   * Test Functions
   *
   * functions description: (TermFunction instance, string representation, arity, number of constants, number of variables )
   */
  val functionsDescription = List(
    (TermFunction("foo", List(Constant("bar"))), "foo(bar)", 1, 1, 0),
    (TermFunction("foo", List(Constant("bar")), List(Constant(symbol = true))), "foo(bar)=true", 2, 2, 0),
    (TermFunction("foo", List(Variable("X")), List(Constant(symbol = true))), "foo(X)=true", 2, 1, 1),
    (TermFunction("foo", List(Variable("X"), Variable("Y")), List(Constant(symbol = true))), "foo(X, Y)=true", 3, 1, 2),
    (TermFunction("foo", List(Variable("X"), Variable("Y")), List(Constant("bar"), Constant(symbol = true))), "foo(X, Y)=(bar, true)", 4, 2, 2)
  )

  for ((termFunction, strFunction, arity, nConstant, nVariables) <- functionsDescription) describe("The instance of " + termFunction + " is a function, which") {

    it("has symbol: foo") {
      termFunction.symbol should be("foo")
    }

    it("has arity " + arity) {
      termFunction.arity should be(arity)
    }

    it("has " + nConstant + " constant(s)") {
      termFunction.constants.size should be(nConstant)
    }

    it("has " + nVariables + " variables(s)") {
      termFunction.variables.size should be(nVariables)
    }
  }


  /**
   * Test PROLOG lists (part 1)
   *
   * list description: (TermList instance, string representation, number of constants, number of variables, number of functions)
   */
  val listsDescription = List(
    (TermList(Nil),
      "[]", 0, 0, 0),
    (TermList(List(Variable("X"), Variable("Var"), Constant("const"))),
      "[X, Var, const]", 1, 2, 0),
    (TermList(List(Constant(10), Variable("Var"), Constant("const"), TermFunction("f", List(Variable("X"))))),
      "[10, Var, const, f(X)]", 2, 2, 1),
    (TermList(List(TermFunction("f", List(Variable("X"))), Variable("Var"), Constant("const"), Constant("anotherConstant"))),
      "[f(X), Var, const, anotherConstant]", 2, 2, 1),
    (TermList(List(Constant(10), Variable("Var"), Constant("const"), TermFunction("f", List(Variable("X"))), TermList(List(Constant("another"), Constant("list"))))),
      "[10, Var, const, f(X), [another, list]]", 4, 2, 1),
    (TermList(List(Constant(10), Variable("Var"), Constant("const"), TermFunction("f", List(Variable("X"))), TermList(List(TermFunction("g", List(Variable("X"))), Constant("list"))))),
      "[10, Var, const, f(X), [g(X), list]]", 3, 2, 2),
    (TermList(List(Constant(10), Variable("Var"), Constant("const"), TermFunction("f", List(Variable("X"))), TermList(List(TermFunction("f", List(Variable("X"))), Constant("list"))))),
      "[10, Var, const, f(X), [f(X), list]]", 3, 2, 1)

  )

  for ((termList, strTermList, nConstants, nVariables, nFunctions) <- listsDescription) describe("The term " + strTermList) {
    val result = parser.parseTermList(strTermList)

    it("is a list " + termList) {
      result should equal(termList)
    }

    it("prints as: " + strTermList) {
      result.toText should equal(strTermList)
      result.toText should equal(termList.toText)
    }

    it("contains " + nConstants + " constant(s)") {
      result.constants.size should be(nConstants)
      result.constants should equal(termList.constants)
    }

    it("contains " + nVariables + " variable(s)") {
      result.variables.size should be(nVariables)
      result.variables should equal(termList.variables)
    }

    it("contains " + nFunctions + " function(s)") {
      result.functions.size should be(nFunctions)
      result.functions should equal(termList.functions)
    }
  }


  /**
   * Test PROLOG lists (part 2)
   *
   * Elements of a list:
   *
   * <ul>
   * <li>[a,b,c,d] = head: a, tail: [b,c,d] <li>
   * <li>[element] = head: element, tail: [] <li>
   * <li>[ [foo, bar], last ] = head: [foo, bar], tail: [last] <li>
   * <li>[foo(bar), X] = head: foo(bar) , tail: X <li>
   * <li>[ single, [foo, bar], [hello, there] ] = head: single , tail: [ [foo, bar], [hello, there] ] <li>
   * </ul>
   */

  val listsDescription2 = List(
    ("[a,b,c,d]", Constant("a"), TermList(List(Constant("b"), Constant("c"), Constant("d")))),
    ("[element]", Constant("element"), TermList(Nil)),
    ("[[foo, bar], last]", TermList(List(Constant("foo"), Constant("bar")))),
    ("[foo(bar), X]", TermList(List(TermFunction("foo", List(Variable("X")))))),
    ("[single, [foo, bar], [hello, there]]", Constant("single"), TermList(List(TermList(List(Constant("foo"), Constant("bar"))), TermList(List(Constant("hello"), Constant("there"))))))
  )

  for ((strTermList: String, head: Term, tail: TermList) <- listsDescription2) describe("The term " + strTermList) {
    val result = parser.parseTermList(strTermList)

    it("is a list of terms") {
      assert(result.isList)
      assert(result.symbol == "{L}")
    }

    it("has head: " + head.toText) {
      result.terms.head should be(head)
    }

    it("has tail: " + tail.toText) {
      result.terms.tail should be(tail.terms)
    }
  }

  /**
   * The following lists, although expressed differently, should be equal:
   *
   * [1,2,3] = [1 | [2, 3]] = [1, 2 | [3] ] = [1, 2, 3 | [] ]
   */
  val listsDescription3 = List("[1 | [2, 3]]", "[1, 2 | [3]]", "[1, 2, 3 | []]")
  val sampleList = TermList(List(Constant(1), Constant(2), Constant(3)))
  val sampleList2 = TermList(Constant("1"), Constant("2"), Constant("3"))

  for (strTermList <- listsDescription3) describe("The expression " + strTermList) {
    val result = parser.parseTermList(strTermList)

    it("is a list") {
      assert(result.isList)
    }

    it("prints as [1, 2, 3]") {
      result.toText should be("[1, 2, 3]")
      result.toText should be(sampleList.toText)
    }

    it("is equal with the list of Int constants " + sampleList) {
      result should equal(sampleList)
    }

    it("is not equal with the list of String constants " + sampleList2) {
      result should not equal sampleList2
    }
  }

}