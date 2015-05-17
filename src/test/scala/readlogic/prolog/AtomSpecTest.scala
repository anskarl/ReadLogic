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

import org.scalatest._


class AtomSpecTest extends FunSpec with Matchers {


  val parser = new PrologParser()


  /**
   * Parse atomic formulas, where each one is composed of two terms (one integer and one numeric).
   * Four cases:
   */
  val groundAtomArgs = List(
    (Constant("foo"), Constant(10)), // string and positive integer
    (Constant("foo"), Constant(-10)), // string and negative integer
    (Constant("foo"), Constant(10.1)), // string and positive double precision floating point
    (Constant("foo"), Constant(-10.1))) // string and negative double precision floating point

  for ((arg0, arg1) <- groundAtomArgs) {
    val strHappensAt = "happensAt(" + arg0.symbol + ", " + arg1.symbol + ")"
    val atomHappensAt = Atom("happensAt", List(arg0, arg1))
    val result = parser.parseAtom(strHappensAt)
    val term0 = result.args.head
    val term1 = result.args.last

    describe("The sentence: '" + strHappensAt + "'") {
      it("is an atomic formula 'happensAt(" + arg0 + ", " + arg1 + ")'") {
        result should equal(atomHappensAt)
        result.toText should equal(atomHappensAt.toText)
      }

      it("prints as '" + strHappensAt + "'") {
        result.toText should be(strHappensAt)
      }

      arg1.symbol match {
        case _: Long =>
          it("has two constants, '" + arg0.symbol + "' (string) and '" + arg1.symbol + "' (long)") {
            result.constants should equal(Set[Constant](arg0, arg1))
          }
        case _: Double =>
          it("has two constants, '" + arg0.symbol + "' (string) and '" + arg1.symbol + "' (double)") {
            result.constants should equal(Set[Constant](arg0, arg1))
          }
        case _ => sys.error("Unknown test case.")
      }

      it("has no variables") {
        result.variables.isEmpty should be(true)
      }

      it("has no functions") {
        result.functions.isEmpty should be(true)
      }

      it("is ground") {
        result.isGround should be(true)
      }
    }
  }

  /**
   * Parse an atomic formula with one constant and one variable
   */
  describe("The sentence: 'happensAt(foo, X)'") {
    val strHappensAt = "happensAt(foo, X)"
    val atomHappensAt = Atom("happensAt", List(Constant("foo"), Variable("X")))
    val result = parser.parseAtom(strHappensAt)
    val term0 = result.args.head
    val term1 = result.args.last

    it("is an atomic formula 'happensAt(Constant(foo), Variable(X))'") {
      assert(result == atomHappensAt)
    }

    it("prints as 'happensAt(foo, X)'") {
      result.toText should equal(strHappensAt)
      result.toText should equal(atomHappensAt.toText)
    }

    it("has one constant, i.e., first term is 'foo' (type of String) and one variable, i.e., second term is 'X'") {
      result.constants should equal(Set[Constant](Constant("foo")))
      result.variables should equal(Set[Variable](Variable("X")))

      term0.isConstant should be(true)
      term0.isVariable should be(false)
      term0.isFunction should be(false)

      term1.isVariable should be(true)

      assert(term0.symbol.isInstanceOf[String])
      assert(!term0.symbol.isInstanceOf[Double])

      assert(term1.symbol.isInstanceOf[String])
      assert(!term1.symbol.isInstanceOf[Long])

    }

    it("is not ground, since it contains one variable") {
      result.isGround should be(false)
    }
  }


  /**
   * Parse an atomic formulas with one function and one constant.
   * (instance of TermFunction, string representation of the function, no. of constants, no. of variables)
   */
  val functionsToTest = List(
    // happensAt(walking(), 1)            : 1 constant and 0 variables
    (TermFunction("walking", Nil), "walking()", 1, 0),

    // happensAt(walking(mike), 1)        : 2 constants and 0 variables
    (TermFunction("walking", List(Constant("mike"))), "walking(mike)", 2, 0),

    // happensAt(walking(X), 1)           : 1 constant and 1 variable
    (TermFunction("walking", List(Variable("X"))), "walking(X)", 1, 1),

    // happensAt(walking()=true, 1)       : 2 constants and 0 variables
    (TermFunction("walking", Nil, List(Constant(true))), "walking()=true", 2, 0),

    // happensAt(walking(mike)=true, 1)   : 3 constants and 0 variables
    (TermFunction("walking", List(Constant("mike")), List(Constant(true))), "walking(mike)=true", 3, 0),

    // happensAt(walking(X)=fast, 1)      : 2 constants and 1 variable
    (TermFunction("walking", List(Variable("X")), List(Constant("fast"))), "walking(X)=fast", 2, 1),

    // happensAt(walking(X)=Y, 1)         : 1 constant and 2 variables
    (TermFunction("walking", List(Variable("X")), List(Variable("Y"))), "walking(X)=Y", 1, 2),

    // happensAt(walking(X)=(Y, fast), 1) : 2 constants and 2 variables
    (TermFunction("walking", List(Variable("X")), List(Variable("Y"), Constant("fast"))), "walking(X)=(Y, fast)", 2, 2),

    // happensAt(walking(X)=(Y, [forward, fast]), 1) : 3 constants and 2 variables
    (TermFunction("walking", List(Variable("X")), List(Variable("Y"), TermList(List(Constant("forward"), Constant("fast"))))), "walking(X)=(Y, [forward, fast])", 3, 2),

    // happensAt(walking(X, [A, Y])=(Y, [forward, fast]), 1) : 3 constants and 3 variables
    (TermFunction("walking", List(Variable("X"), TermList(List(Variable("A"), Variable("Y")))), List(Variable("Y"), TermList(List(Constant("forward"), Constant("fast"))))), "walking(X, [A, Y])=(Y, [forward, fast])", 3, 3)
  )

  for ((f, str, noConstants, noVariables) <- functionsToTest) {

    val strHappensAt = "happensAt(" + str + ", 1)"
    val atomHappensAt = Atom("happensAt", List(f, Constant(1)))

    val result = parser.parseAtom(strHappensAt)

    describe("The sentence: '" + strHappensAt + "'") {
      it("is an atomic formula '" + atomHappensAt + "'") {
        assert(result == atomHappensAt)
      }

      it("prints as '" + strHappensAt + "'") {
        result.toText should equal(strHappensAt)
        result.toText should equal(atomHappensAt.toText)
      }

      it("has '" + noConstants + "' constant(s)") {
        result.constants.size should equal(noConstants)
        result.constants should equal(atomHappensAt.constants)
      }

      it("has '" + noVariables + "' variable(s)") {
        result.variables.size should equal(noVariables)
        result.variables should equal(atomHappensAt.variables)
      }

      if (noVariables > 0) it("is ground") {
        result.isGround should not be (true)
      }
      else it("is not ground") {
        result.isGround should be(true)
      }

    }
  }


  /**
   * Parse an atomic formula with one constant and four variables and a list of terms
   */
  describe("The sentence: 'holdsFor(foo(X, Y)=bar, [T1, T2])'") {
    val sentence = "holdsFor(foo(X, Y)=bar, [T1, T2])"

    val function = TermFunction("foo", List(Variable("X"), Variable("Y")), List(Constant("bar")))

    val list = TermList(List(Variable("T1"), Variable("T2")))

    val atomHappensAt = Atom("holdsFor", List(function, list))

    val result = parser.parseAtom(sentence)

    it("is an atomic formula 'holdsFor(foo(X, Y)=bar, [T1, T2])'") {
      assert(result == atomHappensAt)
    }

    it("prints as 'holdsFor(foo(X, Y)=bar, [T1, T2])'") {
      result.toText should equal(sentence)
      result.toText should equal(atomHappensAt.toText)
    }

    it("has one constant (bar), four variables (X, Y, T1, T2), one function (foo(X, Y)=bar) and one list of term [T1, T2]") {
      result.constants.size should be(1)
      result.constants should equal(Set[Constant](Constant("bar")))

      result.variables.size should be(4)
      result.variables should equal(
        Set[Variable](Variable("X"), Variable("Y"), Variable("T1"), Variable("T2"))
      )

      result.functions.size should be(1)
      result.functions.contains(function)

      val termLists = result.args.filter(_.isList)
      termLists.size should be(1)
      termLists.head should be(list)
    }

    it("is not ground, since it contains one variable") {
      result.isGround should be(false)
    }
  }

  /**
   * Parse an atomic formula with one constant and four variables and a list of terms
   */
  describe("The sentence: 'holdsFor(foo(X, Y)=bar, [H | T])'") {
    val inputSentence = "holdsFor(foo(X, Y)=bar, [H | T])"
    val printSentence = "holdsFor(foo(X, Y)=bar, [H, T])"

    val function = TermFunction("foo", List(Variable("X"), Variable("Y")), List(Constant("bar")))

    val list = TermList(List(Variable("H"), Variable("T")))

    val atomHappensAt = Atom("holdsFor", List(function, list))

    val result = parser.parseAtom(inputSentence)

    it("is an atomic formula 'holdsFor(foo(X, Y)=bar, [H | T])'") {
      assert(result == atomHappensAt)
    }

    it("prints as '" + printSentence + "'") {
      result.toText should equal(printSentence)
      result.toText should equal(atomHappensAt.toText)
    }

    it("has one constant (bar), four variables (X, Y, H, T), one function (foo(X, Y)=bar) and one list of term [T1, T2]") {
      result.constants.size should be(1)
      result.constants should equal(Set[Constant](Constant("bar")))

      result.variables.size should be(4)
      result.variables should equal(
        Set[Variable](Variable("X"), Variable("Y"), Variable("H"), Variable("T"))
      )

      result.functions.size should be(1)
      result.functions.contains(function)

      val termLists = result.args.filter(_.isList)
      termLists.size should be(1)
      termLists.head should be(list)
    }

    it("is not ground, since it contains one variable") {
      result.isGround should be(false)
    }
  }

  describe("The sentence: happensAtIE(move(38072, '83', 'HN', -298), 1357086183000000)") {
    val inputSentence = "happensAtIE(move(38072, '83', 'HN', -298), 1357086183000000)"
    val function = TermFunction("move", List(LongConstant(38072), StringConstant("\'83\'"), StringConstant("\'HN\'"), LongConstant(-298)))

    val atomHappensAt = Atom("happensAtIE", List(function, LongConstant(1357086183000000L)))
    val result = parser.parseAtom(inputSentence)

    it("is a valid atomic formula") {
      assert(result.toText == atomHappensAt.toText)
      assert(result == atomHappensAt)
    }

  }


  describe("The sentence: happensAtIE(move(38072, '-83', 'HN', -298), 1357086183000000)") {
    val inputSentence = "happensAtIE(move(38072, '-83', 'HN', -298), 1357086183000000)"
    val function = TermFunction("move", List(LongConstant(38072), StringConstant("\'-83\'"), StringConstant("\'HN\'"), LongConstant(-298)))

    val atomHappensAt = Atom("happensAtIE", List(function, LongConstant(1357086183000000L)))
    val result = parser.parseAtom(inputSentence)

    it("is a valid atomic formula") {
      assert(result.toText == atomHappensAt.toText)
      assert(result == atomHappensAt)
    }

  }


  describe("The sentence: holdsForIESI( abrupt_acceleration(75, bus)=abrupt, (3, 5))") {
    val inputSentence = "holdsForIESI( abrupt_acceleration(75, bus)=abrupt, (3, 5))"
    val term1 = TermFunction("abrupt_acceleration", List(LongConstant(75), StringConstant("bus")), List(StringConstant("abrupt")) )
    val term2 = TermFunction("", List(LongConstant(3), LongConstant(5)) )

    val atomHappensAt = Atom("holdsForIESI", List(term1, term2))
    val result = parser.parseAtom(inputSentence)

    it("is a valid atomic formula") {
      assert(result.toText == atomHappensAt.toText)
      assert(result == atomHappensAt)
    }

  }


}
