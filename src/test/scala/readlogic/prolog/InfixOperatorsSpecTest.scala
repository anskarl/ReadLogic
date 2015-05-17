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

import org.scalatest.{Matchers, FunSpec}

class InfixOperatorsSpecTest extends FunSpec with Matchers {
  val parser = new PrologParser()

  /**
   * Infix relational operators (rewrite)
   */
  val listRelOps = List(
    ("=:=", "equals"),
    ("""=\=""", "not_equals"),
    ("<", "lessThan"),
    (">", "greaterThan"),
    ("=<", "lessThanEq"),
    (">=", "greaterThanEq")
  )
  val leftTermStr = "X"
  val rightTermStr = "Y"
  val leftTerm = Variable(leftTermStr)
  val rightTerm = Variable(rightTermStr)

  for((opSymbol: String, rewrittenSymbol: String) <- listRelOps) {

    val expression = leftTermStr+" "+opSymbol+" "+rightTermStr
    val rewrittenExpression = rewrittenSymbol+"("+leftTermStr+", "+rightTermStr+")"
    val resultAtom = parser.parseAtom(expression)
    val atom = Atom(rewrittenSymbol, List(leftTerm, rightTerm))

    describe("The expression '" + expression + "' with the infix relational operator '" + opSymbol + "'") {
      it("is rewritten as the utility predicate '"+rewrittenExpression+"'"){
        assert(resultAtom == atom)
        assert(resultAtom.toText == rewrittenExpression)
      }
    }
  }
}
