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

final class RuleSpecTest extends FunSpec with Matchers {

  private val parser = new PrologParser()

  private def asRuleExpression(head: String, body: String) = head + " :- " + reformat(body) + "."

  /**
   * Basic rule parsing
   */
  val ruleList = List(
    """
      |initiatedAt(foo(X, Y)=value, T) :-
      | happensAt(a(X), T),
      | happensAt(b(Y), T).
    """.stripMargin,

    """
      |initiatedAt(foo(X, Y)=value, T) :-
      | happensAt(a(X), T),
      | not happensAt(b(Y), T).
    """.stripMargin,

    """
      |initiatedAt(foo(X, Y)=value, T1) :-
      | happensAt(a(X), T1),
      | \+ happensAt(b(Y), T1).
    """.stripMargin,

    """
      |holdsFor(foo(X, Y)=bar, [T1, T2]) :-
      | holdsAt(foo(X, Y)=bar, T1),
      | \+ holdsAt(foo(X, Y)=bar, T2).
    """.stripMargin,

   """
      |holdsFor(foo(X, Y)=bar, [H | T]) :-
      | holdsAt(foo(X, Y)=bar, H),
      | \+ holdsAt(foo(X, Y)=bar, T).
    """.stripMargin,

    """
      |holdsFor(foo(X, Y)=bar, [T1, T2]) :-
      | holdsAt(foo(X, Y)=bar, T1),
      | \+ (holdsAt(foo(X, Y)=bar, T2), happensAt(something, T2)).
    """.stripMargin,

    """
      |holdsFor(foo(X, Y)=bar, [T1, T2]) :-
      | holdsAt(foo(X, Y)=bar, T1),
      | not (holdsAt(foo(X, Y)=bar, T2), happensAt(something1(X), T1), happensAt(something2(Y), T2)).
    """.stripMargin
  )

  for( rule <- ruleList ) {

    val ruleExpression = reformat(rule)
    val splitted = ruleExpression.split(":-")

    assert(splitted.length == 2, "The given rule is invalid: "+ruleExpression)

    val headExpression = splitted(0).trim
    val bodyExpression = splitted(1).trim.stripSuffix(".")

    describe("The following expression:\n '" + ruleExpression + "'") {

      val rule = parser.parseRule(ruleExpression)

      it("is a rule with head '" + headExpression + "'") {
        rule.head.toText should be(headExpression)
      }

      it("is a rule with body '" + bodyExpression + "'") {
        rule.body.toText should be(bodyExpression)
      }

      it("prints the same rule expression") {
        rule.toText should be(ruleExpression)
      }
    }
  }

}
