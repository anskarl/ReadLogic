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

/**
 * Atom signature defines uniquely an atom. A signature is composed of the atom's name and
 * atom's arity (i.e., the number of its arguments).
 *
 * @example {{{
 *  HappensAt(event, time)
 *  HoldsAt(fluent, time)
 * }}}
 *
 * In the above example, the atom signature of ''HappensAt(event, time)'' is Happens/2, while
 * the signature of ''HoldsAt(fluent, time)'' is HoldsAt/2.
 * <br/>
 * <br/>
 *
 * The same signature representation can be used of FOL functions.
 *
 * @example {{{
 *   HappensAt(walking(person1), time)
 *   HoldsAt(meeting(X,Y)=SomeVal, time)
 * }}}
 *
 * The predicate HappensAt/2, contains the function (representing an event) ''walking(person1)'',
 * which has the atom signature walking/1. Similarly, the predicate HoldsAt/2 contains the
 * function (representing a multivalued fluent) ''meeting(X,Y)=SomeVal'', with atom signature
 * meeting/3 (i.e., symbol: meeting and the arguments X,Y and Val).
 *
 * @param symbol atom's/function's name (e.g., predicates ''Happens'', ''HoldsAt'',
 *               as well as functions ''walking'' and ''meeting'').
 *
 * @param arity atom's arity (e.g., both example predicates have 2 arguments).
 *
 * @author Anastasios Skarlatidis
 */
final class AtomSignature private(val symbol: String, val arity: Int) extends Serializable {

  require(symbol != null, "Cannot use null string as symbol.")
  require(symbol.length > 0, "Cannot use an empty string as symbol.")
  require(arity >=0, "The arity of an atom cannot be a negative number.")

  /**
   * Hash code is a simple XOR between symbol's hash and arity number
   */
  lazy val hash = symbol.hashCode ^ arity

  override def equals(obj: Any): Boolean = obj match {
    case other: AtomSignature if other != null && this != null =>
      other.arity == this.arity && other.symbol == this.symbol
    case _ => false
  }

  override def hashCode() = hash

  override def toString = symbol + "/" + arity

}

/**
 * Companion object for AtomSignature
 */
object AtomSignature {

  /**
   * Constructs an atom signature from atom's/function's name and arity.
   *
   * @param symbol atom/function name
   * @param arity number of arguments
   *
   * @return the resulting signature
   */
  def apply(symbol: String, arity: Int): AtomSignature = new AtomSignature(symbol, arity)

}

