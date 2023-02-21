package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case object Empty extends RegularLanguage
case object Epsilon extends RegularLanguage
case class Character(c: Char) extends RegularLanguage
case class Union(l1: RegularLanguage, l2: RegularLanguage) extends RegularLanguage
case class Concat(l1: RegularLanguage, l2: RegularLanguage) extends RegularLanguage
case class Star(l: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage =
  lang match
    case Concat(Epsilon, l) => simplify(l)
    case Concat(l, Epsilon) => simplify(l)
    case Concat(Empty, _) => simplify(Empty)
    case Concat(_, Empty) => simplify(Empty)
    case Concat(l1, l2) => Concat(simplify(l1), simplify(l2))
    case Union(Empty, l) => simplify(l)
    case Union(l, Empty) => simplify(l)
    case Union(l1, l2) => Union(simplify(l1), simplify(l2))
    case Star(Epsilon) => Epsilon
    case Star(Empty) => Empty
    case Star(l) => Star(simplify(l))
    case _ => lang

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean =
  simplify(lang) match
    case Epsilon => true
    case Union(l1, l2) => nullable(l1) | nullable(l2)
    case Concat(l1, l2) => nullable(l1) & nullable(l2)
    case Star(_) => true
    case _ => false
  

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage =
  simplify(l) match
    case Empty => Empty
    case Epsilon => Empty
    case Character(d) => if c == d then Epsilon else Empty
    case Union(l1, l2) => Union(derivative(l1)(c), derivative(l2)(c))
    case Concat(l1, l2) => if nullable(l1) then {
      Union(Concat(derivative(l1)(c), l2), derivative(l2)(c))
    } else Concat(derivative(l1)(c), l2)
    case Star(l) => Concat(derivative(l)(c), Star(l))
  

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
