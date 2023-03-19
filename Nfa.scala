import java.util
import java.util.regex.Pattern
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable

class Nfa[A](var alphabet: List[Char], var q: Set[A], var init: A, var fin: A, var funct: mutable.HashMap[(A, String), Set[A]]) {

  def epsClosure(state:A): Set[A] = {
    var res: Set[A] = Set(state)
    funct.getOrElse((state, "eps"), Set()).foreach{
      s =>
          res = res ++ epsClosure(s)
    }
    res
  }
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
  def map[B](f: A => B) : Nfa[B] = {
    new Nfa[B](
      alphabet,
      q.map(f),
      f(init),
      f(fin),
      funct.map( (x:((A, String), Set[A])) => ((f(x._1._1),x._1._2), x._2.map(f)) )
    )
  } // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    funct.getOrElse((state, s"$c"), Set())
  }// TODO implement next

  def accepts(str: String): Boolean = {
    auxAccept(init, str)

  }//TODO implement accepts
  def auxAccept(state: A, str:String): Boolean = {
    if(str.isEmpty && isFinal(state)) {
      println("true")
      return true
    }
    val y = funct.getOrElse((state, "eps"), Set())
    for(s <- y){
      if(auxAccept(s, str))
        return true
    }
    if(str.isEmpty)
      return false

    val x = funct.getOrElse((state, str.head.toString), Set())

    for(s <- x){
      if(auxAccept(s, str.tail))
        return true
    }

    false
  }

  def getStates : Set[A] = q

  def isFinal(state: A): Boolean = fin == state  // TODO implement isFinal

  override def toString: String = {
    val x = ""
    funct.keys.foreach{
      i =>
        println(s"Key = (${i._1}, ${i._2}): " + funct(i))
    }
    init + ""
  }

}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  // TODO implement Prenex -> Nfa transformation.
  def fromPrenex(str: String): Nfa[Int] = {
    val stack1 = new mutable.Stack[Nfa[Int]]()
    val matchList = new util.ArrayList[String]
    val regex = Pattern.compile("[^\\s\"']+|\"([^\"]*)\"|'(')'|'([^']*)'")
    val regexMatcher = regex.matcher(str)
    while ( {
      regexMatcher.find
    }) if (regexMatcher.group(1) != null) { // Add double-quoted string without the quotes
      matchList.add(regexMatcher.group(1))
    }
    else if (regexMatcher.group(2) != null) {
      matchList.add(regexMatcher.group(2))
    }
    else if (regexMatcher.group(3) != null) {
      matchList.add(regexMatcher.group(3))
    }
    else { // Add unquoted word
      matchList.add(regexMatcher.group)
    }
    var x = matchList.toList.reverse

    def aux(l: List[String], state: Int):Int = {
      if(l.isEmpty) {
        state
      }else if(l.head == "eps") {
        stack1.push(eps(state))
        aux(l.tail, state+1)
      }
      else if(l.head == "void") {
        stack1.push(void(state,state+1))
        aux(l.tail, state+2)
      }
      else if(l.head == "STAR") {
        stack1.push(star(stack1.pop(), state, state+1))
        aux(l.tail, state+2)
      }
      else if(l.head == "UNION") {
        stack1.push(union(stack1.pop(), stack1.pop(), state, state+1))
        aux(l.tail, state+2)
      }
      else if(l.head == "CONCAT") {
        stack1.push(concat(stack1.pop(), stack1.pop()))
        aux(l.tail, state)
      }
      else if(l.head == "PLUS") {
        val nfa = stack1.pop()
        stack1.push(plus(nfa, state, state+1, x => x +state+2 ))
        aux(l.tail, state + nfa.q.knownSize + 2)
      }
      else if(l.head == "MAYBE") {
        val nfa = stack1.pop()
        stack1.push(maybe(nfa, state, state+1))
        aux(l.tail, state + 4)
      }
      else {
        stack1.push(char(state, state+1, l.head))
        aux(l.tail, state+2)
      }
    }

    aux(x, 0)
    stack1.pop()
  }

  // You can add more methods to this object
  def eps(init: Int): Nfa[Int] = {
    val q = Set(init)
    val f =  new mutable.HashMap[(Int, String), Set[Int]]()
    new Nfa(List(), q, init, init, f)
  }
  def void(init: Int, fin: Int): Nfa[Int] = {
    val q = Set(init, fin)
    val f = new mutable.HashMap[(Int, String), Set[Int]]()
    new Nfa(List(), q, init, fin, f)
  }
  def char(init:Int, fin:Int, c:String): Nfa[Int] = {
    val states = Set(init, fin)
    val f = new mutable.HashMap[(Int, String), Set[Int]]().addOne((init, c),Set(fin))
    new Nfa(c.toList, states, init, fin, f)
  }

  def concat(nfa1:Nfa[Int], nfa2:Nfa[Int]): Nfa[Int] = {
    val alph = nfa1.alphabet.appendedAll(nfa2.alphabet).distinct
    val q = nfa1.q.++(nfa2.q)
    val init = nfa1.init
    val fin = nfa2.fin
    val f = nfa1.funct.addAll(nfa2.funct).addOne((nfa1.fin,"eps"),Set(nfa2.init))
    new Nfa(alph, q, init, fin, f)
  }

  def union(nfa1:Nfa[Int], nfa2:Nfa[Int], init:Int, fin:Int): Nfa[Int] = {
    val alph = nfa1.alphabet.appendedAll(nfa2.alphabet).distinct
    val q = nfa1.q.++(nfa2.q)++Set(init, fin)
    val f = nfa1.funct.addAll(nfa2.funct).addOne((init,"eps"),Set(nfa1.init, nfa2.init))
    f.addOne((nfa1.fin, "eps"), Set(fin)).addOne((nfa2.fin, "eps"), Set(fin))
    new Nfa(alph, q, init, fin, f)
  }

  def star(nfa:Nfa[Int], init:Int, fin:Int): Nfa[Int] = {
    val q = nfa.q++Set(init, fin)
    val f = nfa.funct.addOne((init, "eps"), Set(nfa.init, fin)).addOne((nfa.fin, "eps"), Set(nfa.init, fin))
    new Nfa(nfa.alphabet, q, init, fin, f)
  }
  def plus(nfa:Nfa[Int], init:Int, fin:Int, f: Int => Int): Nfa[Int] = {
    concat(nfa.map(f), star(nfa, init, fin))
  }
  def maybe(nfa:Nfa[Int], init:Int, fin:Int): Nfa[Int] = {
    union(nfa, eps(fin+2), init, fin)
  }
}