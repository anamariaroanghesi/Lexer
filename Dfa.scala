import scala.collection.mutable
import scala.util.control.Breaks.break

class Dfa[A] (var alphabet: List[Char], var q: Set[A], var init: A, var fin: Set[A], var funct: mutable.HashMap[(A, String), A]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    new Dfa[B](
      alphabet,
      q.map(f),
      f(init),
      fin.map(f),
      funct.map( (x:((A, String), A)) => ((f(x._1._1),x._1._2), f(x._2) ))
    )
  } // TODO implement map

  def next(state:A, c: Char): A = {
    funct.getOrElse((state, s"$c"), state)
  } // TODO implement next

  def accepts(str: String): Boolean = {

    def aux(state:A, str:String): Boolean = {
      if(str.isEmpty && isFinal(state))
        return true

      if(str.isEmpty)
        return false
      if(alphabet.contains(str.head)) {
        val x = next(state, str.head)
        aux(x, str.tail)
      }else
        false
    }
    aux(init, str)
  } // TODO implement accepts

  def getStates : Set[A] = q // TODO implement getStates

  def isFinal(state: A): Boolean = {
    fin.contains(state)
  }  // TODO implement isFinal

  override def toString: String = {
    val x = ""
    funct.keys.foreach{
      i =>
        println(s"Key = (${i._1}, ${i._2}): " + funct(i))
    }
    init + ""
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    val nfa = Nfa.fromPrenex(str)
    toDfa(nfa)
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa

  // You can add more methods to this object
  def isFin(nfa:Nfa[Int], state:Set[Int]): Boolean = {
    state.contains(nfa.fin)
  }

  def toDfa(nfa:Nfa[Int]):Dfa[Int] = {
    val init2 = nfa.epsClosure(nfa.init)
    val funct2 = new mutable.HashMap[(Set[Int], String), Set[Int]]()
    var q:Set[Set[Int]] = Set()
    var finall:Set[Set[Int]] = Set()
    if(isFin(nfa, init2))
      finall = finall + init2

    def visit(init2:Set[Int]): Unit = {
      if (!q.contains(init2)){
        q = q + init2

        nfa.alphabet.foreach {
          a =>
            var aux: Set[Int] = Set()

            //toate tranzitiile pe a
            init2.foreach { s => aux = aux ++ nfa.funct.getOrElse((s, s"$a"), Set())}

            if (aux.isEmpty) {
              funct2.addOne(((init2, s"$a"), Set()))
              q = q + Set()
            } else {
              var closure: Set[Int] = Set()
              aux.foreach { s => closure = closure ++ nfa.epsClosure(s)}
              funct2.addOne(((init2, s"$a"), closure))
              visit(closure)
              if(isFin(nfa, closure))
                finall = finall + closure

            }
        }
      }
    }
    visit(init2)
    if(q.contains(Set())) {
      nfa.alphabet.foreach(a => funct2.addOne((Set(),s"$a"), Set()))
    }
    val q2 = q.toList
    new Dfa(nfa.alphabet, q, init2, finall, funct2).map[Int](x => q2.indexOf(x))
  }
}
