import java.util
import java.util.regex.Pattern
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    val operators = List('|','*','+', '.', '?','(',')')
    if(s.isEmpty)
      return List()

    if(operators.contains(s.head)){
      preprocess(s.tail).appended(Right(s.head))
    }else{
      preprocess(s.tail).appended(Left(s.head))
    }

  }

  def isOpp(opp: Char): Boolean = {
    val operators = List('|', '*', '.', '+', '?', '(', ')')
    operators.contains(opp)
  }
  def isOpp2(opp: Char): Boolean = {
    val operators = List('|', '*', '.', '+', '?', ')')
    operators.contains(opp)
  }

  def evaluateOpp(opp: String): String = {
    opp match {
      case "|" => "UNION "
      case "." => "CONCAT "
      case "*" => "STAR "
      case "+" => "PLUS "
      case "?" => "MAYBE "
      case _ => opp
    }
  }

  def priorityOpp(opp: String): Int = {
    opp match {
      case "|" => 1
      case "." => 2
      case "*" => 3
      case "+" => 3
      case "?" => 3
      case _ => 0
    }
  }
  def withConcat(s1: String): String = {
    val s2 = s1.foldLeft(('_',"")){
      case ((p,acc),c) if ((!isOpp(p) && !isOpp(c)) || (")*?+".contains(p) && !isOpp2(c)) || (!isOpp(p) && c == '('))
        && acc.nonEmpty => ( c , s"$acc.$c")
      case ((_,acc),c)               => ( c , s"$acc$c")
    }._2
    s2
  }

  def fixRegex(regex: String): String = {
    val operators = List('|', '*', '.', '+', '?', '(', ')')
    var regex1 = regex
    operators.foreach{
      x =>
        regex1 = regex1.replace(s"$x.'", s"$x'").replace(s"\'.$x", s"\'$x")
    }
    regex1
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val caps = "(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|Q|Y|Z)"
    val letters = "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|q|y|z)"
    val nums = "(0|1|2|3|4|5|6|7|8|9)"

    if(str == "eps")
      return str

    val s1 = str.replace("[0-9]", nums).replace("[a-z]", letters).replace("[A-Z]", caps)

    val matchList = new util.ArrayList[String]
    val reg = Pattern.compile("[^\\s\"']+|('[^']*')")
    val regexMatcher = reg.matcher(s1)
    while ( {
      regexMatcher.find
    }) if (regexMatcher.group(1) != null) { //de forma '...'
      matchList.add(regexMatcher.group(1))
    }
    else {
      matchList.add(withConcat(regexMatcher.group(0)))
    }

    val regex = fixRegex(matchList.toList.mkString(".")).reverse
    val stack = new mutable.Stack[String]

    def aux(infix: String): String ={
      if(infix.isEmpty)
        return ""

      var elem = infix.head

      if(!isOpp(elem)) {
        if (elem == '\'') {
          var op = " \'"
          var infix2 = infix.tail
          while (infix2.nonEmpty && infix2.head != '\'') {
            op = op.concat(s"${infix2.head}")
            infix2 = infix2.tail
          }
          return aux(infix2.tail).concat(s"$op\' ")
        } else {
          return aux(infix.tail).concat(elem +" ")
        }
      }

      //operator
      if (elem == ')') {
        stack.push(elem + "")
        return aux(infix.tail)
      }

        //if ‘)’, pop and output from the stack until '('
      if (elem == '(') {
        var op = ""
        while (stack.nonEmpty && stack.top.head != ')') {
          op = evaluateOpp(stack.pop()).concat(op)
        }
        stack.pop()
        return aux(infix.tail).concat(op)
      }

      var res = ""
      if (stack.nonEmpty && priorityOpp(elem+"") < priorityOpp(stack.top)) {
        while (stack.nonEmpty && priorityOpp(elem + "") < priorityOpp(stack.top)) {
          res = res.concat(evaluateOpp(stack.pop()))
        }
        stack.push(elem + "")
        return aux(infix.tail).concat(res)
      }
      if (priorityOpp(elem+"") == 3) {
        while (stack.nonEmpty && priorityOpp(elem+"") < priorityOpp(stack.top)) {
          res = res.concat(evaluateOpp(stack.pop()))
        }
        stack.push(elem + "")
        return aux(infix.tail).concat(res)

      } else{
        stack.push(elem + "")
        return aux(infix.tail)
      }
    }

    var x = aux(regex)
    var res = ""
    while(stack.nonEmpty)
      res = evaluateOpp(stack.pop()).concat(res)

    res.concat(x).replace("  ", " ")
  }
}
