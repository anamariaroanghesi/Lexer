import scala.math.Numeric.IntIsIntegral

case class Lexer(spec: String) {
  def DfaLength(word: String, dfa:Dfa[Int]): Int = {
    var length = word.length
    while(length >=0 ){
      if(dfa.accepts(word.substring(0,length)))
        return length
      length = length - 1
    }
    length + 1
  }



  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String, List[(String, String)]] = {
    //extragere (tokens, regex) din spec
    val lines = spec.split(";\n").toList
    val tokensAndReg =  lines.map(_.split(": ") match { case Array(a, b) => (a, b) }) //(token, regex)
    val tokensAndDfa = tokensAndReg.map(x => (x._1, Dfa.fromPrenex(Regex.toPrenex(x._2))))  //(token, Dfa)

    def aux(word:String):List[(String, String)] = {
      if(word.isEmpty) {
        return List()
      }

      val tokensDfaAndLen = tokensAndDfa.map(x => (x._1, x._2, DfaLength(word, x._2)))
      val maxLen = tokensDfaAndLen.maxBy{x => x._3}._3
      if(maxLen == 0)
        return List()
      val token = tokensDfaAndLen.filter(x => x._3 == maxLen).head._1

      aux(word.substring(maxLen)).appended((word.substring(0,maxLen), token))
    }

    val res = aux(word).reverse
    Right(res)
  }
}
