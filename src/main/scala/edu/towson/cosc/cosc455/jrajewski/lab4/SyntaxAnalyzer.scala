package edu.towson.cosc.cosc455.jrajewski.lab4


class SyntaxAnalyzer {

  // For ease, store the terminal literals in a List
  val ARTICLES : List[String] = List("teh", "a")
  val VERBS : List[String] = List("ates", "lovez", "hatez")
  val NOUNS : List[String] = List("kat", "dawg", "rat")
  val ADJECTIVES : List[String] = List("fat", "hungry", "happy", "mean")
  val ADVERBS : List[String] = List("accidently", "quickly", "secretly")

  // Flag for errors and helper methods
  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError : Boolean = errorFound


  // This method implements the BNF rule for a sentence <S> ::= <NP> <V> <NP>
  def Sentence() = {
    resetError()
    if(!errorFound) NounPhrase()
    if(!errorFound) Adverb()
    if(!errorFound) Verb()
    if(!errorFound) NounPhrase()
  }

  // This method implements the BNF rule for a noun phrase <NP> ::= <A> <N>
  def NounPhrase() = {
    if(!errorFound) Article()
    if(!errorFound) Adjective()
    if(!errorFound) Noun()
  }


  // Implementation for BNF rule for an adjective <adjective> :: fat | hungry | happy | mean
  def Adjective() = {
    if(ADJECTIVES contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
  }

  def Adverb() = {
    if(ADVERBS contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else{
      //No need for error message here because it is optional to have an adverb, so the lexical analyzer will
      //just move onto the next token
      // println("SYNTAX ERROR - An adverb was expected when '" + Compiler.currentToken + "' was found.")
      //setError()
    }
  }

  // This method implements the BNF rule for a verb <V> ::= ates | hatez | hatez
  def Verb() = {
    if (VERBS contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
      println("SYNTAX ERROR - A verb was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  // This method implements the BNF rule for a noun <N> ::= dawg | kat | rat
  def Noun() = {
    if (NOUNS contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
        println("SYNTAX ERROR - A noun was expected when '" + Compiler.currentToken + "' was found.")
        setError()
      }
  }

  // This method implements the BNF rule for an article <N> ::= teh | a
  def Article() = {
    if (ARTICLES contains Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    else {
        println("SYNTAX ERROR - An article was expected when '" + Compiler.currentToken + "' was found.")
        setError()
      }
  }
}
