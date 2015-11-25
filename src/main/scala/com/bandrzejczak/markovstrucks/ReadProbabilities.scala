package com.bandrzejczak.markovstrucks

import scala.annotation.tailrec
import scala.collection.immutable.Map

case class ReadProbabilities(correctCharacter: Char, mistakes: Map[Char, Double]) {
  private val probabilitiesSum: Double = mistakes.values.sum
  require(0 <= probabilitiesSum && probabilitiesSum <= 1)

  def read(): Char = {
    val possibleOutcomes = mistakes + (correctCharacter -> (1 - probabilitiesSum))
    chooseCharacter(possibleOutcomes.toList, Math.random())
  }

  @tailrec
  private def chooseCharacter(possibleOutcomes: List[(Char, Double)], valueLeft: Double): Char = possibleOutcomes match {
    case (character, _) :: Nil => character
    case (character, probability) :: rest =>
      if(valueLeft - probability <= 0)
        character
      else
        chooseCharacter(rest, valueLeft - probability)
    case _ => throw new RuntimeException("Cannot read a character")
  }
}

object ReadProbabilities {
  type MistakesModel = Map[Char, ReadProbabilities]
  def emptyMistakesModel: MistakesModel = Map()
}
