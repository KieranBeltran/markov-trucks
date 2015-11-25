package com.bandrzejczak.markovstrucks

import scala.collection.immutable.Map

case class ReadProbabilities(correctCharacter: Char, mistakes: Map[Char, Double]) {
  private val probabilitiesSum: Double = mistakes.values.sum
  require(0 <= probabilitiesSum && probabilitiesSum <= 1)

  def read(): Char = {
    val possibleOutcomes = mistakes + (correctCharacter -> (1 - probabilitiesSum))
    chooseCharacter(possibleOutcomes.toList, Math.random())
  }

  private def chooseCharacter(possibleOutcomes: List[(Char, Double)], valueLeft: Double): Char = possibleOutcomes match {
    case (character, _) :: Nil => character
    case (character, probability) :: rest =>
      if(valueLeft - probability <= 0)
        character
      else
        chooseCharacter(rest, valueLeft - probability)
  }
}

object ReadProbabilities {
  type MistakesModel = Map[Char, ReadProbabilities]
  def emptyMistakesModel: MistakesModel = Map()
}
