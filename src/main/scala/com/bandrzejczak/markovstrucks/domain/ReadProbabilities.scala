package com.bandrzejczak.markovstrucks.domain

import scala.annotation.tailrec
import scala.collection.immutable.Map

case class ReadProbabilities(correctCharacter: Char, mistakes: Map[Char, Double]) {
  private val mistakesProbabilitiesSum: Double = mistakes.values.sum
  lazy val possibleOutcomes = mistakes + (correctCharacter -> (1 - mistakesProbabilitiesSum))
  require(0 <= mistakesProbabilitiesSum && mistakesProbabilitiesSum <= 1)

  def read(): Char = {
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
  type ObservationModel = Map[Char, ReadProbabilities]
  def emptyObservationModel: ObservationModel = Map()

  implicit class ObservationModelOps(observationModel: ObservationModel) {
    def probabilityOf(char: Char) = {
      new ProbabilityOf(observationModel, char)
    }
  }

  class ProbabilityOf(observationModel: ObservationModel, char: Char) {
    def inState(state: Char) = {
      observationModel.get(state).map(_.possibleOutcomes.getOrElse(char, 0.0)).getOrElse(if(char == state) 1.0 else 0.0)
    }
  }
}
