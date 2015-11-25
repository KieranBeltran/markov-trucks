package com.bandrzejczak.markovstrucks

import com.bandrzejczak.markovstrucks.StatesModel.{EndState, InitialState}

class StatesModel(registrationNumbers: List[String]) {

  val availableStates = registrationNumbers.foldLeft(Set.empty[Char])(_ ++ _.toSet)

  lazy val stateTransitionProbabilities: Map[Char, Map[Char, Double]] = computeStateTransitionProbabilities()

  private def computeStateTransitionProbabilities(): Map[Char, Map[Char, Double]] = {
    registrationNumbers.foldLeft(Map.empty[Char, Map[Char, Double]]) {
      case (probabilities, registrationNumber) =>
        val transitions = (InitialState + registrationNumber).zip(registrationNumber + EndState)
        transitions.foldLeft(probabilities)(addTransition)
    }
  }

  private def addTransition(probabilities: Map[Char, Map[Char, Double]], transition: (Char, Char)): Map[Char, Map[Char, Double]] = {
    probabilities.get(transition._1) match {
      case Some(p) => probabilities + (transition._1 -> (p + (transition._2 -> 0.0)).mapValues(_ => 1.0 / (p.size + 1)))
      case None => probabilities + (transition._1 -> Map(transition._2 -> 1.0))
    }
  }

  def probabilityOfTransition(from: Char, to: Char): Double = {
    stateTransitionProbabilities.getOrElse(from, Map()).getOrElse(to, 0.0)
  }

  def add(registrationNumber: String): StatesModel = {
    new StatesModel(registrationNumber :: registrationNumbers)
  }

  def remove(registrationNumber: String): StatesModel = {
    new StatesModel(registrationNumbers.filter(_ != registrationNumber))
  }

}

object StatesModel {
  val InitialState: Char = '\0'
  val EndState: Char = '$'
}
