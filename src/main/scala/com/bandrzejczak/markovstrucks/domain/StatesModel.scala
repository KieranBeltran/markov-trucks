package com.bandrzejczak.markovstrucks.domain

import com.bandrzejczak.markovstrucks.domain.StatesModel.{EndState, InitialState}

class StatesModel(registrationNumbers: List[String]) {

  val availableStates = registrationNumbers.foldLeft(Set.empty[Char])(_ ++ _.toSet)

  lazy val stateTransitionProbabilities: Map[Char, List[Char]] = computeStateTransitionProbabilities()

  private def computeStateTransitionProbabilities(): Map[Char, List[Char]] = {
    registrationNumbers.foldLeft(Map.empty[Char, List[Char]]) {
      case (probabilities, registrationNumber) =>
        val transitions = (InitialState + registrationNumber).zip(registrationNumber + EndState)
        transitions.foldLeft(probabilities)(addTransition)
    }
  }

  private def addTransition(probabilities: Map[Char, List[Char]], transition: (Char, Char)): Map[Char, List[Char]] = {
    probabilities.get(transition._1) match {
      case Some(p) => probabilities + (transition._1 -> (transition._2 :: p))
      case None => probabilities + (transition._1 -> List(transition._2))
    }
  }

  def probabilityOfTransition(from: Char, to: Char): Double = {
    stateTransitionProbabilities.get(from).map { transitionsTo =>
      transitionsTo.count(_ == to).toDouble / transitionsTo.size
    }.getOrElse(0.0)
  }

  def add(registrationNumber: String): StatesModel = {
    new StatesModel(registrationNumber :: registrationNumbers)
  }

  def remove(registrationNumber: String): StatesModel = {
    new StatesModel(registrationNumbers diff List(registrationNumber))
  }

}

object StatesModel {
  val InitialState: Char = '#'
  val EndState: Char = '$'
  lazy val empty = new StatesModel(Nil)
}
