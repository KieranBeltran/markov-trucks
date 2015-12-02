package com.bandrzejczak.markovstrucks.domain

import com.bandrzejczak.markovstrucks.domain.StatesModel.{EndState, InitialState}

class StatesModel(val stateTransitionProbabilities: Map[Char, List[Char]]) {

  def statesWithTransitionTo(currentState: Char): Iterable[Char] = stateTransitionProbabilities.filter {
    case (_, states) => states.contains(currentState)
  }.keys

  def probabilityOfTransition(from: Char, to: Char): Double = {
    stateTransitionProbabilities.get(from).map { transitionsTo =>
      transitionsTo.count(_ == to).toDouble / transitionsTo.size
    }.getOrElse(0.0)
  }

  def add(registrationNumber: String): StatesModel = {
    val transitions = (InitialState + registrationNumber).zip(registrationNumber + EndState)
    val newProbabilities = transitions.foldLeft(stateTransitionProbabilities)(addTransition)
    new StatesModel(newProbabilities)
  }

  private def addTransition(probabilities: Map[Char, List[Char]], transition: (Char, Char)): Map[Char, List[Char]] = {
    probabilities.get(transition._1) match {
      case Some(p) => probabilities + (transition._1 -> (transition._2 :: p))
      case None => probabilities + (transition._1 -> List(transition._2))
    }
  }

  def remove(registrationNumber: String): StatesModel = {
    val transitions = (InitialState + registrationNumber).zip(registrationNumber + EndState)
    val newProbabilities = transitions.foldLeft(stateTransitionProbabilities)(removeTransition)
    new StatesModel(newProbabilities)
  }

  private def removeTransition(probabilities: Map[Char, List[Char]], transition: (Char, Char)): Map[Char, List[Char]] = {
    val (from, to) = transition
    probabilities.get(from) match {
      case Some(`to` :: Nil) => probabilities - transition._1
      case Some(transitions) => probabilities + (transition._1 -> (transitions diff List(to)))
      case None => probabilities
    }
  }

}

object StatesModel {
  val InitialState: Char = '#'
  val EndState: Char = '$'
  lazy val empty = new StatesModel(Map())
}
