package com.bandrzejczak.markovstrucks

import com.bandrzejczak.markovstrucks.ReadProbabilities.ObservationModel
import com.bandrzejczak.markovstrucks.StatesModel.{EndState, InitialState}


class MostProbablePath private(statesModel: StatesModel, observationModel: ObservationModel) {

  trait ForObservations {
    def forObservations(observationsReverse: List[Char]): PathProbability
  }

  case class PathProbability(path: String, probability: Double)

  def forObservations(registrationNumber: String): String = {
    // observations list is reversed so the last operation is always first in the list
    val observations = (InitialState + registrationNumber + EndState).reverse.toList
    mostProbablePathEndingInState(EndState)
      .forObservations(observations)
      .path
      .drop(1) //drop initial state
      .dropRight(1) //drop end state
  }

  def mostProbablePathEndingInState(currentState: Char): ForObservations = new ForObservations {

    def forObservations(observations: List[Char]): PathProbability = observations match {
      // Probability of initial state is always 1.0
      case InitialState :: Nil => PathProbability(InitialState.toString, 1.0)

      case currentObservation :: remainingObservations =>
        val probabilityOfObservingCurrentState = observationModel.probabilityOf(currentObservation).inState(currentState)
        val PathProbability(path, pathProbability) = mostProbablePathWithTransitionToCurrentState(remainingObservations)
        PathProbability(
          path,
          pathProbability * probabilityOfObservingCurrentState
        )
    }

    private def mostProbablePathWithTransitionToCurrentState(remainingObservations: List[Char]): PathProbability = {
      statesModel.availableStates.map { previousState =>
        mostProbablePathEndingInState(previousState).forObservations(remainingObservations)
      }.map {
        case PathProbability(previousPath, previousProbability) =>
          PathProbability(
            previousPath + currentState,
            previousProbability * statesModel.probabilityOfTransition(previousPath.last, currentState)
          )
      }.maxBy(_.probability)
    }
  }
}

object MostProbablePath {
  def withModels(statesModel: StatesModel, observationModel: ObservationModel): MostProbablePath = {
    new MostProbablePath(statesModel, observationModel)
  }
}
