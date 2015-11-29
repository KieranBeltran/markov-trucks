package com.bandrzejczak.markovstrucks.domain

import com.bandrzejczak.markovstrucks.domain.ReadProbabilities.ObservationModel
import com.bandrzejczak.markovstrucks.domain.StatesModel.{EndState, InitialState}


class MostProbablePath private(statesModel: StatesModel, observationModel: ObservationModel) {

  trait ForObservations {
    def forObservations(observationsReverse: List[Char]): Option[PathProbability]
  }

  case class PathProbability(path: String, probability: Double)

  def forObservations(registrationNumber: String): Option[String] = {
    // observations list is reversed so the last operation is always first in the list
    val observations = (InitialState + registrationNumber + EndState).reverse.toList
    mostProbablePathEndingInState(EndState)
      .forObservations(observations)
      .map {
        _.path
          .drop(1) //drop initial state
          .dropRight(1) //drop end state
      }
  }

  def mostProbablePathEndingInState(currentState: Char): ForObservations = new ForObservations {

    def forObservations(observations: List[Char]): Option[PathProbability] = observations match {
      // Probability of initial state is always 1.0
      case InitialState :: Nil => Some(PathProbability(InitialState.toString, 1.0))

      case currentObservation :: remainingObservations =>
        val probabilityOfObservingCurrentState = observationModel.probabilityOf(currentObservation).inState(currentState)
        mostProbablePathWithTransitionToCurrentState(remainingObservations).flatMap {
          case PathProbability(path, pathProbability) =>
            if (pathProbability == 0)
              None
            else
              Some(
                PathProbability(
                  path,
                  pathProbability * probabilityOfObservingCurrentState
                )
              )
        }
    }

    private def mostProbablePathWithTransitionToCurrentState(remainingObservations: List[Char]): Option[PathProbability] = {
      statesModel.statesWithTransitionTo(currentState).map { previousState =>
        mostProbablePathEndingInState(previousState).forObservations(remainingObservations)
      }.collect {
        case Some(PathProbability(previousPath, previousProbability)) =>
          PathProbability(
            previousPath + currentState,
            previousProbability * statesModel.probabilityOfTransition(previousPath.last, currentState)
          )
      } match {
        case xs if xs.isEmpty => None
        case probabilitiesOfAllPathsWithTransitionToCurrentState =>
          Some(probabilitiesOfAllPathsWithTransitionToCurrentState.maxBy(_.probability))
      }
    }
  }
}

object MostProbablePath {
  def withModels(statesModel: StatesModel, observationModel: ObservationModel): MostProbablePath = {
    new MostProbablePath(statesModel, observationModel)
  }
}
