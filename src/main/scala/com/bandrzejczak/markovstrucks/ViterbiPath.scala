package com.bandrzejczak.markovstrucks

import com.bandrzejczak.markovstrucks.ReadProbabilities.ObservationModel

class ViterbiPath(statesModel: StatesModel, observationModel: ObservationModel) {

  def forObservations(registrationNumber: String): String = {
    registrationNumber.foldLeft((1.0, StatesModel.InitialState.toString)) {
      case ((prev, actualNumber), observedCharacter) =>
        val possibleDestinations: Set[Char] = statesModel.stateTransitionProbabilities(actualNumber.last).toSet
        val nextState = possibleDestinations.map { state =>
          state -> observationModel.probabilityOf(observedCharacter).inState(state) * prev * statesModel.probabilityOfTransition(actualNumber.last, state)
        }.toMap
        println(s"Current text: $actualNumber")
        println(nextState)
        println()
        val bestState: (Char, Double) = nextState.max(Ordering.by((_: (Char, Double))._2))
        if(bestState._2 == 0.0)
          throw new Exception("Waat")
        (bestState._2, actualNumber + bestState._1)
    }._2.tail
  }

  def forObservations2(registrationNumber: String): String = {
    def recurse(observationsReverse: List[Char], currentState: Char): (String, Double) = observationsReverse match {
      case last :: Nil => ("", 1.0)
      case observation :: rest =>
//        println(s"Checking $observation, last state = $")
//        statesModel.availableStates.map { currentState =>
          val bestPrevious = statesModel.availableStates.map { previousState =>
            val recursed = recurse(rest, previousState)
            (recursed._1, recursed._2 * statesModel.probabilityOfTransition(previousState, currentState))
          }.maxBy(_._2)
        (bestPrevious._1 + currentState, bestPrevious._2)
//          (currentState + bestPrevious._1, bestPrevious._2)
//        }.maxBy(_._2)
    }
    recurse(("@" + registrationNumber + '$').reverse.toList, '$')._1.init
  }
}

object ViterbiPath {
  def withModels(statesModel: StatesModel, observationModel: ObservationModel): ViterbiPath = {
    new ViterbiPath(statesModel, observationModel)
  }
}
