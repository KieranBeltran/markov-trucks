package com.bandrzejczak.markovstrucks.simulation

import akka.actor.{Actor, ActorRef, Props}
import com.bandrzejczak.markovstrucks.domain.ReadProbabilities.ObservationModel
import com.bandrzejczak.markovstrucks.domain.{MostProbablePath, RegistrationNumber, StatesModel}
import com.bandrzejczak.markovstrucks.simulation.Warehouse._

class Warehouse(observationModel: ObservationModel, simulationSettings: SimulationSettings, statistics: ActorRef) extends Actor {
  override def receive: Receive = warehouse(StatesModel.empty)

  def warehouse(statesModel: StatesModel): Receive = {
    case In(registrationNumber) =>
      registerContainer(statesModel, registrationNumber)

    case Out(registrationNumber) =>
      unregisterContainer(statesModel, registrationNumber)

    case Remove(foundNumber) =>
      context.become(warehouse(statesModel.remove(foundNumber)))

  }

  def registerContainer(statesModel: StatesModel, registrationNumber: RegistrationNumber): Unit = {
    val readNumber = registrationNumber.read(observationModel)
    statistics ! Registered(registrationNumber.number, readNumber)
    context.become(warehouse(statesModel.add(readNumber)))
    scheduleLettingOut(registrationNumber)
  }

  def scheduleLettingOut(registrationNumber: RegistrationNumber) = {
    import context.dispatcher
    context.system.scheduler.scheduleOnce(simulationSettings.driveOutTime(), self, Out(registrationNumber))
  }

  def unregisterContainer(statesModel: StatesModel, registrationNumber: RegistrationNumber): Unit = {
    val readNumber = registrationNumber.read(observationModel)
    import scala.concurrent.ExecutionContext.Implicits.global
    MostProbablePath.withModels(statesModel, observationModel)
      .forObservations(readNumber).onSuccess {
      case Some(foundNumber) =>
        statistics ! LetOut(registrationNumber.number, readNumber, foundNumber)
        self ! Remove(foundNumber)
      case None =>
        statistics ! Denied(registrationNumber.number, readNumber)
    }
  }
}

object Warehouse {
  def props(observationModel: ObservationModel, simulationSettings: SimulationSettings, statistics: ActorRef) =
    Props(new Warehouse(observationModel, simulationSettings, statistics))

  case class In(registrationNumber: RegistrationNumber)
  case class Out(registrationNumber: RegistrationNumber)
  case class Remove(foundNumber: String)

  case class Registered(actualNumber: String, readNumber: String)
  case class LetOut(actualNumber: String, readNumber: String, foundNumber: String)
  case class Denied(actualNumber: String, readNumber: String)
}
