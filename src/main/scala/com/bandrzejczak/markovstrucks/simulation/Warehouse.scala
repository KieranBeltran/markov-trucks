package com.bandrzejczak.markovstrucks.simulation

import akka.actor.{Actor, ActorRef, Props}
import com.bandrzejczak.markovstrucks.domain.ReadProbabilities.ObservationModel
import com.bandrzejczak.markovstrucks.domain.{MostProbablePath, RegistrationNumber, StatesModel}
import com.bandrzejczak.markovstrucks.simulation.Warehouse._

import scala.concurrent.duration._
import scala.util.Random

class Warehouse(observationModel: ObservationModel, statistics: ActorRef) extends Actor {
  override def receive: Receive = warehouse(StatesModel.empty)

  def warehouse(statesModel: StatesModel): Receive = {
    case In(registrationNumber) =>
      registerContainer(statesModel, registrationNumber)

    case Out(registrationNumber) =>
      unregisterContainer(statesModel, registrationNumber)
  }

  def registerContainer(statesModel: StatesModel, registrationNumber: RegistrationNumber): Unit = {
    val readNumber = registrationNumber.read(observationModel)
    statistics ! Registered(registrationNumber.number, readNumber)
    context.become(warehouse(statesModel.add(readNumber)))
    scheduleLettingOut(registrationNumber)
  }

  def scheduleLettingOut(registrationNumber: RegistrationNumber) = {
    import context.dispatcher
    context.system.scheduler.scheduleOnce((Random.nextInt(9) + 1).seconds, self, Out(registrationNumber))
  }

  def unregisterContainer(statesModel: StatesModel, registrationNumber: RegistrationNumber): Unit = {
    val readNumber = registrationNumber.read(observationModel)
    MostProbablePath.withModels(statesModel, observationModel)
      .forObservations(readNumber) match {
      case Some(foundNumber) =>
        statistics ! LetOut(registrationNumber.number, readNumber, foundNumber)
        context.become(warehouse(statesModel.remove(foundNumber)))
      case None =>
        statistics ! Denied(registrationNumber.number, readNumber)
    }
  }
}

object Warehouse {
  def props(observationModel: ObservationModel, statistics: ActorRef) = Props(new Warehouse(observationModel, statistics))

  case class In(registrationNumber: RegistrationNumber)
  case class Out(registrationNumber: RegistrationNumber)

  case class Registered(actualNumber: String, readNumber: String)
  case class LetOut(actualNumber: String, readNumber: String, foundNumber: String)
  case class Denied(actualNumber: String, readNumber: String)
}
