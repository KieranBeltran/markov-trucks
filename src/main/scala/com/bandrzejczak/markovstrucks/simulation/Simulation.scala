package com.bandrzejczak.markovstrucks.simulation

import akka.actor.{Actor, ActorRef, Props}
import com.bandrzejczak.markovstrucks.domain.ReadProbabilities.ObservationModel
import com.bandrzejczak.markovstrucks.domain.{ReadProbabilities, RegistrationNumber}
import com.bandrzejczak.markovstrucks.simulation.Simulation.{RegisterNext, Start}
import com.bandrzejczak.markovstrucks.simulation.Warehouse.In

import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.Random

class Simulation(simulationSettings: SimulationSettings, warehouse: ActorRef) extends Actor {
  override def receive: Receive = waitingForStart

  def waitingForStart: Receive = {
    case Start =>
      scheduleNextRegistration()
      context.become(
        inProgress(
          simulationSettings.simulationTimeout.map(_.seconds.fromNow)
        )
      )
  }

  def inProgress(deadline: Option[Deadline]): Receive = {
    case RegisterNext =>
      warehouse ! In(simulationSettings.nextRegistrationNumber())
      if (deadline.isDefined && deadline.get.hasTimeLeft()) {
        scheduleNextRegistration()
      }
  }

  def scheduleNextRegistration() = {
    import context.dispatcher
    context.system.scheduler.scheduleOnce(simulationSettings.nextRegistrationTime(), self, RegisterNext)
  }
}

object Simulation {
  def props(simulationSettings: SimulationSettings, warehouse: ActorRef) = Props(new Simulation(simulationSettings, warehouse))

  case object Start

  case object RegisterNext

}

case class SimulationSettings(
                               minRegistrationsPause: Int,
                               maxRegistrationsPause: Int,
                               minWarehouseStay: Int,
                               maxWarehouseStay: Int,
                               registrationNumberLength: Int,
                               lettersInRegistrationNumber: Boolean,
                               digitsInRegistrationNumber: Boolean,
                               simulationTimeout: Option[Int],
                               readProbabilities: List[ReadProbabilities]
                             ) {

  def nextRegistrationTime(): FiniteDuration =
    randomTimeBetween(minRegistrationsPause, maxRegistrationsPause)

  def driveOutTime(): FiniteDuration =
    randomTimeBetween(minWarehouseStay, maxWarehouseStay)

  private def randomTimeBetween(start: Int, end: Int): FiniteDuration = {
    ((start * 1000) + Random.nextInt((end - start) * 1000 + 1)).millis
  }

  def nextRegistrationNumber(): RegistrationNumber =
    RegistrationNumber(
      Random.alphanumeric
        .map(_.toUpper)
        .filter(canBeAPartOfRegistrationNumber)
        .take(registrationNumberLength)
        .mkString
    )

  private def canBeAPartOfRegistrationNumber(char: Char) =
    (digitsInRegistrationNumber && char.isDigit) || (lettersInRegistrationNumber && char.isLetter)

  lazy val observationModel: ObservationModel = readProbabilities.map(p => p.correctCharacter -> p).toMap
}
