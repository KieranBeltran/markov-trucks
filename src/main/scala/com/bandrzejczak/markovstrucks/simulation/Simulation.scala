package com.bandrzejczak.markovstrucks.simulation

import akka.actor.{Actor, ActorRef, Props}
import com.bandrzejczak.markovstrucks.simulation.Simulation.{RegisterNext, Start}
import com.bandrzejczak.markovstrucks.simulation.Warehouse.In

import scala.concurrent.duration._

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
      if (deadline.isEmpty || deadline.get.hasTimeLeft()) {
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


