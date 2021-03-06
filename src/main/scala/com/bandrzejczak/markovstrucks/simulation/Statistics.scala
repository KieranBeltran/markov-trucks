package com.bandrzejczak.markovstrucks.simulation

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.stream.actor.ActorPublisher
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.bandrzejczak.markovstrucks.simulation.Statistics.{Denial, LeaveRead, LeaveStatus, Mistake, OK, Read, RegistrationRead, StatisticsInfo, WarehouseState}
import com.bandrzejczak.markovstrucks.simulation.Warehouse.{Denied, LetOut, Registered}
import spray.json.DefaultJsonProtocol

class Statistics extends ActorPublisher[StatisticsInfo] {

  override def receive: Receive = gatherStatistics(Map())

  def gatherStatistics(
                        warehouseRegistry: Map[String, String], // readNumber => actualNumber
                        mistakenlyLetOut: Int = 0,
                        denied: Int = 0,
                        successful: Int = 0
                      ): Receive = {
    reportState(warehouseRegistry, mistakenlyLetOut, denied, successful);
    {
      case Registered(actual, read) =>
        registrationRead(actual, read)
        context.become(gatherStatistics(warehouseRegistry + (read -> actual), mistakenlyLetOut, denied, successful))

      case letOut @ LetOut(actual, read, _) =>
        val status = handleLeave(warehouseRegistry, mistakenlyLetOut, denied, successful, letOut)
        leaveRead(actual, read, status)

      case Denied(actual, read) =>
        leaveRead(actual, read, Denial)
        context.become(gatherStatistics(warehouseRegistry, mistakenlyLetOut, denied + 1, successful))
    }
  }

  def reportState(warehouseRegistry: Map[String, String], mistakenlyLetOut: Int, denied: Int, successful: Int): Unit = {
    ifActive {
      onNext(WarehouseState(warehouseRegistry.keys, mistakenlyLetOut, denied, successful))
    }
  }

  def registrationRead(actual: String, read: String): Unit = {
    ifActive {
      onNext(RegistrationRead(Read(actual, read)))
    }
  }

  def leaveRead(actual: String, read: String, status: LeaveStatus): Unit = {
    ifActive {
      onNext(LeaveRead(Read(actual, read), status.toString))
    }
  }

  def handleLeave(warehouseRegistry: Map[String, String], mistakenlyLetOut: Int, denied: Int, successful: Int, letOut: LetOut): LeaveStatus = {
    val ActualNumber = letOut.actualNumber
    warehouseRegistry.get(letOut.foundNumber) match {
      case Some(ActualNumber) => //we let out the right one
        context.become(gatherStatistics(
          warehouseRegistry - letOut.foundNumber,
          mistakenlyLetOut,
          denied,
          successful + 1
        ))
        OK
      case Some(_) => //we let out some other
        context.become(gatherStatistics(
          warehouseRegistry - letOut.foundNumber,
          mistakenlyLetOut + 1,
          denied,
          successful
        ))
        Mistake
      case None => //we let out some nonexistent container!
        context.become(gatherStatistics(
          warehouseRegistry,
          mistakenlyLetOut + 1,
          denied,
          successful
        ))
        Mistake
    }
  }

  def ifActive(run: => Unit): Unit = {
    if (isActive && totalDemand > 0) {
      run
    }
  }
}

object Statistics extends DefaultJsonProtocol with SprayJsonSupport {
  def props = Props(new Statistics)

  def create(implicit system: ActorSystem): (ActorRef, Flow[Any, StatisticsInfo, Unit]) = {
    val ref = system.actorOf(Statistics.props)
    val source = Source.apply(ActorPublisher(ref))
    val statisticsFlow = Flow.fromSinkAndSource[Any, StatisticsInfo](Sink.ignore, source)
    (ref, statisticsFlow)
  }

  sealed trait StatisticsInfo {
    implicit def ReadFormat = jsonFormat2(Read)
    implicit def RegistrationReadFormat = jsonFormat1(RegistrationRead)
    implicit def LeaveReadFormat = jsonFormat2(LeaveRead)
    implicit def WarehouseStateFormat = jsonFormat4(WarehouseState)

    def write: String = this match {
      case x: RegistrationRead => x.toJson.prettyPrint
      case x: LeaveRead => x.toJson.prettyPrint
      case x: WarehouseState => x.toJson.prettyPrint
    }
  }

  case class Read(actualNumber: String, readNumber: String)
  case class RegistrationRead(in: Read) extends StatisticsInfo
  case class LeaveRead(out: Read, status: String) extends StatisticsInfo
  case class WarehouseState(containers: Iterable[String], mistakenlyLetOut: Int, denied: Int, successful: Int) extends StatisticsInfo

  sealed abstract class LeaveStatus(name: String) {
    override def toString: String = name
  }
  case object OK extends LeaveStatus("ok")
  case object Mistake extends LeaveStatus("mistake")
  case object Denial extends LeaveStatus("denial")

}