package com.bandrzejczak.markovstrucks.simulation

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.stream.actor.ActorPublisher
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.bandrzejczak.markovstrucks.simulation.Statistics.{LeaveRead, Read, RegistrationRead, StatisticsInfo, WarehouseState}
import com.bandrzejczak.markovstrucks.simulation.Warehouse.{Denied, LetOut, Registered}
import spray.json.DefaultJsonProtocol

class Statistics extends ActorPublisher[StatisticsInfo] {

  override def receive: Receive = gatherStatistics(Map())

  def gatherStatistics(
                        warehouseRegistry: Map[String, String], // readNumber => actualNumber
                        mistakenlyLetOut: Int = 0,
                        denied: Int = 0
                      ): Receive = {
    reportState(warehouseRegistry, mistakenlyLetOut, denied);
    {
      case Registered(actual, read) =>
        registrationRead(actual, read)
        context.become(gatherStatistics(warehouseRegistry + (read -> actual), mistakenlyLetOut, denied))

      case letOut @ LetOut(actual, read, _) =>
        leaveRead(actual, read)
        handleLeave(warehouseRegistry, mistakenlyLetOut, denied, letOut)

      case Denied(actual, read) =>
        leaveRead(actual, read)
        context.become(gatherStatistics(warehouseRegistry, mistakenlyLetOut, denied + 1))
    }
  }

  def reportState(warehouseRegistry: Map[String, String], mistakenlyLetOut: Int, denied: Int): Unit = {
    ifActive {
      onNext(WarehouseState(warehouseRegistry.keys, mistakenlyLetOut, denied))
    }
  }

  def registrationRead(actual: String, read: String): Unit = {
    ifActive {
      onNext(RegistrationRead(Read(actual, read)))
    }
  }

  def leaveRead(actual: String, read: String): Unit = {
    ifActive {
      onNext(LeaveRead(Read(actual, read)))
    }
  }

  def handleLeave(warehouseRegistry: Map[String, String], mistakenlyLetOut: Int, denied: Int, letOut: LetOut): Unit = {
    val ActualNumber = letOut.actualNumber
    warehouseRegistry.get(letOut.foundNumber) match {
      case Some(ActualNumber) => //we let out the right one
        context.become(gatherStatistics(
          warehouseRegistry - letOut.foundNumber,
          mistakenlyLetOut,
          denied
        ))
      case Some(_) => //we let out some other
        context.become(gatherStatistics(
          warehouseRegistry - letOut.foundNumber,
          mistakenlyLetOut + 1,
          denied
        ))
      case None => //we let out some nonexistent container!
        context.become(gatherStatistics(
          warehouseRegistry,
          mistakenlyLetOut + 1,
          denied
        ))
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
      //.map(info => TextMessage(info.write))
    (ref, statisticsFlow)
  }

  sealed trait StatisticsInfo {
    implicit def ReadFormat = jsonFormat2(Read)
    implicit def RegistrationReadFormat = jsonFormat1(RegistrationRead)
    implicit def LeaveReadFormat = jsonFormat1(LeaveRead)
    implicit def WarehouseStateFormat = jsonFormat3(WarehouseState)

    def write: String = this match {
      case x: RegistrationRead => x.toJson.prettyPrint
      case x: LeaveRead => x.toJson.prettyPrint
      case x: WarehouseState => x.toJson.prettyPrint
    }
  }

  case class Read(actualNumber: String, readNumber: String)
  case class RegistrationRead(in: Read) extends StatisticsInfo
  case class LeaveRead(out: Read) extends StatisticsInfo
  case class WarehouseState(containers: Iterable[String], mistakenlyLetOut: Int, denied: Int) extends StatisticsInfo

}