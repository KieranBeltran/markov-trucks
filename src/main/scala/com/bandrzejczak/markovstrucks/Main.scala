package com.bandrzejczak.markovstrucks

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult._
import akka.stream.ActorMaterializer
import com.bandrzejczak.markovstrucks.domain.{ReadProbabilities, RegistrationNumber}
import com.bandrzejczak.markovstrucks.simulation.Warehouse.In
import com.bandrzejczak.markovstrucks.simulation.{Statistics, Warehouse}

import scala.concurrent.duration._
import scala.util.Random

object Main extends App {
  implicit val system: ActorSystem = ActorSystem("name")
  implicit val ec = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val (statistics, statisticsFlow) = Statistics.create
  val warehouse = system.actorOf(Warehouse.props(ReadProbabilities.emptyObservationModel, statistics))

  val routes =
    pathPrefix("api") {
      path("events") {
        get {
          handleWebsocketMessages {
            statisticsFlow.map(info => TextMessage(info.write))
          }
        }
      } ~
      path("start") {
        get {
          system.scheduler.schedule(1.second, 100.millis) {
            warehouse ! In(RegistrationNumber(Random.alphanumeric.take(6).mkString.toUpperCase))
          }
          complete("ok")
        }
      }
    } ~
    path("") {
      getFromResource(s"webapp/index.html")
    } ~
    getFromResourceDirectory("webapp")

  Http().bindAndHandle(routes, "localhost", 8080)
}
