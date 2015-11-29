package com.bandrzejczak.markovstrucks

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.ws.TextMessage
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult._
import akka.stream.ActorMaterializer
import com.bandrzejczak.markovstrucks.domain.ReadProbabilities
import com.bandrzejczak.markovstrucks.simulation.Simulation.Start
import com.bandrzejczak.markovstrucks.simulation.{Simulation, SimulationSettings, Statistics, Warehouse}
import spray.json.DefaultJsonProtocol

object Main extends App with DefaultJsonProtocol with SprayJsonSupport {
  implicit val system: ActorSystem = ActorSystem("name")
  implicit val ec = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val (statistics, statisticsFlow) = Statistics.create

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
        post {
          implicit def SimulationSettingsFormat = jsonFormat8(SimulationSettings)
          entity(as[SimulationSettings]) { settings =>
            system.actorOf(
              Simulation.props(
                settings,
                system.actorOf(
                  Warehouse.props(
                    ReadProbabilities.emptyObservationModel,
                    settings,
                    statistics
                  )
                )
              )
            ) ! Start
            complete(200 -> "ok")
          }
        }
      }
    } ~
    path("") {
      getFromResource(s"webapp/index.html")
    } ~
    getFromResourceDirectory("webapp")

  Http().bindAndHandle(routes, "localhost", 8080)
}
