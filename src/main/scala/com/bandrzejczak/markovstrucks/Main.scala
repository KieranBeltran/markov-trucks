package com.bandrzejczak.markovstrucks

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult._
import akka.stream.ActorMaterializer

object Main extends App {
  val routes =
    pathPrefix("api") {
      path("test") {
        get {
          complete("test ok")
        }
      }
    } ~
    path("") {
      getFromResource(s"webapp/index.html")
    } ~
    getFromResourceDirectory("webapp")

  implicit val system: ActorSystem = ActorSystem("name")
  implicit val ec = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  Http().bindAndHandle(routes, "localhost", 8080)
}
