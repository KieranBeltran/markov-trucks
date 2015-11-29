package com.bandrzejczak.markovstrucks.simulation

import com.bandrzejczak.markovstrucks.domain.ReadProbabilities.ObservationModel
import com.bandrzejczak.markovstrucks.domain.{ReadProbabilities, RegistrationNumber}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.Random

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
