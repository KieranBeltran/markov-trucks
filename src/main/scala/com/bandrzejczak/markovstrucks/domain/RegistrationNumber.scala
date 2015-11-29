package com.bandrzejczak.markovstrucks.domain

import com.bandrzejczak.markovstrucks.domain.ReadProbabilities.ObservationModel

case class RegistrationNumber(number: String) {
  def read(observationModel: ObservationModel): String = {
    number.map { originalCharacter =>
      observationModel.get(originalCharacter)
        .map(_.read())
        .getOrElse(originalCharacter)
    }
  }
}
