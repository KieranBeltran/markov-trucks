package com.bandrzejczak.markovstrucks

import com.bandrzejczak.markovstrucks.ReadProbabilities.MistakesModel

case class RegistrationNumber(number: String) {
  def read(mistakesModel: MistakesModel): String = {
    number.map { originalCharacter =>
      mistakesModel.get(originalCharacter)
        .map(_.read())
        .getOrElse(originalCharacter)
    }
  }
}
