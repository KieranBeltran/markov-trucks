package com.bandrzejczak.markovstrucks.domain

import org.scalatest.{FlatSpec, Matchers}

class RegistrationNumberSpec extends FlatSpec with Matchers{

  "Registration number" should "be read properly with an empty mistakes model" in {
    // when
    val readValue = RegistrationNumber("ANYNUMBER").read(ReadProbabilities.emptyObservationModel)

    // then
    readValue shouldBe "ANYNUMBER"
  }

  it should "be read properly with a mistakes model without errors" in {
    // given
    val everyCharacterIsMistakenAsX = ('A' to 'Z').map { char =>
      char -> ReadProbabilities(char, Map())
    }.toMap

    // when
    val readValue = RegistrationNumber("ANYNUMBER")
      .read(everyCharacterIsMistakenAsX)

    // then
    readValue shouldBe "ANYNUMBER"
  }

  it should "be read with every character mistaken" in {
    // given
    val everyCharacterIsMistakenAsX = ('A' to 'Z').map { char =>
      char -> ReadProbabilities(char, Map('X' -> 1.0))
    }.toMap

    // when
    val readValue = RegistrationNumber("ANYNUMBER")
      .read(everyCharacterIsMistakenAsX)

    // then
    readValue shouldBe "XXXXXXXXX"
  }

  it should "be read with one character mistaken" in {
    // given
    val NCharacterIsMistakenAsX = Map(
      'N' -> ReadProbabilities('N', Map('X' -> 1.0))
    )

    // when
    val readValue = RegistrationNumber("ANYNUMBER")
      .read(NCharacterIsMistakenAsX)

    // then
    readValue shouldBe "AXYXUMBER"
  }

  it should "be read with all characters mistaken as one of two characters" in {
    // given
    val everyCharacterIsMistakenAsXOrY = ('A' to 'Z').map { char =>
      char -> ReadProbabilities(char, Map('X' -> 0.5, 'Y' -> 0.5))
    }.toMap

    // when
    val readValue = RegistrationNumber("ANYNUMBER")
      .read(everyCharacterIsMistakenAsXOrY)

    // then
    readValue should contain only ('X', 'Y')
  }

}
