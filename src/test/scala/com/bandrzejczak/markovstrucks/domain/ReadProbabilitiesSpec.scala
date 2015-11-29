package com.bandrzejczak.markovstrucks.domain

import org.scalatest.{FlatSpec, Matchers}

class ReadProbabilitiesSpec extends FlatSpec with Matchers {

  "Read probabilities model" should "choose read the only character" in {
    // given
    val readModel = ReadProbabilities('a', Map())

    // expect
    List.fill(100)(readModel.read()) should contain only 'a'
  }

  it should "choose the only, 100% probable mistake" in {
    // given
    val readModel = ReadProbabilities('a', Map('b' -> 1.0))

    // expect
    List.fill(100)(readModel.read()) should contain only 'b'
  }

  it should "choose both correct character and a mistake with an equal probability" in {
    // given
    val readModel = ReadProbabilities('a', Map('b' -> 0.5))

    // when
    val readings = List.fill(100)(readModel.read())

    //then
    readings should contain only ('a', 'b')
    Math.abs(readings.count(_ == 'a') - readings.count(_ == 'b')) should (be >= 0 and be <= 20)
  }

  it should "choose only the correct character if all mistakes have 0% probability" in {
    // given
    val readModel = ReadProbabilities('a', Map('b' -> 0.0, 'c' -> 0.0, 'd' -> 0.0))

    // when
    val readings = List.fill(100)(readModel.read())

    //then
    readings should contain only 'a'
  }

  it should "choose choose from multiple equally probable characters" in {
    // given
    val readModel = ReadProbabilities(
      'a',
      ('b' to 'j').map(_ -> 0.1).toMap
    )

    // when
    val readings = List.fill(100)(readModel.read())

    //then
    readings should contain only ('a' to 'j': _*)
  }

}
