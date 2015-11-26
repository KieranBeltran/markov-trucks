package com.bandrzejczak.markovstrucks

import org.scalatest.{FlatSpec, Matchers}

class ObservationModelOpsSpec extends FlatSpec with Matchers {

  "Observation model" should "give a probability of 1.0 for a state that equals observation but is not in a model" in {
    // given
    val observationModel = ReadProbabilities.emptyObservationModel

    // expect
    observationModel probabilityOf 'a' inState 'a' shouldBe 1.0
  }

  it should "give a probability of 0.0 for a state that doesn't equal observation and is not in a model" in {
    // given
    val observationModel = ReadProbabilities.emptyObservationModel

    // expect
    observationModel probabilityOf 'b' inState 'a' shouldBe 0.0
  }

  it should "give a probability of an observation in a state without mistakes" in {
    // given
    val observationModel = Map('a' -> new ReadProbabilities('a', Map()))

    // expect
    observationModel probabilityOf 'a' inState 'a' shouldBe 1.0
  }

  it should "give a probability of a correct observation in a state with just a mistake" in {
    // given
    val observationModel = Map('a' -> new ReadProbabilities('a', Map('b' -> 1.0)))

    // expect
    observationModel probabilityOf 'a' inState 'a' shouldBe 0.0
  }

  it should "give a probability of a mistaken observation in a state with just a mistake" in {
    // given
    val observationModel = Map('a' -> new ReadProbabilities('a', Map('b' -> 1.0)))

    // expect
    observationModel probabilityOf 'b' inState 'a' shouldBe 1.0
  }

  it should "give a probability of a mistaken observation in a state with just a different mistake" in {
    // given
    val observationModel = Map('a' -> new ReadProbabilities('a', Map('b' -> 0.5)))

    // expect
    observationModel probabilityOf 'c' inState 'a' shouldBe 0.0
  }

  it should "calculate a probability of a correct observation in a state with mistakes" in {
    // given
    val observationModel = Map('a' -> new ReadProbabilities('a', Map('b' -> 0.2, 'c' -> 0.1, 'd' -> 0.4)))

    // expect
    observationModel probabilityOf 'a' inState 'a' shouldBe (0.3 +- 0.000001) //double tolerance
  }

}
