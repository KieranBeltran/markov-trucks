package com.bandrzejczak.markovstrucks

import org.scalatest.{FlatSpec, Matchers}

class ViterbiPathSpec extends FlatSpec with Matchers{

  "test" should "test" in {
    ViterbiPath.withModels(
      new StatesModel(List("abc")),
      Map(
        'a' -> ReadProbabilities('a', Map()),
        'b' -> ReadProbabilities('b', Map()),
        'c' -> ReadProbabilities('c', Map())
      )
    ).forObservations2("abc") shouldBe "abc"
  }

  it should "test2" in {
    ViterbiPath.withModels(
      new StatesModel(List("abc", "adc")),
      Map(
        'a' -> ReadProbabilities('a', Map()),
        'b' -> ReadProbabilities('b', Map('d' -> 0.6)),
        'd' -> ReadProbabilities('d', Map('b' -> 0.39)),
        'c' -> ReadProbabilities('c', Map())
      )
    ).forObservations2("adc") shouldBe "adc"
  }

}
