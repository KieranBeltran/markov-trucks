package com.bandrzejczak.markovstrucks.domain

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class MostProbablePathSpec extends FlatSpec with Matchers with ScalaFutures with OptionValues {

  "Most probable path" should "find the only path in the model" in {
    MostProbablePath.withModels(
      StatesModel.empty.add("abc"),
      Map()
    ).forObservations("abc").futureValue.value shouldBe "abc"
  }

  it should "find the only path in the model with mistakes defined" in {
    MostProbablePath.withModels(
      StatesModel.empty.add("abc"),
      Map(
        'a' -> ReadProbabilities('a', Map('x' -> 0.1)),
        'b' -> ReadProbabilities('b', Map('y' -> 0.1)),
        'c' -> ReadProbabilities('c', Map('z' -> 0.1))
      )
    ).forObservations("xyz").futureValue.value shouldBe "abc"
  }

  it should "find the correct path in the confusing model with almost identical characters" in {
    MostProbablePath.withModels(
      StatesModel.empty.add("abc").add("adc"), //model contains both abc and adc paths
      Map(
        'a' -> ReadProbabilities('a', Map()),
        'b' -> ReadProbabilities('b', Map('d' -> 0.6)),  // 40% = b, 60% = d
        'd' -> ReadProbabilities('d', Map('b' -> 0.41)), // 41% = b, 59% = d
        'c' -> ReadProbabilities('c', Map())
      )
    ).forObservations("adc").futureValue.value shouldBe "abc"
  }

  it should "not find a correct path if the model has no connections to end state" in {
    // given
    val modelWithoutEndState = new StatesModel(
      Map(
        StatesModel.InitialState -> List('a'),
        'a' -> List('b')
      )
    )

    // expect
    MostProbablePath.withModels(
      modelWithoutEndState, //model contains both abc and adc paths
      Map()
    ).forObservations("ab").futureValue shouldBe 'empty
  }

  it should "not find a correct path if observations have no connection to the model" in {
    MostProbablePath.withModels(
      StatesModel.empty.add("abc").add("adc"), //model contains both abc and adc paths
      Map()
    ).forObservations("axc").futureValue shouldBe 'empty
  }

}
