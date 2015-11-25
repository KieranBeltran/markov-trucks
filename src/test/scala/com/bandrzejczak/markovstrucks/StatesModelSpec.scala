package com.bandrzejczak.markovstrucks

import org.scalatest.{FlatSpec, Matchers}

class StatesModelSpec extends FlatSpec with Matchers {

  "States model" should "list available states for empty model" in {
    // expect
    new StatesModel(Nil).availableStates shouldBe 'empty
  }

  it should "list available states" in {
    // expect
    new StatesModel(List("abc", "cde", "aef")).availableStates should contain only ('a' to 'f': _*)
  }

  it should "allow adding new registration numbers" in {
    // given
    val model: StatesModel = new StatesModel(List("abc", "cde"))

    // when
    val newModel = model.add("aef")

      // then
    newModel.availableStates should contain only ('a' to 'f': _*)
    model.availableStates should contain only ('a' to 'e': _*)
  }

  it should "allow removing registration numbers" in {
    // given
    val model: StatesModel = new StatesModel(List("abc", "cde", "aef"))

    // when
    val newModel = model.remove("aef")

      // then
    newModel.availableStates should contain only ('a' to 'e': _*)
    model.availableStates should contain only ('a' to 'f': _*)
  }

  it should "have an empty list of transition probabilities for empty list of registration numbers" in {
    // when
    val model: StatesModel = new StatesModel(List())

    // then
    model.stateTransitionProbabilities shouldBe 'empty
  }

  it should "have one state for a list with one element number" in {
    // when
    val model: StatesModel = new StatesModel(List("a"))

    // then
    model.stateTransitionProbabilities shouldBe Map(
      StatesModel.InitialState -> Map('a' -> 1.0),
      'a' -> Map('$' -> 1.0)
    )
  }

  it should "have two initial states for a list with two one element numbers" in {
    // when
    val model: StatesModel = new StatesModel(List("a", "b"))

    // then
    model.stateTransitionProbabilities shouldBe Map(
      StatesModel.InitialState -> Map('a' -> 0.5, 'b' -> 0.5),
      'a' -> Map(StatesModel.EndState -> 1.0),
      'b' -> Map(StatesModel.EndState -> 1.0)
    )
  }

  it should "have proper state transitions for one number with multiple characters" in {
    // when
    val model: StatesModel = new StatesModel(List("abc"))

    // then
    model.stateTransitionProbabilities shouldBe Map(
      StatesModel.InitialState -> Map('a' -> 1.0),
      'a' -> Map('b' -> 1.0),
      'b' -> Map('c' -> 1.0),
      'c' -> Map(StatesModel.EndState -> 1.0)
    )
  }

  it should "have proper state transitions for multiple number with multiple characters" in {
    // when
    val model: StatesModel = new StatesModel(List("abc", "cba", "xyz"))

    // then
    model.stateTransitionProbabilities shouldBe Map(
      StatesModel.InitialState -> Map('a' -> 1.0/3, 'c' -> 1.0/3, 'x' -> 1.0/3),
      'a' -> Map('b' -> 0.5, StatesModel.EndState -> 0.5),
      'b' -> Map('a' -> 0.5, 'c' -> 0.5),
      'c' -> Map('b' -> 0.5, StatesModel.EndState -> 0.5),
      'x' -> Map('y' -> 1.0),
      'y' -> Map('z' -> 1.0),
      'z' -> Map(StatesModel.EndState -> 1.0)
    )
  }

  it should "delete the only number and leave the model empty" in {
    // when
    val model: StatesModel = new StatesModel(List("abc")).remove("abc")

    // then
    model.stateTransitionProbabilities shouldBe 'empty
  }

  it should "compute probability of transition from state that's not in the model to be 0" in {
    // when
    val model: StatesModel = new StatesModel(List("abc"))

    // then
    model.probabilityOfTransition('x', 'y') shouldBe 0.0
  }

  it should "compute probability of transition to a state to which there's no path to be 0" in {
    // when
    val model: StatesModel = new StatesModel(List("abc"))

    // then
    model.probabilityOfTransition('a', 'x') shouldBe 0.0
  }

  it should "compute probability of transition from one state to another" in {
    // when
    val model: StatesModel = new StatesModel(List("abc", "cba"))

    // then
    model.probabilityOfTransition('a', 'b') shouldBe 0.5
  }

  it should "compute probability of transition from initial state" in {
    // when
    val model: StatesModel = new StatesModel(List("abc", "cba"))

    // then
    model.probabilityOfTransition(StatesModel.InitialState, 'a') shouldBe 0.5
  }

  it should "compute probability of transition to end state" in {
    // when
    val model: StatesModel = new StatesModel(List("abc", "cba", "bca"))

    // then
    model.probabilityOfTransition('c', StatesModel.EndState) shouldBe 1.0/3
  }

}
