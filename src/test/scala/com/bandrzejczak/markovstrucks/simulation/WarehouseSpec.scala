package com.bandrzejczak.markovstrucks.simulation

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.bandrzejczak.markovstrucks.domain.{ReadProbabilities, RegistrationNumber}
import com.bandrzejczak.markovstrucks.simulation.Warehouse._
import org.scalatest.{FlatSpecLike, Matchers}

import scala.concurrent.duration._

class WarehouseSpec extends TestKit(ActorSystem("WarehouseSystem")) with FlatSpecLike with Matchers with ImplicitSender {

  val TestSimulationSettings = SimulationSettings(
    0,
    0,
    1,
    2,
    0,
    false,
    false,
    None
  )

  "Warehouse" should "let the container in" in {
    // given
    val statistics = TestProbe()
    val warehouse = system.actorOf(Warehouse.props(Map(), TestSimulationSettings, statistics.ref))

    // when
    warehouse ! In(RegistrationNumber("ABC"))

    // then
    statistics expectMsg Registered("ABC", "ABC")
  }

  it should "let the container with mistaken number" in {
    // given
    val statistics = TestProbe()
    val warehouse = system.actorOf(Warehouse.props(Map('A' -> ReadProbabilities('A', Map('X' -> 1.0))), TestSimulationSettings, statistics.ref))

    // when
    warehouse ! In(RegistrationNumber("ABC"))

    // then
    statistics expectMsg Registered("ABC", "XBC")
  }

  it should "not let out the container with an empty model" in {
    // given
    val statistics = TestProbe()
    val warehouse = system.actorOf(Warehouse.props(Map(), TestSimulationSettings, statistics.ref))

    // when
    warehouse ! Out(RegistrationNumber("ABC"))

    // then
    statistics expectMsg Denied("ABC", "ABC")
  }

  it should "let out the container that was previously admitted" in {
    // given
    val statistics = TestProbe()
    val warehouse = system.actorOf(Warehouse.props(Map(), TestSimulationSettings, statistics.ref))
    warehouse ! In(RegistrationNumber("ABC"))
    statistics expectMsg Registered("ABC", "ABC")

    // when
    warehouse ! Out(RegistrationNumber("ABC"))

    // then
    statistics expectMsg LetOut("ABC", "ABC", "ABC")
  }

  it should "let out both admitted containers" in {
    // given
    val statistics = TestProbe()
    val warehouse = system.actorOf(
      Warehouse.props(
        Map(
          'A' -> ReadProbabilities('A', Map()),
          'B' -> ReadProbabilities('B', Map('D' -> 0.4)),
          'D' -> ReadProbabilities('D', Map('B' -> 0.6)),
          'C' -> ReadProbabilities('C', Map())
        ),
        TestSimulationSettings,
        statistics.ref
      )
    )
    warehouse ! In(RegistrationNumber("ABC"))
    statistics expectMsgClass classOf[Registered]
    warehouse ! In(RegistrationNumber("ADC"))
    statistics expectMsgClass classOf[Registered]

    // when
    warehouse ! Out(RegistrationNumber("ABC"))
    warehouse ! Out(RegistrationNumber("ADC"))

    // then
    statistics.expectMsgAllClassOf(classOf[LetOut], classOf[LetOut])
      .map(_.actualNumber) should contain theSameElementsAs Seq("ABC", "ADC")
  }

  it should "let out container automatically after a random period of time between 1 and 2 seconds (defined in settings)" in {
    // given
    val statistics = TestProbe()
    val warehouse = system.actorOf(Warehouse.props(Map(), TestSimulationSettings, statistics.ref))

    //when
    warehouse ! In(RegistrationNumber("ABC"))
    statistics expectMsgClass classOf[Registered]

    // then
    within(TestSimulationSettings.minWarehouseStay.seconds, TestSimulationSettings.maxWarehouseStay.seconds) {
      statistics.expectMsg(TestSimulationSettings.maxWarehouseStay.seconds, LetOut("ABC", "ABC", "ABC"))
    }
  }

}
