package com.bandrzejczak.markovstrucks.simulation

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.testkit.TestKit
import com.bandrzejczak.markovstrucks.simulation.Statistics._
import com.bandrzejczak.markovstrucks.simulation.Warehouse.{Denied, LetOut, Registered}
import org.scalatest.{FlatSpecLike, Matchers}

class StatisticsSpec extends TestKit(ActorSystem("StatisticsSystem")) with FlatSpecLike with Matchers {

  implicit val actorMaterializer = ActorMaterializer()

  "Statistics" should "report registration read" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(1)

    // when
    statistics ! Registered("ABC", "XYZ")

    // then
    mockSink.requestNext() shouldBe RegistrationRead(Read("ABC", "XYZ"))
  }

  it should "add registered container to the state" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(2)

    // when
    statistics ! Registered("ABC", "XYZ")
    mockSink.requestNext()

    // then
    mockSink.requestNext() shouldBe WarehouseState(Set("XYZ"), 0, 0, 0)
  }

  it should "report leave read for letting container out" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(2)

    // when
    statistics ! LetOut("ABC", "XYZ", "DDD")
    mockSink.requestNext() shouldBe LeaveRead(Read("ABC", "XYZ"))

    // then
    mockSink.requestNext() shouldBe WarehouseState(Set(), 1, 0, 0)
  }

  it should "report one mistake for letting out container that was never registered" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(2)

    // when
    statistics ! LetOut("ABC", "XYZ", "DDD")
    mockSink.requestNext()

    // then
    mockSink.requestNext() shouldBe WarehouseState(Set(), 1, 0, 0)
  }

  it should "report one mistake for letting out different container" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(4)
    statistics ! Registered("ABC", "XYZ")
    mockSink.requestNext()
    mockSink.requestNext() shouldBe WarehouseState(Set("XYZ"), 0, 0, 0)

    // when
    statistics ! LetOut("XYZ", "XYZ", "XYZ")
    mockSink.requestNext()

    // then
    mockSink.requestNext() shouldBe WarehouseState(Set(), 1, 0, 0)
  }

  it should "not report any mistakes when letting out the right container" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(4)
    statistics ! Registered("ABC", "XYZ")
    mockSink.requestNext()
    mockSink.requestNext() shouldBe WarehouseState(Set("XYZ"), 0, 0, 0)

    // when
    statistics ! LetOut("ABC", "DEF", "XYZ")
    mockSink.requestNext()

    // then
    mockSink.requestNext() shouldBe WarehouseState(Set(), 0, 0, 1)
  }

  it should "report leave read if some container was denied" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(1)

    // when
    statistics ! Denied("ABC", "XYZ")

    // then
    mockSink.requestNext() shouldBe LeaveRead(Read("ABC", "XYZ"))
  }

  it should "report one denial" in {
    // given
    val (statistics, statisticsFlow) = Statistics.create
    val (_, mockSink) = statisticsFlow.runWith(TestSource.probe[Nothing], TestSink.probe[StatisticsInfo])
    mockSink.request(2)

    // when
    statistics ! Denied("ABC", "XYZ")
    mockSink.requestNext()

    // then
    mockSink.requestNext() shouldBe WarehouseState(Set(), 0, 1, 0)
  }

}
