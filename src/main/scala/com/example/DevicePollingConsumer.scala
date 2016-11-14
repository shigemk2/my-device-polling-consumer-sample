package com.example

import java.util.Date

import akka.actor._

import scala.util.Random

object DevicePollingConsumerDriver extends CompletableApp(10) {
}

case class Monitor()

class EvenNumberDevice() {
  val random = new Random(99999)

  def nextEvenNumber(waitFor: Int): Option[Int] = {
    val timeout = new Timeout(waitFor)
    var nextEvenNumber: Option[Int] = None

    while (!timeout.isTimedOut && nextEvenNumber.isEmpty) {
      Thread.sleep(waitFor / 2)

      val number = random.nextInt(100000)

      if (number % 2 == 0) nextEvenNumber = Option(number)
    }

    nextEvenNumber
  }
}

class Timeout(withinMillis: Int) {
  val mark = currentTime

  def isTimedOut(): Boolean = {
    if (withinMillis == -1) false
    else currentTime - mark >= withinMillis
  }

  private def currentTime(): Long = {
    new Date().getTime
  }
}