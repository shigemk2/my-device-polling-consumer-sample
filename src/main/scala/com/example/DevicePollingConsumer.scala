package com.example

import akka.actor._

object DevicePollingConsumerDriver extends CompletableApp(10) {
}

case class Monitor()
