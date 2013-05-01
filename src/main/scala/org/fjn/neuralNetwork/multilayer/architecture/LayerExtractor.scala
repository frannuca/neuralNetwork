package org.fjn.neuralNetwork.multilayer.architecture

object InputLayerExtractor {
  def unapply(layer:INNLayer):Option[Int]={
    layer match { case x:InputLayer=> Some(x.key); case _=> None }
  }
}

object HiddenLayerExtractor {
  def unapply(layer:INNLayer):Option[Int]={
    layer match { case x:HiddenLayer=> Some(x.key); case _=> None }
  }
}

object OutputLayerExtractor {
  def unapply(layer:INNLayer):Option[Int]={
    layer match { case x:OutputLayer=> Some(x.key); case _=> None }
  }
}
