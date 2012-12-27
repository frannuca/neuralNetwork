package org.fjn.neuralNetwork.multilayer.activation

trait ActivationFunction {
  def trigger:Function1[Double,Double]
  def diffTrigger:Function1[Double,Double]
}
