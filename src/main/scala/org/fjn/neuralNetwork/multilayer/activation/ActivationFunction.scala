package org.fjn.neuralNetwork.multilayer.activation

trait ActivationFunction extends Serializable {
  def trigger:Function1[Double,Double]
  def diffTrigger:Function1[Double,Double]
  val maxXLimit:Double
  val minXLimit:Double
  val saturationCoefficient:Double
}
