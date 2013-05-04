package org.fjn.neuralNetwork.multilayer.activation

class AsymetricSigmoidea(val c:Double=1.0) extends ActivationFunction  {
  def trigger = f _
  def diffTrigger = df _


  def f(x: Double): Double = {
    val a = (1.0) / (1.0 + math.exp(-c * x))
    a
  }


  def df(x: Double): Double = {
    val fx = f(x)
    fx*(1.0-fx)
  }

  val maxXLimit = 100d
  val minXLimit = -100d
  val saturationCoefficient = 0.95d
}

