package org.fjn.neuralNetwork.multilayer.activation

class Sigmoidea(val c:Double = 1.0) extends ActivationFunction  {
  def trigger = f _
  def diffTrigger = df _



  def f(x: Double): Double = {
    val a = (1.0 - math.exp(-c * x)) / (1.0 + math.exp(-c * x))
    a
  }

//  def f(x: Double): Double = {
//    val a = (1.0) / (1.0 + math.exp(-c * x))
//    a
//  }


  def df(x: Double): Double = {
    val a = (f(x + 1e-7) - f(x - 1e-7)) / 2e-7
    a
//    2.0*c/(1.0 + math.exp(-c * x))
  }

  val maxXLimit = 100d
  val minXLimit = -100d
  val saturationCoefficient = 0.95d
}

