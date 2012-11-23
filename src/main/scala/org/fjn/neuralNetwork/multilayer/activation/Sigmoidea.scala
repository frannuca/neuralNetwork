package org.fjn.neuralNetwork.multilayer.activation

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 23/11/12
 * Time: 08:02
 * To change this template use File | Settings | File Templates.
 */
class Sigmoidea extends ActivationFunction  {
  def trigger = f _
  def diffTrigger = df _

  val c = 1.0
  def f(x: Double): Double = {
    val a = (1.0 - math.exp(-c * x)) / (1.0 + math.exp(-c * x))
    a
  }

  def df(x: Double): Double = {
    val a = (f(x + 1e-7) - f(x - 1e-7)) / 2e-7
    a
  }
}

