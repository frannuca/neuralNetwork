package org.fjn.neuralNetwork.reader.activation

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 18/11/12
 * Time: 18:26
 * To change this template use File | Settings | File Templates.
 */
trait ActivationFunction {
  def trigger:Function1[Double,Double]
  def diffTrigger:Function1[Double,Double]
}
