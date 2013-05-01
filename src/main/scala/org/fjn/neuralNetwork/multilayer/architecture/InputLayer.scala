package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.Normalizer

case class InputLayer(size:Int,normalizer:Normalizer) extends INNLayer {


  val key=0
  val activationFunc:ActivationFunction = new ActivationFunction {
    def diffTrigger =  x => 1.0

    def trigger =  x=>x

    val saturationCoefficient = 1.0
    val maxXLimit = 1e9d
    val minXLimit = -1e9d
  }


  /**
   * Normalizes the input row data into the normalized input suitable to be propagated into the next
   * the hidden layer
   * @param x input vector of matching size with the number of cells in this layer
   * @return normalized input vector
   */
  override def process(x:Matrix[Double]):Matrix[Double]={
    super.process(normalizer.normalise(x))
  }

}
