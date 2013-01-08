package org.fjn.neuralNetwork.multilayer.algorithm

import org.fjn.neuralNetwork.reader.TrainingData
import org.fjn.neuralNetwork.multilayer.architecture.Network

trait BackPropagation extends LearningAlgorithm{

  self:Network =>

  def learn(sample:TrainingData){

    val x = sample.input
    val t = sample.output


    //We save the current dW for later momentum addition

    if (layers.length < 3)
      sys.error("invalid architecture layout. No architecture can have less than 3 layers: Input-Hidden-Output")
    else {
      var n = layers.size - 1

      val oAux = forward(x)

      val o = layers.last.getProcessedOutput

      val err = o - t
      val D = layers.last.getDMatrix

      var deltasPlus = D * err
      val dW = (deltasPlus * fillOnes(layers(n-1).getProcessedOutput).transpose).transpose

      dWs((n-1,n)) = dWs((n-1,n)) + dW

      n -= 1
      while (n > 0) {
        val Ds = self.layers(n).getDMatrix
        val delta = Ds * sub(self.Ws((n,n+1))) * deltasPlus
        val dWb = (delta *fillOnes(layers(n-1).getProcessedOutput).transpose).transpose

        deltasPlus = delta
        self.dWs((n-1,n)) = self.dWs((n-1,n)) + dWb
        n -= 1
      }


    }

  }


}
