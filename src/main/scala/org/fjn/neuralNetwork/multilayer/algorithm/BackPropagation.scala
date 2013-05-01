package org.fjn.neuralNetwork.multilayer.algorithm

import org.fjn.neuralNetwork.reader.TrainingData
import org.fjn.neuralNetwork.multilayer.architecture.Network

trait BackPropagation extends LearningAlgorithm{

  self:Network =>

  def learn(sample:TrainingData):Double={

    val x = sample.input
    val t = sample.output


    //We save the current dW for later momentum addition

    if (layers.length < 3)
      sys.error("invalid architecture layout. No architecture can have less than 3 layers: Input-Hidden-Output")
    else {

      ///We apply the current input vector to fill the cell structure with intermediate evaluations and gradients
      forward(x)


      ///Obtaining the output computed for this input vector
      val o = layers.last.getProcessedOutput

      val err = o - t
      val D = layers.last.getDMatrix

      var deltasPlus = D * err


      val n = layers.indices.last
      val dW = (deltasPlus * fillOnes(layers(n-1).getProcessedOutput).transpose).transpose

      dWs((n-1,n)) = dWs((n-1,n)) + dW

      for(n<- layers.indices.reverse.tail) {
        val Ds = self.layers(n).getDMatrix
        val delta = Ds * sub(self.Ws((n,n+1))) * deltasPlus
        val dWb = (delta *fillOnes(layers(n-1).getProcessedOutput).transpose).transpose

        deltasPlus = delta
        self.dWs((n-1,n)) += dWb
      }


    }

    forward(x)
    val eRR = (layers.last.getProcessedOutput - t)
    (eRR * eRR.transpose)(0,0)

  }


}
