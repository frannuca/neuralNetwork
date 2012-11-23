package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.common.NNMatrixExtensions


trait LearningAlgorithm extends NNMatrixExtensions{
   self: Network =>
  def learn(sample:TrainingData)

 protected def forward(x:Matrix[Double]):Matrix[Double]={

    var in = x
    self.layers(0)(in,true)
    (1 until self.layers.length).foreach( n => {

      self.Ws((n-1,n)) <:= self.masks((n-1,n))

      val o = fillOnes(in).transpose * self.Ws((n-1,n))


      in = self.layers(n)(o.transpose)

    })

    in
  }
}


trait BackPropagation extends LearningAlgorithm{

  self:Network =>

  def learn(sample:TrainingData){
    val x = sample.input
    val t = sample.output

    if (layers.length < 3)
      sys.error("invalid network layout. No network can have less than 3 layers: Input-Hidden-Output")
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
        //TODO: is this correct?
        Ws((n-1,n)) <:= masks((n-1,n))

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