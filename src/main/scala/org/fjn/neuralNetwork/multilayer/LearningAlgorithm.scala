package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.common.NNMatrixExtensions
import org.fjn.neuralNetwork.reader.TrainingData


trait LearningAlgorithm extends Serializable with NNMatrixExtensions with OptimizationCtes{
   self: Network =>
  def learn(sample:TrainingData)

 protected def forward(x:Matrix[Double]):Matrix[Double]={

   applyMasks
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


