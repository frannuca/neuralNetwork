package org.fjn.neuralNetwork.multilayer.algorithm

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.common.NNMatrixExtensions
import org.fjn.neuralNetwork.reader.TrainingData
import org.fjn.neuralNetwork.multilayer.architecture.{OutputLayerExtractor, HiddenLayerExtractor, InputLayerExtractor, Network}


trait LearningAlgorithm extends Serializable with NNMatrixExtensions with OptimizationCtes {
  self: Network =>
  def learn(sample: TrainingData): Double

  protected def forward(x: Matrix[Double]): Matrix[Double] = {


    var in = new Matrix[Double](1, 1)
    applyMasks
    self.layers.foreach {
      case InputLayerExtractor(n) => in = self.layers(n).process(x)
      case HiddenLayerExtractor(n) => self.layers(n).process(fillOnes(in).transpose * self.Ws((n - 1, n)))
      case OutputLayerExtractor(n) => self.layers(n).process(fillOnes(in).transpose * self.Ws((n - 1, n)))
    }

    in
  }

}


