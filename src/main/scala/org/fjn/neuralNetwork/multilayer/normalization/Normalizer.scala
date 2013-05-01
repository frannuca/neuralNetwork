package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.reader.TrainingData

trait Normalizer  extends Serializable {

  val triggerFunc:(Double) => Double

  val originalTrainingSet:Array[Matrix[Double]]

  val normalizedSamples: Array[Matrix[Double]]

  def normalise(x:Matrix[Double]):Matrix[Double]
  def deNormalise(x:Matrix[Double]):Matrix[Double]
}

