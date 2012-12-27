package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.{NetworkData}
import org.fjn.neuralNetwork.reader.TrainingData

trait Normalizer {

  val triggerFunc:Function1[Double,Double]

  val originalTrainingSet:Array[TrainingData]
  val normalizedTrainingSet:Array[TrainingData]

  def normaliseX(x:Matrix[Double]):Matrix[Double]
  def normaliseY(y:Matrix[Double]):Matrix[Double]
  def deNormaliseX(x:Matrix[Double]):Matrix[Double]
  def deNormaliseY(x:Matrix[Double]):Matrix[Double]


}
