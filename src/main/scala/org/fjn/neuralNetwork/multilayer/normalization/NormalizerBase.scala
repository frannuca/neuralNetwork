package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.TrainingSet
import org.fjn.neuralNetwork.reader.TrainingData

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 05/12/12
 * Time: 23:51
 * To change this template use File | Settings | File Templates.
 */
trait NormalizerBase {

  val originalTrainingSet:Iterable[TrainingData]
  val normalizedTrainingSet:Iterable[TrainingData]

  def normaliseX(x:Matrix[Double]):Matrix[Double]
  def normaliseY(y:Matrix[Double]):Matrix[Double]
  def deNormaliseX(x:Matrix[Double]):Matrix[Double]
  def deNormaliseY(x:Matrix[Double]):Matrix[Double]


}
