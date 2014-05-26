package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.architecture.NetworkData
import org.fjn.neuralNetwork.reader.TrainingData
import scala.collection.immutable.IndexedSeq

/**
 * Normalizer which process input data as follows:
 * 1) Principal component analysis of the training set
 * 2) optional elimination of the redundant dimensions
 * 3) normalization of the results into the dynamic ranges of the trigger function coordinate-axis
 */
class PrincipalValueDecompositionNormalizer(val originalTrainingSet: Array[Matrix[Double]],val triggerFunc: (Double) => Double) extends Normalizer {


  val mean: Matrix[Double] =
    originalTrainingSet.fold(originalTrainingSet.head.clone().zeros)((acc, y) => acc + y) / originalTrainingSet.length

  private def pca = {

    val X = new Matrix[Double](originalTrainingSet.head.numberRows, originalTrainingSet.length)

    for{i <- originalTrainingSet.indices
        j <- originalTrainingSet(i).getArray().indices}{
      X.set(j,i,originalTrainingSet(i)(j,0)-mean(j,0))
    }

    val Cx = (X*X.transpose)  * 1.0 / (X.numberCols - 1.0)

    Cx.eigVectors._2.transpose
  }

  lazy private val P = pca
  lazy private val Pinv = P.transpose



  private def intoPricipalComponentSpace(x:Matrix[Double])={
    P * (x - mean)
  }

  lazy private val normCoeff: Matrix[Double] = computeNormCoeff

  private def computeNormCoeff={
    val norm0 = originalTrainingSet.map(intoPricipalComponentSpace)
    val dVal =
    for(i <- norm0.head.getArray().indices) yield{
      val aux = norm0.map(v => v(i,0))

      aux.max-aux.min
    }


   new Matrix[Double](dVal.size,1) <= dVal
  }


  def normalise(x: Matrix[Double]) = {
    val xn =intoPricipalComponentSpace(x)
    xn <:= normCoeff
    xn

  }

  def deNormalise(x: Matrix[Double]) = {

    Pinv*x+mean

  }

  lazy val normalizedSamples: Array[Matrix[Double]] = originalTrainingSet.map(intoPricipalComponentSpace)

}
