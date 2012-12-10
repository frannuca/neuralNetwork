package org.fjn.neuralNetwork.multilayer

import activation.ActivationFunction
import normalization.NormalizerBase
import org.fjn.neuralNetwork.common.NNMatrixExtensions
import collection.mutable.ListBuffer
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

import org.fjn.matrix.Scalar
import org.fjn.matrix.Scalar2MatrixConversions._
import collection.immutable

case class NetworkData(layerDimensions:Seq[Int],activationFunction:ActivationFunction,samplesFilename:String)

abstract class Network(nnData:NetworkData)
  extends NNMatrixExtensions
  with LearningAlgorithm
  with Weights{

  val normalizer:NormalizerBase
  import nnData._



  lazy protected val layers: Seq[Layer] = for (n <- 0 until layerDimensions.length) yield {new Layer(layerDimensions(n),activationFunction)}


  def setMask(nlayer0:Int, scentil:Matrix[Double])={
    masks((nlayer0,nlayer0+1)).copyFrom(scentil)
  }




  def apply(x:Matrix[Double]):Matrix[Double]={
    val out = forward(normalizer.normaliseX(x))
    normalizer.deNormaliseY(out)
  }



  def computeError:Double={
    val e = (for (sample <- normalizer.normalizedTrainingSet)yield{
      val o = forward(sample.input)
      val d = (o -sample.output)
      math.sqrt((d * d.transpose)(0,0))
    }).toSeq.sum

    e
  }
  def solve(maxIter:Int):Double={


    var counter = 0

    for (iteration <- 0 until maxIter){

      var error0 = computeError
      dWs.foreach(m => m._2 <= (x => 0.0))
      for (sample <- normalizer.normalizedTrainingSet){
        learn(sample)
      }

      savedW
      updateWeights

      var error1 = computeError


      if (error1>error0){
        lr = lr *0.5
        println("lr="+lr.toString)
        if (lr<1e-6) lr = 1e-6
        undoBackUp
      }
      else{
          backUp
        if (counter > 10){
          lr= lr * 1.1
          println("lr="+lr.toString)
          counter = 0
        }

        counter += 1

        error0 = error1

        println(error0)
      }




  }


    computeError

  }

}
