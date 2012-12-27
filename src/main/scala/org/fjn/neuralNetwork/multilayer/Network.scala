package org.fjn.neuralNetwork.multilayer

import activation.ActivationFunction
import normalization.Normalizer
import org.fjn.neuralNetwork.common.NNMatrixExtensions
import collection.mutable.ListBuffer
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

import org.fjn.matrix.Scalar
import org.fjn.matrix.Scalar2MatrixConversions._
import collection.immutable
import org.fjn.neuralNetwork.reader.{TrainingData, DataReader}

case class NetworkData(layerDimensions:Seq[Int],activationFunction:ActivationFunction,dataSet:Seq[TrainingData])

trait Network
  extends NNMatrixExtensions
  with LearningAlgorithm
  with Weights{


  val nnData:NetworkData
  lazy protected val layers: Seq[Layer] = nnData.layerDimensions.map(dim =>  new Layer(dim,nnData.activationFunction))

  def setMask(nlayer0:Int, scentil:Matrix[Double])={
    masks((nlayer0,nlayer0+1)).copyFrom(scentil)
  }

  def apply(x:Matrix[Double]):Matrix[Double]={
     forward(x)
  }

  def computeError:Double={
    val e = nnData.dataSet.map{sample =>
      val o = forward(sample.input)
      val d = (o -sample.output)
      math.sqrt((d * d.transpose)(0,0))
    }.toSeq.sum

    e
  }

  def solve(maxIter:Int):Double={

    var counter = 0

    for (iteration <- 0 until maxIter){

      var error0 = computeError
      dWs.foreach(_._2.zeros)

      println("iteration"+iteration.toString)
      nnData.dataSet.foreach(learn)


      savedW
      updateWeights
      var error1 = computeError
      println("error0="+error0.toString)
      println("error1="+error1.toString)


      if (error1>error0){
        lr = lr *0.5
        println("lr="+lr.toString)
        undoBackUp
        if (lr<1e-6){
          println("*****************************Randomizing Weights")
          randomizeWeigths
          counter = 0
          lr = 0.01
        }
      }
      else{

        println("*****************************GOOD!!!!")
          backUp
        if (counter > 10){
          lr= lr * 1.05
          counter = 0
        }
        println("lr="+lr.toString)
        counter += 1

        error0 = error1

        println(error0)
      }




  }


    computeError

  }

}
