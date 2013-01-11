package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.common.NNMatrixExtensions
import collection.mutable.ListBuffer
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

import org.fjn.matrix.Scalar
import org.fjn.matrix.Scalar2MatrixConversions._
import collection.immutable
import org.fjn.neuralNetwork.reader.{TrainingData, DataReader}
import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction
import org.fjn.neuralNetwork.multilayer.algorithm.LearningAlgorithm

case class NetworkData(layerDimensions:Seq[Int],activationFunction:ActivationFunction,dataSet:Seq[TrainingData])

trait Network
  extends Serializable
  with NNMatrixExtensions
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


    lr = lr0
    momentum= momentum0
    var counter = 0
    var counterErrors = 0
    var minError = computeError
    backUp


    for (iteration <- 0 until maxIter){



      var error0 = computeError
      dWs.foreach(_._2.zeros)

      println("iteration"+iteration.toString)
      nnData.dataSet.foreach(learn)

      updateWeights
      savedW
      var error1 = computeError



      if (error1>=error0){

        println("lr="+lr.toString)

        counterErrors = counterErrors + 1
        if (counterErrors > 15){
          lr = lr *0.25

          undoBackUp
          val a= dWsHistory
          println("*****************************Randomizing Weights")
          randomizeWeigths
          counterErrors = 0

        }
        counter = 0
      }
      else{

        counterErrors = 0
        println("*****************************GOOD!!!!")
        if (error1<minError)
        {
          backUp
          minError= error1
        }

        if (counter > 10 && error1 < minError){
          lr= lr * 1.5
          counter = 0
        }
        else if  (counter > 10 && error1 >  minError){
          lr= lr * 0.75
          counter = 0
        }


        println("lr="+lr.toString)
        counter += 1

        error0 = error1

        println(error0)
      }



      println("error0="+error0.toString)
      println("error1="+error1.toString)
      println("errorMin="+minError.toString)

    }

    undoBackUp
    computeError

  }

}
