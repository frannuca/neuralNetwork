package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.common.NNMatrixExtensions
import collection.mutable.ListBuffer
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

import org.fjn.matrix.Scalar
import org.fjn.matrix.Scalar2MatrixConversions._
import collection.immutable
import org.fjn.neuralNetwork.reader.{TrainingData}
import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction
import org.fjn.neuralNetwork.multilayer.algorithm.LearningAlgorithm
import org.math.plot.Plot2DPanel
import javax.swing.JFrame
import util.Random
import org.fjn.neuralNetwork.multilayer.normalization.{Normalizer, PrincipalValueDecompositionNormalizer}

case class NetworkData(layerDimensions:Seq[Int],activationFunction:ActivationFunction,dataSet:Seq[TrainingData])

trait Network
  extends Serializable
  with NNMatrixExtensions
  with LearningAlgorithm
  with Weights
  with Normalizer{


  /**
   * training set and trigger function applied to all neurons is given inside the nnData instance
   */
  val nnData:NetworkData

  /** generate the sequence of layers composing our neural network*/
  lazy protected val layers: Seq[INNLayer] =
    nnData.layerDimensions.zipWithIndex.map{
      case (dim,idx)=>
        if(idx == 0)
          new InputLayer(size = dim,normalizer = this)

        else if (idx == nnData.layerDimensions.length-1)
          new OutputLayer(size = dim,activationFunc = nnData.activationFunction, key=idx)

        else
          new HiddenLayer(size = dim,activationFunc = nnData.activationFunction, key = idx)

    }


/** resets the saturation coefficients associated to the   **/
  def clearSaturationMeasures={
        layers.foreach(l => l.clearSaturationMeasure)
  }

  def setMask(nlayer0:Int, scentil:Matrix[Double])={
    masks((nlayer0,nlayer0+1)).copyFrom(scentil)
  }


  def apply(x:Matrix[Double]):Matrix[Double]={
     forward(x)
  }

  def solve(maxIter:Int):Double={

    lr = lr0
    momentum= momentum0
    var counterErrors = 0
    var minError = 1e16
    saveW


    for (iteration <- 0 until maxIter){

      dWs.foreach(_._2.zeros)

      println("iteration"+iteration.toString)
      val rnd = new Random(iteration)

      clearSaturationMeasures

      val errorNow = nnData.dataSet.indices.map(i =>{
        val orig = i//(rnd.nextDouble()*nnData.dataSet.length).toInt
        learn( nnData.dataSet(orig%nnData.dataSet.length))
      }).fold(0.0)((acc,v)=> acc+v)

       Ws.foreach{case((i1,i2),w) => println((i1,i2).toString+"--"+w.toString())}


      updateWeights

       Ws.foreach{case((i1,i2),w) => println((i1,i2).toString+"--"+w.toString())}


      if (errorNow<minError){

        saveW
        println("lr="+lr.toString)
        //lr = lr *0.75
        minError=errorNow

      }
     else{
        counterErrors = counterErrors + 1
      }

      println("ERROR %s".format(errorNow))

//      if(counterErrors>20)
//      {
//        undoBackUp
//        counterErrors=0
//        Ws.foreach{
//          case ((i,j),w) => w <:= ( (w.clone.random -0.5) * 0.25 <= ((x) => 1+x))
//        }
//
//      }

    }

    undoBackUp

    minError

  }

}
