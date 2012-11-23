package org.fjn.neuralNetwork.multilayer

import activation.ActivationFunction
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

  import nnData._


  lazy protected val trainingSet:TrainingSet= new TrainingSet(samplesFilename)


  lazy val Ymax = nnData.activationFunction.trigger(100)
  val Ymin = nnData.activationFunction.trigger(-100)
  lazy val Xmax: Double =  (0 until 1000).map(i => {
    if(nnData.activationFunction.trigger(i.toDouble/100.0) > Ymax*0.87)
      Some(i/100.0)
    else None
  }
    ).toSeq.flatten.head

  lazy val Xmin = -Xmax
  lazy val normalizer =  new Normalizer(
                        originalTrainingSet=trainingSet,
                        triggerMaxY = Ymax,
                        triggerMinY  = Ymin,
                        triggerMaxX = Xmax,
                        triggerMinX = Xmin
                        )

  lazy protected val normTrainingSet = normalizer.normalizedTrainingSet

  lazy protected val layers: Seq[Layer] = for (n <- 0 until layerDimensions.length) yield {new Layer(layerDimensions(n),activationFunction)}







  lazy val dWs = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until layers.length-1).map(l =>{
    dWs += (l,l+1) -> Ws((l,l+1)).clone().zeros
  })

  def setMask(nlayer0:Int, scentil:Matrix[Double])={
    masks((nlayer0,nlayer0+1)).copyFrom(scentil)
  }



  def apply(x:Matrix[Double]):Matrix[Double]={
    val out = forward(normalizer.normaliseX(x))
    normalizer.deNormaliseY(out)
  }



  def computeError:Double={
    val e = (for (sample <- normTrainingSet)yield{
      val o = forward(sample.input)
      val d = (o -sample.output)
      math.sqrt((d * d.transpose)(0,0))
    }).toSeq.sum

    e
  }
  def solve(maxIter:Int):Double={

    //TODO: continue here

    var lr = 0.2

    var counter = 0

    for (iteration <- 0 until maxIter){

      var error0 = computeError
      dWs.foreach(m => m._2 <= (x => 0.0))
      for (sample <- normTrainingSet){
        learn(sample)
      }

      (0 until Ws.size).foreach(i =>{
        Ws((i,i+1)) = Ws((i,i+1)) - lr * dWs((i,i+1))
        println(Ws((i,i+1)))
        println("--------------------------")
    })
      var error1 = computeError


      if (error1>error0){
        lr = lr *0.85
        if (lr<0.01) lr = 0.01
        undoBackUp
      }
      else{
          backUp
        if (counter > 10){
          lr= lr * 1.05
          counter = 0
        }

        counter += 1

        error0 = error1

      }




  }


    computeError

  }

}
