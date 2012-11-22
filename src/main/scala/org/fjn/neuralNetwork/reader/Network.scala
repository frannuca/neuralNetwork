package org.fjn.neuralNetwork.reader

import activation.ActivationFunction
import org.fjn.neuralNetwork.common.NNMatrixExtensions
import collection.mutable.ListBuffer
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

import org.fjn.matrix.Scalar
import org.fjn.matrix.Scalar2MatrixConversions._
import collection.immutable

case class NetworkData(layerDimensions:Seq[Int],activationFunction:ActivationFunction,samplesFilename:String)

class Network(nnData:NetworkData) extends NNMatrixExtensions{

  import nnData._


  protected val trainingSet:TrainingSet= new TrainingSet(samplesFilename)


  val Ymax = nnData.activationFunction.trigger(100)
  val Ymin = nnData.activationFunction.trigger(-100)
  val Xmax: Double =  (0 until 1000).map(i => {
    if(nnData.activationFunction.trigger(i.toDouble/100.0) > Ymax*0.87)
      Some(i/100.0)
    else None
  }
    ).toSeq.flatten.head

  val Xmin = -Xmax
  val normalizer =  new Normalizer(
                  originalTrainingSet=trainingSet,
                  triggerMaxY = Ymax,
                  triggerMinY  = Ymin,
                  triggerMaxX = Xmax,
                  triggerMinX = Xmin
                  )

  protected val normTrainingSet = normalizer.normalizedTrainingSet

  protected val layers: Seq[Layer] = for (n <- 0 until layerDimensions.length) yield {new Layer(layerDimensions(n),activationFunction)}

  val WsBackup = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until layers.length-1).map(l =>{
    WsBackup += (l,l+1) -> new Matrix[Double](layers(l).size+1,layers(l+1).size).random
  })

  val Ws = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until layers.length-1).map(l =>{
    Ws += (l,l+1) -> new Matrix[Double](layers(l).size+1,layers(l+1).size).random
  })


  def backUp{
    WsBackup.clear()
    Ws.foreach(item => WsBackup += (item._1)-> item._2.clone())

  }
  def undoBackUp{
    Ws.clear()
    WsBackup.foreach(item => Ws += (item._1)-> item._2.clone())

  }


  val masks = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until layers.length-1).map(l =>{
    masks += (l,l+1) -> (Ws((l,l+1)).clone() <= (x => 1.0))
  })


  val dWs = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
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
  protected def forward(x:Matrix[Double]):Matrix[Double]={

    var in = x
     layers(0)(in,true)
    (1 until layers.length).foreach( n => {

      Ws((n-1,n)) <:= masks((n-1,n))

      val o = fillOnes(in).transpose * Ws((n-1,n))


     in = layers(n)(o.transpose)

    })

    in
  }

  protected def backward(x:Matrix[Double],t:Matrix[Double]){

    if (layers.length < 3)
      sys.error("invalid network layout. No network can have less than 3 layers: Input-Hidden-Output")
    else {
      var n = layers.size - 1

      val oAux = forward(x)


      val o = layers.last.getProcessedOutput

      val err = o - t
      val D = layers.last.getDMatrix


      var deltasPlus = D * err
      val dW = (deltasPlus * fillOnes(layers(n-1).getProcessedOutput).transpose).transpose

      dWs((n-1,n)) = dWs((n-1,n)) + dW

      n -= 1
      while (n > 0) {
        //TODO: is this correct?
        Ws((n-1,n)) <:= masks((n-1,n))

        val Ds = layers(n).getDMatrix
        val delta = Ds * sub(Ws((n,n+1))) * deltasPlus
        val dWb = (delta *fillOnes(layers(n-1).getProcessedOutput).transpose).transpose

        deltasPlus = delta
        dWs((n-1,n)) = dWs((n-1,n)) + dWb
        n -= 1
      }
    }
  }


  def computeError:Double={
    val e = (for (sample <- normTrainingSet)yield{
      val o = forward(sample.input)
      val d = (o -sample.output)
      math.sqrt((d * d.transpose)(0,0))
    }).toSeq.sum

    e
  }
  def solve(tolerance:Double,maxIter:Int):Double={

    //TODO: continue here

    var lr = 0.2

    var counter = 0
    for (iteration <- 0 until maxIter ){

      var error0 = computeError
      dWs.foreach(m => m._2 <= (x => 0.0))
      for (sample <- normTrainingSet){
        backward(sample.input,sample.output)
      }

      (0 until Ws.size).foreach(i =>{
        Ws((i,i+1)) = Ws((i,i+1)) - lr * dWs((i,i+1))
        println(Ws((i,i+1)))
        println("--------------------------")
    })
      var error1 = computeError

      println("error1="+(error1).toString)
      println("diff="+(error0-error1).toString)
      println("lr="+lr.toString)

      if (error1>error0){
        lr = lr *0.95
        if (lr<0.01) lr = 0.01
        undoBackUp
      }
      else{
          backUp
        if (counter > 10){
          lr= lr * 1.01
          counter = 0
        }

        counter += 1

        error0 = error1

      }




  }


    computeError

  }

}
