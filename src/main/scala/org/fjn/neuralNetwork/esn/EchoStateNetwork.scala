package org.fjn.neuralNetwork.esn

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.reader.TrainingData
import org.apache.log4j.Logger
import org.fjn.optimization.evolutionary.pso._
import org.fjn.matrix.Scalar2MatrixConversions._


/**
 * Created by fran on 26/05/2014.
 */
case class EchoStateNetwork(Nin:Int,N:Int,Nout:Int,alpha:Double,trigger:(Double)=>Double,data:Array[TrainingData],
                             Wsparsity:Int,ridgeCoeff:Double,WInScale:Double){


  val logger = Logger.getLogger(classOf[EchoStateNetwork].getName());

  var W:Matrix[Double]=null
  var WOut:Matrix[Double]=null
  var WIn:Matrix[Double]=null
  var states = new Matrix[Double](N,1)


  private def generateMatrices{
    W = new Matrix[Double](N,N).random <= ((x) => (x-0.5))
    for(i <- 0 until N;
        j <- 0 until N){
      if(math.abs(i-j)>Wsparsity)
       W(i,j)=0
    }

    WIn=new Matrix[Double](N,Nin + 1).random <= ((x) => (x-0.5))
    WIn <= ((x)=>x * WInScale)

    //spectral radius:
    val (l,_) = W.eigVectors
    val maxL = l.getArray().map(math.abs(_)).max
    W = W <= ((x)=> x*0.1)

  }

  generateMatrices

  def computeOutput(u:Matrix[Double]):Matrix[Double]={
    updateState(u)
    val lst = states.getArray().toList ++ u.getArray().toList ++ List(1.0)
    val yold =  WOut.transpose * (new Matrix[Double](N+Nin+1,1) <= lst.toSeq)
    yold
  }


  private def updateState(u:Matrix[Double]){

    val uExtended = new Matrix[Double](Nin+1,1,false,Some(u.getArray() ++ Array(1.0)))

    val statesAux: Matrix[Double] = (WIn * uExtended + W * states) <= trigger

    states =  states *(1.0 - alpha)  +  statesAux * alpha

  }


  val drop4Transient = 50
  def learn={

    //leaving transient state:
    for( i <- 0 until drop4Transient)
    {
      updateState(data(i).input)
    }

    data.drop(drop4Transient)
    val X = new Matrix[Double](data.length,N+Nin+1)
    val Y = new Matrix[Double](data.length,Nout)


    for(i <- data.indices){

      println(s"$i/${data.length}")
      updateState(data(i).input)
      for(j <- 0 until N){
        X(i,j) = states(j,0)
      }

      for(j <- 0 until Nin){
        X(i,j+N) = data(i).input(j,0)
      }

      X(i,N+Nin) = 1.0


      for(j <- 0 until Nout){
        Y(i,j) = data(i).output(j,0)
      }

    }



    var bI = new Matrix[Double](N+Nin+1,N+Nin+1)
    bI.eye
    bI = bI * ridgeCoeff

    val aux = (X.transpose * X +  bI)
    aux.invert
    WOut = (aux * (X.transpose * Y))

    X
  }



}
