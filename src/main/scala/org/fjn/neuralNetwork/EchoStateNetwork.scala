package org.fjn.neuralNetwork

import org.fjn.matrix.Matrix
import org.fjn.matrix.Scalar2MatrixConversions
import org.fjn.matrix.Scalar
import org.fjn.neuralNetwork.reader.TrainingData

/**
 * Created by fran on 26/05/2014.
 */
case class EchoStateNetwork(Nin:Int,N:Int,Nout:Int,alpha:Double,trigger:(Double)=>Double,data:Array[TrainingData]){

  var W = new Matrix[Double](N,N)
  var WOut= new Matrix[Double](Nout,N)
  var WIn= new Matrix[Double](N,Nin+1)
  var WBack = new Matrix[Double](N,Nout)
  var states = new Matrix[Double](N,1)
  var yold = new Matrix[Double](Nout,1).zeros


  private def generateMatrices{
    W = new Matrix[Double](N,N).random <= ((x) => 0.01*x)
    WOut=new Matrix[Double](Nout,N).random <= ((x) => 0.01*x)
    WIn=new Matrix[Double](N,Nin + 1).random <= ((x) => 0.01*x)

  }

  generateMatrices

  def computeOutput(u:Matrix[Double]):Matrix[Double]={
    updateState(u)
    yold = WOut * states
    yold
  }

  private def updateState(u:Matrix[Double]){

    val uExtended = new Matrix[Double](Nin+1,1,false,Some(u.getArray() ++ Array(1.0)))

    val statesAux: Matrix[Double] = (WIn * uExtended + W * states) <= trigger

    states =  states *(1.0 - alpha)  +  statesAux * alpha

  }


  def learn{

    val X = new Matrix[Double](data.length,N)
    val Y = new Matrix[Double](data.length,Nout)

    for(i <- data.indices){
      updateState(data(i).input)
      for(j <- 0 until N){
        X(i,j) = states(j,1)
      }


      for(j <- 0 until Nout){
        Y(i,j) = data(i).output(j,1)
      }

    }

    val aux = (X.transpose * X)
    aux.invert
      WOut = (aux * (X.transpose * Y)).transpose

  }

}
