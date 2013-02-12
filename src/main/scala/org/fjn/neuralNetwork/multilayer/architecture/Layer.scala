package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq
import org.fjn.neuralNetwork.multilayer.architecture
import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction

class Layer(dim:Int,activationFunc:ActivationFunction) extends Serializable{

  def size = dim
  val cells: IndexedSeq[architecture.Cell] = (0 until dim).map(i => new architecture.Cell(activationFunc))

  def clearSaturationMeasure={
    cells.foreach(c => c.clearSaturationMeasure)
  }

  def apply(x:Matrix[Double],isInput:Boolean=false):Matrix[Double]={
    require(x.numberRows == cells.length && x.numberCols == 1)


    if (!isInput)
      new Matrix[Double](cells.length,1) <= cells.indices.map(i => cells(i)(x(i,0)).out)
    else
      new Matrix[Double](cells.length,1) <= cells.indices.map(i =>{cells(i).cellOutput = CellData( x(i,0),0); x(i,0)})


  }


  def getDMatrix:Matrix[Double]={
    val D = new Matrix[Double](size,size).zeros
    for (i<- 0 until size){
      D.set(i,i,cells(i).cellOutput.diffOut)
    }
    D
  }
  def getProcessedOutput:Matrix[Double] = new Matrix[Double](size,1) <= cells.map(c => c.cellOutput.out)

}
