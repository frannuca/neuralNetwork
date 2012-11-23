package org.fjn.neuralNetwork.multilayer

import activation.ActivationFunction
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 18/11/12
 * Time: 18:10
 * To change this template use File | Settings | File Templates.
 */
class Layer(dim:Int,activationFunc:ActivationFunction){


  def size = dim
  val cells: IndexedSeq[Cell] = (0 until dim).map(i => new Cell(activationFunc))
  def apply(x:Matrix[Double],isInput:Boolean=false):Matrix[Double]={
    require(x.numberRows == cells.length && x.numberCols == 1)


    if (!isInput)
      new Matrix[Double](cells.length,1) <= (0 until cells.length).map(i => cells(i)(x(i,0)).out)
    else
      new Matrix[Double](cells.length,1) <= (0 until cells.length).map(i =>{cells(i).cellOutput = CellStimulus( x(i,0),0); x(i,0)})


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
