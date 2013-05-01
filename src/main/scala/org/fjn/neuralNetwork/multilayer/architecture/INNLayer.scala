package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction
import org.fjn.matrix.Matrix
import scala.collection.immutable.IndexedSeq
import org.fjn.neuralNetwork.multilayer.architecture

trait INNLayer {

  val activationFunc:ActivationFunction
  val size:Int

  val key:Int

  lazy val cells: IndexedSeq[architecture.Cell] =
    (0 until size).map(i => new architecture.Cell(activationFunc))

  def clearSaturationMeasure={
    cells.foreach(c => c.clearSaturationCounter)
  }

  def getDMatrix:Matrix[Double]={
    val D = new Matrix[Double](size,size).zeros
    for (i<- 0 until size){
      D.set(i,i,cells(i).cellOutput.diffOut)
    }
    D
  }

  def getProcessedOutput:Matrix[Double] =
    new Matrix[Double](size,1) <= cells.map(c => c.cellOutput.out)


  /**
   * Applies the provide input vector (of size dim equal to the number of cells in the layer).
   * In case of having configured this layers as input, the obtained output will be just a copy
   * of the input vector x, since input layer is only considers as a transformation layer
   * @param x input vector of matching size with the number of cells in this layer
   * @return processed vector of output after applying the the trigger functions of each cell
   */
  def process(x:Matrix[Double]):Matrix[Double]={
    require(x.numberRows == cells.length && x.numberCols == 1)

    new Matrix[Double](cells.length,1) <=
      cells.indices.map(i => cells(i)(x(i,0)).out)

  }


}
