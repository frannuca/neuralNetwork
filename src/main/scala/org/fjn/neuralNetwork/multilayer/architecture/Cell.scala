package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction


/**
 * basic cell data to contain excitation and its first derivative outputs
 * @param out
 * @param diffOut
 */
case class CellData(out:Double,diffOut:Double)

/**
 * A cell is a discriminative unit with a given discrimination function.
 * Each cell has a saturation counter which helps to trace how man time the
 * output of the cell has reach any limit in abcissa axis (top or bottom)
 * @param activation discrimination function
 */
class Cell(activation:ActivationFunction) extends Serializable{

  var cellOutput =  CellData(0d,0d)


  private var saturationCounter = 0
  def isSaturated =    cellOutput.out > activation.trigger(activation.maxXLimit)*activation.saturationCoefficient ||
    cellOutput.out < activation.trigger(activation.minXLimit)*activation.saturationCoefficient


  def clearSaturationCounter = saturationCounter=0

  def apply(x:Double):CellData={
    cellOutput = CellData(activation.trigger(x),activation.diffTrigger(x))
    if (isSaturated) saturationCounter += 1
    cellOutput
  }

}
