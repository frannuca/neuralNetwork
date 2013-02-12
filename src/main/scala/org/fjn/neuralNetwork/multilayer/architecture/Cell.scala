package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction


case class CellData(out:Double,diffOut:Double)

class Cell(activation:ActivationFunction) extends Serializable {
  private var saturationCoeff:Int = 0
  var cellOutput =  CellData(0d,0d)

  def isSaturated =    cellOutput.out > activation.trigger(100)*0.95 || cellOutput.out < activation.trigger(-100)*0.95

  def getSaturationCoeff = saturationCoeff
  def clearSaturationMeasure= saturationCoeff = 0

  def apply(x:Double):CellData={
    cellOutput = CellData(activation.trigger(x),activation.diffTrigger(x))
    if (isSaturated){
      saturationCoeff += 1
    }

    cellOutput
  }

}
