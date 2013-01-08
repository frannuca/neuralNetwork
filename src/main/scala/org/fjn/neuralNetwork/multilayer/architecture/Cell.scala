package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction


case class CellData(out:Double,diffOut:Double)

class Cell(activation:ActivationFunction) extends Serializable {
  var cellOutput =  CellData(0d,0d)

  def apply(x:Double):CellData={
    cellOutput = CellData(activation.trigger(x),activation.diffTrigger(x))
    cellOutput
  }

}
