package org.fjn.neuralNetwork.multilayer

import activation.ActivationFunction


case class CellData(out:Double,diffOut:Double)

class Cell(activation:ActivationFunction) extends Serializable {
  var cellOutput =  CellData(0d,0d)

  def apply(x:Double):CellData={
    cellOutput = CellData(activation.trigger(x),activation.diffTrigger(x))
    cellOutput
  }

}
