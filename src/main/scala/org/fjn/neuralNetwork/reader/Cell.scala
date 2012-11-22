package org.fjn.neuralNetwork.reader

import activation.ActivationFunction


case class CellStimulus(out:Double,diffOut:Double)

class Cell(activation:ActivationFunction) {
  var cellOutput =  CellStimulus(0d,0d)

  def apply(x:Double):CellStimulus={
    cellOutput = CellStimulus(activation.trigger(x),activation.diffTrigger(x))
    cellOutput
  }

}
