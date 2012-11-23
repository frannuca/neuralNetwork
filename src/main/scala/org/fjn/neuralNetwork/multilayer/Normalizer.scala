package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

class Normalizer(originalTrainingSet: TrainingSet,triggerMaxY:Double,triggerMinY:Double,triggerMaxX:Double,triggerMinX:Double){

  private val maxMinMeanInput: IndexedSeq[(Double, Double, Double)] = {
    for (i <- 0 until originalTrainingSet.inputVectorDimension())
    yield {
      val itemi =
        for (sample <- originalTrainingSet) yield {
          sample.input(i, 0)
        }
      (itemi.max, itemi.min,itemi.sum/itemi.size.toDouble)
    }

  }


  private val maxMinMeanOutput: IndexedSeq[(Double, Double,Double)] = {
    for (i <- 0 until originalTrainingSet.outputVectorDimension())
    yield {
      val itemi =
        for (sample <- originalTrainingSet) yield {
          sample.output(i, 0)
        }
      (itemi.max, itemi.min,itemi.sum/itemi.size.toDouble)
    }

  }

  private val maxDifTriggerX = (triggerMaxX-triggerMinX)
  private val maxDifTriggerY = (triggerMaxY-triggerMinY)
  //normalizing inputs and outputs training set
  val normalizedTrainingSet = originalTrainingSet.map(s =>{

     val x = normaliseX(s.input)
     val y = normaliseY(s.output)

     new TrainingData(x,y)
  })


  def normaliseX(x:Matrix[Double]):Matrix[Double]={
    val r = x.clone()
    for (i <- 0 until x.numberRows){
      val y = (x(i,0) - maxMinMeanInput(i)._3)/(maxMinMeanInput(i)._1-maxMinMeanInput(i)._2)*maxDifTriggerX
      r.set(i,0,y)
    }
    r
  }

  def normaliseY(y:Matrix[Double]):Matrix[Double]={
    val r = y.clone()
    for (i <- 0 until y.numberRows){
      val yy = (y(i,0) - maxMinMeanOutput(i)._3)/(maxMinMeanOutput(i)._1-maxMinMeanOutput(i)._2) * maxDifTriggerY
      r.set(i,0,yy)
    }
    r
  }

  def deNormaliseX(x:Matrix[Double]):Matrix[Double]={
    val r = x.clone()
    for (i <- 0 until x.numberRows){
      val xx = r(i,0) * (maxMinMeanInput(i)._1-maxMinMeanInput(i)._2)/maxDifTriggerX + maxMinMeanInput(i)._3
      r.set(i,0,xx)
    }
    r
  }

  def deNormaliseY(x:Matrix[Double]):Matrix[Double]={
    val r = x.clone()
    for (i <- 0 until x.numberRows){
      val xx = r(i,0) * (maxMinMeanOutput(i)._1-maxMinMeanOutput(i)._2)/maxDifTriggerY + maxMinMeanOutput(i)._3
      r.set(i,0,xx)
    }
    r
  }

}
