package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq
import org.fjn.neuralNetwork.reader.{DataReader, TrainingData}
import org.fjn.neuralNetwork.multilayer.{NetworkData, TrainingSet}

trait Normalizer extends NormalizerBase {
  val nnData:NetworkData

  lazy val triggerMaxY:Double = this.nnData.activationFunction.trigger(100)
  lazy val triggerMinY:Double = this.nnData.activationFunction.trigger(-100)


  lazy private val xvals = (-1000 until 1000).map(i => this.nnData.activationFunction.trigger(i.toDouble/100.0))
  lazy val triggerMaxX:Double = xvals.filter(d => d>triggerMaxY*0.9).head
  lazy val triggerMinX:Double = xvals.filter(d => d<triggerMinY*0.9).last

  lazy val originalTrainingSet =  new TrainingSet(nnData.samplesFilename)

  lazy private val maxMinMeanInput: IndexedSeq[(Double, Double, Double)] = {
    for (i <- 0 until originalTrainingSet.inputVectorDimension())
    yield {
      val itemi =
        for (sample <- originalTrainingSet) yield {
          sample.input(i, 0)
        }
      (itemi.max, itemi.min,itemi.sum/itemi.size.toDouble)
    }

  }


  lazy private val maxMinMeanOutput: IndexedSeq[(Double, Double,Double)] = {
    for (i <- 0 until originalTrainingSet.inputVectorDimension())
    yield {
      val itemi =
        for (sample <- originalTrainingSet) yield {
          sample.output(i, 0)
        }
      (itemi.max, itemi.min,itemi.sum/itemi.size.toDouble)
    }

  }

  lazy private val maxDifTriggerX = (triggerMaxX-triggerMinX)
  lazy private val maxDifTriggerY = (triggerMaxY-triggerMinY)

  //normalizing inputs and outputs training set
  lazy val normalizedTrainingSet = originalTrainingSet.map(s =>{

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
