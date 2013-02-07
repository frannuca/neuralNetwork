package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{ Normalizer, MeanNormalizer}
import org.fjn.optimization.Regression
import org.fjn.neuralNetwork.multilayer.finance.TimeSeriesData
import collection.immutable.IndexedSeq

/**
 * financial data reader. This class transform historical data presented with the yahoo history format into valid
 * time series data given a variety of parameters including time window forecast etc...
 */
case class FinancialDataReader(data: TimeSeriesData) extends FinancialDataReaderBase{

  outer:FinancialDataReader =>

  import data._
  //val regressor = new Regression(regressionOrder)

  private def movingAverage(data:Array[TrainingData],nSamples:Int):Seq[TrainingData]={
    if (nSamples<=0) data
    else{
      val zeroInput = data.head.input.clone().zeros
      val zeroOut = data.head.output.clone().zeros
      val zeroTraining = new TrainingData(zeroInput,zeroOut)

      for (i<- data.indices.drop(nSamples))yield{
        val a = data.slice(i-nSamples,i).foldLeft(zeroTraining)((b,td)=> new TrainingData((td.input+b.input),(td.output+b.output)))
        new TrainingData(input= a.input/nSamples.toDouble,output=a.output/nSamples.toDouble)

      }
    }

  }

  private def toIncremental(data:Array[Double],tol:Double=1e-6):Seq[Double]={

    data.indices.drop(1).map(n =>{
       (data(n)-data(n-1))/(math.abs(data(n-1)) max tol)
    })
  }

  private val rawSamples: Array[TrainingData] = {
    val aux1 = movingAverage(movingAverage(movingAverage(DataReader.readSamples(fileName),nAverage).toArray,nAverage).toArray,nAverage).toArray//rawSamples.toSeq//

    val numberPar = aux1.head.input.getArray().length
    val paramSeriesVector = (0 until numberPar).map(n => toIncremental(aux1.map(_.input.getArray()(n))))

    for (p <- paramSeriesVector.indices;
         nPar <- paramSeriesVector(p).indices){
        aux1(nPar).input.set(p,0,paramSeriesVector(p)(nPar))
    }


    aux1.take(paramSeriesVector.head.length)

  }

  val samples = rawSamples.toSeq

  def processOutput(vIn:Seq[Double]):Matrix[Double]={
//    val (hi,lo) = (vIn.max,vIn.min)
//    val mu = vIn.sum/vIn.length.toDouble
//    val norm = vIn.map(x => (x-mu)/(hi-lo))
//    val le = norm.length.toDouble-1.0
//    val xCoord = norm.indices.map(i => i.toDouble/le)
//    val outputs: Seq[Double] = regressor.getRegressionCoefficients(xCoord zip norm)
//     new Matrix[Double](outputs.length,1) <= outputs

    new Matrix[Double](vIn.length,1) <= vIn
  }
  val timeSeriesSequence = DataReader.readSamples(writeTrainingSet)
//  val normalizer = new Normalizer {
//    def deNormaliseX(x: Matrix[Double]) = x
//
//    def normaliseY(y: Matrix[Double]) = y
//
//    def deNormaliseY(x: Matrix[Double]) = x
//
//    def normaliseX(x: Matrix[Double]) = x
//
//    val normalizedTrainingSet = timeSeriesSequence.toArray
//    val originalTrainingSet = timeSeriesSequence.toArray
//    val triggerFunc = data.triggerFunc.trigger
//  }

  val normalizer = new MeanNormalizer(timeSeriesFileName,triggerFunc.trigger)
}
