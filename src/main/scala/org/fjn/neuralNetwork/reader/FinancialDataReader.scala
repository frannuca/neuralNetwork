package org.fjn.neuralNetwork.reader

import java.io.{FileWriter, InputStreamReader, StringReader}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{DummyNormalizer, Normalizer, MeanNormalizer}
import collection.immutable.IndexedSeq
import org.fjn.optimization.Regression
import org.fjn.neuralNetwork.multilayer.architecture.NetworkData
import com.sun.xml.internal.bind.v2.TODO

/**
 * financial data reader. This class transform historical data presented with the yahoo history format into valid
 * time series data given a variety of parameters including time window forecast etc...
 * @param fileName   File containing yahoo style historical data
 * @param triggerFunc activation function to be used in the cells. this is needed to calibrate boundaries of the normalization
 * @param nT size in sample of the time window
 * @param outputIndex sequence of indices denoting which input parameters  are selected to be output of the neural network
 * @param outputDelay forecast in number of sample in the future to pick the parameter output in neural network
 * @param nAverage in case of smoothing this parameter set the moving average window size
 * @param regressionOrder polynomial order to approximate the foretasted sequence of values.
 */
case class FinancialDataReader(fileName: String, triggerFunc: Function1[Double, Double], nT: Int, outputIndex: Seq[Int], outputDelay: Seq[Int],nAverage:Int=5,regressionOrder:Int = 2) {

  outer:FinancialDataReader =>
  val regressor = new Regression(regressionOrder)

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


  private val rawSamples: Array[TrainingData] = DataReader.readSamples(fileName)
  //processing raw samples into percentage ones:
//  var formerArray:Array[Double] = rawSamples.head.input.getArray()
//  val sampleInDelta: Array[TrainingData] = rawSamples.indices.drop(1).map(
//    n =>{
//
//      val aux = (rawSamples(n).input.getArray() zip formerArray)
//        .map(item=> (item._1 - item._2) / ((math.abs(item._2)) max 1e-6))
//
//
//      rawSamples(n).input.getArray().copyToArray(formerArray,0)
//      (rawSamples(n).input <= aux, rawSamples(n).output)
//  }
//  ).map(s => TrainingData(s._1,s._2)).toArray

  //val samples = movingAverage(sampleInDelta,nAverage)
  val samples = movingAverage(rawSamples,nAverage)

  val numberOfParameters = samples.head.input.numberRows

  val timeSeriesFileName = fileName.substring(0, fileName.indexOf(".")) + "_nt" ++ nT.toString + "_dt" + outputDelay.head + ".finance"


  //number of parameters if given by the number of rows on the input vector
  val nParameter = samples.head.input.numberRows

  Closeable.using(new FileWriter(timeSeriesFileName, false)) {
    writer => {
      for (i <- 0 until samples.length - nT - outputDelay.last) {

        for (n <- 0 until nParameter) {

          for (j <- i until i + nT) {

            val sep =
              if (n == nParameter - 1 && j == i + nT - 1)
                ";"
              else
                ","

            writer.write(samples(j).input(n, 0).toString() + sep)

          }


        }


        val outArr: Seq[Double] = outputDelay.flatMap(nOutDelay =>
           samples(i + nT - 1 + nOutDelay).input.sub(outputIndex, Seq(0)).getArray())

        val mean = outArr.sum / outArr.length.toDouble
        val pointsLength = outArr.length.toDouble

        val aux1 = outArr.map(_ - mean)
        val (lo,hi) = (aux1.min,aux1.max)
        val sliceNorm = aux1.map(x => x/(hi-lo))

        val outputs: Seq[Double] = regressor.getRegressionCoefficients( sliceNorm.indices.map(_ /(pointsLength-1.0)) zip sliceNorm )
        val outs = new Matrix[Double](outputs.length,1) <= outputs

        //TODO: include here the regression parameters. For regressions it is necessary to include a sweep
        // on time. outputDelay must be converted into a Seq of delays.
        val arr2= outs.getArray()
          outs.getArray().indices.foreach(i =>{
              writer.write(outs.getArray()(i).toString)
              if(i<arr2.length-1) writer.write(",")
              })

        writer.write("\r\n")

      }

      ()
    }
  }



  //private val ss = DataReader.readSamples(timeSeriesFileName)
  val normalizer = new MeanNormalizer(timeSeriesFileName,triggerFunc)
//  val normalizer = new DummyNormalizer {
//    val normalizedTrainingSet = ss
//    val originalTrainingSet = ss
//    val triggerFunc =  outer.triggerFunc
//  }
  val normalizedSamples: Array[TrainingData] = normalizer.normalizedTrainingSet


}
