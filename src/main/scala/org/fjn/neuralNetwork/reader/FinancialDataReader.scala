package org.fjn.neuralNetwork.reader

import java.io.{FileWriter, InputStreamReader, StringReader}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{Normalizer, MeanNormalizer}
import collection.immutable.IndexedSeq
import org.fjn.optimization.Regression
import org.fjn.neuralNetwork.multilayer.architecture.NetworkData

case class FinancialDataReader(fileName: String, triggerFunc: Function1[Double, Double], nT: Int, outputIndex: Seq[Int], outputDelay: Seq[Int],nAverage:Int=5,regressionOrder:Int = 2) {

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



  val samples = movingAverage(DataReader.readSamples(fileName),nAverage)

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

        val outputs: Seq[Double] = regressor.getRegressionCoefficients(outArr)
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


  val normalizer = new MeanNormalizer(timeSeriesFileName, triggerFunc)
  val normalizedSamples: Array[TrainingData] = normalizer.normalizedTrainingSet


}
