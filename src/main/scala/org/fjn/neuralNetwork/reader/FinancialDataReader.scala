package org.fjn.neuralNetwork.reader

import java.io.{FileWriter, InputStreamReader, StringReader}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{Normalizer, MeanNormalizer}
import org.fjn.neuralNetwork.multilayer.NetworkData

class FinancialDataReader(fileName: String, triggerFunc: Function1[Double, Double], nT: Int, outputIndex: Int, outputDelay: Int) {

  val samples = DataReader.readSamples(fileName)
  val timeSeriesFileName = fileName.substring(0, fileName.indexOf(".")) + "_nt" ++ nT.toString + "_dt" + outputDelay + ".finance"


  //number of parameters if given by the number of rows on the input vector
  val nParameter = samples.head.input.numberRows

  Closeable.using(new FileWriter(timeSeriesFileName, false)) {
    writer => {
      for (i <- 0 until samples.length - nT - outputDelay) {

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

        writer.write(samples(i + nT - 1 + outputDelay).input(outputIndex, 0).toString() + "\r\n")

      }

      ()
    }
  }


  val normalizer = new MeanNormalizer(timeSeriesFileName, triggerFunc)
  val normalizedSamples: Array[TrainingData] = normalizer.normalizedTrainingSet


}
