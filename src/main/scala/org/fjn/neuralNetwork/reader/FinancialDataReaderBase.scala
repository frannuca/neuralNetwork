package org.fjn.neuralNetwork.reader

import java.io.FileWriter
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.Normalizer
import org.fjn.neuralNetwork.multilayer.finance.TimeSeriesData


trait FinancialDataReaderBase extends Serializable {

  val data: TimeSeriesData
  val normalizer: Normalizer
  def samples: Seq[TrainingData]
  def processOutput(outVector: Seq[Double]): Matrix[Double]


  val timeSeriesFileName = data.fileName.substring(0, data.fileName.indexOf(".")) + "_nt" ++ data.nT.toString + "_dt" +
    data.outputDelay.toString + ".finance"

  def nParameter = samples.head.input.numberRows

  def writeTrainingSet: String = {

    Closeable.using(new FileWriter(timeSeriesFileName, false)) {
      writer => {
        for (i <- 0 until samples.length - data.nT - data.outputDelay-data.outWindowSize) {

          for (n <- 0 until nParameter) {

            for (j <- i until i + data.nT) {

              val sep =
                if (n == nParameter - 1 && j == i + data.nT - 1)
                  ";"
                else
                  ","

              writer.write(samples(j).input(n, 0).toString() + sep)

            }


          }


          val outArr: Seq[Double] = (0 until data.outWindowSize).flatMap(t =>{
            val i1 =t + data.nT - 1 + data.outputDelay
            val ii = samples(t + data.nT - 1 + data.outputDelay).input.sub(Seq(data.outputIndex), Seq(0)).getArray()
            samples(i +t + data.nT - 1 + data.outputDelay).input.sub(Seq(data.outputIndex), Seq(0)).getArray()
          }
            )

          val outs = processOutput(outArr)

          val arr2 = outs.getArray()
          outs.getArray().indices.foreach(i => {
            writer.write(outs.getArray()(i).toString)
            if (i < arr2.length - 1) writer.write(",")
          })

          writer.write("\r\n")

        }

        timeSeriesFileName
      }
    }
  }

  lazy val normalizedSamples: Array[TrainingData] = normalizer.normalizedTrainingSet

}

