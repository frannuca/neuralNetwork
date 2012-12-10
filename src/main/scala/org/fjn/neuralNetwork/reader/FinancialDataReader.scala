package org.fjn.neuralNetwork.reader

import java.io.{FileWriter, InputStreamReader, StringReader}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{NormalizerBase, Normalizer}
import org.fjn.neuralNetwork.multilayer.NetworkData

trait FinancialDataReader extends DataReader{

  outer:DataReader=>
  val nT:Int /// time slice used to built up the different time vectors
  val outputIndex:Int ///given the parameters the output is just one of the futures inputs delayed by some time
  val outputDelay:Int


  override def getData:(Iterable[TrainingData],Normalizer)={

    val timeSeriesFileName = nnData.samplesFilename.substring(0,nnData.samplesFilename.indexOf("."))+"_nt"++nT.toString+"_dt"+outputDelay +".finance"

    //reading the samples from the file
    val norm = new Normalizer {
      val nnData = outer.nnData
    }
    val samples = norm.normalizedTrainingSet.toArray

      //number of paramaters if given by the number of rows on the input vector
     val nParameter = samples.head.input.numberRows


      Closeable.using(new FileWriter(timeSeriesFileName,false)){
        writer =>{
            for (i <- 0 until samples.length-nT-outputDelay) {

              for (n <- 1 until nParameter) {

                 for (j<- i until i+nT) {

                val sep=
                  if (n == nParameter-1 && j == i+nT-1)
                    ";"
                  else
                     ","

                  writer.write(samples(j).input(n,0).toString()+sep)

              }


            }

            writer.write(samples(i+nT-1+outputDelay).input(outputIndex+1,0).toString() + "\r\n")

          }

           ()
        }
      }


     val r: Iterable[TrainingData] = norm.normalizedTrainingSet

     (r,norm)
  }
}
