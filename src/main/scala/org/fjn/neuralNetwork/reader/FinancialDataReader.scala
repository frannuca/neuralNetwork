package org.fjn.neuralNetwork.reader

import java.io.{FileWriter, InputStreamReader, StringReader}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

trait FinancialDataReader extends DataReader{

  val nT:Int /// time slice used to built up the different time vectors
  val outputIndex:Int ///given the parameters the output is just one of the futures inputs delayed by some time
  val outputDelay:Int

  override def getData:Array[TrainingData]={

    //reading the samples from the file
    val samples: Seq[TrainingData] = DataReader.readSamples(fileName)



      //number of paramaters if given by the number of rows on the input vector
      val nParameter = samples.head.input.numberRows
      val timeSeriesFileName = fileName+".finance"

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


     val r: Seq[TrainingData] = DataReader.readSamples(fileName+".finance")

     r.toArray
  }
}
