package org.fjn.neuralNetwork.multilayer

import java.io._
import collection.mutable.ListBuffer
import org.slf4j.LoggerFactory
import org.fjn.matrix.Matrix

trait  trainingSetLoader {

  def logger = LoggerFactory.getLogger(classOf[trainingSetLoader])
  def LoadTrainningSet(filename:String):Option[Seq[(Matrix[Double], Matrix[Double])]] ={


    try
    {
      val resultList = new ListBuffer [(Matrix[Double],Matrix[Double])]
          val dataStr = getDataFromFile(filename) match{
            case(Right(str)) => str
            case(Left(e)) =>  logger.error("getDataFromFile",e); return None
          }
          val rows:Array[String] = dataStr.split('\n')

          for (row <- rows) {

            val inout = row.split(";")
            val inputs = inout(0).split(",")
            val outputs = inout(1).split(",")

            val input = new Matrix[Double](inputs.length,1)
            input.zeros

            for (i <- 0 until input.numberRows)
               input.set(i,0,inputs(i).toDouble)

            val output =  new Matrix[Double](outputs.length,1)
            output.zeros
            for (i <- 0 until output.numberRows)
            {
              output.set(i,0,outputs(i).toDouble)
            }

            resultList+= Tuple2(input,output)

          }

          Some(resultList.toSeq)

    }
    catch {
      case(e:Exception)=> logger.error("Train loader",e);  None;
      case _ => logger.error("unknown error in Train loader"); None;
    }



  }

  private def getDataFromFile(filename:String):Either[Exception,StringBuilder]={

    try
    {
     val  in = new BufferedReader(new FileReader(filename))

    val sb = new StringBuilder()
    var s:String = in.readLine()
    while(s != null)
    {
      sb.append(s + "\n")
      s = in.readLine()
    }

    in.close();
    return Right(sb)
    }
    catch{
      case(ioe:IOException) => Left(ioe)
      case (e:Exception) => Left(e)
      case _ => Left(new Exception("Unknown exception"))
    }

  }

}