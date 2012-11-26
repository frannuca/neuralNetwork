package org.fjn.neuralNetwork.reader

import java.io.{FileWriter, InputStreamReader, StringReader}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

trait FinancialDataReader extends DataReader{

  override def getData:Array[TrainingData]={
    val reader = scala.io.Source.fromFile(fileName)
    val writer = new FileWriter(fileName+".finance")

        throw new NotImplementedException()
  }
}
