package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix

trait DataReader {

  val fileName:String

  def getData:Array[TrainingData]={
    readSamples(fileName).toArray
  }



  protected def readSamples(filename:String):Seq[TrainingData]={

    (for( line <- scala.io.Source.fromFile(fileName).getLines())
    yield{
      val ioData = line.split(";")
      val input = ioData(0).split(",")
      val out = ioData(1).split(",")


      TrainingData( new Matrix[Double](input.length,1) <= input.map(t => t.toDouble),
        new Matrix[Double](out.length,1) <= out.map(t => t.toDouble))
    }).toSeq
  }
}
