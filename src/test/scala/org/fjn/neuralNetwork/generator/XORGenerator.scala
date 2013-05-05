package org.fjn.neuralNetwork.generator

import org.fjn.neuralNetwork.reader.TrainingData
import org.fjn.matrix.Matrix


object XORGenerator {

  def apply():Seq[TrainingData] ={
    val in0 = new Matrix[Double](2,1) <= Seq(0.0,0.0)
    val out0 = new Matrix[Double](1,1) <= Seq(-1.0)

    val in1 = new Matrix[Double](2,1) <= Seq(0.0,1.0)
    val out1 = new Matrix[Double](1,1) <= Seq(1.0)

    val in2 = new Matrix[Double](2,1) <= Seq(1.0,0.0)
    val out2 = new Matrix[Double](1,1) <= Seq(1.0)

    val in3 = new Matrix[Double](2,1) <= Seq(1.0,1.0)
    val out3 = new Matrix[Double](1,1) <= Seq(-1.0)

    Seq(
      new TrainingData(input = in0,output = out0),
      new TrainingData(input = in1,output = out1),
      new TrainingData(input = in2,output = out2),
      new TrainingData(input = in3,output = out3)
    )

  }
}
