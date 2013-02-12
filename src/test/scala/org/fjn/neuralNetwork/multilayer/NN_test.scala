package org.fjn.neuralNetwork.multilayer

import architecture.{NetworkData, FeedForwardNetwork}
import finance.{TrainingAlgorithmData, TimeSeriesData, FinancialTimeSeriesNN}
import org.specs2.mutable.Specification
import java.io.{FileInputStream, ObjectInputStream, ObjectOutputStream, FileOutputStream}
import activation.{Sigmoidea, ActivationFunction}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.reader.{DataReader, TrainingData, FinancialDataReader, MaskFactory}
import javax.swing.JFrame


class NN_test extends  Specification {

  "training a NN" should {
    "run" in {

      val filepath = getClass().getResource("/"+ "IBEX35Future_2006-2012.csv").getFile;
    `testAlgorithm` mustEqual true
//     refineAlgorithm mustEqual true
      testDeSerializer mustEqual true
   //   extrapolationTest mustEqual true

    }
  }

  def `testAlgorithm`={

    val filepath = getClass().getResource("/"+ "dowjones.csv").getFile;
    val timeSerData = TimeSeriesData(
      fileName = filepath ,//"C:\\Users\\fran\\Downloads\\sinus.csv",,
      triggerFunc = new Sigmoidea(),
      nT = 10,
      outputIndex = 0,
      outputDelay=1,
      outWindowSize = 1,
      nAverage = 2,
      regressionOrder=0)

    val pso = FinancialTimeSeriesNN(
      seriesData = timeSerData,
      hiddenLayerSizes=Seq(200),
      trainingData=TrainingAlgorithmData(lr0=0.001,momentum0=0.7))



    val err = pso.solve(2000)
    pso.serializeObj("C:\\temp\\test.obj")


    println("total Err"+err.toString)

    true
  }

  def testDeSerializer={


    val filepath = getClass().getResource("/"+ "IBEX35Future_2006-2012.csv").getFile;
    val m2 = FinancialTimeSeriesNN.deserialize("C:\\temp\\test.obj")



    val result = m2.compute(filepath)



//    import org.math.plot._
//    // create your PlotPanel (you can use it as a JPanel)
//
//    val plot = new Plot2DPanel();
//    //plot.addLinePlot("real IBEX 35", result.indices.map(_.toDouble).toArray, result.map(_._2).toArray);
//    //plot.addLinePlot("simulated IBEX 35", result.indices.map(_.toDouble).toArray, result.map(_._1).toArray);
//
//    // put the PlotPanel in a JFrame, as a JPanel
//    val frame = new JFrame("a plot panel");
//    frame.setContentPane(plot);
//    frame.setVisible(true);


    val ln = readLine()

    true
  }

}

object NNTestUtils{


  def generateXOR():StringBuilder={
    val sbld = new StringBuilder()
    sbld.append(0).append(",").append(0).append(";").append(1).append("\r\n")
    sbld.append(0).append(",").append(1).append(";").append(0).append("\r\n")
    sbld.append(1).append(",").append(0).append(";").append(0).append("\r\n")
    sbld.append(1).append(",").append(1).append(";").append(1).append("\r\n")

    sbld
  }

  def genetateFunc():StringBuilder = {
    val dx=0.1
        val dy = 0.2
        val sbld = new StringBuilder()
        for ( ix <- 0 until 4)
        {
           val x = ix*dx
          for ( iy <- 0 until  4)
          {

            val y = iy*dy
            sbld.append(x).append(",").append(y).append(";").append((30.0*x*x+15.0*y*y)).append("\r\numberOfIteration")

          }

        }

    sbld
  }
  def generateSet1():String = {




    val sbld = generateXOR()
    val writer = new FileOutputStream("C:\\temp\\test.txt")

     writer.write(sbld.toString().getBytes)


    writer.close()

    "C:\\temp\\test.txt"


  }
}