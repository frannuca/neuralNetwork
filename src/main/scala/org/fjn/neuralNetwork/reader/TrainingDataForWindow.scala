package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix
import org.fjn.matrix._
import scala.collection.immutable.IndexedSeq
import java.io.{FileReader, BufferedReader, FileInputStream, InputStream}
import scala.io.BufferedSource
import org.math.plot.Plot2DPanel
import javax.swing.JFrame
import scala.collection.JavaConverters._
/**
 * Created by fran on 28/05/2014.
 */
object TrainingDataForWindow {


  private def movingAverage(xSeries:Array[Double],n:Int)={
    (for(i <- 0 until xSeries.length-n)yield{

      val g: Double = (for(j <- 0 until n) yield{
        xSeries(i+j)
      }).fold(0.0)((a,b)=> a+b)

      g
    }).toArray

  }

  def read(file: BufferedSource, index: Int, timeWindowIn: Int,tOffset:Int, timeWindowOut: Int)(fin:Array[Double] => Array[Double],fout:Array[Double] => Array[Double]): (Seq[TrainingData],Double,Double) = {

    val lines = file.getLines()
    var xSeries = lines.filter(l => l.head != '#').map(line => line.split(",")(index).toDouble).toList.reverse.toArray
    val mean = xSeries.sum / xSeries.length.toDouble


      xSeries = movingAverage(xSeries,5)


  //}

    xSeries = xSeries.map(_ - mean)

    val max = xSeries.map(math.abs(_)).max



    xSeries = xSeries.map(_/max)


    val plot = new Plot2DPanel();



    plot.addLinePlot("time series",xSeries.toArray)

    // put the PlotPanel in a JFrame, as a JPanel
    val frame = new JFrame("Dow Jones");
    frame.setContentPane(plot);
    frame.setVisible(true);


    val samples =

      (for (i <- 0 until xSeries.length-tOffset - timeWindowIn - timeWindowOut - 1) yield {


       val x =  fin(xSeries.slice(i,i + timeWindowIn))
       val y = fout(xSeries.slice(i +tOffset+ timeWindowIn,i+tOffset + timeWindowIn + timeWindowOut))

      TrainingData(input = new Matrix[Double](timeWindowIn, 1) <= x, output = new Matrix[Double](y.length,1) <= y )

    })

    (samples,mean,max)
  }
}
