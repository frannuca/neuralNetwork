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



  def read(file: BufferedSource, index: Int, timeWindowIn: Int,tOffset:Int, timeWindowOut: Int): (Seq[TrainingData],Double,Double) = {

    val lines = file.getLines()
    var xSeries = lines.filter(l => l.head != '#').map(line => line.split(",")(index).toDouble).toList.reverse.toArray
    val mean = xSeries.sum / xSeries.length.toDouble

    xSeries = xSeries.map(_ - mean)

        for(i <- 0 until xSeries.length-5)yield{

            val g: Double = (for(j <- 0 until 5) yield{
              xSeries(i+j)
            }).fold(0.0)((a,b)=> a+b)

           xSeries(i)=g
        }


  //}
    val max = xSeries.map(math.abs(_)).max



    xSeries = xSeries.map(_/max)
    xSeries = (for(i <- 0 until xSeries.length) yield{
      if(i%2 == 0){
        xSeries(i)
      }
      else
        -1
    }).filter(x => x >= 0).toArray


    val plot = new Plot2DPanel();



    plot.addLinePlot("time series",xSeries.toArray)

    // put the PlotPanel in a JFrame, as a JPanel
    val frame = new JFrame("Dow Jones");
    frame.setContentPane(plot);
    frame.setVisible(true);


    val samples = (for (i <- 0 until xSeries.length-tOffset - timeWindowIn - timeWindowOut - 1) yield {

      val x = for (t <- i until i + timeWindowIn) yield {
        xSeries(t)
      }

      val y = for (t <- i +tOffset+ timeWindowIn until i+tOffset + timeWindowIn + timeWindowOut) yield {
        xSeries(t)
      }


      TrainingData(input = new Matrix[Double](timeWindowIn, 1) <= x, output = new Matrix[Double](timeWindowOut, 1) <= y)

    })

    (samples,mean,max)
  }
}
