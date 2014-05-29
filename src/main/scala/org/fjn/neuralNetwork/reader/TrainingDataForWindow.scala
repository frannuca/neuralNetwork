package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix
import org.fjn.matrix._
import scala.collection.immutable.IndexedSeq
import java.io.{FileReader, BufferedReader, FileInputStream, InputStream}
import scala.io.BufferedSource

/**
 * Created by fran on 28/05/2014.
 */
object TrainingDataForWindow {



  def read(file: BufferedSource, index: Int, timeWindowIn: Int, timeWindowOut: Int): Seq[TrainingData] = {

    val lines = file.getLines()
    val xSeries = lines.filter(l => l.head != '#').map(line => line.split(",")(index).toDouble).toList

    (for (i <- 0 until xSeries.length - timeWindowIn - timeWindowOut - 1) yield {

      val x = for (t <- i until i + timeWindowIn) yield {
        xSeries(t)
      }

      val y = for (t <- i + timeWindowIn until i + timeWindowIn + timeWindowOut) yield {
        xSeries(t)
      }

      TrainingData(input = new Matrix[Double](timeWindowIn, 1) <= x, output = new Matrix[Double](timeWindowOut, 1) <= y)

    })
  }
}
