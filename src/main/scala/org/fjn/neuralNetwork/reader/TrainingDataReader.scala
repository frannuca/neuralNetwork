package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 01/05/13
 * Time: 15:32
 * To change this template use File | Settings | File Templates.
 */
object TrainingDataReader {

  def read(fileName:String): Iterator[TrainingData] ={

    for(line <- scala.io.Source.fromFile(fileName).getLines()) yield{
      line.split(";") match{
        case Array(in,out)=>{
          val inputSet= in.split(",")
          val outSet = out.split(",")

           TrainingData(
             input = (new Matrix[Double](inputSet.length,1) <= inputSet.map(_.toDouble)),
             output = (new Matrix[Double](outSet.length,1) <= outSet.map(_.toDouble))
           )
        }
      }

    }
  }
}
