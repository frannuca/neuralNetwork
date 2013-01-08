package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{MeanNormalizer, Normalizer}
import org.fjn.neuralNetwork.multilayer.architecture.NetworkData


object DataReader{

  def convertString2Double(s:String):Double={

    try{
      s.toDouble
    }
    catch{
      case _ => Double.NaN
    }
  }
  def readSamples(fileName:String,skip:Int=0):Array[TrainingData]={

    val r = Closeable.using(scala.io.Source.fromFile(fileName)){
      reader =>{
        val a = (for( line <- reader.getLines() if !line.contains('#'))//it is not very correct since it will skip any line with this char
        yield{
          val (input,out): (Array[String], Array[String]) =
            line.split(";") match{
              case Array(ni,out) => (ni.split(",").slice(skip,ni.length),out.split(","))
              case Array(ni)=> (ni.split(",").slice(skip,ni.length),Array("-1"))
              case _=> throw new Exception("Invalid input format")
            }


          val aa = input
          val bb = out


          val filteredInput = input.map(convertString2Double _).filter(v => !v.isNaN)
          val mIn = new Matrix[Double](filteredInput.length,1) <=   filteredInput
          val mOut = new Matrix[Double](out.length,1) <= out.map(t => t.toDouble)

          TrainingData(mIn,mOut)
        }).toArray

          a
      }

    }
    r.toArray
  }
}
