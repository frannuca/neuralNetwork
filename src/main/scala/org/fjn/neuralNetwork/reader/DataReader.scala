package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.NetworkData
import org.fjn.neuralNetwork.multilayer.normalization.{Normalizer, NormalizerBase}


object DataReader{

  def convertString2Double(s:String):Double={

    try{
      s.toDouble
    }
    catch{
      case _ => -1.0
    }
  }
  def readSamples(fileName:String):Array[TrainingData]={

    val r = Closeable.using(scala.io.Source.fromFile(fileName)){
      reader =>{
        val a = (for( line <- reader.getLines() if !line.contains('#'))//it is not very correct since it will skip any line with this char
        yield{
          val (input,out): (Array[String], Array[String]) =
            line.split(";") match{
              case Array(ni,out) => (ni.split(","),out.split(","))
              case Array(ni)=> (ni.split(","),Array("-1"))
              case _=> throw new Exception("Invalid input format")
            }


          val aa = input
          val bb = out


          val mIn = new Matrix[Double](input.length,1) <= input.map(convertString2Double _)
          val mOut = new Matrix[Double](out.length,1) <= out.map(t => t.toDouble)

          TrainingData(mIn,mOut)
        }).toArray

          a
      }

    }
    r.toArray
  }
}
trait DataReader {

  outer:DataReader =>
  val nnData:NetworkData

  def getData:(Iterable[TrainingData],Normalizer)={
    (readSamples(nnData.samplesFilename),new Normalizer {
      val nnData =   outer.nnData
    })
  }



  protected def readSamples(filename:String):Seq[TrainingData]={
   DataReader.readSamples(filename)
  }
}
