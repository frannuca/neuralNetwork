package org.fjn.neuralNetwork.multilayer.normalization

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq
import org.fjn.neuralNetwork.reader.{DataReader, TrainingData}

class MeanNormalizer(fileName:String,val triggerFunc:Function1[Double,Double]) extends Normalizer {

  val originalTrainingSet: Array[TrainingData] = DataReader.readSamples(fileName)

  val triggerMaxY:Double = triggerFunc(100)
  val triggerMinY:Double = triggerFunc(-100)



  private val xVals = (-1000 until 1000).map(i => (i.toDouble/100.0,triggerFunc(i.toDouble/100.0)))
  val triggerMaxX:Double = xVals.filter(d => d._2>triggerMaxY*0.99).head._1
  val triggerMinX:Double = -triggerMaxX //xVals.filter(d => d<triggerMinY*0.9).last

  private val maxMinMeanInput: IndexedSeq[(Double, Double, Double)] = {
    (0 until originalTrainingSet(0).input.numberRows).map(i =>{
      val componentVector = originalTrainingSet.map(s => s.input(i,0))
      (componentVector.max, componentVector.min,componentVector.sum/componentVector.size.toDouble)
    })
  }


  private val maxMinMeanOutput: IndexedSeq[(Double, Double,Double)] = {
    (0 until originalTrainingSet(0).output.numberRows).map( i=> {
      val componentVector = originalTrainingSet.map(s => s.output(i,0))
      (componentVector.max, componentVector.min,componentVector.sum/componentVector.size.toDouble)
    })


  }

  private val maxDifTriggerX = (triggerMaxX-triggerMinX)
  private val maxDifTriggerY = (triggerMaxY-triggerMinY)

  //normalizing inputs and outputs training set
  lazy val normalizedTrainingSet = originalTrainingSet.map(s =>{
     new TrainingData(normaliseX(s.input),normaliseY(s.output))
  })


  def normaliseX(x:Matrix[Double]):Matrix[Double]={

    def norm = (x:Double,index:Int) =>{
      (x - maxMinMeanInput(index)._3)/(maxMinMeanInput(index)._1-maxMinMeanInput(index)._2)*maxDifTriggerX
    }

    val r = x.clone()
    x.getArray().indices.foreach{i=>
      r.set(i,0,norm(x(i,0),i))
    }
    r
  }

  def normaliseY(y:Matrix[Double]):Matrix[Double]={

    def norm = (y:Double,index:Int) =>
      (y - maxMinMeanOutput(index)._3)/(maxMinMeanOutput(index)._1-maxMinMeanOutput(index)._2) * maxDifTriggerY

    val r = y.clone()
    y.getArray().indices.foreach{i=>
      r.set(i,0,norm(y(i,0),i))
    }
    r
  }

  def deNormaliseX(x:Matrix[Double]):Matrix[Double]={

    def deNorm = (x:Double,index:Int)=> x * (maxMinMeanInput(index)._1-maxMinMeanInput(index)._2)/maxDifTriggerX + maxMinMeanInput(index)._3
    val r = x.clone()
    x.getArray().indices.foreach{i=>
      r.set(i,0,deNorm(x(i,0),i))
    }
    r
  }

  def deNormaliseY(x:Matrix[Double]):Matrix[Double]={


    try {
      def deNorm = (x:Double,index:Int) =>{
        val nn = index
        val a1 = maxMinMeanOutput(index)._1
        val a2 = maxMinMeanOutput(index)._2
        val a3 = maxMinMeanOutput(index)._3

        x * (maxMinMeanOutput(index)._1-maxMinMeanOutput(index)._2)/maxDifTriggerY + maxMinMeanOutput(index)._3
      }


      val r = x.clone()
      x.getArray().indices.foreach{ i=>
        r.set(i,0,deNorm(x(i,0),i))
      }
      r
    } catch{
      case e:Throwable=> {
        val str = e.getMessage
        println(e.getMessage); throw e
      }
    }


  }

}
