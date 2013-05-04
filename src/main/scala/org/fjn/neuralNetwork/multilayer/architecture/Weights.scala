package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.matrix.Matrix
import scala.collection.mutable
import org.fjn.matrix.Scalar2MatrixConversions._
import java.io.{FileReader, FileWriter, BufferedWriter, File}
import org.fjn.neuralNetwork.reader.Closeable
import scala.collection.immutable.Iterable

trait Weights extends Serializable{
  self:Network =>

  lazy val generateLayerIndices = layers.indices.take(layers.length-1)
  def generateListOfWeightMatrices: mutable.Map[(Int, Int), Matrix[Double]] ={
    val mutableMap = new mutable.HashMap[(Int,Int),Matrix[Double]]()
    generateLayerIndices.foreach(l =>{
       mutableMap += (l,l+1) -> new Matrix[Double](layers(l).size+1,layers(l+1).size).zeros
    })

    mutableMap


  }

  //generating Weight list strucuture
  lazy val Ws: mutable.Map[(Int, Int), Matrix[Double]] =
    generateListOfWeightMatrices.map{case (key,value)=> self.layers(key._1) match {
      case InputLayerExtractor(_) => key -> value.random*0.01
      case _ => key -> value.random*0.1
    }
    }



  //generating masks list structure
  lazy val masks: mutable.Map[(Int, Int), Matrix[Double]] =
    generateListOfWeightMatrices.map{case (key,value) => key -> (value <=(_=>1.0))}



  //generating weights backup list
  lazy val WsBackup: mutable.Map[(Int, Int), Matrix[Double]] =
    generateListOfWeightMatrices

  lazy val dWs: mutable.Map[(Int, Int), Matrix[Double]] =
    generateListOfWeightMatrices

  lazy val dWsBackup: mutable.Map[(Int, Int), Matrix[Double]] =
    generateListOfWeightMatrices

  def savedW{
    dWsBackup.keys.foreach(key => dWsBackup(key).copyFrom(dWs(key)))
  }

  def saveW{
    WsBackup.keys.foreach(key => WsBackup(key).copyFrom(Ws(key)))
  }
  def undoBackUp{
    WsBackup.keys.foreach(key => Ws(key).copyFrom(WsBackup(key)))
  }

  def applyMasks={
    Ws.foreach{case (key,value) => value <:= masks(key)}
    dWs.foreach{case (key,value) => value <:= masks(key)}
    dWsBackup.foreach{case (key,value) => value <:= masks(key)}
    WsBackup.foreach{case (key,value) => value <:= masks(key)}
  }


  def cleardW={
    dWs.foreach{
      case (key,_)=> { dWs(key).zeros}
    }
  }
  def updateWeights{
    Ws.foreach{
      case (key,_)=> { Ws(key).copyFrom(Ws(key) - self.lr * dWs(key))}
    }

    cleardW
    applyMasks

  }

}
