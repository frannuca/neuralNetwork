package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.matrix.Matrix
import collection.mutable
import org.fjn.matrix.Scalar2MatrixConversions._
import java.io.{FileReader, FileWriter, BufferedWriter, File}
import org.fjn.neuralNetwork.reader.Closeable

trait Weights extends Serializable{
  self:Network =>

  def generateLayerIndices = 0 until layers.length-1


  lazy val Ws = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  def randomizeWeigths{
    val rngen = new scala.util.Random()

    Ws.foreach(item => item match{
      case ((l1,l2),w)=> w.getArray().indices.foreach(i=> w.getArray()(i)= w.getArray()(i)*(1.0+(rngen.nextDouble()-0.5) ))
    })
  }

  generateLayerIndices.map(l =>{
    Ws += (l,l+1) -> (new Matrix[Double](layers(l).size+1,layers(l+1).size).random)* (if(l==0) 1/self.nnData.dataSet.head.input.numberRows.toDouble/self.layers(1).size.toDouble else 5/self.layers(l).size.toDouble)
  })

  lazy val masks = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  generateLayerIndices.map(l =>{
    masks += (l,l+1) -> (Ws((l,l+1)).clone() <= (x => 1.0))
  })

  lazy val WsBackup = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  generateLayerIndices.map(l =>{
    WsBackup += (l,l+1) -> new Matrix[Double](layers(l).size+1,layers(l+1).size).random
  })

  lazy val dWs = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  generateLayerIndices.map(l =>{
    dWs += (l,l+1) -> Ws((l,l+1)).clone().zeros
  })

  lazy val dWsHistory =new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  generateLayerIndices.map(l =>{
    dWsHistory += (l,l+1) -> Ws((l,l+1)).clone().zeros
  })

  lazy val dWsOld = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  generateLayerIndices.map(l =>{
    dWs += (l,l+1) -> Ws((l,l+1)).clone().zeros
  })

  def savedW{
    dWsHistory
    dWsOld.clear()
    dWs.foreach(item => dWsOld += (item._1)-> item._2.clone())
  }

  def backUp{
    WsBackup.clear()
    Ws.foreach(item => WsBackup += (item._1)-> item._2.clone())

  }
  def undoBackUp{
    Ws.clear()
    WsBackup.foreach(item => Ws += (item._1)-> item._2.clone())
    dWsHistory.foreach(item => item match{
      case((a,b),w)=> w.zeros
    })

  }

  def applyMasks={
    Ws.foreach(w => w._2  <:= masks(w._1))
    dWs.foreach(w => w._2  <:= masks(w._1))
    dWsOld.foreach(w => w._2  <:= masks(w._1))
    WsBackup.foreach(w => w._2  <:= masks(w._1))
  }

  lazy val wArraySize = {
    applyMasks
    Ws.map(w => w._2.getArray().filter(c => c!=0.0)).flatten.toArray.length
  }

  def getWeightArray:Array[Double]={
    applyMasks
    Ws.map(w => w._2.getArray().filter(c => c!=0.0)).flatten.toArray
  }

  def addtoHistory={
    dWsHistory
    (0 until dWsHistory.size).foreach(i =>{
      dWsHistory((i,i+1)) =  momentum *(dWsHistory((i,i+1)) +   self.lr * dWs((i,i+1)))
    })
  }

  def updateWeights{
    applyMasks
    addtoHistory
    (0 until Ws.size).foreach(i =>{
      Ws((i,i+1)) = Ws((i,i+1)) - self.lr * dWs((i,i+1)) -  dWsHistory((i,i+1))
    })
  }
  def setWeightArray(x:Seq[Double])={

    if (wArraySize != x.length){
      throw new Exception("size of the given array of weights does not match")
    }

    var offset = 0
    Ws.foreach(w =>  {
      val y = x.slice(offset,w._2.getArray().length)
      offset += y.length
      y.copyToArray(w._2.getArray(),0)
    })
  }


}
