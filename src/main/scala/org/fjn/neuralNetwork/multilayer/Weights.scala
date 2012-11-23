package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 23/11/12
 * Time: 20:33
 * To change this template use File | Settings | File Templates.
 */
trait Weights {
  self:Network =>


  lazy val Ws = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until layers.length-1).map(l =>{
    Ws += (l,l+1) -> new Matrix[Double](layers(l).size+1,layers(l+1).size).random
  })

  lazy val masks = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until self.layers.length-1).map(l =>{
    masks += (l,l+1) -> (Ws((l,l+1)).clone() <= (x => 1.0))
  })

  lazy val WsBackup = new scala.collection.mutable.HashMap[(Int,Int),Matrix[Double]]()
  (0 until layers.length-1).map(l =>{
    WsBackup += (l,l+1) -> new Matrix[Double](layers(l).size+1,layers(l+1).size).random
  })


  def backUp{
    WsBackup.clear()
    Ws.foreach(item => WsBackup += (item._1)-> item._2.clone())

  }
  def undoBackUp{
    Ws.clear()
    WsBackup.foreach(item => Ws += (item._1)-> item._2.clone())

  }

  def applyMasks={
    Ws.foreach(w => w._2  <:= masks(w._1))
  }

  lazy val wArraySize = {
    applyMasks
    Ws.map(w => w._2.getArray().filter(c => c!=0.0)).flatten.toArray.length
  }
  def getWeightArray:Array[Double]={

    applyMasks
    Ws.map(w => w._2.getArray().filter(c => c!=0.0)).flatten.toArray
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