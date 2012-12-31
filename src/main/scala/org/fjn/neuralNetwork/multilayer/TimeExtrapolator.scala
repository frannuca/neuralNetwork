package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

object TimeExtrapolator {



  def extrapolation(x0:Matrix[Double],numberOfIterations:Int,neuralNetwork:Network,nT:Int):Seq[Matrix[Double]]={
    var x = x0

    def updateX:Function1[Matrix[Double],Matrix[Double]] =  v =>{
      (for(i <- 0 until v.numberRows) yield{
        x.getArray().slice(i*nT,(i+1)*nT-1)++Seq(v(i,0))
      }).flatten.toArray.copyToArray(x.getArray(),0)


      x

    }

    (0 until  numberOfIterations).map(n =>{
      var o = neuralNetwork(x)
      updateX(o)
      o
    }).toSeq
  }
}
