package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

object TimeExtrapolator {



  def extrapolation(x0:Matrix[Double],numberOfIterations:Int,neuralNetwork:Network,nT:Int):Seq[Matrix[Double]]={
    var x = x0

    def updateX:Function2[Matrix[Double],Matrix[Double],Matrix[Double]] =  (o,y) =>{
      (for(i <- 0 until o.numberRows) yield{
        y.getArray().slice(i*nT,(i+1)*nT-1)++Seq(o(i,0))
      }).flatten.toArray.copyToArray(y.getArray(),0)


      y

    }

    (0 until  numberOfIterations).map(n =>{
      var o = neuralNetwork(x)
      updateX(o,x)
      o
    }).toSeq
  }
}
