package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix

object MaskFactory {

  def getMask(nParam:Int,nT:Int,addOffset:Boolean):Matrix[Double]={

    val sz= if(addOffset) nParam*nT+1 else nParam*nT

    val msk: Matrix[Double] = new Matrix[Double](sz,nParam).zeros
    for (i<- 0 until nParam;
         j <- 0 until nT){
      msk.set(i*nT+j,i,1.0)
    }
    if (addOffset)
      for (i <- 0 until nParam){
        msk.set(msk.numberRows-1,i,1.0)
      }
    msk
  }

}
