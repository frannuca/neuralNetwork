package org.fjn.neuralNetwork.common

import org.fjn.matrix.Matrix


trait NNMatrixExtensions extends Serializable{
  def setOnes(m: Matrix[Double]): Unit = {
    for (i <- 0 until m.numberCols) {
      m.set(m.numberRows - 1, i, 1.0)
    }
  }



  def sub(m: Matrix[Double]): Matrix[Double] ={
    m.sub(0 until m.numberRows - 1, 0 until m.numberCols)
  }


  def copyInExtendedMatrix(dest: Matrix[Double], src: Matrix[Double]) = {
    require(dest.numberCols == src.numberCols && (dest.numberRows - 1) == src.numberRows)


    var i: Int = 0
    var j: Int = 0
    while (i < src.numberRows) {
      j = 0
      while (j < src.numberCols) {
        dest.set(i, j, src(i, j))
        j += 1
      }
      i += 1
    }

  }

  def fillOnes(m: Matrix[Double]): Matrix[Double] = {
    val m2 = new Matrix[Double](m.numberRows + 1, m.numberCols).zeros
    copyInExtendedMatrix(m2, m)
    setOnes(m2)
    m2
  }


  def toEye(d: Matrix[Double]): Matrix[Double] = {
    val r = new Matrix[Double](d.numberRows, d.numberRows)
    (0 until d.numberRows).map(i =>(i,d(i,0))).toSeq.foreach(v => r.set(v._1,v._1,v._2));
    r
  }


}