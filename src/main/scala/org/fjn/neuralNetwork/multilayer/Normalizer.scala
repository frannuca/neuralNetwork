package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 11/25/11
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */

trait Normalizer {
           self : NeuralNetworkBase =>
  var trainingSet: Seq[(Matrix[Double], Matrix[Double])] = null
   var trigger: (Double => Double) = null
   var mu_x: Matrix[Double]= null
   var mu_y: Matrix[Double]= null
   var minLimitX: Double  = 0
   var maxLimitX: Double  = 0
   var minLimitY: Double  = 0
   var maxLimitY: Double = 0
   var maxX: Matrix[Double] = null
   var minX: Matrix[Double]  = null
   var maxY: Matrix[Double] = null
   var minY: Matrix[Double]  = null

  protected def mean(x: Seq[Matrix[Double]], isX:Boolean = true): Unit = {

      var average = new Matrix[Double](x.head.numberRows, x.head.numberCols)
      average.zeros

      var counter = 0;
      x.foreach(input => {
        average += input
        counter += 1
      })

      require(counter > 0)
     if (isX)
     {
      mu_x = average * 1.0 / counter.toDouble
     }
      else
     {
           mu_y = average * 1.0 / counter.toDouble
     }



    }


  private def getMinMax(x: Seq[Matrix[Double]],isX:Boolean = true): Unit = {
    mean(x,isX)
    var mu = mu_x
    if (!isX)
      mu = mu_y

    var auxMaxX = new Matrix[Double](x.head.numberRows, 1)
    auxMaxX.zeros - 1e9

    var auxMinX = new Matrix[Double](x.head.numberRows, 1)
    auxMinX.zeros + 1e9




    x.foreach(m2 => {
      val m: Matrix[Double] = m2 - mu
      var k: Int = 0

      while (k < m.numberRows) {
        if (auxMaxX(k, 0) < m(k, 0)) auxMaxX.set(k, 0,m(k, 0))
        if (auxMinX(k, 0) > m(k, 0)) auxMinX.set(k, 0 , m(k, 0))
        k += 1
      }

    })

     if(isX)
    {
      maxX = auxMaxX
      minX = auxMinX
    }
    else
    {
      maxY = auxMaxX
      minY = auxMinX
    }
  }

    def initialize(f: (Double => Double), trainingSetv: Seq[(Matrix[Double], Matrix[Double])]): Unit = {
     maxLimitY = f(100d)
     minLimitY = f(-100d)
     minLimitX = -100d;
     maxLimitX = 100d;


     while (f(minLimitX) < minLimitY*0.8  )
       minLimitX += 1e-4

     while (f(maxLimitX) > maxLimitY*0.8 )
       maxLimitX -= 1e-4

     this.trainingSet = trainingSetv

     val xx = trainingSet.map(t => t._1).toArray
     val yy = trainingSet.map(t => t._2).toArray
     mean(xx)
     mean(yy,false)
     getMinMax(xx)
     getMinMax(yy,false)
   }



   def normalizeX(s: Matrix[Double]): Matrix[Double]

   def deNormalizeX(s: Matrix[Double]): Matrix[Double]

   def normalizeY(s: Matrix[Double]): Matrix[Double]

   def deNormalizeY(s: Matrix[Double]): Matrix[Double]
}


trait UnityNormalizer extends Normalizer{
      self:NeuralNetworkBase =>


  def normalizeX(x: Matrix[Double]): Matrix[Double]=
  {
    val s = x - mu_x
        for (j <- 0 until s.numberRows) {
          val d = s(j, 0)/maxX(j,0)
          s.set(j, 0, d)
        }

        s

  }

  def deNormalizeX(x: Matrix[Double]): Matrix[Double]={

    val s = x
        for (j <- 0 until s.numberRows) {
          val d = s(j, 0)*maxX(j,0)
          s.set(j, 0, d)
        }

        s + mu_x
  }

   def normalizeY(x: Matrix[Double]): Matrix[Double]={

     val s = x - mu_y
             for (j <- 0 until s.numberRows) {
               val d = s(j, 0)/maxY(j,0)
               s.set(j, 0,d)
             }

             s


   }

  def deNormalizeY(x: Matrix[Double]): Matrix[Double]={
        val s = x.clone
        for (j <- 0 until s.numberRows) {
          val d = s(j, 0)*maxY(j,0)
          s.set(j, 0,d)
        }

        s + mu_y
  }
}
trait MaxMinNormalizer extends Normalizer {
         self:NeuralNetworkBase =>

  def normalizeX(x: Matrix[Double]): Matrix[Double] = {

    val s = x - mu_x
    for (j <- 0 until s.numberRows) {
      val d = (s(j, 0) - minX(j, 0)) / (maxX(j, 0) - minX(j, 0)) * (maxLimitX - minLimitX) + minLimitX
      s.set(j, 0,d)
    }

    s

  }

  def deNormalizeX(x: Matrix[Double]): Matrix[Double] = {

    val s = x.clone
    for (j <- 0 until s.numberRows) {
      val id = (s(j, 0) - minLimitX) / (maxLimitX - minLimitX) * (maxX(j, 0) - minX(j, 0)) + minX(j, 0)
      s.set(j, 0, id)
    }
   s + mu_x
  }


  def normalizeY(y: Matrix[Double]): Matrix[Double] = {

     val s = y - mu_y
     for (j <- 0 until s.numberRows) {
       val d = (s(j, 0) - minY(j, 0)) / (maxY(j, 0) - minY(j, 0)) * (maxLimitY - minLimitY) + minLimitY
       s.set(j, 0, d)
     }


     s

   }

   def deNormalizeY(y: Matrix[Double]): Matrix[Double] = {

     val s = y.clone
     for (j <- 0 until s.numberRows) {
       val id = (s(j, 0) - minLimitY) / (maxLimitY - minLimitY) * (maxY(j, 0) - minY(j, 0)) + minY(j, 0)
       s.set(j, 0,id)
     }
    s  + mu_y

   }


  //  private def variance(x: Seq[Matrix[Double]]): Matrix[Double] = {
  //
  //    val mu = mean(x)
  //    var counter = 0
  //    var sigma = Matrix.zeros[Double](x.head.numRows, 1)
  //
  //    x.foreach(input => {
  //      val d = (input - mu)
  //      sigma += d * d
  //      counter += 1
  //    })
  //
  //    require(counter > 0)
  //
  //    sigma *= 1.0 / counter.toDouble
  //  }



}