package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.{Matrix,Scalar}
import org.fjn.matrix.Scalar2MatrixConversions._
import org.fjn.neuralNetwork.common.NNMatrixExtensions

import collection.mutable.ListBuffer
import scala.util.Random

abstract class NeuralNetworkBase(layerDim: Seq[Int]) extends triggerFunction with Normalizer
with WeightUpdater
with trainingSetLoader
with NNMatrixExtensions{


  self: NNTrainingCtes =>

  private var numberOfLayers: Int = 0
  private val Ds = new ListBuffer[Matrix[Double]]()

  private var out = new ListBuffer[Matrix[Double]]()
  private val deltas = new ListBuffer[Matrix[Double]]()

  var Size: Seq[Int] = null


  setSize(layerDim)

  def setSize(size: Seq[Int]) = {

    Size = size
    numberOfLayers = size.length
    Ds.clear()
    deltas.clear()
    clearWeights()
    out.clear()

    for (n <- 1 until size.length) {
      Ds += new Matrix[Double](size(n), size(n)).zeros
      deltas += new Matrix[Double](size(n), 1).zeros
      out += new Matrix[Double](size(n) + 1, 1).zeros
    }

    initWeights(size)
    numberOfLayers -= 1
  }


  def apply(x: Matrix[Double]): Matrix[Double] = {
    if (numberOfLayers < 2)
      sys.error("invalid network layout. No network can have less than 3 layers: Input-Hidden-Output")
    else {

      var n: Int = 0

      var o = fillOnes(x)
      setOnes(o <= f _)

      while (n < numberOfLayers) {


        val outr = fillOnes((o.transpose * Ws(n)).transpose <= f _)

        o = outr
        n += 1
      }


      o.sub(0 until o.numberRows-1,0 until o.numberCols)
    }

  }


  def forward(x: Matrix[Double], t: Matrix[Double]): Unit = {
    if (numberOfLayers < 2)
      sys.error("invalid network layout. No network can have less than 3 layers: Input-Hidden-Output")
    else {
      var o = fillOnes(x)

      setOnes(o <= f _)

      var n: Int = 0


      while (n < numberOfLayers) {


        out(n) = fillOnes((o.transpose * Ws(n)).transpose)
        Ds(n) = toEye(sub(out(n)))
        Ds(n) <= df _
        setOnes(out(n) <= f _)


        o = out(n).clone()
        n += 1
      }
    }

  }


  var totalErr: Double = 0


  def backward(x: Matrix[Double], t: Matrix[Double]): Unit = {
    if (numberOfLayers < 2)
      sys.error("invalid network layout. No network can have less than 3 layers: Input-Hidden-Output")
    else {
      var n = numberOfLayers - 1


      // println(err.toString)
      val err = (sub(out(n)) - t)
      deltas(n) = Ds(n) * err
      val dW = (deltas(n) * out(n - 1).transpose).transpose

      dWs(n) = dWs(n)+ dW

      n -= 1
      while (n > 0) {
        deltas(n) = Ds(n) * sub(Ws(n + 1)) * deltas(n + 1)
        val m1 = deltas(n)
        val m2 = out(n - 1)
        val dWb = (deltas(n) * out(n - 1).transpose).transpose

        dWs(n) += dWb
        n -= 1
      }
    }
  }


  def train(filename: String,isOnline:Boolean=false): Double = {

    LoadTrainningSet(filename) match {
      case (Some(trainingSet)) => {

        initialize(f, trainingSet)



        var minErr = 1e9;
        var wCopy = GetWeightCopy()
        var cc = 0
        var keepGoing = true
        while (cc < 90000 && keepGoing) {

          cc += 1

          cleardW()

          totalErr = 0.0
          var counter:Int = 0
          val L = trainingSet.length

          val start:Int = new Random().nextInt(trainingSet.length)
          while(counter<L)
          {

            val p = trainingSet((start+counter)%L)
            val normalizedInput = normalizeX(p._1)
            val normalizedOutput = normalizeY(p._2)
            forward(normalizedInput, normalizedOutput)
            backward(normalizedInput,normalizedOutput)

            if(isOnline)
              resolveWeights()

            counter += 1
          }

          if (!isOnline)
            resolveWeights()



          val totalErrOld = totalErr
          totalErr = 0;

          trainingSet.foreach(
            ts => {
              val input = normalizeX((ts._1))
              val input2 = deNormalizeX(input)
              val output = normalizeY((ts._2))
              val output2 = deNormalizeY(output)

              val oonn = this.apply(input)
              val errM = (oonn - output)
              val xx = (errM.transpose * errM)
              totalErr += xx(0, 0)

            }


          )

          if (totalErr < minErr)
          {

              wCopy = GetWeightCopy();
              minErr = totalErr
              alpha = alpha*1.01;
          }
          else
          {
            alpha =   alpha * 0.99
            Ws = wCopy
          }


          if (alpha>5.0)
            alpha=5.0

          if(alpha<1e-3)
              alpha=1e-3

          keepGoing = math.abs(minErr) > 1e-3
          println(cc.toString + "-->" + totalErr.toString + "-->"+alpha.toString)


          cc += 1

        }

        for ( k <- 0 until trainingSet.length)
        {
          //println(normalizeX(trainingSet(k)._1).t.toString + " -> " + deNormalizeY(this.apply(normalizeX(trainingSet(k)._1))).toString() + "--->" +  trainingSet(k)._2.toString)
          println((trainingSet(k)._1).transpose.toString + " -> " + deNormalizeY(this.apply(normalizeX(trainingSet(k)._1))).toString() + "--->" +  (trainingSet(k)._2).toString)
        }


        Ws.foreach(m => println(m.toString + "\n"))

        minErr
      }
      case _ => return 1e9
    }


  }


}


class NeuralNetwork(layerDim: Seq[Int]) extends NeuralNetworkBase(layerDim) with Sigmoidea with MaxMinNormalizer with WeightUpdaterSimple with NNTrainingCtes {

  alpha = 0.01
  override val beta = 0.1
  override val maxWindow = 5


  val c = 1d;


}