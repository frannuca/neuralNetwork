package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.common.NNMatrixExtensions
import collection.mutable.ListBuffer
import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

import org.fjn.matrix.Scalar
import org.fjn.matrix.Scalar2MatrixConversions._
import collection.immutable
import org.fjn.neuralNetwork.reader.{TrainingData, DataReader}
import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction
import org.fjn.neuralNetwork.multilayer.algorithm.LearningAlgorithm
import org.math.plot.Plot2DPanel
import javax.swing.JFrame
import util.Random

case class NetworkData(layerDimensions:Seq[Int],activationFunction:ActivationFunction,dataSet:Seq[TrainingData])

trait Network
  extends Serializable
  with NNMatrixExtensions
  with LearningAlgorithm
  with Weights{

  val nnData:NetworkData
  lazy protected val layers: Seq[Layer] = nnData.layerDimensions.map(dim =>  new Layer(dim,nnData.activationFunction))

  def setMask(nlayer0:Int, scentil:Matrix[Double])={
    masks((nlayer0,nlayer0+1)).copyFrom(scentil)
  }


  def apply(x:Matrix[Double]):Matrix[Double]={
     forward(x)
  }

  def computeError:Double={
    val e = nnData.dataSet.map{sample =>
      val o = forward(sample.input)
        val d = (o -sample.output)
      val a = math.sqrt((d * d.transpose)(0,0))
      if (a.isInfinity || a.isNaN())
      {
        val aaa = 1
      }
      a
    }.toSeq.sum

    e
  }


  def solve(maxIter:Int):Double={

    val frames = (0 until nnData.dataSet.head.output.numberRows).map(i => new JFrame("a plot panel" + i.toString))

    lr = lr0
    momentum= momentum0
    var counter = 0
    var counterErrors = 0
    var minError = computeError
    backUp


    for (iteration <- 0 until maxIter){



      var error0 = computeError
      dWs.foreach(_._2.zeros)

      println("iteration"+iteration.toString)
      val rnd = new Random(iteration)

      nnData.dataSet.indices.foreach(i =>{

        val orig = (rnd.nextDouble()*nnData.dataSet.length).toInt
        learn(nnData.dataSet(orig%nnData.dataSet.length))
      })
      //nnData.dataSet.foreach(learn)

      updateWeights
      savedW
      var error1 = computeError



      if (error1>error0){

        println("lr="+lr.toString)

        counterErrors = counterErrors + 1

        if (counterErrors >= 0){
          lr = lr *0.75

          undoBackUp
          val a= dWsHistory
          println("*****************************Randomizing Weights")
          randomizeWeigths
          counterErrors = 0

        }
      }
      else if (error1 < error0){

        counterErrors = 0
        println("*****************************GOOD!!!!")
        if (error1<minError)
        {
          backUp
          minError= error1
          val plot = (0 until nnData.dataSet.head.output.numberRows).map( _ => new Plot2DPanel());
          for (o <- 0 until nnData.dataSet.head.output.numberRows)      {

            plot(o).addLinePlot("real IBEX 35"+o.toString, nnData.dataSet.indices.map(_.toDouble).toArray, nnData.dataSet.map(_.output(o,0)).toArray);
            plot(o).addLinePlot("simulated IBEX 35"+o.toString, nnData.dataSet.indices.map(_.toDouble).toArray, nnData.dataSet.map(v => this.apply(v.input)(o,0)).toArray);
            frames(o).setContentPane(plot(o));
            frames(o).setVisible(true);
            frames(o).repaint(500)


          }

          // put the PlotPanel in a JFrame, as a JPanel



        }
        if  (counter > 10){
          lr= lr * 1.05
        }


        println("lr="+lr.toString)
        counter += 1

        error0 = error1

        println(error0)
      }



      println("error0="+error0.toString)
      println("error1="+error1.toString)
      println("errorMin="+minError.toString)



    }

    undoBackUp
    computeError

  }

}
