package org.fjn.neuralNetwork.esn

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import java.net.URL
import scala.io.{Source, BufferedSource}
import org.fjn.neuralNetwork.reader.{TrainingData, TrainingDataForWindow}
import org.math.plot._
import javax.swing.JFrame
import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import java.lang
import org.fjn.neuralNetwork.multilayer.activation.Sigmoidea
import scala.collection.immutable.IndexedSeq
import org.fjn.matrix.Matrix

/**
 * Created by fran on 29/05/2014.
 */
class ESNTests extends AssertionsForJUnit{



  def getDowJonesTrainingData: (Seq[TrainingData],Double,Double) = {
    val path = "/dowjones.csv"
    val file0: URL = this.getClass().getResource(path)
    val file: BufferedSource = Source.fromURL(file0)



    def processSeriesRegression(x:Array[Double]):Array[Double]= {

      val V= new Matrix[Double](x.length,3)
      val Y= new Matrix[Double](x.length,1)
      for(n <- x.indices){
        V(n,0) = 1.0
        V(n,1)= n
        V(n,2)= n*n
        Y(n,0)=x(n)-x(0)
      }

      val A = V.transpose*V//3x3
      A.invert
      val w = A*V.transpose*Y

      (x.indices.map(n => w(0,0)*1.0+w(1,0)*n+w(2,0)*n*n)).toArray
    }

    TrainingDataForWindow.read(file,2,120,0,10)(processSeriesRegression,processSeriesRegression)

  }
  @Test def testDowJones{

    val (samples,mean,norm) = getDowJonesTrainingData
    val Nin = samples.head.input.numberRows
    val Nout = samples.head.output.numberRows
    val N = 500

    val s = new Sigmoidea()
    val enn= EchoStateNetwork(Nin=Nin,N=N,Nout=Nout,alpha=1.0,
      trigger = (x)=>s.f(x),data=samples.toArray,
      Wsparsity=30,
      ridgeCoeff=1.0,
      WInScale=2.0/Nin.toDouble)
    val X=enn.learn

    val r5: IndexedSeq[Double] =for(i <- 0 until X.numberRows) yield{
      X(i,5)
    }

    val r19: IndexedSeq[Double] =for(i <- 0 until X.numberRows) yield{
      X(i,19)
    }



    def foo(titles:String,p:Array[Double],p2:Array[Double]=null){
      val plot = new Plot2DPanel();

      plot.addLinePlot(titles,p)
      Option(p2).foreach(b => plot.addLinePlot("second",b))

      val frame = new JFrame(titles);
      frame.setContentPane(plot);
      frame.setVisible(true);
      frame.setBounds(0,0,300,200)
    }

    // put the PlotPanel in a JFrame, as a JPanel


    val series_approx = samples.drop(100).map(item => enn.computeOutput(item.input)).take(30)
    val series_expected = samples.drop(100).map(item => item.output).take(30)

    val r: Seq[(Matrix[Double], Matrix[Double])] = (series_approx zip series_expected)
      r.foreach(pair => foo("expected",pair._1.getArray(),pair._2.getArray()))

//    foo("r5",r5.toArray)
//    foo("r19",r19.toArray)

    val in = readLine()
  }

}
