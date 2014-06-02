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

    TrainingDataForWindow.read(file,2,90,0,10)

  }
  @Test def testDowJones{

    val (samples,mean,norm) = getDowJonesTrainingData
    val Nin = samples.head.input.numberRows
    val Nout = samples.head.output.numberRows
    val N = 700

    val s = new Sigmoidea()
    val enn= EchoStateNetwork(Nin=Nin,N=N,Nout=Nout,alpha=1.0,
      trigger = (x)=>s.f(x),data=samples.toArray,
      Wsparsity=30,
      ridgeCoeff=1.0e-1,
      WInScale=2.0/Nin.toDouble)
    val X=enn.learn

    val r5: IndexedSeq[Double] =for(i <- 0 until X.numberRows) yield{
      X(i,5)
    }

    val r19: IndexedSeq[Double] =for(i <- 0 until X.numberRows) yield{
      X(i,19)
    }
//    val err = (for(s <- samples) yield{
//      val o =enn.computeOutput(s.input)
//      val eo = s.output
//      val d = (o-eo)
//      val er=(d.transpose * d)(0,0)
//
//      println(s"computed=$o \n expected=$eo \n diff=$er \n")
//
//      er
//    })
//    println(enn.WOut)
//    println(s"size=${err.length} error=$err")

    val series_approx: IndexedSeq[Array[Double]] =  for(i<- 0 until samples.length/10-1) yield{ enn.computeOutput(samples(i*10).input).getArray()}
    val series_expected: IndexedSeq[Array[Double]] = for(i<- 0 until samples.length/10-1) yield{samples(i*10).output.getArray()}



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



    val r = (series_approx zip series_expected)
      r.toSeq.foreach(pair => foo("expected",pair._1,pair._2))

    foo("r5",r5.toArray)
    foo("r19",r19.toArray)

    val in = readLine()
  }

}
