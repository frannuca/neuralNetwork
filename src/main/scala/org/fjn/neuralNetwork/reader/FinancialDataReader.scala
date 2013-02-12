package org.fjn.neuralNetwork.reader

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.multilayer.normalization.{ Normalizer, MeanNormalizer}
import org.fjn.optimization.Regression
import org.fjn.neuralNetwork.multilayer.finance.TimeSeriesData
import collection.immutable.IndexedSeq
import org.math.plot.Plot2DPanel
import javax.swing.JFrame

object OFinancialDataReader {
   def movingAverage(data:Array[TrainingData],nSamples:Int):Seq[TrainingData]={
    if (nSamples<=0) data
    else{
      val zeroInput = data.head.input.clone().zeros
      val zeroOut = data.head.output.clone().zeros
      val zeroTraining = new TrainingData(zeroInput,zeroOut)

      for (i<- data.indices)yield{
        data.slice(i-nSamples,i+nSamples).toList match{
          case Nil => data.head
          case a0:List[TrainingData] =>{
            if (a0.length<2.0*nSamples)
              data(i)
            else{
              val a = a0.foldLeft(zeroTraining)((b,td)=> new TrainingData((td.input+b.input),(td.output+b.output)))
              new TrainingData(input= a.input/nSamples.toDouble/2.0,output=a.output/nSamples.toDouble/2.0)
            }

          }
        }



      }
    }

  }

  def toIncremental(data:Array[Double],tol:Double=1e-6):Seq[Double]={

    data.indices.drop(1).map(n =>{
      (data(n)-data(n-1))/(math.abs(data(n-1)) max tol)
    })
  }

}
/**
 * financial data reader. This class transform historical data presented with the yahoo history format into valid
 * time series data given a variety of parameters including time window forecast etc...
 */
case class FinancialDataReader(data: TimeSeriesData) extends FinancialDataReaderBase{
  outer:FinancialDataReader =>

  import OFinancialDataReader._
  import data._
  val regressor = new Regression(regressionOrder)



  //Incremental data extraction mode
  protected val rawSamples: Array[TrainingData] = {

    val aux1 = DataReader.readSamples(fileName)

    val numberPar = aux1.head.input.getArray().length
    val paramSeriesVector = (0 until numberPar).map(n => toIncremental(aux1.map(_.input.getArray()(n))))

    for (p <- paramSeriesVector.indices;
         nPar <- paramSeriesVector(p).indices){
        aux1(nPar).input.set(p,0,paramSeriesVector(p)(nPar))
    }


    aux1.take(paramSeriesVector.head.length)
  }

  //Smoothing of the data
  val samples = movingAverage(rawSamples,nAverage).toSeq//rawSamples.toSeq

  //Dummy output process. It is not necessary to further process output when performing incremental
  def processOutput(vIn:Seq[Double]):Matrix[Double]={
//    val outputs: Seq[Double] = regressor.getRegressionCoefficients(vIn.indices.map(_.toDouble) zip vIn)
//     new Matrix[Double](outputs.length,1) <= outputs

    new Matrix[Double](vIn.length,1) <= vIn
  }

  val normalizer = new MeanNormalizer(writeTrainingSet,triggerFunc.trigger)

  val plot = new Plot2DPanel();
  //plot.addLinePlot("real IBEX 35", result.indices.map(_.toDouble).toArray, result.map(_._2).toArray);
  plot.addLinePlot("simulated IBEX 35", normalizedSamples.indices.map(_.toDouble).toArray, normalizedSamples.map(_.output(0,0)).toArray);

  // put the PlotPanel in a JFrame, as a JPanel
  val frame = new JFrame("a plot panel");
  frame.setContentPane(plot);
  frame.setVisible(true);
}

object testMovingAverage extends App{

  import OFinancialDataReader._

  val filepath = getClass().getResource("/"+ "dowjones.csv").getFile;
  val dataTraining: Array[TrainingData] = DataReader.readSamples(fileName = filepath)

  val b0= List(OFinancialDataReader.movingAverage(dataTraining.toArray,-1))++
    List(OFinancialDataReader.movingAverage(dataTraining.toArray,3))
  val plot = new Plot2DPanel();
  b0.foreach(x =>{

    plot.addLinePlot("smoothing", x.indices.map(_.toDouble).toArray, x.map(_.input(0,0)).toArray)
  } )

  //plot.addLinePlot("real IBEX 35", result.indices.map(_.toDouble).toArray, result.map(_._2).toArray);


  // put the PlotPanel in a JFrame, as a JPanel
  val frame = new JFrame("a plot panel");
  frame.setContentPane(plot);
  frame.setVisible(true);
}