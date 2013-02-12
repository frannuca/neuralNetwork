package org.fjn.neuralNetwork.multilayer.finance

import org.fjn.neuralNetwork.reader.{DataReader, TrainingData, MaskFactory, FinancialDataReader}
import org.fjn.neuralNetwork.multilayer.architecture.{NetworkData, FeedForwardNetwork}
import org.fjn.neuralNetwork.multilayer.activation.Sigmoidea
import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}
import org.fjn.matrix.Matrix
import Jama.Matrix
import org.fjn.matrix.Matrix
import collection.mutable.ListBuffer


object FinancialTimeSeriesNN{

  def deserialize(fileName:String):FinancialTimeSeriesNN={
    val input = new ObjectInputStream(new FileInputStream(fileName))
    val obj = input.readObject()
    input.close()
    obj.asInstanceOf[FinancialTimeSeriesNN]
  }
}
case class FinancialTimeSeriesNN(seriesData:TimeSeriesData,hiddenLayerSizes:Seq[Int],trainingData:TrainingAlgorithmData) {

  private val normalizer =  new FinancialDataReader( seriesData )


  private val parameterSize = normalizer.nParameter
  private val timeSeriesParameterSize = normalizer.normalizedSamples.head.input.numberRows

  private val network =  new FeedForwardNetwork(
    new NetworkData(layerDimensions = Seq(timeSeriesParameterSize,parameterSize)++hiddenLayerSizes++Seq(normalizer.data.outWindowSize), activationFunction= new Sigmoidea(),
      dataSet = normalizer.normalizedSamples),lr0 = trainingData.lr0,momentum0=trainingData.momentum0 )

  private val mask1 = MaskFactory.getMask(nParam=parameterSize,nT=seriesData.nT,true)
  network.setMask(0,mask1)


  val solve = network.solve _


  def serializeObj(fileName:String){
    val output = new ObjectOutputStream(new FileOutputStream(fileName))
    output.writeObject(this)
    output.close()
  }


  def extractTimeSeriesInput(input:Seq[TrainingData]): org.fjn.matrix.Matrix[Double] ={

    val timeSeries = new ListBuffer[Double]()

    val nPar = input.head.input.numberRows
    for (
      j <- 0 until nPar;
      t <- input.length-seriesData.nT until input.length){
         timeSeries += input(t).input(j,0)
    }

    new org.fjn.matrix.Matrix[Double](timeSeries.length,1) <= timeSeries
  }

  ///TODO: this function is wrong. It needs to assemble the vector of training data from the given time series
  //Given a time series file compatible with the one used for training it returns the projected output
  def compute(inputFileName:String): org.fjn.matrix.Matrix[Double] ={

    val input = extractTimeSeriesInput(DataReader.readSamples(inputFileName))

    normalizer.normalizer.deNormaliseY(network(normalizer.normalizer.normaliseX(input)))
  }

}

