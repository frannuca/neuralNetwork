package org.fjn.neuralNetwork.multilayer.finance

import org.fjn.neuralNetwork.reader.{MaskFactory, FinancialDataReader}
import org.fjn.neuralNetwork.multilayer.architecture.{NetworkData, FeedForwardNetwork}
import org.fjn.neuralNetwork.multilayer.activation.Sigmoidea
import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}
import collection.immutable.IndexedSeq


object FinancialTimeSeriesNN{

  def deserialize(fileName:String):FinancialTimeSeriesNN={
    val input = new ObjectInputStream(new FileInputStream(fileName))
    val obj = input.readObject()
    input.close()
    obj.asInstanceOf[FinancialTimeSeriesNN]
  }
}
case class FinancialTimeSeriesNN(seriesData:TimeSeriesData,hiddenLayerSizes:Seq[Int],trainingData:TrainingAlgorithmData) {

  private val normalizer =  new FinancialDataReader(
    fileName = seriesData.fileName,
    triggerFunc = seriesData.triggerFunc.trigger,
    nT = seriesData.nT,
    outputIndex=Seq(seriesData.outputIndex),
    outputDelay=0 until seriesData.outputDelayLength,
    nAverage = seriesData.nAverage ,
    regressionOrder = seriesData.regressionOrder
  )


  private val parameterSize = normalizer.numberOfParameters
  private val timeSeriesParameterSize = normalizer.normalizedSamples.head.input.numberRows

  private val network =  new FeedForwardNetwork(
    new NetworkData(layerDimensions = Seq(timeSeriesParameterSize,parameterSize)++hiddenLayerSizes++Seq(normalizer.regressionOrder+1), activationFunction= new Sigmoidea(),
      dataSet = normalizer.normalizedSamples),lr0 = trainingData.lr0,momentum0=trainingData.momentum0 )

  private val mask1 = MaskFactory.getMask(nParam=parameterSize,nT=seriesData.nT,true)
  network.setMask(0,mask1)


  val solve = network.solve _

  val samplesFilename:String = normalizer.fileName


  def serializeObj(fileName:String){
    val output = new ObjectOutputStream(new FileOutputStream(fileName))
    output.writeObject(this)
    output.close()
  }


  def compute(inputFileName:String):Seq[Double]={

    val localNormalizer =  new FinancialDataReader(
      fileName = inputFileName,
      triggerFunc = seriesData.triggerFunc.trigger,
      nT = seriesData.nT,
      outputIndex=Seq(seriesData.outputIndex),
      outputDelay=0 until seriesData.outputDelayLength,
      nAverage = seriesData.nAverage ,
      regressionOrder = seriesData.regressionOrder
    )

    val dy = normalizer.normalizer.deNormaliseY _
    val dx = normalizer.normalizer.deNormaliseX _


    def reg:Function2[Double,Seq[Double],Double] = (xval:Double,cs:Seq[Double]) => {
      cs.indices.map(n => cs(n)*math.pow(xval,n)).sum
    }



      val coefs =  dy(network(normalizer.normalizedSamples.last.input))
      (0 until normalizer.outputDelay.length).map(v => reg(v.toDouble,coefs.getArray()))



  }

}
