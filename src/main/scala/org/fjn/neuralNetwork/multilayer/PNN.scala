package org.fjn.neuralNetwork.multilayer

import normalization.{DummyNormalizer, Normalizer}
import org.fjn.neuralNetwork.reader.FinancialDataReader

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 06/12/12
 * Time: 18:48
 * To change this template use File | Settings | File Templates.
 */
class PNN(nnDatav:NetworkData,outputTimeDelay:Int,index4Output:Int,timePeriod:Int) extends Network(nnDatav) with DummyNormalizer with BackPropagation{


  val financial = new FinancialDataReader {
    val outputDelay = outputTimeDelay
    val outputIndex = index4Output
    val nT = timePeriod
    val nnData = nnDatav
  }

  val (originalTrainingSet,normalizer) = financial.getData
  val normalizedTrainingSet = originalTrainingSet
  val nnData = nnDatav
}
