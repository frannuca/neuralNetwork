//package org.fjn.neuralNetwork.multilayer
//
//import activation.Sigmoidea
//import finance.TimeSeriesData
//import org.specs2.mutable.Specification
//import org.fjn.neuralNetwork.reader.{TrainingData, FinancialDataReader}
//
///**
// * Created with IntelliJ IDEA.
// * User: fran
// * Date: 04/12/12
// * Time: 10:39
// * To change this template use File | Settings | File Templates.
// */
//class SamplesReaderTest  extends  Specification {
//
//
//  "training a NN" should {
//    "run" in {
//
//      val samples: Seq[TrainingData] = new FinancialDataReader(
//        TimeSeriesData(
//          fileName = "C:\\Users\\fran\\Downloads\\IBEX35.txt",
//        triggerFunc = new Sigmoidea().trigger,
//        outputDelay = Seq(1),
//        outputIndex = 0 until 6 ,
//        nT = 5      ) ).normalizedSamples
//
//
//       val a = 0
//
//    }
//  }
//
//}
