package org.fjn.neuralNetwork.multilayer.finance

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 07/01/13
 * Time: 18:58
 * To change this template use File | Settings | File Templates.
 */
case class TimeSeriesData(
                            fileName:String,
                            triggerFunc:ActivationFunction,
                            nT:Int,
                            outputIndex:Int,
                            outputDelay:Int,
                            outWindowSize:Int,
                            nAverage:Int,
                            regressionOrder:Int)


case class FinancialDataReaderData(fileName: String, triggerFunc: Function1[Double, Double],
                                   nT: Int, outputIndex: Seq[Int], outputDelay: Seq[Int], nAverage: Int, regressionOrder: Int)
