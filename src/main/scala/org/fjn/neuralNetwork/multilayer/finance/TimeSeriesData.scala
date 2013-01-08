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
                            outputDelayLength:Int,
                            nAverage:Int,
                            regressionOrder:Int)
