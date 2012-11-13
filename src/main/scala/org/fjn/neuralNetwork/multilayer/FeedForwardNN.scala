package org.fjn.neuralNetwork.multilayer

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 13/11/12
 * Time: 19:09
 * To change this template use File | Settings | File Templates.
 */

class FeedForwardNN(layerDim: Seq[Int]) extends NeuralNetworkBase(layerDim) with Sigmoidea with MaxMinNormalizer with WeightUpdaterSimple with NNTrainingCtes {

  alpha = 0.01
  override val beta = 0.1
  override val maxWindow = 5


  val c = 1d;


}