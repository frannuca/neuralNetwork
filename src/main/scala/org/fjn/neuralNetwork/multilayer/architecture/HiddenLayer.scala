package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq
import org.fjn.neuralNetwork.multilayer.architecture
import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction

/**
 * lineal array of Cells sharing a same set of inputs.
 * Layers can be of three types:
 * 1) input layer
 * 2) hidden layers
 * 3) output layer
 * @param size: number of cells hosted in this layer
 * @param activationFunc: excitation function hosted in each cell
 */
case class HiddenLayer(size:Int,activationFunc:ActivationFunction,key:Int)
  extends INNLayer{

}
