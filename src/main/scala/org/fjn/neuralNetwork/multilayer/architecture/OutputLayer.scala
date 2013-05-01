package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.activation.ActivationFunction

case class OutputLayer(size:Int,activationFunc:ActivationFunction,key:Int)
  extends INNLayer {

}