package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.algorithm.BackPropagation


class FeedForwardNetwork(val nnData:NetworkData,val lr0:Double,val momentum0:Double)
  extends Network
  with BackPropagation
