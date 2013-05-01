package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.algorithm.BackPropagation
import org.fjn.neuralNetwork.multilayer.normalization.PrincipalValueDecompositionNormalizer


class FeedForwardNetwork(val nnData:NetworkData,val lr0:Double,val momentum0:Double)
  extends PrincipalValueDecompositionNormalizer(nnData.dataSet.map(s => s.input).toArray,nnData.activationFunction.trigger)
  with Network
  with BackPropagation
   {}
