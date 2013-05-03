package org.fjn.neuralNetwork.multilayer.architecture

import org.fjn.neuralNetwork.multilayer.algorithm.BackPropagation
import org.fjn.neuralNetwork.multilayer.normalization.PrincipalValueDecompositionNormalizer


case class FeedForwardNetwork(nnData:NetworkData,lr0:Double,momentum0:Double)
  extends PrincipalValueDecompositionNormalizer(nnData.dataSet.map(s => s.input).toArray,nnData.activationFunction.trigger)
  with Network
  with BackPropagation
   {}
