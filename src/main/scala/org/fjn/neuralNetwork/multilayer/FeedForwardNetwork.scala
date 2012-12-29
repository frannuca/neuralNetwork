package org.fjn.neuralNetwork.multilayer

import normalization.{DummyNormalizer, MeanNormalizer}
import org.fjn.neuralNetwork.reader.FinancialDataReader

class FeedForwardNetwork(val nnData:NetworkData,val lr0:Double,val momentum0:Double)
  extends Network
  with BackPropagation
