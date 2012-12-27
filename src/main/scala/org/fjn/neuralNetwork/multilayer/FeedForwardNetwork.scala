package org.fjn.neuralNetwork.multilayer

import normalization.{DummyNormalizer, MeanNormalizer}
import org.fjn.neuralNetwork.reader.FinancialDataReader

class FeedForwardNetwork(val nnData:NetworkData)
  extends Network
  with BackPropagation
