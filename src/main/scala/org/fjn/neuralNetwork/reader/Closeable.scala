package org.fjn.neuralNetwork.reader


object Closeable{

  def using[T<: {def close():Unit}, TR](item:T)(f:(T) => TR):TR={

    try{
      f(item)
    } finally{
      item.close()
    }
  }


}
