package org.fjn.neuralNetwork.multilayer


import collection.mutable.ListBuffer
import org.fjn.matrix._


trait WeightUpdater {

  self: NNTrainingCtes =>

  def clearWeights(): Unit = {
    Ws.clear()
    dWs.clear()
  }

  protected var Ws = new ListBuffer[Matrix[Double]]
  protected var dWs = new ListBuffer[Matrix[Double]]

  def resolveWeights(): Unit = {


    for (i <- 0 until dWs.length) {
      dWs(i) = dWs(i) * alpha
      Ws(i) = Ws(i) - dWs(i)
    }
  }

  def initWeights(size: Seq[Int]): Unit = {

    for (n <- 1 until size.length) {
      val nM = new Matrix[Double](size(n - 1) + 1, size(n))
      nM.random
      Ws += nM

      //applyFunction(x => x*0.01,Ws.last)

      val nM2 = new Matrix[Double](size(n - 1) + 1, size(n))
      dWs += nM2
    }

    //val dd = math.sqrt ( Ws.head.numRows.toDouble)
    //applyFunction(x => x,Ws.last)
    //applyFunction(x => x/dd,Ws.head)
  }

  def GetWeightCopy(): ListBuffer[Matrix[Double]] = {
    val cpyW = new ListBuffer[Matrix[Double]]()
    Ws.foreach(w => cpyW += w.clone())

    cpyW
  }

  def cleardW(): Unit = {
    dWs.foreach(dw => dw.zeros)
  }
}

trait WeightUpdaterSimple extends WeightUpdater {

  self: NNTrainingCtes =>

  type WNet = ListBuffer[Matrix[Double]]
  val timeWindow = new ListBuffer[WNet]


  private def addWNet() {
    val w = dWs.toArray
    val w2 = new ListBuffer[Matrix[Double]]()
    w.foreach(x => w2 += x)

    timeWindow.insert(0, w2)

    if (timeWindow.length > maxWindow) {
      while (timeWindow.length > maxWindow)
        timeWindow.remove(timeWindow.length - 1)
    }
  }

  override def resolveWeights(): Unit = {

    super.resolveWeights()
    timeWindow.foreach(wnet => {
      var i = 0;
      while (i < wnet.length) {
        wnet(i) = wnet(i) * self.beta

        Ws(i) -= wnet(i)
        i += 1
      }
    })

    addWNet()


  }

}




