package org.fjn.neuralNetwork.multilayer.algorithm

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 24/11/12
 * Time: 10:10
 * To change this template use File | Settings | File Templates.
 */
trait OptimizationCtes extends Serializable{

  val lr0:Double
  val momentum0:Double
  protected var lr:Double = 0.1
  protected var momentum:Double = 0.5
}
