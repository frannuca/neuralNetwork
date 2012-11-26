package org.fjn.neuralNetwork.multilayer

import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.reader.{TrainingData, DataReader}





/**
 * given a file containing training data, this class loads the file and stores its
 * contents into a sequence of TrainingData
 * Input format for data is as follows:
 *      x11,x12,x13, ...,x1n; o11,o12,..,o1m
 *      x21,x22,x23, ...,x2n; o21,o22,..,o2m
 *      x31,x32,x33, ...,x3n; o31,o32,..,o3m
 *
 * @param fileName file containing input data
 */
class TrainingSet(val fileName:String) extends  Iterable[TrainingData] with DataReader{


  val data: Array[TrainingData] = getData

  private var startPos:Int = 0


  def inputVectorDimension():Int={
    if(data.length>0){
      data(0).input.numberRows
    }else 0
  }

  def inputSetDimension:Int = data.length


  def outputVectorDimension():Int={
    if(data.length>0){
      data(0).output.numberRows
    }else 0
  }


  def withStartPosition(pos:Int):TrainingSet={
   startPos = pos
    this
  }




  def iterator = new Iterator[TrainingData]{
    private val maxL = data.length
    private var counter = 0


    def hasNext = counter<maxL

    def next() = {
      counter = counter + 1
      data(((counter-1+startPos)%maxL))
    }
  }
}
