package org.fjn.neuralNetwork.multilayer

import org.specs2.mutable.Specification
import java.io.{FileInputStream, ObjectInputStream, ObjectOutputStream, FileOutputStream}
import activation.{Sigmoidea, ActivationFunction}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.reader.{TrainingData, FinancialDataReader, MaskFactory}


class NN_test extends  Specification {

  "training a NN" should {
    "run" in {
      //`testAlgorithm` mustEqual true
      testDeSerializer mustEqual true

    }
  }

  def `testAlgorithm`={



    val normalizer =  new FinancialDataReader(
      fileName = "C:\\Users\\fran\\Downloads\\IBEX35_2011.txt",
      triggerFunc = new Sigmoidea().trigger,
      nT = 15,
      outputIndex=0,
      outputDelay=1
    )
    val data = normalizer.normalizedSamples

    val nn2 =  new FeedForwardNetwork(
      new NetworkData(layerDimensions = Seq(data.head.input.numberRows,6,50,50,1), activationFunction= new Sigmoidea(),
        dataSet = data),lr0 = 0.01,momentum0=0.8 )

    val mask1 = MaskFactory.getMask(nParam=6,nT=15,true)

    nn2.setMask(0,mask1)
    val err = nn2.solve(500)

    val output = new ObjectOutputStream(new FileOutputStream("C:\\temp\\test.obj"))
    output.writeObject(nn2)
    output.close()

    val input = new ObjectInputStream(new FileInputStream("C:\\temp\\test.obj"))
    val obj = input.readObject()
    input.close()
    val m2 = obj.asInstanceOf[FeedForwardNetwork]


    println("total Err"+err.toString)
     //val nn = new FeedForwardNN(List(2,5,5,1))

    val t1 = nn2.nnData.dataSet.head.input
    val o1 = nn2.nnData.dataSet.head.output
    val b1 = nn2(t1)
    val bn = normalizer.normalizer.deNormaliseY(b1)
    println("res = "+bn.toString)


    val sbld = new StringBuilder()
    sbld.append(0).append(",").append(0).append(";").append(1).append(";").append(nn2(new Matrix[Double](2,1) <= Seq(0.0,0.0))).append("\r\n")
    sbld.append(0).append(",").append(1).append(";").append(0).append(";").append(nn2(new Matrix[Double](2,1) <= Seq(0.0,1.0))).append("\r\n")
    sbld.append(1).append(",").append(0).append(";").append(0).append(";").append(nn2(new Matrix[Double](2,1) <= Seq(1.0,0.0))).append("\r\n")
    sbld.append(1).append(",").append(1).append(";").append(1).append(";").append(nn2(new Matrix[Double](2,1) <= Seq(1.0,1.0))).append("\r\n")

    println(sbld.toString())

    //val err = nn.train(NNTestUtils.generateSet1(),true)


    val e1 = nn2.computeError
    val a1 = nn2.getWeightArray

    a1(3)=a1(3)*0.5
    nn2.setWeightArray(a1)

    val e2 = nn2.computeError

    err/4.0 < 0.1


  }

  def testDeSerializer={

    val input = new ObjectInputStream(new FileInputStream("C:\\temp\\test.obj"))
    val obj = input.readObject()
    input.close()
    val m2 = obj.asInstanceOf[FeedForwardNetwork]

    val normalizer =  new FinancialDataReader(
      fileName = "C:\\Users\\fran\\Downloads\\IBEX35_2011.txt",
      triggerFunc = new Sigmoidea().trigger,
      nT = 15,
      outputIndex=0,
      outputDelay=1
    )
    val data = normalizer.normalizedSamples

    def dx = normalizer.normalizer.deNormaliseX _
    def dy = normalizer.normalizer.deNormaliseY _

    val inPar = normalizer.normalizer.deNormaliseX(normalizer.normalizedSamples.head.input)
    val outPar = normalizer.normalizer.deNormaliseX(normalizer.normalizedSamples.head.output)
    val nnout = m2(normalizer.normalizedSamples.head.input)

    println("error="+m2.computeError.toString)
    println("input vector="+inPar.toString)

    println("output vector="+outPar.toString)
    println("NN output vector="+dy(nnout).toString)

    true
  }
}

object NNTestUtils{


  def generateXOR():StringBuilder={
    val sbld = new StringBuilder()
    sbld.append(0).append(",").append(0).append(";").append(1).append("\r\n")
    sbld.append(0).append(",").append(1).append(";").append(0).append("\r\n")
    sbld.append(1).append(",").append(0).append(";").append(0).append("\r\n")
    sbld.append(1).append(",").append(1).append(";").append(1).append("\r\n")

    sbld
  }

  def genetateFunc():StringBuilder = {
    val dx=0.1
        val dy = 0.2
        val sbld = new StringBuilder()
        for ( ix <- 0 until 4)
        {
           val x = ix*dx
          for ( iy <- 0 until  4)
          {

            val y = iy*dy
            sbld.append(x).append(",").append(y).append(";").append((30.0*x*x+15.0*y*y)).append("\r\numberOfIteration")

          }

        }

    sbld
  }
  def generateSet1():String = {




    val sbld = generateXOR()
    val writer = new FileOutputStream("C:\\temp\\test.txt")

     writer.write(sbld.toString().getBytes)


    writer.close()

    "C:\\temp\\test.txt"


  }
}