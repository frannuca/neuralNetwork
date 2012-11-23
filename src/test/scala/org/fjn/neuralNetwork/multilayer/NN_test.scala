package org.fjn.neuralNetwork.multilayer

import org.specs2.mutable.Specification
import java.io.FileOutputStream
import activation.{Sigmoidea, ActivationFunction}
import org.fjn.matrix.Matrix


class NN_test extends  Specification {

  "training a NN" should {
    "run" in {
      `testAlgorithm` mustEqual true

    }
  }

  def `testAlgorithm`={




    val ndata = NetworkData(
                  layerDimensions = List(2,2,2,1),
                  activationFunction= new Sigmoidea(),
                  samplesFilename=NNTestUtils.generateSet1()
    )

    val nn2 = new Network(ndata) with BackPropagation
    val err = nn2.solve(1000)
    println("total Err"+err.toString)
     //val nn = new FeedForwardNN(List(2,5,5,1))



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