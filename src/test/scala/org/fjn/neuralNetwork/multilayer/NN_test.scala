package org.fjn.neuralNetwork.multilayer

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.fjn.neuralNetwork.multilayer.architecture.{FeedForwardNetwork, NetworkData}
import org.fjn.neuralNetwork.multilayer.activation.Sigmoidea
import org.fjn.neuralNetwork.generator.XORGenerator


class NN_test extends  AssertionsForJUnit {

  @Test def test1 {


    assert(`testAlgorithm` == true)

  }

  def `testAlgorithm`={

    //val filepath = getClass().getResource("/"+ "dowjones.csv").getFile;






    val data = new NetworkData(
      layerDimensions = Seq(2,6,2,1),
      activationFunction = new Sigmoidea(),
      dataSet = XORGenerator()
    )

    val ffnn = new FeedForwardNetwork(
    nnData = data,
    lr0=0.5,
    momentum0 = 0.7
    )


    val err = ffnn.solve(200)

    println("total Err"+err.toString)

    XORGenerator().foreach(d => println("in (%s), out = %s, eou=%s".format(d.input.toString,ffnn(d.input).toString,d.output.toString)))
    true
  }

}