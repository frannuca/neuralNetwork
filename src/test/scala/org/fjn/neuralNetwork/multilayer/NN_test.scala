package org.fjn.neuralNetwork.multilayer

import architecture.{NetworkData, FeedForwardNetwork}
import org.specs2.mutable.Specification
import activation.{Sigmoidea, ActivationFunction}

import org.fjn.neuralNetwork.reader.{TrainingData}
import org.fjn.neuralNetwork.generator.XORGenerator


class NN_test extends  Specification {

  "training a NN" should {
    "run" in {

    `testAlgorithm` mustEqual true
    }
  }

  def `testAlgorithm`={

    //val filepath = getClass().getResource("/"+ "dowjones.csv").getFile;






    val data = new NetworkData(
      layerDimensions = Seq(2,3,3,1),
      activationFunction = new Sigmoidea(),
      dataSet = XORGenerator()
    )

    val ffnn = new FeedForwardNetwork(
    nnData = data,
    lr0=1e-1,
    momentum0 = 0.7
    )


    val err = ffnn.solve(50)

    println("total Err"+err.toString)

    XORGenerator().foreach(d => println("in (%s), out = %s, eou=%s".format(d.input.toString,ffnn(d.input).toString,d.output.toString)))
    true
  }

}