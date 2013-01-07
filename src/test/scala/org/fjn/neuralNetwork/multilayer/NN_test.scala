package org.fjn.neuralNetwork.multilayer

import org.specs2.mutable.Specification
import java.io.{FileInputStream, ObjectInputStream, ObjectOutputStream, FileOutputStream}
import activation.{Sigmoidea, ActivationFunction}
import org.fjn.matrix.Matrix
import org.fjn.neuralNetwork.reader.{DataReader, TrainingData, FinancialDataReader, MaskFactory}
import javax.swing.JFrame


class NN_test extends  Specification {

  "training a NN" should {
    "run" in {
    `testAlgorithm` mustEqual true
     refineAlgorithm mustEqual true
      testDeSerializer mustEqual true
   //   extrapolationTest mustEqual true

    }
  }

  val normalizer =  new FinancialDataReader(
    fileName = "C:\\Users\\fran\\Downloads\\IBEX35_06_12_2012.csv",
    triggerFunc = new Sigmoidea().trigger,
    nT = 15,
    outputIndex=Seq(0),
    outputDelay=0 until 5,
    nAverage = 5 ,
    regressionOrder = 2
  )

  def refineAlgorithm={
    val input = new ObjectInputStream(new FileInputStream("C:\\temp\\test.obj"))
    val obj = input.readObject()
    input.close()
    val nn2 = obj.asInstanceOf[FeedForwardNetwork]

    val data = normalizer.normalizedSamples

    val mask1 = MaskFactory.getMask(nParam=4,nT=normalizer.nT,true)

    nn2.setMask(0,mask1)
    val err = nn2.solve(500)


    val output = new ObjectOutputStream(new FileOutputStream("C:\\temp\\test.obj"))
    output.writeObject(nn2)
    output.close()

    val t1 = nn2.nnData.dataSet.head.input
    val o1 = nn2.nnData.dataSet.head.output
    val b1 = nn2(t1)
    val bn = normalizer.normalizer.deNormaliseY(b1)
    println("res = "+bn.toString)

    true
  }
  def `testAlgorithm`={

    val data = normalizer.normalizedSamples

    val nn2 =  new FeedForwardNetwork(
      new NetworkData(layerDimensions = Seq(data.head.input.numberRows,4,100,25,normalizer.regressionOrder+1), activationFunction= new Sigmoidea(),
        dataSet = data),lr0 = 0.01,momentum0=0.7 )

    val mask1 = MaskFactory.getMask(nParam=4,nT=normalizer.nT,true)

    nn2.setMask(0,mask1)
    val err = nn2.solve(200)

    val output = new ObjectOutputStream(new FileOutputStream("C:\\temp\\test.obj"))
    output.writeObject(nn2)
    output.close()


    println("total Err"+err.toString)
     //val nn = new FeedForwardNN(List(2,5,5,1))

    val t1 = nn2.nnData.dataSet.head.input
    val o1 = nn2.nnData.dataSet.head.output
    val b1 = nn2(t1)
    val bn = normalizer.normalizer.deNormaliseY(b1)
    println("res = "+bn.toString)


 //err/4.0 < 0.1
    true


  }

  def extrapolationTest={

    val input = new ObjectInputStream(new FileInputStream("C:\\temp\\test.obj"))
    val obj = input.readObject()
    input.close()
    val m2 = obj.asInstanceOf[FeedForwardNetwork]

    val x = TimeExtrapolator.extrapolation(normalizer.normalizedSamples.head.input,15,m2,normalizer.nT).map(normalizer.normalizer.deNormaliseY _)

    import org.math.plot._
    // create your PlotPanel (you can use it as a JPanel)
    val plot = new Plot2DPanel();

    // add a line plot to the PlotPanel

    plot.addLinePlot("real IBEX 35", (0 until x.length).map(s => s.asInstanceOf[Double]).toArray,x.map(s => s(0,0)).toArray)

    // put the PlotPanel in a JFrame, as a JPanel
    val frame = new JFrame("a plot panel");
    frame.setContentPane(plot);
    frame.setVisible(true);

    val ln = readLine()
    true

  }
  def testDeSerializer={

    val input = new ObjectInputStream(new FileInputStream("C:\\temp\\test.obj"))
    val obj = input.readObject()
    input.close()
    val m2 = obj.asInstanceOf[FeedForwardNetwork]

    def dy = normalizer.normalizer.deNormaliseY _

    val outTraining: Array[Double] = DataReader.readSamples(normalizer.fileName).map(_.input(0,0)).drop(normalizer.nT+normalizer.outputDelay.head)
    val x0 = outTraining.indices.toArray.map(_.asInstanceOf[Double])
    import org.math.plot._
    // create your PlotPanel (you can use it as a JPanel)

    val plot = new Plot2DPanel();
    plot.addLinePlot("real IBEX 35", x0, outTraining.toArray);

    var offset = 0
    for(sample <- normalizer.normalizedSamples){
      val coefs =  dy(m2(sample.input))
      def reg:Function1[Double,Double] = xval =>{coefs(0,0)+coefs(1,0)*xval+coefs(2,0)*xval*xval}
      val nnout = (0 until normalizer.outputDelay.length).map(v => reg(v.asInstanceOf[Double])+ normalizer.normalizer.deNormaliseX(sample.input)(normalizer.nT*(normalizer.outputIndex.head+1)-1,0))

      //val nnout: Array[Double] =  normalizer.normalizedSamples.map(s => m2(s.input)).map(dy).map(c=> c(0,0)).toSeq.toArray

      var x = nnout.indices.toArray.map(_.asInstanceOf[Double]+offset+normalizer.outputDelay.head)

      offset += 1

      // add a line plot to the PlotPanel
      plot.addLinePlot("neural Network", x, nnout.toArray);

    }


    // put the PlotPanel in a JFrame, as a JPanel
    val frame = new JFrame("a plot panel");
    frame.setContentPane(plot);
    frame.setVisible(true);


    val ln = readLine()

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