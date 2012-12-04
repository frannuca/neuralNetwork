package org.fjn.neuralNetwork.multilayer

import org.specs2.mutable.Specification
import org.fjn.neuralNetwork.reader.FinancialDataReader

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 04/12/12
 * Time: 10:39
 * To change this template use File | Settings | File Templates.
 */
class SamplesReaderTest  extends  Specification {


  "training a NN" should {
    "run" in {

      val reader = new FinancialDataReader {
        val outputDelay = 1
        val outputIndex = 0
        val nT = 5
        val fileName = "C:\\Users\\fran\\Downloads\\IBEX35.txt"
      }



      val data = reader.getData

    }
  }

}
