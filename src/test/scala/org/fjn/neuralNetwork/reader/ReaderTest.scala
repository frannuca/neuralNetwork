package org.fjn.neuralNetwork.reader

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import java.io.{FileInputStream, BufferedInputStream}
import java.net.URL
import scala.io.{BufferedSource, Source}

/**
 * Created by fran on 29/05/2014.
 */
class ReaderTest extends AssertionsForJUnit{


  @Test def runReaderInDowJones{

    val path = "/dowjones.csv"
    val file0: URL = this.getClass().getResource(path)
    val file: BufferedSource = Source.fromURL(file0)

    val sample = TrainingDataForWindow.read(file,2,60,5,15)


  }

}
