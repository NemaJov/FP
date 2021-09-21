package jn203273m

import scala.collection.mutable.ArrayBuffer

class Sequence (val name: String){
  val operations = new ArrayBuffer[((ArrayBuffer[Double]=>Unit),ArrayBuffer[Double])]()
  def addOperation(f:(ArrayBuffer[Double]=>Unit),kArr: ArrayBuffer[Double]): Unit ={
    operations.addOne(f,kArr)
  }

  def execute()={
    operations.foreach(operation=>{
      operation._1(operation._2)
    })
  }
}
