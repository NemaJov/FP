package jn203273m

import jn203273m.Operations.{absF, addF, divF, divRevF, invertF, logF, maxF, minF, mulF, powF, subF, subRevF}

class SimpleOperation (val name:String){
  var f:(Double, Double)=>Double = null

}

object SimpleOperation {


  def createSimpleOperation(f: (Double, Double)=>Double, g: (Double, Double)=> Double, name: String, kOld: Double): SimpleOperation ={
    def newOperation(x: Double, k: Double) ={
      def toString()= name
      f(g(x,kOld),k)
    }
    val simple = new SimpleOperation(name){f = newOperation}
    UI.project.simpleOperations.addOne(simple)
    simple
  }
}
