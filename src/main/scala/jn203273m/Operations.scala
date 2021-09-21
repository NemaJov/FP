package jn203273m

import java.awt.image.BufferedImage
import java.util.function.DoubleToLongFunction
import scala.collection.mutable.ArrayBuffer

object Operations {


  //Tradeoff - either we make f a function that takes r, g and b parameters, thus making it more difficult to
  //create new simpler functions that each take a single parameter, or we

def splitIntoARGB(argb: Int):(Int,Int,Int,Int)={
  val a = argb&0xff000000
  val r = (argb&0x00ff0000)>>16
  val g = (argb&0x0000ff00)>>8
  val b = argb&0x000000ff
  (a,r,g,b)
}

  def applyToLayer(f: (Double, Double) => Double): (ArrayBuffer[Layer], ArrayBuffer[Double]) => Unit = {
    def apply(layers: ArrayBuffer[Layer], kArr: ArrayBuffer[Double]) = {
      val k = kArr(0)
      layers.foreach(layer => if(layer.active){
        for(i <- Range(0,layer.bufferedImage.getWidth()); j <- Range(0,layer.bufferedImage.getHeight())) {
          var noSelectionsActive = UI.project.noActiveSelections()
          UI.project.selections.foreach(selection=>{
            if (selection.active && selection.isInSelection(i,j)){
              val rgb=layer.bufferedImage.getRGB(i,j)
              val (a,r,g,b) = splitIntoARGB(rgb)
              layer.bufferedImage.setRGB(i,j,a+(keepWithinLimits(f(r,k)).toInt<<16)+(keepWithinLimits(f(g,k)).toInt<<8)+keepWithinLimits(f(b,k)).toInt)
            }
          })
          if(noSelectionsActive) {
            val rgb=layer.bufferedImage.getRGB(i,j)
            val (a,r,g,b) = splitIntoARGB(rgb)
            layer.bufferedImage.setRGB(i,j,a+(keepWithinLimits(f(r,k)).toInt<<16)+(keepWithinLimits(f(g,k)).toInt<<8)+keepWithinLimits(f(b,k)).toInt)
          }
        }
      })
    }
    apply
  }


  def keepWithinLimits(x: Double) = {
    Math.min(255,Math.max(0,x))
  }


  def addF = (x:Double,k:Double) => {
    def toString = "Add"
    val name = "Add"
    (x+k)
  }
  def subF = (x:Double,k:Double) => {
    def toString  = "Sub"
    (x-k)
  }
  def subRevF =(x:Double,k:Double) => {
    def toString = "SubRev"
    (k - x)
  }
  def mulF = (x:Double,k:Double) => {
    def toString = "Mul"
    (x*k)
  }
  def divF = (x:Double,k:Double) => {
    def toString = "Div"
    (x/k)
  }
  def divRevF = (x:Double,k:Double) => {
    def toString = "DivRev"
    (k/x)
  }
  def powF = (x:Double,k:Double) => {
    def toString = "Pow"
    (Math.pow(x,k))
  }
  def logF = (x:Double,k:Double) => {
    def toString = "Log"
    (Math.log(x))
  }
  def absF = (x:Double,k:Double) => {
    def toString = "Abs"
    (Math.abs(x))
  }
  def minF = (x:Double,k:Double) => {
    def toString = "Min"
    (Math.min(x,k))
  }
  def maxF = (x:Double,k:Double) => {
    def toString = "Max"
    (Math.max(x,k))
  }
  def invertF = (x:Double,k:Double) => {
    def toString = "Invert"
    (255-x)
  }



  def addWL = applyToLayer(addF)
  def subWL = applyToLayer(subF)
  def subRevWL = applyToLayer(subRevF)
  def mulWL = applyToLayer(mulF)
  def divWL = applyToLayer(divF)
  def divRevWL = applyToLayer(divRevF)
  def powWL = applyToLayer(powF)
  def logWL = applyToLayer(logF)
  def absWL = applyToLayer(absF)
  def minWL = applyToLayer(minF)
  def maxWL = applyToLayer(maxF)
  def invertWL = applyToLayer(invertF)

  def add = addWL(UI.project.getLayers(),_)
  def sub = subWL(UI.project.getLayers(),_)
  def subRev = subRevWL(UI.project.getLayers(),_)
  def mul = mulWL(UI.project.getLayers(),_)
  def div = divWL(UI.project.getLayers(),_)
  def divRev = divRevWL(UI.project.getLayers(),_)
  def pow = powWL(UI.project.getLayers(),_)
  def log = logWL(UI.project.getLayers(),_)
  def abs = absWL(UI.project.getLayers(),_)
  def min = minWL(UI.project.getLayers(),_)
  def max = maxWL(UI.project.getLayers(),_)
  def invert = invertWL(UI.project.getLayers(),_)

  //def invert = subRev(255)


  def grayscaleWL(layers: ArrayBuffer[Layer], kArr: ArrayBuffer[Double]) = {
    val k = kArr(0)
    layers.foreach(layer => if(layer.active){
      for(i <- Range(0,layer.bufferedImage.getWidth()); j <- Range(0,layer.bufferedImage.getHeight())) {
        var noSelectionsActive = UI.project.noActiveSelections()
        UI.project.selections.foreach(selection=>{
          if (selection.active && selection.isInSelection(i,j)){
            val rgb=layer.bufferedImage.getRGB(i,j)
            val (a,r,g,b) = splitIntoARGB(rgb)
            val avg = keepWithinLimits((r+g+b)/3)
            layer.bufferedImage.setRGB(i,j,a+(avg.toInt<<16)+(avg.toInt<<8)+avg.toInt)
          }
        })
        if(noSelectionsActive) {
          val rgb=layer.bufferedImage.getRGB(i,j)
          val (a,r,g,b) = splitIntoARGB(rgb)
          val avg = keepWithinLimits((r+g+b)/3)
          layer.bufferedImage.setRGB(i,j,a+(avg.toInt<<16)+(avg.toInt<<8)+avg.toInt)
        }
      }
    })
  }

  def grayscale = grayscaleWL(UI.project.getLayers(),_)

  def medianWL(layers: ArrayBuffer[Layer], kArr: ArrayBuffer[Double]) = {
    val k:Double = kArr(0)
    layers.foreach(layer => if(layer.active){
      val helperBufferedImage = new BufferedImage(layer.bufferedImage.getWidth(),layer.bufferedImage.getHeight(),BufferedImage.TYPE_INT_ARGB)
      for(i <- Range(0,layer.bufferedImage.getWidth()); j <- Range(0,layer.bufferedImage.getHeight())) {
        var noSelectionsActive = UI.project.noActiveSelections()
        UI.project.selections.foreach(selection=>{
          if (selection.active && selection.isInSelection(i,j)){
            val left = (i-k).toInt
            val right = (i+k).toInt
            val bot = (j+k).toInt
            val top = (j-k).toInt
            val red = new ArrayBuffer[Int]()
            var num = 0
            val green = new ArrayBuffer[Int]()
            val blue = new ArrayBuffer[Int]()
            for(filterI <- Range(left,right); filterJ<-Range(top,bot)){
              if(filterI>=0 && filterI<layer.bufferedImage.getWidth() && filterJ>=0 && filterJ<layer.bufferedImage.getHeight()){
                val(a,r,g,b) = splitIntoARGB(layer.bufferedImage.getRGB(filterI,filterJ))
                num+=1
                red.addOne(r)
                green.addOne(g)
                blue.addOne(b)
              }
            }
            red.sortInPlace()
            green.sortInPlace()
            blue.sortInPlace()
            val a = layer.bufferedImage.getRGB(0,0)&0xff000000
            var r = if(num%2==0) (red(num/2)+red(num/2-1))/2 else red(num/2)
            var g = if(num%2==0) (green(num/2)+green(num/2-1))/2 else green(num/2)
            var b = if(num%2==0) (blue(num/2)+blue(num/2-1))/2 else blue(num/2)
            helperBufferedImage.setRGB(i,j,a+(r<<16)+(g<<8)+b)
          }
          else helperBufferedImage.setRGB(i,j,layer.bufferedImage.getRGB(i,j))
        })
        if(noSelectionsActive) {
          val left = (i-k).toInt
          val right = (i+k).toInt
          val bot = (j+k).toInt
          val top = (j-k).toInt
          var num = 0
          val red = new ArrayBuffer[Int]()
          val green = new ArrayBuffer[Int]()
          val blue = new ArrayBuffer[Int]()
          for(filterI <- Range(left,right); filterJ<-Range(top,bot)){
            if(filterI>=0 && filterI<layer.bufferedImage.getWidth() && filterJ>=0 && filterJ<layer.bufferedImage.getHeight()){
              val(a,r,g,b) = splitIntoARGB(layer.bufferedImage.getRGB(filterI,filterJ))
              num+=1
              red.addOne(r)
              green.addOne(g)
              blue.addOne(b)
            }
          }
          red.sortInPlace()
          green.sortInPlace()
          blue.sortInPlace()
          val a = layer.bufferedImage.getRGB(0,0)&0xff000000
          var r = 0
          var g = 0
          var b = 0
          if(num%2==0){
            r = (red(num/2)+red(num/2-1))/2
            g = (green(num/2)+green(num/2-1))/2
            b = (blue(num/2)+blue(num/2-1))/2
          }
          else{
            r = red(num/2)
            g = green(num/2)
            b = blue(num/2)
          }
          helperBufferedImage.setRGB(i,j,a+(r<<16)+(g<<8)+b)
        }
      }
      layer.bufferedImage = helperBufferedImage
    })
  }

  def median = medianWL(UI.project.getLayers(),_)

  def ponderedWL(layers: ArrayBuffer[Layer], kArr: ArrayBuffer[Double]) = {
    val k:Double = kArr(0)
    kArr.remove(0)
    layers.foreach(layer => if(layer.active){
      val helperBufferedImage = new BufferedImage(layer.bufferedImage.getWidth(),layer.bufferedImage.getHeight(),BufferedImage.TYPE_INT_ARGB)
      val noSelectionsActive = UI.project.noActiveSelections()
      for(i <- Range(0,layer.bufferedImage.getWidth()); j <- Range(0,layer.bufferedImage.getHeight())) {
        UI.project.selections.foreach(selection=>{
          if (selection.active && selection.isInSelection(i,j)){
            val left = (i-k).toInt
            val right = (i+k).toInt
            val bot = (j+k).toInt
            val top = (j-k).toInt
            var num = 0
            var sumRed = 0.0
            var sumGreen = 0.0
            var sumBlue = 0.0
            for(filterI <- Range(left,right); filterJ<-Range(top,bot)){
              if(filterI>=0 && filterI<layer.bufferedImage.getWidth() && filterJ>=0 && filterJ<layer.bufferedImage.getHeight()){
                val(a,r,g,b) = splitIntoARGB(layer.bufferedImage.getRGB(filterI,filterJ))
                val factor = kArr((filterI-left)*(2*k.toInt+1)+filterJ-top)
                num+=1
                sumRed += r*factor
                sumGreen += g*factor
                sumBlue +=b*factor
              }
            }
            val a = layer.bufferedImage.getRGB(0,0)&0xff000000
            val r = (sumRed/num).toInt
            val g = (sumGreen/num).toInt
            val b = (sumBlue/num).toInt
            helperBufferedImage.setRGB(i,j,a+(r<<16)+(g<<8)+b)
          }
          else helperBufferedImage.setRGB(i,j,layer.bufferedImage.getRGB(i,j))
        })
        if(noSelectionsActive) {
          val left = (i-k).toInt
          val right = (i+k).toInt
          val bot = (j+k).toInt
          val top = (j-k).toInt
          var num = 0
          var sumRed = 0.0
          var sumGreen = 0.0
          var sumBlue = 0.0
          for(filterI <- Range(left,right); filterJ<-Range(top,bot)){
            if(filterI>=0 && filterI<layer.bufferedImage.getWidth() && filterJ>=0 && filterJ<layer.bufferedImage.getHeight()){
              val(a,r,g,b) = splitIntoARGB(layer.bufferedImage.getRGB(filterI,filterJ))
              val factor = kArr((filterI-left)*(2*k.toInt+1)+filterJ-top)
              num+=1
              sumRed += r*factor
              sumGreen += g*factor
              sumBlue +=b*factor
            }
          }
          val a = layer.bufferedImage.getRGB(0,0)&0xff000000
          val r = (sumRed/num).toInt
          val g = (sumGreen/num).toInt
          val b = (sumBlue/num).toInt
          helperBufferedImage.setRGB(i,j,a+(r<<16)+(g<<8)+b)
        }
      }
      layer.bufferedImage = helperBufferedImage
    })
  }

  def pondered = ponderedWL(UI.project.getLayers(),_)

  //median
  //pondered - Input => Same textbox but separate values with ',' and create matrix like that. Can be added to sequence
  //matrix 2*k+1
  val operations = new ArrayBuffer[(Int,Double)=>Int]()

  val operationLabels = new ArrayBuffer[String]
  operationLabels.addOne("add")
  operationLabels.addOne("sub")
  operationLabels.addOne("subRev")
  operationLabels.addOne("mul")
  operationLabels.addOne("div")
  operationLabels.addOne("divRev")
  operationLabels.addOne("pow")
  operationLabels.addOne("log")
  operationLabels.addOne("abs")
  operationLabels.addOne("min")
  operationLabels.addOne("max")
  operationLabels.addOne("invert")
  operationLabels.addOne("grayscale")
  operationLabels.addOne("median")
  operationLabels.addOne("pondered")

}
