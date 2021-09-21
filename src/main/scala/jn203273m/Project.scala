package jn203273m

import jn203273m.Operations.{absF, addF, divF, divRevF, invertF, logF, maxF, minF, mulF, powF, subF, subRevF}

import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.{BufferedWriter, File, FileWriter}
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Project (var Name: String){
  val layers = new ArrayBuffer[Layer]()
  val selections = new ArrayBuffer[Selection]()
  val sequences = new ArrayBuffer[Sequence]()
  val simpleOperations = new ArrayBuffer[SimpleOperation]()

  def fillSimpleOperations(): Unit ={
    simpleOperations.addOne(new SimpleOperation("Add"){f = addF})
    simpleOperations.addOne(new SimpleOperation("Sub"){f = subF})
    simpleOperations.addOne(new SimpleOperation("SubRev"){f = subRevF})
    simpleOperations.addOne(new SimpleOperation("Mul"){f = mulF})
    simpleOperations.addOne(new SimpleOperation("Div"){f = divF})
    simpleOperations.addOne(new SimpleOperation("DivRev"){f = divRevF})
    simpleOperations.addOne(new SimpleOperation("Pow"){f = powF})
    simpleOperations.addOne(new SimpleOperation("Log"){f = logF})
    simpleOperations.addOne(new SimpleOperation("Abs"){f = absF})
    simpleOperations.addOne(new SimpleOperation("Min"){f = minF})
    simpleOperations.addOne(new SimpleOperation("Max"){f = maxF})
    simpleOperations.addOne(new SimpleOperation("Invert"){f = invertF})
  }
  fillSimpleOperations()

  def getLayers()=layers
  def getSelections()=selections
  def addLayer(l: Layer): Unit ={
    layers.addOne(l)
  }

  def addSelection(s: Selection): Unit = {
    selections.addOne(s)
  }

  def addSequence(s: Sequence): Unit = {
    sequences.addOne(s)
  }

  def removeSelection(s: Selection): Unit={
    var i = 0
    selections.foreach(selection=>{
      if(selection==s) selections.remove(i)
      i+=1
    })
  }

  def noActiveSelections(): Boolean ={
    var res = true
    selections.foreach(selection =>{
      if(selection.active) res = false
    })
    res
  }

  def exportImage(name: String)={
    var maxW = 0
    var maxH = 0
    layers.foreach(layer => {
      if(layer.bufferedImage.getWidth()>maxW) maxW = layer.bufferedImage.getWidth()
      if(layer.bufferedImage.getHeight()>maxH) maxH = layer.bufferedImage.getHeight()
    })
    val blendedImage = new BufferedImage(maxW, maxH, BufferedImage.TYPE_INT_ARGB)
    val g: Graphics = blendedImage.getGraphics()
    layers.foreach(layer =>{
      if(layer.active) g.drawImage(layer.bufferedImage,0,0,null)
    })
    g.dispose()
    ImageIO.write(blendedImage,"PNG",new File(name+".png"))
  }

  def readLayer(line: String): Unit ={
    val info = line.split("&")
    val name = info(0)
    val imagePath = info(1)
    val opacity = info(2).toDouble
    val active = info(3).toBoolean
    val width = info(4).toInt
    val height = info(5).toInt
    val bufferedImage = new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB)
    val rgbs = info(6).split(",")
    var k = 0
    for(i<-Range(0,width);j<-Range(0,height)){
      val rgb = rgbs(k).toInt
      bufferedImage.setRGB(i,j,rgb)
      k+=1
    }
    val newLayer = new Layer(name,imagePath)
    newLayer.setActive(active)
    newLayer.setOpacity(opacity)
    newLayer.bufferedImage = bufferedImage
    layers.addOne(newLayer)
  }

  def readFromFile(fileName:String): Unit ={
    println("Usao u readFromFile")
    val lines = Source.fromFile(fileName).getLines()
    Name = lines.next()
    lines.next()
    val numLayers = lines.next().toInt
    for (i<-Range(0,numLayers)) {
      val layer = lines.next()
      readLayer(layer)
    }
  }

  def printToFile(fileName:String):Unit={
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(this.Name+"\n")
    bw.write("Layers\n")
    bw.write(layers.length+"\n")
    layers.foreach(layer=>{
      bw.write(layer.name+ "&")
      bw.write(layer.imagePath+"&")
      bw.write(layer.opacity+"&")
      bw.write(layer.active+"&")
      bw.write(layer.bufferedImage.getWidth()+"&")
      bw.write(layer.bufferedImage.getHeight()+"&")
      for(i<-Range(0,layer.bufferedImage.getWidth());j<-Range(0,layer.bufferedImage.getHeight())){
        bw.write(layer.bufferedImage.getRGB(i,j)+",")
      }
      bw.write("\n")
    })
    bw.write("Selections\n")
    bw.write("Seqences\n")
    bw.write("Simple operations\n")
    bw.close()
  }
}
