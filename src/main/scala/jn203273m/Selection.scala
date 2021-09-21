package jn203273m

import java.awt.{Color, Graphics2D, Rectangle}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Selection(val name: String) {
  val rectangles = new ArrayBuffer[Rectangle]()
  var active = true
  var color: Int = 0
  var changedLayers: mutable.LinkedHashMap[Layer, mutable.LinkedHashMap[(Int, Int), Int]] = new mutable.LinkedHashMap[Layer, mutable.LinkedHashMap[(Int, Int), Int]]()
  def addRectangle(r: Rectangle): Unit = rectangles.addOne(r)

  def setColor(newC: Int):Unit = {
    println("Changed color to "+color)
    color = newC
  }

  def paintSelection(g: Graphics2D) = {
    rectangles.foreach(rectangle=>{
      g.setColor(new Color((color&0x00ffffff)+(50<<24),true))
      g.fill(rectangle)
    })
  }

  def setActive(b: Boolean) = active = b

  def isInSelection(x: Int, y: Int): Boolean = {
    var isThere = false
    rectangles.foreach(rectangle=>{
      if(x>=rectangle.getX && x<=rectangle.getX+rectangle.getWidth && y>=rectangle.getY && y<=rectangle.getY+rectangle.getHeight) isThere = true
    })
    isThere
  }

  def restoreOldRGB()={
    changedLayers.foreach(kv => {
      val layer = kv._1
      val values = kv._2
      values.foreach(oldValue=>{
        val a = layer.bufferedImage.getRGB(oldValue._1._1,oldValue._1._2)&0xff000000
        layer.bufferedImage.setRGB(oldValue._1._1,oldValue._1._2,(oldValue._2&0x00ffffff)+a)
      })
    })
  }

  def fillLayersWithColor(layers: ArrayBuffer[Layer])={
    val newR = color&0x00ff0000
    val newG = color&0x0000ff00
    val newB = color&0x000000ff
    layers.foreach((layer)=>{
      if(layer.active){
        var newMap = new mutable.LinkedHashMap[(Int,Int),Int]
        for(i <- Range(0,layer.bufferedImage.getWidth()); j<- Range(0,layer.bufferedImage.getHeight())){
          if(isInSelection(i,j)){
            val oldRgb = layer.bufferedImage.getRGB(i,j)
            newMap+=((i,j)->oldRgb)
            val a = oldRgb&0xff000000
          layer.bufferedImage.setRGB(i,j,a+newR+newG+newB)
          }
        }
        changedLayers+=(layer->newMap)
      }
    })
  }
}
