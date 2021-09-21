package jn203273m

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.swing.Color
class Layer(val name: String, val imagePath: String) {
  //Add check for nonexistent file
  var helperBufferedImage: BufferedImage = ImageIO.read(new File(imagePath))
  var bufferedImage = new BufferedImage(helperBufferedImage.getWidth,helperBufferedImage.getHeight(),BufferedImage.TYPE_INT_ARGB)
  bufferedImage.getGraphics.drawImage(helperBufferedImage,0,0,null)
  println("The type of my buffered image is "+bufferedImage.getType)
  var opacity: Double = 0.3
  var active: Boolean = true
  applyOpacity()
  def setActive(act: Boolean): Unit = active = act
  def setOpacity(o: Double): Unit = {
    opacity = o
    applyOpacity()
  }
  def applyOpacity()= {
    for (i <- Range(0, bufferedImage.getWidth()); j <- Range(0, bufferedImage.getHeight())) {
      var rgb: Int = bufferedImage.getRGB(i, j)
      //println(rgb)
      var c = new Color(rgb)
      var newOpacity:Int = (opacity*255).toInt
      rgb= rgb&0x00ffffff
      rgb = rgb+((newOpacity<<24)&0xff000000)
      //println(c.getAlpha)
      //println(((255-c.getRed)<<16)+((255-c.getGreen)<<8)+(255-c.getBlue))
      //println(10<<24+((255-c.getRed)<<16)+((255-c.getGreen)<<8)+(255-c.getBlue))
      bufferedImage.setRGB(i, j, rgb)
    }
  }
}
