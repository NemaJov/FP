package jn203273m

import jn203273m.UI.{contents, preferredSize, project, title}

import java.awt.{Color, Dimension, _}
import java.awt.event._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.border.{Border, EmptyBorder}
import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, CheckBox, Color, Dialog, Graphics2D, Label, MainFrame, Orientation, Panel, Point, TextField}

class ImagePanel extends Panel
{
  var combined: BufferedImage = null
  var editingSelection: Selection = null
  var activeRectangle: Rectangle = null
  def setEditingSelection(selection: Selection): Unit ={
    editingSelection = selection
  }

  def refresh()={
    println("Refreshing")
    combined = new BufferedImage(2000,2000,BufferedImage.TYPE_INT_ARGB)
    val g = combined.createGraphics()
    project.getLayers().foreach(layer=>{
      if(layer.active) g.drawImage(layer.bufferedImage,0,0,null)
    })
    project.getSelections().foreach(selection=>{
      if(selection.active) selection.paintSelection(g)
    })
    if(activeRectangle!=null) {
      println("I should paint rectangles")
      g.setColor(new Color((editingSelection.color&0x00ffffff)+(50<<24),true))
      g.fill(activeRectangle)
    }
    if(editingSelection!=null) {
      editingSelection.paintSelection(g)
    }
    g.dispose()
    validate()
    revalidate()
    repaint()
  }


  override def paintComponent(g:Graphics2D) =
  {
    super.paintComponent(g)
    if (null != combined) g.drawImage(combined, 0, 0, null)
  }
}

object UI extends MainFrame {
  title = "FP projekat"
  preferredSize = new Dimension(800, 600)
  val imagePanel = new ImagePanel()
  this.listenTo(imagePanel.mouse.clicks)
  this.listenTo(imagePanel.mouse.moves)
  contents = new BoxPanel(Orientation.Vertical){
    contents += imagePanel
    contents += new BoxPanel(Orientation.Horizontal){
      contents+=Button("File")(openFileDialog())
      contents+=Button("Layers")(openLayersDialog())
      contents+=Button("Operations")(openOperationsDialog())
      contents += Button("Selections")(openSelectionsDialog())
    }
    border = Swing.EmptyBorder(30, 30, 50, 30)
  }
  reactions+= {

    case scala.swing.event.MousePressed(imagePanel: ImagePanel,point,modifiers,clicks,triggersPopup) =>{
      println("Mouse pressed at "+point.toString)
      if(imagePanel.editingSelection!=null){
        imagePanel.activeRectangle = new Rectangle(point)
        imagePanel.refresh()
      }
    }
    case scala.swing.event.MouseReleased(imagePanel: ImagePanel,point,modifiers,clicks,triggersPopup) =>{
      println("Mouse released at "+point.toString)
      if(imagePanel.editingSelection!=null){
        imagePanel.activeRectangle.setSize(new Dimension(point.x-imagePanel.activeRectangle.x,point.y-imagePanel.activeRectangle.y))
        imagePanel.editingSelection.rectangles.addOne(imagePanel.activeRectangle)
        imagePanel.activeRectangle = null
      }
    }
    case scala.swing.event.MouseDragged(imagePanel: ImagePanel,point,modifiers) =>{
      //println("Mouse moved at "+point.toString)
      if(imagePanel.editingSelection!=null && imagePanel.activeRectangle!=null){
        imagePanel.activeRectangle.setSize(new Dimension(point.x-imagePanel.activeRectangle.x,point.y-imagePanel.activeRectangle.y))
        imagePanel.refresh()
      }
    }
  }

  var project = new Project("Prvi")

  def openFileDialog()={
    val dialog = new Dialog(UI.super.owner)
    refreshFileDialog(dialog)
  }

  def refreshFileDialog(dialog: Dialog): Unit ={
    dialog.title = "File"
    dialog.location = new Point(600,300)
    dialog.minimumSize = new Dimension(600,500)
    dialog.contents = new BoxPanel(Orientation.Vertical) {
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("File name: ")
        val imageName = new TextField(){
          maximumSize = new Dimension(300,30)
        }
        contents+=imageName
        contents+=Button("Export to file"){
          project.exportImage(imageName.text)
        }
      }
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Project name: ")
        val newProjectName = new TextField(){
          maximumSize = new Dimension(300,30)
        }
        contents+=newProjectName
        contents+=Button("New project"){
          project = new Project(newProjectName.text)
          imagePanel.refresh()
        }
      }
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Project name: ")
        val saveProjectName = new TextField(){
          maximumSize = new Dimension(300,30)
        }
        contents+=saveProjectName
        contents+=Button("Save project"){
          project.printToFile(saveProjectName.text)
          imagePanel.refresh()
        }
      }
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Project name: ")
        val loadProjectName = new TextField(){
          maximumSize = new Dimension(300,30)
        }
        contents+=loadProjectName
        contents+=Button("Load project"){
          project = new Project("placeholder")
          project.readFromFile(loadProjectName.text)
          imagePanel.refresh()
        }
      }
    }
    dialog.visible = true
  }

  def openLayersDialog() = {
    val dialog = new Dialog(UI.super.owner)
    refreshLayersDialog(dialog)
  }

  def refreshLayersDialog(dialog: Dialog): Unit = {
    dialog.title = "Layers"
    dialog.location = new Point(600,300)
    dialog.minimumSize = new Dimension(600,500)
    dialog.contents = new BoxPanel(Orientation.Vertical) {
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Name")
        contents+=new Label("Active")
        contents+=new Label("Opacity")
      }
      project.getLayers().foreach((layer)=>{
        contents+=new BoxPanel(Orientation.Horizontal){
          contents+=new Label(layer.name)
          val checkBox=new CheckBox(){
            selected = layer.active
            name = layer.name
            //layer.setActive(this.selected)
          }
          contents+=checkBox
          dialog.listenTo(checkBox)
          val textField = new TextField(){
            text = layer.opacity.toString
            name = layer.name
            maximumSize = new Dimension(300,30)
          }
          contents+=textField
          dialog.listenTo(textField)
        }
      })
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Name")
        val newLayerName = new TextField()
        newLayerName.maximumSize = new Dimension(300,30)
        contents+= newLayerName
        contents+=new Label("Image path")
        val newLayerImagePath = new TextField()
        newLayerImagePath.maximumSize = new Dimension(300,30)
        contents+= newLayerImagePath
        contents+= Button("New layer"){
          val newLayer = new Layer(newLayerName.text,newLayerImagePath.text)
          project.addLayer(newLayer)
          refreshLayersDialog(dialog)
          if(newLayer.bufferedImage.getWidth()>UI.size.getWidth) {
            UI.size = new Dimension(newLayer.bufferedImage.getWidth(),UI.size.height)
            UI.validate()
            UI.repaint()
          }
          if(newLayer.bufferedImage.getHeight()>UI.size.getHeight-150) {
            UI.size = new Dimension(UI.size.width,newLayer.bufferedImage.getHeight()+150)
            UI.validate()
            UI.repaint()
          }
          imagePanel.refresh()
        }
      }
    }
    dialog.reactions+={
      case scala.swing.event.EditDone(textField: TextField) => {
        println("Edit is done")
        textField.text = Math.min(1.0,Math.max(textField.text.toDouble,0)).toString
        project.getLayers().foreach(layer => if(textField.name == layer.name) layer.setOpacity(textField.text.toDouble))
        imagePanel.refresh()
      }
      case scala.swing.event.ButtonClicked(checkBox: CheckBox) => {
        println("Checkbox edit is done")
        project.getLayers().foreach(layer => if(checkBox.name == layer.name) layer.setActive(checkBox.selected))
        imagePanel.refresh()
      }
    }
    dialog.visible = true
  }

  def openOperationsDialog(): Unit ={
    val dialog = new Dialog(UI.super.owner)
    refreshOperationsDialog(dialog)
  }

  def refreshOperationsDialog(dialog: Dialog): Unit ={
    dialog.title = "Operations"
    dialog.location = new Point(900,300)
    dialog.minimumSize = new Dimension(600,500)
    val allOperations = new ArrayBuffer[String]()
    project.simpleOperations.foreach(operation=>{
      allOperations.addOne(operation.name)
    })
    allOperations.addOne("Pondered")
    allOperations.addOne("Grayscale")
    allOperations.addOne("Median")
    val combo = new ComboBox[String](allOperations){
      maximumSize = new Dimension(100,30)
    }

    val operationTextField = new TextField()
    operationTextField.maximumSize = new Dimension(200,30)
    dialog.contents= new BoxPanel(Orientation.Vertical){
      contents+= new BoxPanel(Orientation.Horizontal){
        border = Swing.EmptyBorder(30, 30, 50, 30)
        contents+=combo
        dialog.listenTo(combo.selection)
        contents+=new Label("Parameter(s): ")
        contents+=operationTextField
        dialog.listenTo(operationTextField)
        contents+=Button("Apply operation"){
          var k: Double = 0
          if(combo.selection.item!="Pondered" && operationTextField.text!="") k = operationTextField.text.toDouble
            if(combo.selection.item=="Grayscale") Operations.grayscale(new ArrayBuffer[Double]().addOne(k))
            if(combo.selection.item=="Median")  Operations.median(new ArrayBuffer[Double]().addOne(k))
            if(combo.selection.item=="Pondered")  {
              val values =  operationTextField.text.split(", ")
              val matrix = new ArrayBuffer[Double]()
              values.foreach(value=>{
                matrix.addOne(value.toDouble)
              })
              Operations.pondered(matrix)
            }
          project.simpleOperations.foreach(operation=>{
            if(combo.selection.item==operation.name){
              Operations.applyToLayer(operation.f)(UI.project.getLayers(),new ArrayBuffer[Double]().addOne(k))
            }
          })
          UI.imagePanel.refresh()
        }
      }
      contents+=new BoxPanel(Orientation.Vertical){
        contents+=new BoxPanel(Orientation.Horizontal){
          contents+=new Label("Name: ")
          val newSequenceName = new TextField()
          newSequenceName.maximumSize = new Dimension(100,30)
          contents+=newSequenceName
          contents+= Button("New sequence"){
            val newSequence = new Sequence(newSequenceName.text)
            project.addSequence(newSequence)
            refreshOperationsDialog(dialog)
          }
        }
        contents+=new BoxPanel(Orientation.Horizontal)
        {
          val allSequences = new ArrayBuffer[String]()
          project.sequences.foreach(sequence=>{
            allSequences.addOne(sequence.name)
          })
          val sequenceName = new ComboBox[String](allSequences){
          maximumSize = new Dimension(100,30)
          }
          contents+=sequenceName
          contents+=Button("Add operation"){
            var k: Double = 0
            project.sequences.foreach(sequence=>{
              if(sequence.name==sequenceName.selection.item) {
                if(combo.selection.item!="Pondered" && operationTextField.text!="") k = operationTextField.text.toDouble
                if(combo.selection.item=="Grayscale") sequence.addOperation(Operations.grayscale,new ArrayBuffer[Double]().addOne(k))
                if(combo.selection.item=="Median")  sequence.addOperation(Operations.median,new ArrayBuffer[Double]().addOne(k))
                if(combo.selection.item=="Pondered")  {
                  val values =  operationTextField.text.split(", ")
                  val matrix = new ArrayBuffer[Double]()
                  values.foreach(value=>{
                    matrix.addOne(value.toDouble)
                  })
                  sequence.addOperation(Operations.pondered,matrix)
                }
                project.simpleOperations.foreach(operation=>{
                  if(combo.selection.item==operation.name){
                    sequence.addOperation(Operations.applyToLayer(operation.f)(UI.project.getLayers(),_),new ArrayBuffer[Double]().addOne(k))
                  }
                })
              }
            })
          }
          contents+= Button("Execute"){
            project.sequences.foreach(sequence=>{
              if(sequence.name==sequenceName.selection.item) {
                println("Executing")
                sequence.execute()
                imagePanel.refresh()
              }
            })
          }
        }

      }
      contents+=new BoxPanel(Orientation.Horizontal){
        this.border = new EmptyBorder(50,50,50,50)
        val simpleOperationsLabels = new ArrayBuffer[String]()
        project.simpleOperations.foreach(operation => {simpleOperationsLabels.addOne(operation.name)})
        contents+=new Label("F1: ")
        val comboF1 =new ComboBox[String](simpleOperationsLabels){
          name = "F1"
          maximumSize = new Dimension(100,30)
        }
        contents+=comboF1
        contents+=new Label("F2: ")
        val comboF2 =new ComboBox[String](simpleOperationsLabels){
          name = "F2"
          maximumSize = new Dimension(100,30)
        }
        contents+=comboF2
        contents+=new Label("F2 parameter: ")
        val f2param=new TextField(){
          name="F2Param"
          maximumSize = new Dimension(100,30)
        }
        contents+=f2param
        contents+=new Label("Name: ")
        val fname=new TextField(){
          name = "FName"
          maximumSize = new Dimension(100,30)
        }
        contents+=fname
        contents+=Button("Create new function"){
          var f1:SimpleOperation = null
          var f2:SimpleOperation = null
          project.simpleOperations.foreach(operation => {
            if(operation.name==comboF1.selection.item) f1 = operation
            if(operation.name==comboF2.selection.item) f2 = operation
          })
          SimpleOperation.createSimpleOperation(f1.f,f2.f,fname.text,f2param.text.toDouble)
          refreshOperationsDialog(dialog)
        }
      }
    }
    dialog.visible = true
  }

  def openSelectionsDialog() = {
    val dialog = new Dialog(UI.super.owner)
    refreshSelectionsDialog(dialog)
  }

  def refreshSelectionsDialog(dialog: Dialog): Unit={
    dialog.title = "Selections"
    dialog.location = new Point(600,300)
    dialog.minimumSize = new Dimension(600,500)
    dialog.contents = new BoxPanel(Orientation.Vertical) {
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Name"){
          minimumSize = (new Dimension(100,30))
        }
        contents+=new Label("Active") {
          minimumSize = (new Dimension(100,30))
        }
        contents+=new Label("Color") {
          minimumSize = (new Dimension(100,30))
        }
        contents+=new Label("") {
          minimumSize = (new Dimension(100,30))
        }
      }
      project.getSelections().foreach((selection)=>{
        contents+=new BoxPanel(Orientation.Horizontal){
          contents+=new Label(selection.name){
            minimumSize = (new Dimension(200,30))
          }
          val checkBox=new CheckBox(){
            minimumSize = (new Dimension(100,30))
            selected = selection.active
            name = selection.name
            //layer.setActive(this.selected)
          }
          contents+=checkBox
          dialog.listenTo(checkBox)
          val textFieldR = new TextField(){
            text = ((selection.color&0x00ff0000)>>16).toString
            name = selection.name + "R"
            maximumSize = new Dimension(80,30)
          }
          contents+=textFieldR
          dialog.listenTo(textFieldR)
          val textFieldG = new TextField(){
            text = ((selection.color&0x0000ff00)>>8).toString
            name = selection.name + "G"
            maximumSize = new Dimension(80,30)
          }
          contents+=textFieldG
          dialog.listenTo(textFieldG)
          val textFieldB = new TextField(){
            text = ((selection.color&0x000000ff)).toString
            name = selection.name + "B"
            maximumSize = new Dimension(80,30)
          }
          contents+=textFieldB
          dialog.listenTo(textFieldB)
          val colorButton = Button("Fill color"){
            println("Usao u fill color")

            selection.fillLayersWithColor(project.getLayers())
            UI.imagePanel.refresh()
          }
          contents+=colorButton
          val editButton = new Button(if(UI.imagePanel.editingSelection == selection) "Finish editing" else "Edit selection"){
            reactions += {
              case ButtonClicked(_) => {
                println("Usao u edit selection")
                if (UI.imagePanel.editingSelection == null) {
                  UI.imagePanel.setEditingSelection(selection)
                  this.text = "Finish editing"
                }
                else {
                  UI.imagePanel.setEditingSelection(null)
                  this.text = "Edit selection"
                }
              }
            }
          }
          contents+=editButton
          contents+=Button("Delete"){
            selection.restoreOldRGB()
            if(UI.imagePanel.editingSelection==selection) UI.imagePanel.editingSelection = null
            project.removeSelection(selection)
            refreshSelectionsDialog(dialog)
            UI.imagePanel.refresh()
          }
        }
      })
      contents+=new BoxPanel(Orientation.Horizontal){
        contents+=new Label("Name")
        val newSelectionName = new TextField()
        newSelectionName.maximumSize = new Dimension(100,30)
        contents+= newSelectionName
        contents+= Button("New selection"){
          val newSelection = new Selection(newSelectionName.text)
          project.addSelection(newSelection)
          refreshSelectionsDialog(dialog)
        }
      }
    }
    dialog.reactions+={
      case scala.swing.event.ButtonClicked(checkBox: CheckBox) => {
        println("Checkbox edit is done")
        project.getSelections().foreach(selection => if(selection.name == checkBox.name) selection.setActive(checkBox.selected))
        imagePanel.refresh()
      }
      case scala.swing.event.EditDone(textField: TextField) => {
        textField.text = Math.min(255,Math.max(textField.text.toInt,0)).toString
        project.getSelections().foreach(selection => {
          if(selection.name + "R" == textField.name) selection.setColor((selection.color&0xff00ffff) + (textField.text.toInt<<16))
          if(selection.name + "G" == textField.name) selection.setColor((selection.color&0xffff00ff) + (textField.text.toInt<<8))
          if(selection.name + "B" == textField.name) selection.setColor((selection.color&0xffffff00) + (textField.text.toInt<<0))
        })
        imagePanel.refresh()
      }
    }
    dialog.visible = true
  }
}
