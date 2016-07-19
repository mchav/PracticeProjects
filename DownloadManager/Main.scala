import sys.process._
import java.net.URL
import java.net.URLEncoder
import java.io.File
import javax.sound.sampled._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.event._
import scalafx.collections._
import scalafx.event.ActionEvent
import scalafx.event.EventHandler
import scalafx.scene.control._
import scalafx.scene.text._
import scalafx.scene.input.MouseEvent
import scalafx.geometry._
import scalafx.scene.layout._

object Main extends JFXApp {
  private val charset = java.nio.charset.StandardCharsets.UTF_8.name()

  val SAMPLE_RATE = 8000f;

  def tone(hz: Int, msecs: Int, vol: Double = 0.5) = {
    val buf = Array[Byte](1)
    println(vol)
    val af = 
        new AudioFormat(
            SAMPLE_RATE, // sampleRate
            8,           // sampleSizeInBits
            1,           // channels
            true,        // signed
            false)       // bigEndian
    val sdl = AudioSystem.getSourceDataLine(af)
    sdl.open(af)
    sdl.start()
    (0 to ((msecs * 8) - 1) ).map { i =>
      val angle = i / (SAMPLE_RATE / hz) * 2.0 * Math.PI
      buf(0) = (Math.sin(angle) * 127.0 * vol).asInstanceOf[Byte]
      sdl.write(buf,0,1)
    }
    sdl.drain()
    sdl.stop()
    sdl.close()
  } 

  val txtMain = new TextField() {
    text = ""
    prefHeight = 25
    prefWidth = 427
    layoutX = 24
    layoutY = 51
    hgrow = Priority.Always
    vgrow = Priority.Never
  }
  val downloadLog = new ObservableBuffer[String]()
  val listView = new ListView[String]() {
    prefHeight = 200
    prefWidth = 527
    items = downloadLog
  }
  val scrollPane = new ScrollPane() {
    layoutX = 24
    layoutY = 118
    prefHeight = 200
    prefWidth = 544
    content = listView
  }

  val btnDownload = new Button() {
    text = "Download"
    layoutX = 471
    layoutY = 51
    filterEvent(MouseEvent.Any) {
        (me: MouseEvent) =>
          me.eventType match {
              case MouseEvent.MousePressed => {
                download(txtMain.getText.toString)
                txtMain.setText("")
              }
              case _ => print("")
          }
    }
  }
  
  val fileMenu = new Menu() {
    text = "File"
    items = Seq(new MenuItem("Close"))
  }

  val helpMenu = new Menu() {
    text = "Help"
    items = Seq(new MenuItem("Contents"), new MenuItem("About"))
  }

  val menuBar = new MenuBar() {
    menus = Seq(fileMenu, helpMenu)
  }

  val anchorPane = new AnchorPane() {
    prefHeight = 180
    prefWidth  = 200
    children   = Seq(menuBar, btnDownload, txtMain, scrollPane)
  }
  val pane = new TitledPane() {
    prefHeight = 400
    prefWidth = 600
    content = anchorPane
  }

  stage = new PrimaryStage() {
    title = "Thatha"
    scene = new Scene(pane)
    resizable = false
  }

  def download(fileLink: String): Unit = {
    val fileName = fileLink.drop((fileLink lastIndexOf "/") + 1)
    val fileLocation = new File(System.getProperty("user.home") + "/Downloads/" + fileName)
    val download = new Thread(new Runnable{
      override def run = {
        val fileUrl  = new URL(fileLink)
        downloadLog += s"Downloading ${fileName} to ${fileLocation.toString}"
        val start = System.currentTimeMillis()
        fileUrl #> fileLocation !! ProcessLogger(fileName => println(s"Finished downloading ${fileName}"))
        val end = System.currentTimeMillis()
        val timeTaken = (end - start) / 1000.0
        downloadLog += s"Finished downloading ${fileName} @ ${(fileLocation.length / 1024.0) / 1024.0} MB in ${timeTaken} seconds"
      }
    })
    download.start
  }
}