// DO NOT MODIFY FOR BASIC SUBMISSION
// scalastyle:off

package tetris.game

import java.awt.event
import java.awt.event.KeyEvent._

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import processing.core.{PApplet, PConstants, PImage}
import processing.event.KeyEvent
import tetris.logic._
import tetris.game.TetrisGame._

class TetrisGame extends GameBase {

  var gameLogic = TetrisLogic()
  val updateTimer = new UpdateTimer(FramesPerSecond)

  val widthInPixels: Int = WidthCellInPixels * gameLogic.nrColumns
  val heightInPixels: Int = HeightCellInPixels * gameLogic.nrRows

  val totalWidth : Int = 20
  val totalHeight : Int = 26

  val screenResX: Int = WidthCellInPixels * totalWidth
  val screenResY: Int = HeightCellInPixels * totalHeight

  val screenArea: Rectangle = Rectangle(Point(WidthCellInPixels * 6, HeightCellInPixels), widthInPixels, heightInPixels)
  val backgroundArea: Rectangle = Rectangle(Point(0,0), screenResX, screenResY)
  val holdBlockArea: Rectangle = Rectangle(Point(WidthCellInPixels, HeightCellInPixels), WidthCellInPixels * 4, HeightCellInPixels * 4)
  var nextBlockArea: Rectangle = Rectangle(Point(WidthCellInPixels, HeightCellInPixels * 6), WidthCellInPixels * 4, HeightCellInPixels * 19)

  var tetrisBlockImages : Array[PImage] = _

  override def draw(): Unit = {
    updateState()
    // Set the strokeWeight
    strokeWeight(5)
    drawNextBlocksArea()
    drawNextBlocks()
    drawOutlines()
    drawHoldBlockArea()
    drawHoldedBlock()
    drawGrid()
    if (gameLogic.isGameOver) drawGameOverScreen()
  }

  def getCell(rectangle: Rectangle, colIndex: Int, rowIndex: Int, widthPerCell: Float, heightPerCell: Float): Rectangle = {
    val leftUp = Point(rectangle.left + colIndex * widthPerCell,
      rectangle.top + rowIndex * heightPerCell)
    Rectangle(leftUp, widthPerCell, heightPerCell)
  }
  def drawCell(area: Rectangle, tetrisImage: PImage): Unit = {
    drawImage(area,tetrisImage)
  }

  def drawNextBlocksArea(): Unit = {
    val widthPerCell = nextBlockArea.width / 4
    val heightPerCell = nextBlockArea.height / 19

    for(i <- 0 until 4; j <- 0 until 19)
      drawCell(getCell(nextBlockArea,i, j, widthPerCell, heightPerCell), getBlockImage(Empty))
  }

  def drawNextBlocks(): Unit = {
    var blockArea : Rectangle = Rectangle(Point(WidthCellInPixels, HeightCellInPixels), WidthCellInPixels * 4, HeightCellInPixels * 4)
    var widthPerCell = blockArea.width / 4
    var heightPerCell = blockArea.height / 4
    for(i <- gameLogic.nextBlocksToSpawn.indices) {
      blockArea = Rectangle(Point((nextBlockArea.width / 2) - (WidthCellInPixels / 2) * (gameLogic.createBlock(gameLogic.indexToTetrisBlock(gameLogic.nextBlocksToSpawn(i))).length - 2),
        HeightCellInPixels * 7 + HeightCellInPixels * 4 * i + HeightCellInPixels - HeightCellInPixels * (gameLogic.createBlock(gameLogic.indexToTetrisBlock(gameLogic.nextBlocksToSpawn(i))).length / 4.0).floor.toInt),
        WidthCellInPixels * 4, HeightCellInPixels * 4)
      widthPerCell = blockArea.width / 4
      heightPerCell = blockArea.height / 4

      for (y <- gameLogic.createBlock(gameLogic.indexToTetrisBlock(gameLogic.nextBlocksToSpawn(i))).indices; x <- gameLogic.createBlock(gameLogic.indexToTetrisBlock(gameLogic.nextBlocksToSpawn(i))).indices if (gameLogic.createBlock(gameLogic.indexToTetrisBlock(gameLogic.nextBlocksToSpawn(i)))(y)(x).block != Empty)) {
        drawCell(getCell(blockArea,x, y, widthPerCell, heightPerCell), getBlockImage(gameLogic.createBlock(gameLogic.indexToTetrisBlock(gameLogic.nextBlocksToSpawn(i)))(y)(x).block))
      }
    }
  }

  def setBackgroundImage(): Unit = {
    val widthPerCell = backgroundArea.width
    val heightPerCell = backgroundArea.height
    //drawCell(getCell(backgroundArea, 0, 0, widthPerCell, heightPerCell), Color.Blueish)
    var img : PImage = loadImage("Background1.png");
    img.resize(backgroundArea.width.toInt, backgroundArea.height.toInt)
    background(img);
  }

  def drawOutlines(): Unit = {
    //strokeCap(0)
    val leftLimit : Int = WidthCellInPixels * 6
    val rightLimit : Int = WidthCellInPixels * 6  + widthInPixels
    val upLimit : Int = HeightCellInPixels
    val downLimit : Int = HeightCellInPixels + heightInPixels
    strokeWeight(10)
    line(leftLimit, upLimit, rightLimit,upLimit)
    line(leftLimit, upLimit, leftLimit , downLimit)
    line(leftLimit, downLimit, rightLimit, downLimit)
    line(rightLimit, downLimit, rightLimit, upLimit)
    strokeWeight(5)
  }

  def drawHoldBlockArea(): Unit = {
    val widthPerCell = holdBlockArea.width / 4
    val heightPerCell = holdBlockArea.height / 4
    for (y <- 0 until  4; x <- 0 until 4) {
      drawCell(getCell(holdBlockArea,x, y,widthPerCell,heightPerCell), getBlockImage(Empty))
    }
  }

  def drawGrid(): Unit = {
    val widthPerCell = screenArea.width / gameLogic.nrColumns
    val heightPerCell = screenArea.height / gameLogic.nrRows

    for (y <- 0 until gameLogic.nrRows; x <- 0 until gameLogic.nrColumns) {
      if (gameLogic.getBlockAt(x,y) == GBlock){
        stroke(  0, 255,   0)
        drawCell(getCell(screenArea,x, y,widthPerCell,heightPerCell), getBlockImage(gameLogic.getBlockAt(x, y)))
        stroke( 0, 0,   0)
      } else if (gameLogic.getBlockAt(x,y) != Empty){
        drawCell(getCell(screenArea,x, y,widthPerCell,heightPerCell), getBlockImage(gameLogic.getBlockAt(x, y)))
      }else{
        strokeWeight(3)
        drawCell(getCell(screenArea,x, y,widthPerCell,heightPerCell), getBlockImage(gameLogic.getBlockAt(x, y)))
        strokeWeight(5)
      }
    }
  }

  def drawHoldedBlock(): Unit = {
    var blockArea : Rectangle = Rectangle(Point(WidthCellInPixels, HeightCellInPixels), WidthCellInPixels * 4, HeightCellInPixels * 4)
    var widthPerCell = blockArea.width / 4
    var heightPerCell = blockArea.height / 4

    if( gameLogic.heldBlock != null) {
      blockArea = Rectangle(Point((holdBlockArea.width / 2) - (WidthCellInPixels / 2) * (gameLogic.heldBlock.length - 2) , HeightCellInPixels +  HeightCellInPixels - HeightCellInPixels * (gameLogic.heldBlock.length / 4.0).floor.toInt),
        WidthCellInPixels * 4, HeightCellInPixels * 4)
      widthPerCell = blockArea.width / 4
      heightPerCell = blockArea.height / 4

      for (y <- gameLogic.heldBlock.indices; x <- gameLogic.heldBlock(0).indices if gameLogic.heldBlock(y)(x).block != Empty) {
        drawCell(getCell(blockArea,x, y,widthPerCell,heightPerCell), getBlockImage(gameLogic.heldBlock(y)(x).block))
      }
    }else{
      for (y <- 0 until  4; x <- 0 until 4) {
        drawCell(getCell(blockArea,x, y,widthPerCell,heightPerCell), getBlockImage(Empty))
      }
    }
  }

  def drawGameOverScreen(): Unit = {
    setFillColor(Color.Red)
    drawTextCentered("GAME OVER!", 40, screenArea.center)
  }
  /** Method that calls handlers for different key press events.
    * You may add extra functionality for other keys here.
    * See [[event.KeyEvent]] for all defined keycodes.
    *
    * @param event The key press event to handle
    */
  override def keyPressed(event: KeyEvent): Unit = {

    event.getKeyCode match {
      case VK_A     => gameLogic.rotateLeft()
      case VK_S     => gameLogic.rotateRight()
      case VK_UP    => gameLogic.rotateRight()
      case VK_DOWN  => gameLogic.moveDown()
      case VK_LEFT  => gameLogic.moveLeft()
      case VK_RIGHT => gameLogic.moveRight()
      case VK_SPACE => gameLogic.doHardDrop()
      case VK_C     => gameLogic.holdBlock()
      case _        => ()
    }
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(screenResX, screenResY, PConstants.P2D)
  }

  override def setup(): Unit = {
    // Fonts are loaded lazily, so when we call text()
    // for the first time, there is significant lag.
    // This prevents it from happening during gameplay.
    text("", 0, 0)

    // This should be called last, since the game
    // clock is officially ticking at this point
    updateTimer.init()

    tetrisBlockImages =  Array(tetrisBlockToImage(IBlock),tetrisBlockToImage(OBlock),tetrisBlockToImage(LBlock),tetrisBlockToImage(JBlock),tetrisBlockToImage(SBlock),tetrisBlockToImage(Empty),
      tetrisBlockToImage(TBlock),tetrisBlockToImage(ZBlock),tetrisBlockToImage(GBlock))
    setBackgroundImage()
  }

  def updateState(): Unit = {
    if (updateTimer.timeForNextFrame()) {
      gameLogic.moveDown()
      updateTimer.advanceFrame()
    }
  }

  def tetrisBlockToImage(block: TetrisBlock): PImage =
    block match {
      case IBlock => loadImage("LightBlueBlock.png")
      case OBlock => loadImage("YellowBlock.png")
      case LBlock => loadImage("OrangeBlock.png")
      case JBlock => loadImage("BlueBlock.png")
      case SBlock => loadImage("GreenBlock.png")
      case Empty  => loadImage("EmptyBlock.png")
      case TBlock => loadImage("PurpleBlock.png")
      case ZBlock => loadImage("RedBlock.png")
      case GBlock => loadImage("GhostBlock.png")
    }

  def getBlockImage(block: TetrisBlock): PImage =
    block match {
      case IBlock => tetrisBlockImages(0)
      case OBlock => tetrisBlockImages(1)
      case LBlock => tetrisBlockImages(2)
      case JBlock => tetrisBlockImages(3)
      case SBlock => tetrisBlockImages(4)
      case Empty  => tetrisBlockImages(5)
      case TBlock => tetrisBlockImages(6)
      case ZBlock => tetrisBlockImages(7)
      case GBlock => tetrisBlockImages(8)
    }
}

object TetrisGame {

  val FramesPerSecond: Int = 5
  val WidthCellInPixels: Int = 50
  val HeightCellInPixels: Int = WidthCellInPixels

  def main(args:Array[String]): Unit = {
    PApplet.main("tetris.game.TetrisGame")
  }

}