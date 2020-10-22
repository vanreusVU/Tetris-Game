package tetris.logic

import java.io.FileInputStream

import engine.random.{RandomGenerator, ScalaRandomGen}
import sun.audio.{AudioPlayer, AudioStream}
import tetris.game._
import tetris.logic.TetrisLogic._

/** To implement Tetris, complete the ``TODOs`` below.
  *
  * If you need additional files,
  * please also put them in the ``tetris`` package.
  */
case class Block(var y: Int, var x: Int, var block: TetrisBlock) {}

class TetrisLogic(val randomGen: RandomGenerator,
                  val nrColumns: Int,
                  val nrRows: Int,
                  val initialBoard: Seq[Seq[TetrisBlock]]) {

  def this(random: RandomGenerator, nrColumns: Int, nrRows: Int) =
    this(random, nrColumns, nrRows, makeEmptyBoard(nrColumns, nrRows))

  def this() =
    this(new ScalaRandomGen(), DefaultWidth, DefaultHeight, makeEmptyBoard(DefaultWidth, DefaultHeight))

  var currentTetromino : Array[Array[Block]] = _
  var gameBoard : Array[Array[TetrisBlock]] = initialBoard.map(_.toArray).toArray
  var blockToSpawn : Array[Array[Block]] = _

  // BONUS: HOLD
  var heldBlock : Array[Array[Block]] = _
  var canHold : Boolean = true

  // BONUS: DISPLAY NEXT BLOCKS
  var nextBlocksToSpawn : Array[Int] = Array(randomGen.randomInt(7))

  // BONUS: GHOST BLOCK
  var ghostTetromino : Array[Array[Block]] = _

  // BONUS: WALL KICK
  var rotations : Array[Char] = Array('0','R','2','L')
  var rotation : Char = '0'
  var blockType : TetrisBlock = _
  var rotationsI : Array[Array[(Int,Int)]] = Array(
    Array((0,0),(-1,0),(-1,1),(0,-2),(-1,-2)),
    Array((0,0),( 1,0),(1,-1),(0, 2),( 1,-2)),
    Array((0,0),( 1,0),(1,-1),(0, 2),( 1,-2)),
    Array((0,0),(-1,0),(-1,1),(0,-2),(-1,-2)),
    Array((0,0),( 1,0),(1,-1),(0,-2),( 1,-2)),
    Array((0,0),(-1,0),(-1,1),(0, 2),(-1,-2)),
    Array((0,0),(-1,0),(-1,1),(0, 2),(-1,-2)),
    Array((0,0),( 1,0),(1,-1),(0,-2),( 1,-2)))

  var rotationsJLSTZ : Array[Array[(Int,Int)]] = Array(
    Array((0,0),(-2,0),( 1,0),(-2,-1),( 1, 2)),
    Array((0,0),( 2,0),(-1,0),( 2, 1),(-1,-2)),
    Array((0,0),(-1,0),( 2,0),(-1, 2),( 2,-1)),
    Array((0,0),( 1,0),(-2,0),( 1,-2),(-2, 1)),
    Array((0,0),( 2,0),(-1,0),( 2, 1),(-1,-2)),
    Array((0,0),(-2,0),( 1,0),(-2,-1),( 1, 2)),
    Array((0,0),( 1,0),(-2,0),( 1,-2),(-2, 1)),
    Array((0,0),(-1,0),( 2,0),(-1, 2),( 2,-1)))

  spawnNewTetromino()
  playSound("Theme.wav")

  def getTestLocs(blockRotLocs : Array[Array[(Int,Int)]], rot : Char) : Array[(Int,Int)] = {
    if(rotation == '0' && rot == 'R')  blockRotLocs(0)
    else if(rotation == 'R' && rot == '0')  blockRotLocs(1)
    else if(rotation == 'R' && rot == '2')  blockRotLocs(2)
    else if(rotation == '2' && rot == 'R')  blockRotLocs(3)
    else if(rotation == '2' && rot == 'L')  blockRotLocs(4)
    else if(rotation == 'L' && rot == '2')  blockRotLocs(5)
    else if(rotation == 'L' && rot == '0')  blockRotLocs(6)
    else blockRotLocs(7)
  }

  def playSound(name : String) : Unit = {
    val in = new FileInputStream(name)
    val audioStream = new AudioStream(in)
    AudioPlayer.player.start(audioStream)
  }

  def setGhostBlock() : Unit = {
    ghostTetromino = currentTetromino.map(_.map(_.copy()))
  }

  def setGhostBlockLocation() : Unit = {
    var newY : Int = 0
    for(row <- 1 until nrRows){ // From top to bot
      if(!isCollidingWall(ghostTetromino,0,row) && !isCollidingBlock(ghostTetromino,0,row)){
        newY += 1
      } else {
        for(y <- ghostTetromino.indices; x <- ghostTetromino(y).indices) { // Set The Location
          ghostTetromino(y)(x).y += newY
          ghostTetromino(y)(x).x = currentTetromino(y)(x).x
        }
        return
      }
    }
  }

  def indexToTetrisBlock(index : Int) : TetrisBlock = {
    val blockTypes : Array[TetrisBlock] = Array(IBlock,JBlock,LBlock,OBlock,SBlock,TBlock,ZBlock)
    blockTypes(index)
  }

  def initNextBlocks(): Unit = {
    for(i <- 0 until 4 - nextBlocksToSpawn.length){
      nextBlocksToSpawn = randomGen.randomInt(7) +: nextBlocksToSpawn
    }
  }

  def createBlock(tetrisBlock: TetrisBlock): Array[Array[Block]] = {
    tetrisBlock match {
      case IBlock =>
        return Array(Array( Block(0,0,Empty),  Block(0,1,Empty),  Block(0,2,Empty),  Block(0,3,Empty)),
                     Array( Block(1,0,IBlock), Block(1,1,IBlock), Block(1,2,IBlock), Block(1,3,IBlock)),
                     Array( Block(2,0,Empty),  Block(2,1,Empty),  Block(2,2,Empty),  Block(2,3,Empty)),
                     Array( Block(3,0,Empty),  Block(3,1,Empty),  Block(3,2,Empty),  Block(3,3,Empty)))
      case JBlock =>
        return Array(Array( Block(0,0,JBlock), Block(0,1,Empty),  Block(0,2,Empty)),
                     Array( Block(1,0,JBlock), Block(1,1,JBlock), Block(1,2,JBlock)),
                     Array( Block(2,0,Empty),  Block(2,1,Empty),  Block(2,2,Empty)))
      case LBlock =>
        return Array(Array( Block(0,0,Empty),  Block(0,1,Empty),  Block(0,2,LBlock)),
                     Array( Block(1,0,LBlock), Block(1,1,LBlock), Block(1,2,LBlock)),
                     Array( Block(2,0,Empty),  Block(2,1,Empty),  Block(2,2,Empty)))
      case OBlock =>
        return Array(Array( Block(0,0,OBlock), Block(0,1,OBlock)),
                     Array( Block(1,0,OBlock), Block(1,1,OBlock)))
      case SBlock =>
        return Array(Array( Block(0,0,Empty),  Block(0,1,SBlock), Block(0,2,SBlock)),
                     Array( Block(1,0,SBlock), Block(1,1,SBlock), Block(1,2,Empty)),
                     Array( Block(2,0,Empty),  Block(2,1,Empty),  Block(2,2,Empty)))
      case TBlock =>
        return Array(Array( Block(0,0,Empty),  Block(0,1,TBlock), Block(0,2,Empty)),
                     Array( Block(1,0,TBlock), Block(1,1,TBlock), Block(1,2,TBlock)),
                     Array( Block(2,0,Empty),  Block(2,1,Empty),  Block(2,2,Empty)))
      case ZBlock =>
        return Array(Array( Block(0,0,ZBlock), Block(0,1,ZBlock), Block(0,2,Empty)),
                     Array( Block(1,0,Empty),  Block(1,1,ZBlock), Block(1,2,ZBlock)),
                     Array( Block(2,0,Empty),  Block(2,1,Empty),  Block(2,2,Empty)))
    }
    null
  }

  def centerTetrominoInX(tetromino : Array[Array[Block]]) : Unit = {
    for(i <- tetromino.indices; j <- tetromino(i).indices){
      tetromino(i)(j).x = j + (nrColumns / 2.0).round.toInt - (tetromino.length / 2.0).round.toInt
    }
  }

  def isCollidingBlock(tetromino : Array[Array[Block]], directionX : Int, directionY : Int) : Boolean = {
    for(i <- tetromino.indices; j <- tetromino(i).indices){
      if(tetromino(i)(j).block != Empty && gameBoard(tetromino(i)(j).y + directionY)(tetromino(i)(j).x + directionX) != Empty) {
        return true
      }
    }
    false
  }

  def isCollidingBlockWhileTurning(tetromino : Array[Array[Block]], directionX : Int, directionY : Int) : Boolean = {
    for(i <- tetromino.indices; j <- tetromino(i).indices){
      if(tetromino(i)(j).y + directionY < 0 || tetromino(i)(j).y + directionY >= nrRows || tetromino(i)(j).x + directionX < 0 || tetromino(i)(j).x + directionX >= nrColumns)
        return true
      else if(tetromino(i)(j).block != Empty && gameBoard(tetromino(i)(j).y + directionY)(tetromino(i)(j).x + directionX) != Empty) {
        return true
      }
    }
    false
  }

  def isCollidingWall(tetromino : Array[Array[Block]], directionX : Int, directionY : Int) : Boolean = {
    for(i <- tetromino.indices; j <- tetromino(i).indices){
      if(tetromino(i)(j).block != Empty && (tetromino(i)(j).x + directionX < 0 || tetromino(i)(j).x + directionX >= nrColumns || tetromino(i)(j).y + directionY >= nrRows)){
        return true
      }
    }
    false
  }

  def canMove(x : Int,y : Int): Boolean = {
    if(isCollidingWall(currentTetromino,x,y) || isCollidingBlock(currentTetromino,x,y))
      return false
    true
  }

  def canTurn(turnedTetromino : Array[Array[Block]], toRotation : Char) : (Boolean, Array[Array[Block]]) = {
    println(rotation + " -> " + toRotation)
    var srsResults : (Boolean,Int) = (false,0)
    var rotationTests : Array[Array[(Int,Int)]] = Array(Array((0,0)))

    if (!isCollidingWall(turnedTetromino,0, 0) && !isCollidingBlockWhileTurning(turnedTetromino, 0, 0))
      return (true, turnedTetromino)

    if (blockType == IBlock)
      rotationTests = rotationsI
    else
      rotationTests = rotationsJLSTZ

    srsResults = checkSRSLocationsForRotation()
    if(srsResults._1) {
      println("Moving block to " +  getTestLocs(rotationsI,toRotation)(srsResults._2)._1 + " " +  getTestLocs(rotationsI,toRotation)(srsResults._2)._2)
      for(i <- currentTetromino.indices; j <- currentTetromino(i).indices){
        turnedTetromino(i)(j).x +=  getTestLocs(rotationsI,toRotation)(srsResults._2)._1
        turnedTetromino(i)(j).y +=  getTestLocs(rotationsI,toRotation)(srsResults._2)._2
      }
      return (true, turnedTetromino)
    }

    def checkSRSLocationsForRotation(): (Boolean,Int) ={
      val testLocs = getTestLocs(rotationTests,toRotation)
      for(index <- testLocs.indices) {
        println("Checks :" + testLocs(index)._1 + " " + testLocs(index)._2)
        if (!isCollidingWall(turnedTetromino, testLocs(index)._1, testLocs(index)._2) && !isCollidingBlockWhileTurning(turnedTetromino ,testLocs(index)._1, testLocs(index)._2))
          return (true, index)
      }
      (false, 0)
    }
    (false,turnedTetromino)
  }

  def getRotationIndex() : Int = {
    rotation match {
      case '0' => 0
      case 'R' => 1
      case '2' => 2
      case 'L' => 3
    }
  }

  def getRotationCounterClockwise() : Char = {
    if(getRotationIndex() == 0){
      rotations(rotations.length - 1)
    }else{
      rotations(getRotationIndex() - 1)
    }
  }

  def getRotationClockwise() : Char = {
    if(getRotationIndex() == rotations.length - 1){
      rotations(0)
    }else{
      rotations(getRotationIndex() + 1)
    }
  }

  def rotateLeft(): Unit = {
    val rotatedBlock : Array[Array[Block]] = currentTetromino.map(_.map(_.copy()))
    for(i <- currentTetromino.indices; j <- currentTetromino(i).indices) {
      rotatedBlock(i)(j).block = currentTetromino(j)(rotatedBlock.length - i - 1).block
    }
    if(canTurn(rotatedBlock, getRotationCounterClockwise())._1) {
      currentTetromino = canTurn(rotatedBlock, getRotationCounterClockwise())._2.map(_.map(_.copy()))
      rotation = getRotationCounterClockwise()
    }

    setGhostBlock()
    setGhostBlockLocation()
  }

  def rotateRight(): Unit = {
    val rotatedBlock : Array[Array[Block]] = currentTetromino.map(_.map(_.copy()))
    for(i <- currentTetromino.indices; j <- currentTetromino(i).indices) {
      rotatedBlock(i)(j).block = currentTetromino(currentTetromino.length - j - 1)(i).block
    }
    if(canTurn(rotatedBlock, getRotationClockwise())._1) {
      currentTetromino = canTurn(rotatedBlock, getRotationClockwise())._2.map(_.map(_.copy()))
      rotation = getRotationClockwise()
    }
    setGhostBlock()
    setGhostBlockLocation()
  }

  def shiftRowDown(row: Int): Unit = {
    val tempBoard : Array[Array[TetrisBlock]] = gameBoard
    for(i <- (1 to row).reverse; j <- gameBoard.head.indices){
      gameBoard = gameBoard.updated(i,gameBoard(i).updated(j,tempBoard(i - 1)(j)))
    }
  }

  def isRowFull(row: Int): Boolean = {
    for(i <- gameBoard.head.indices){
      if(gameBoard(row)(i) == Empty){
        return false
      }
    }
    true
  }

  def checkBoardRows(): Unit = {
    for(i <- gameBoard.indices if isRowFull(i)){
      shiftRowDown(i)
    }
  }

  def placeBlock(): Unit = {
    for(i <- currentTetromino.indices; j <- currentTetromino.indices){
      if(currentTetromino(i)(j).block != Empty){
        gameBoard(currentTetromino(i)(j).y)(currentTetromino(i)(j).x) = currentTetromino(i)(j).block
      }
    }
    canHold = true
  }

  def spawnNewTetromino(): Unit = {
    blockToSpawn = createBlock(indexToTetrisBlock(nextBlocksToSpawn.last))
    centerTetrominoInX(blockToSpawn)
    blockType = indexToTetrisBlock(nextBlocksToSpawn.last)
    nextBlocksToSpawn = nextBlocksToSpawn.init
    initNextBlocks()

    if(!isGameOver) {
      currentTetromino = blockToSpawn.map(_.map(_.copy()))
      rotation = '0'
      setGhostBlock()
    }
  }

  def moveLeft(): Unit = {
    if(canMove(- 1,0))
      for(i <- currentTetromino.indices; j <- currentTetromino.indices) {
        currentTetromino(i)(j).x -= 1
      }
    setGhostBlockLocation()
  }

  def moveRight(): Unit = {
    if(canMove(+1,0))
      for(i <- currentTetromino.indices; j <- currentTetromino.indices) {
        currentTetromino(i)(j).x += 1
      }
    setGhostBlockLocation()
  }

  def moveDown(): Unit = {
    if(canMove(0,+1)) {
      for(i <- currentTetromino.indices; j <- currentTetromino.indices) {
        currentTetromino(i)(j).y += 1
      }
      setGhostBlock()
      setGhostBlockLocation()
    }else{
      blockLanded()
    }
  }

  def blockLanded() : Unit = {
    placeBlock()
    checkBoardRows()
    spawnNewTetromino()
  }

  def doHardDrop(): Unit = {
    var newY : Int = 0
    for(row <- 1 until nrRows){ // From top to bot
      if(canMove(0, row)){
        newY += 1
      } else {
        for(y <- currentTetromino.indices; x <- currentTetromino(y).indices) { // Set The Location
          currentTetromino(y)(x).y += newY
        }
        blockLanded()
        return
      }
    }
  }

  def isGameOver: Boolean = {
    for(y <- blockToSpawn.indices; x <- blockToSpawn.indices){
      if(blockToSpawn(y)(x).block != Empty && gameBoard(blockToSpawn(y)(x).y)(blockToSpawn(y)(x).x) != Empty)
        return true
    }
    false
  }

  def getBlockAt(x: Int, y: Int): TetrisBlock = {
    if (gameBoard(y)(x) != Empty)
      return gameBoard(y)(x)

    for(i <- currentTetromino.indices; j <- currentTetromino(i).indices)
      if(currentTetromino(i)(j).block != Empty && currentTetromino(i)(j).x == x && currentTetromino(i)(j).y == y){
        return currentTetromino(i)(j).block
      }

    for(i <- ghostTetromino.indices; j <- ghostTetromino(i).indices)
      if(ghostTetromino(i)(j).block != Empty && ghostTetromino(i)(j).x == x && ghostTetromino(i)(j).y == y){
        return GBlock
      }
    Empty
  }

  def holdBlock(): Unit = {
    if(canHold){
      canHold = false
      if (heldBlock == null){
        heldBlock = blockToSpawn.map(_.map(_.copy()))
        spawnNewTetromino()
      } else {
        val tempBlock : Array[Array[Block]] = blockToSpawn.map(_.map(_.copy()))
        currentTetromino = heldBlock.map(_.map(_.copy()))
        heldBlock = tempBlock.map(_.map(_.copy()))
        centerTetrominoInX(heldBlock)
        zeroTetrominoInY(heldBlock)
      }
    }
  }

  def zeroTetrominoInY(tetromino : Array[Array[Block]]) : Unit = {
    for(i <- tetromino.indices; j <- tetromino(i).indices){
      tetromino(i)(j).y = i
    }
  }

}

object TetrisLogic {
  def makeEmptyBoard(nrColumns: Int, nrRows: Int): Seq[Seq[TetrisBlock]] = {
    val emptyLine = Seq.fill(nrColumns)(Empty)
    Seq.fill(nrRows)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines

  def apply() = new TetrisLogic(new ScalaRandomGen(),
                                DefaultWidth,
                                DefaultHeight,
                                makeEmptyBoard(DefaultWidth, DefaultHeight))
}