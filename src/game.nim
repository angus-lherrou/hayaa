# Angus L'Herrou 8/1/2020
# github.com/angus-lherrou/hayaa

import sequtils
import illwill

type
  Cell = enum
    dead, alive
  Board = seq[seq[Cell]]
  Coordinate = tuple
    x: int
    y: int
  CellContext = tuple
    board: Board
    coord: Coordinate
  Neighbors = seq[Cell]
  Seed = seq[Coordinate]


func newboard(x: int, y: int): Board =
  dead.repeat(x).repeat(y)


func flip(c: Cell): Cell =
  case c
  of alive:
    dead
  of dead:
    alive


func count_live_neighbors(n: Neighbors): int =
  n.filter(proc(n: Cell): bool = n == alive).len


func age(c: CellContext): bool =
  let x = c.coord.x
  let y = c.coord.y
  if x == 0 or x >= c.board[0].len - 1 or y == 0 or y >= c.board.len - 1:
    return
      case c.board[y][x]
        of dead: false
        of alive: true
  let neighbors: Neighbors = 
    [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    .map(proc(coord: Coordinate): Cell = c.board[coord.y][coord.x])
  case c.board[y][x]
  of dead:
    if neighbors.count_live_neighbors == 3: true else: false
  of alive:
    if neighbors.count_live_neighbors in {2, 3}: false else: true


func tick(b: Board): Seed =
  let fac: int = b[0].len
  (0..fac*b.len-1).toSeq.map(
    proc(pos: int): CellContext = (b, (pos %% fac, pos /% fac))
  ).filter(age).map(proc(cc: CellContext): Coordinate = cc.coord)


proc apply(b: var Board, s: Seed): void =
  for coord in s:
    b[coord.y][coord.x] = b[coord.y][coord.x].flip


proc put(tb: var TerminalBuffer, b: Board): void =
  for y in 3..b.len-3:
    for x in 2..b[0].len-3:
      if b[y][x] == alive:
        tb.write(x, y, "O")

  
proc drawSeed(tb: var TerminalBuffer): Board =
  result = newboard(terminalWidth(), terminalHeight())
  tb.write(2, 0, "Move cursor with wasd and press space to fill.")
  tb.write(2, 1, "Press Enter to run the game.")
  
  var xPos = 2
  var yPos = 3
  
  tb.write(xPos, yPos, " ")
  var oldChar = tb[xPos, yPos]
  tb.write(xPos, yPos, "X")
  
  tb.display()
  
  while true:
    let key = getKey()
    
    case key
    of W, A, S, D:
      tb[xPos, yPos] = oldChar
      case key
      of W:
        yPos = min(terminalHeight()-3, max(3, yPos-1))
      of A:
        xPos = min(terminalWidth()-3, max(2, xPos-1))
      of S:
        yPos = min(terminalHeight()-3, max(3, yPos+1))
      of D:
        xPos = min(terminalWidth()-3, max(2, xPos+1))
      else:
        continue
      oldChar = tb[xPos, yPos]
      tb.write(xPos, yPos, "X")
    of Space:
      if result[yPos][xPos] == dead:
        tb.write(xPos, yPos, "O")
      else:
        tb.write(xPos, yPos, " ")
      oldChar = tb[xPos, yPos]
      result[yPos][xPos] = result[yPos][xPos].flip
    of Enter:
      break
    else:
      continue
    tb.display()
  tb.display()


proc exitProc() {.noconv.} =
  illwillDeinit()
  showCursor()
  quit(0)


proc runGame(tb: var TerminalBuffer, b: var Board, transition: proc) =
  hideCursor()
  tb.write(2, 1, "Press Ctrl+C to quit.")
  while true:
    tb.fill(2, 3, terminalWidth()-3, terminalHeight()-3)
    tb.put(b)
    tb.display()
    let t = b.tick
    b.apply(t)
    transition()


proc main =
  var tb = newTerminalBuffer(terminalWidth(), terminalHeight())
  var bb = newBoxBuffer(terminalWidth(), terminalHeight())
  bb.drawRect(1, 2, terminalWidth()-2, terminalHeight()-2)
  
  illwillInit(fullscreen=true)
  setControlCHook(exitProc)
  
  tb.write(bb)
  
  var b = tb.drawSeed()
  
  tb.clear()
  tb.write(bb)
  
  runGame(tb, b, proc = return)
