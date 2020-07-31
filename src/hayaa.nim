# Angus L'Herrou 7/31/2020
# github.com/angus-lherrou/hayaa

import sequtils
import unicode
import os

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
    if neighbors.count_live_neighbors in {1, 4}: true else: false


func tick(b: Board): Seed =
  let fac: int = b[0].len
  (0..fac*b.len-1).toSeq.map(
    proc(pos: int): CellContext = (b, (pos %% fac, pos /% fac))
  ).filter(age).map(proc(cc: CellContext): Coordinate = cc.coord)


proc apply(b: var Board, s: Seed): void =
  for coord in s:
    b[coord.y][coord.x] = b[coord.y][coord.x].flip


proc put(tb: var TerminalBuffer, b: Board): void =
  for y in 2..b.len-3:
    for x in 2..b[0].len-3:
      if b[y][x] == alive:
        tb.write(y, x, fgWhite, "O")

  
proc drawSeed(tb: var TerminalBuffer): Seed =
  result = Seed.new[]
  tb.write(2, 0, "Move cursor with wasd and press space to fill.")
  tb.write(2, 1, "Press Q to run the game.")
  tb.display()
  var xPos = 2
  var yPos = 2
  tb.write(xPos, yPos, " ")
  var oldChar = tb[xPos, yPos]
  tb.write(xPos, yPos, "X")
  while true:
    let key = getKey()
    tb.write(28, 1, $key)
    case key
    of W:
      tb[xPos, yPos] = oldChar
      yPos = min(terminalHeight()-3, max(2, yPos-1))
      oldChar = tb[xPos, yPos]
      tb.write(xPos, yPos, "X")
    of A:
      tb[xPos, yPos] = oldChar
      xPos = min(terminalWidth()-3, max(2, xPos-1))
      oldChar = tb[xPos, yPos]
      tb.write(xPos, yPos, "X")
    of S:
      tb[xPos, yPos] = oldChar
      yPos = min(terminalHeight()-3, max(2, yPos+1))
      oldChar = tb[xPos, yPos]
      tb.write(xPos, yPos, "X")
    of D:
      tb[xPos, yPos] = oldChar
      xPos = min(terminalWidth()-3, max(2, xPos+1))
      oldChar = tb[xPos, yPos]
      tb.write(xPos, yPos, "X")
    of Space:
      if tb[xPos, yPos].ch != "O".toRunes[0]:
        tb.write(xPos, yPos, "O")
        oldChar = tb[xPos, yPos]
        result.add((xPos, yPos))
      else:
        tb.write(xPos, yPos, " ")
        oldChar = tb[xPos, yPos]
        result.add((xPos, yPos))
    of Q:
      break
    else:
      continue
    tb.display()
  tb.display()

proc exitProc() {.noconv.} =
  illwillDeinit()
  showCursor()
  quit(0)


proc main =
  var tb = newTerminalBuffer(terminalWidth(), terminalHeight())
  tb.setForegroundColor(fgBlack, true)

  illwillInit(fullscreen=true)
  setControlCHook(exitProc)

  var b = newboard(terminalWidth(), terminalHeight())
  #b.apply(@[(2, 4), (3, 4), (4, 4), (4, 3), (3, 2)])
  b.apply(tb.drawSeed())
  tb.clear()
  while true:
    tb.clear()
    hideCursor()
    tb.put(b)
    tb.display()
    hideCursor()
    let t = b.tick
    if t.len == 0: break
    b.apply(t)
    100.sleep
  tb.write(0, terminalHeight()-1, "Game over!")

main()