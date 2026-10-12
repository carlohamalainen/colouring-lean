import Datastar
import DatastarBrotli

import Std.Async
import Std.Http
import Std.Net.Addr
import Std.Sync.Notify

open Std Async Http Server
open Datastar

/-! # Tunables -/

/-- Cells per side of the board. -/
def gridSize : Nat := 10

/-- Colours in the palette in `index.html`, `--p0` to `--p8`; 0 is white. -/
def paletteSize : Nat := 9

/-- A freshly painted cell has this many steps of life; each fade tick takes one away. -/
def maxLife : Nat := 12

/--
Milliseconds a viewer waits after a change before sending the next frame, so that a burst of
paints goes out as one frame.
-/
def frameMs : Nat := 250

/-- Milliseconds between a viewer's checks of the board when no notification arrives. -/
def heartbeatMs : Nat := 1000

/-! # State -/

structure Cell where
  colour : Nat := 0
  life : Nat := 0

structure Board where
  cells : Array Cell := Array.replicate (gridSize * gridSize) {}
  online : Nat := 0
  paints : Nat := 0
  version : Nat := 0

/-- State shared by every connection: the board, and a way to wake the viewers when it changes. -/
structure SharedState where
  board : IO.Ref Board
  changed : Notify

/-- Change the board, and wake every viewer. -/
def SharedState.update (state : SharedState) (f : Board → Board) : IO Unit := do
  state.board.modify fun b => { f b with version := b.version + 1 }
  state.changed.notify

/-- The signals the browser sends with a paint. -/
structure Signals where
  colour : Nat
deriving Lean.FromJson

/-! # Rendering -/

/-- The colour of a cell: its palette colour, mixed towards white as its life runs out. -/
def Cell.style (c : Cell) : String :=
  if c.colour == 0 || c.life == 0 then
    "background: var(--p0)"
  else if c.life ≥ maxLife then
    s!"background: var(--p{c.colour})"
  else
    s!"background: color-mix(in oklab, var(--p{c.colour}) {c.life * 100 / maxLife}%, var(--p0))"

def renderCell (i : Nat) (c : Cell) : String :=
  s!"<button id=\"c-{i}\" class=\"cell\" style=\"{c.style}\" data-on:click=\"@post('/paint/{i}')\"></button>"

/-- The whole board as one patch: the grid and the two counters. -/
def renderBoard (b : Board) : String :=
  let cells := b.cells.mapIdx renderCell
  s!"<div id=\"grid\" style=\"grid-template-columns: repeat({gridSize}, 1fr)\">{String.join cells.toList}</div>"
    ++ s!"<span id=\"online\">{b.online}</span>"
    ++ s!"<span id=\"paints\">{b.paints}</span>"

/-! # Handlers -/

def indexHtml : String := include_str "index.html"

/-- Datastar, vendored in. -/
def datastarJs : String := include_str "static/datastar.js"

/--
One SSE stream per open page: send the whole board whenever it changes.
-/
def updates (state : SharedState) (req : Request Body.Stream) : ContextAsync (Response Body.Any) :=
  -- Quality 4 compresses these frames as well as the default 6 and needs a quarter of the memory
  -- per open stream. A 64 KB window holds several frames of history, which is all the next frame
  -- needs to compress against.
  sseResponseWith [brotli (quality := 4) (windowLog := 16)] req fun sse => do
    state.update fun b => { b with online := b.online + 1 }
    IO.eprintln s!"viewer joined, {(← state.board.get).online} online"
    try
      let mut seen : Option Nat := none
      repeat
        let board ← state.board.get
        if seen != some board.version then
          seen := some board.version
          sse.send <| patchElements (renderBoard board)
        else
          -- Nothing changed since the heartbeat. Do a no-op patch - if this fails
          -- then we know sooner rather than later that the viewer has left.
          sse.send <| patchSignals "{}"

        -- Wait for a change. `Notify` does not buffer, so the heartbeat covers a notification
        -- that arrived before we started waiting.
        let heartbeat ← Selector.sleep (.ofNat heartbeatMs)
        Selectable.one #[.case heartbeat              fun _ => pure (),
                         .case state.changed.selector fun _ => pure ()]

        -- Let a burst of changes settle, so that they go out as one frame.
        sleep (.ofNat frameMs)
    finally
      state.update fun b => { b with online := b.online - 1 } -- sse stream has closed, so we lost a viewer
      IO.eprintln s!"viewer left, {(← state.board.get).online} online"

/-- Paint one cell with the painter's colour. -/
def paint (state : SharedState) (cell : Nat) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match ← readSignals (α := Signals) req with
  | .error err => Response.badRequest |>.text s!"Bad signals: {err}"
  | .ok signals =>
    if cell ≥ gridSize * gridSize || signals.colour ≥ paletteSize then
      Response.badRequest |>.text "No such cell or colour"
    else
      let painted : Cell := { colour := signals.colour, life := if signals.colour == 0 then 0 else maxLife }
      state.update fun b => { b with cells := b.cells.setIfInBounds cell painted, paints := b.paints + 1 }
      IO.eprintln s!"cell {cell} painted colour {signals.colour}"
      sseResponse fun _ => pure ()

/-- Every `seconds`, take one step of life from every painted cell. -/
def fade (state : SharedState) (seconds : Nat) : Async Unit := do
  repeat
    sleep (.ofNat (seconds * 1000))
    if (← state.board.get).cells.any (·.life > 0) then
      state.update fun b => { b with cells := b.cells.map fun c => { c with life := c.life - 1 } }

def app (state : SharedState) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, (toString req.line.uri.path).splitOn "/" with
  | .get, ["", ""] => Response.ok |>.html indexHtml
  | .get, ["", "datastar.js"] =>
    Response.ok
      |>.header Header.Name.contentType (Header.Value.ofString! "text/javascript")
      |>.fromBytes datastarJs.toUTF8
  | .get, ["", "updates"] => updates state req
  | .post, ["", "paint", n] =>
    match n.toNat? with
    | some cell => paint state cell req
    | none => Response.notFound |>.text "Not found"
  | _, _ => Response.notFound |>.text "Not found"

/-- `colouring [port] [fade-seconds] [address]`; a fade of 0 switches the fade off. -/
def main (args : List String) : IO Unit := Async.block do
  let port := (args[0]? >>= String.toNat?).getD 3000
  let fadeSeconds := (args[1]? >>= String.toNat?).getD 10
  let host := (args[2]? >>= Std.Net.IPv4Addr.ofString).getD (.ofParts 127 0 0 1)
  let addr : Std.Net.SocketAddressV4 := ⟨host, port.toUInt16⟩

  let state : SharedState := { board := ← IO.mkRef {}, changed := ← Notify.new }
  if fadeSeconds > 0 then
    background (fade state fadeSeconds)

  let server ← Server.serve addr <| Handler.ofFn (app state)
  IO.println s!"Colouring page on http://{host}:{port}, fade step every {fadeSeconds}s"
  (← IO.getStdout).flush
  server.waitShutdown
