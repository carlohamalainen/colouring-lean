import Datastar
import DatastarBrotli
import DatastarLeanIn

import LeanIn
import LeanIn.Http.StdHandler
import LeanIn.Runtime.Net

import Std.Data.HashMap
import Std.Net.Addr

open LeanIn LeanIn.Http LeanIn.Runtime
open LeanIn.Task.Sync

open Datastar Datastar.LeanIn

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
def frameMs : Nat := 50

/-- Milliseconds between a viewer's checks of the board when no change arrives. -/
def heartbeatMs : Nat := 1000

/-- Open streams allowed at once. -/
def maxViewers : Nat := 200

/-- Paints one viewer may make in a second. -/
def maxPaintsPerSecond : Nat := 100

/-! # LeanIn plumbing -/

/-- Where a step that runs off its own carrier delivers the continuations it wakes. -/
abbrev Push := Task.Item → BaseIO Unit

/-- Run a `Task.Async` from `VM`, resuming on the shared route. -/
def awaitAsync (ctx : Task.Ctx) (a : Task.Async α) : VM α :=
  let shared := { ctx with resume := ctx.resumeShared }
  VM.ioE fun resume _ => a.step (fun v _ => resume v) shared

/-! # State -/

structure Cell where
  colour : Nat := 0
  life : Nat := 0

/--
One open stream: the paints it has made in the current second, and the channel that wakes it when
the board changes.
-/
structure Viewer where
  /--
  Capacity 1: a change while a wake is already pending is folded into that wake, and a change
  while the viewer is busy sending is kept for its next wait, so no change is lost.
  -/
  wake : Channel Unit
  second : Nat := 0
  paints : Nat := 0

structure Board where
  cells : Array Cell := Array.replicate (gridSize * gridSize) {}
  /-- The open streams, by the token each was given. -/
  viewers : Std.HashMap String Viewer := {}
  paints : Nat := 0
  version : Nat := 0

/-- State shared by every connection: the board, and what it needs to wake the viewers. -/
structure SharedState where
  board : IO.Ref Board
  push : Push
  clock : Clock

/-- Change the board, and wake every viewer. -/
def SharedState.update (state : SharedState) (f : Board → Board) : IO Unit := do
  let viewers ← state.board.modifyGet fun b =>
    let b := { f b with version := b.version + 1 }
    (b.viewers, b)
  for (_, v) in viewers do
    -- A full channel already holds a wake, which will show the viewer this change too.
    let _ ← v.wake.trySend () state.push

/-- Why a paint was refused. -/
inductive Refusal where
  /-- No open stream has this token. -/
  | unknownViewer
  /-- The viewer has used up this second's paints. -/
  | tooFast

/-- Count a paint against its viewer's allowance for the current second, or say why it is refused. -/
def SharedState.allowPaint (state : SharedState) (token : String) : IO (Except Refusal Unit) := do
  let second := (← IO.monoMsNow) / 1000
  state.board.modifyGet fun b =>
    match b.viewers[token]? with
    | none => (.error .unknownViewer, b)
    | some v =>
      let v := if v.second == second then v else { v with second, paints := 0 }
      if v.paints < maxPaintsPerSecond then
        (.ok (), { b with viewers := b.viewers.insert token { v with paints := v.paints + 1 } })
      else
        (.error .tooFast, b)

/-- A token for a new stream: 16 random bytes as hex. -/
def newToken : IO String := do
  let bytes ← IO.getRandomBytes 16
  return bytes.toList.foldl (fun s b => s ++ hexByte b) ""
where
  hexByte (b : UInt8) : String :=
    let h := String.ofList (Nat.toDigits 16 b.toNat)
    if h.length < 2 then "0" ++ h else h

/-- The signals the browser sends with a paint: the chosen colour, and the token of its stream. -/
structure Signals where
  colour : Nat
  token : String
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

/-- A cell. Painting is handled once on `#board` in `index.html`, which reads the cell from its id. -/
def renderCell (i : Nat) (c : Cell) : String :=
  s!"<button id=\"c-{i}\" class=\"cell\" style=\"{c.style}\"></button>"

/-- The whole board as one patch: the grid and the two counters. -/
def renderBoard (b : Board) : String :=
  let cells := b.cells.mapIdx renderCell
  s!"<div id=\"grid\" style=\"grid-template-columns: repeat({gridSize}, 1fr)\">{String.join cells.toList}</div>"
    ++ s!"<span id=\"online\">{b.viewers.size}</span>"
    ++ s!"<span id=\"paints\">{b.paints}</span>"

/-! # Responses -/

abbrev Resp := Std.Http.Response ResponseBody

def respond (status : Std.Http.Status) (contentType : String) (body : ByteArray) : Resp :=
  let name := Std.Http.Header.Name.contentType
  let headers := (Std.Http.Header.Value.ofString? contentType).map (Std.Http.Headers.empty.insert name)
  { line := { status, headers := headers.getD .empty }, body := .bytes body }

def text (status : Std.Http.Status) (body : String) : Resp :=
  respond status "text/plain; charset=utf-8" body.toUTF8

/-! # Handlers -/

def indexHtml : String := include_str "index.html"

/-- Datastar, vendored in. -/
def datastarJs : String := include_str "static/datastar.js"

/-- Log to stderr from `VM`. -/
def logErr (msg : String) : VM Unit := monadLift (IO.eprintln msg : IO Unit)

/--
One SSE stream per open page: send the whole board whenever it changes. The first event gives the
page its token, which Datastar sends back as a signal with every paint.
-/
def updates (state : SharedState) (ctx : Task.Ctx) (req : Std.Http.Request Server.BodyReader) : VM Resp := do
  if (← (state.board.get : IO Board)).viewers.size ≥ maxViewers then
    return text .serviceUnavailable "Too many viewers, try again later"
  -- Quality 4 compresses these frames as well as the default 6 and needs a quarter of the memory
  -- per open stream. A 64 KB window holds several frames of history, which is all the next frame
  -- needs to compress against.
  sseResponseWith [brotli (quality := 4) (windowLog := 16)] req fun sse => do
    let token ← (newToken : IO String)
    let wake ← (Channel.new Unit 1 : IO (Channel Unit))
    let online ← (do
      state.update fun b => { b with viewers := b.viewers.insert token { wake } }
      return (← state.board.get).viewers.size : IO Nat)
    logErr s!"viewer joined, {online} online"
    try
      sse.send <| patchSignals ("{\"token\": \"" ++ token ++ "\"}")
      let mut seen : Option Nat := none
      repeat
        let board ← (state.board.get : IO Board)
        if seen != some board.version then
          seen := some board.version
          sse.send <| patchElements (renderBoard board)
        else
          -- Nothing changed since the heartbeat. Do a no-op patch - if this fails
          -- then we know sooner rather than later that the viewer has left.
          sse.send <| patchSignals "{}"

        -- Wait for a change, or the heartbeat. The channel keeps a wake that arrived while we
        -- were sending, so the heartbeat is only for noticing a viewer that has left.
        let _ ← awaitAsync ctx (Runtime.withTimeout state.clock (.ofNat heartbeatMs) (Channel.recv wake))

        -- Let a burst of changes settle, so that they go out as one frame.
        VM.sleep state.clock (.ofNat frameMs)
    finally
      -- The stream has closed, so we lost a viewer.
      let online ← (do
        state.update fun b => { b with viewers := b.viewers.erase token }
        return (← state.board.get).viewers.size : IO Nat)
      logErr s!"viewer left, {online} online"

/-- Paint one cell with the painter's colour, if the painter has a stream open and is within their allowance. -/
def paint (state : SharedState) (cell : Nat) (req : Std.Http.Request Server.BodyReader) : VM Resp := do
  match (← signalsText req) >>= decodeSignals (α := Signals) with
  | .error err => return text .badRequest s!"Bad signals: {err}"
  | .ok signals =>
    if cell ≥ gridSize * gridSize || signals.colour ≥ paletteSize then
      return text .badRequest "No such cell or colour"
    match ← (state.allowPaint signals.token : IO _) with
    | .error .unknownViewer => return text .forbidden "Painting needs an open page"
    | .error .tooFast => return text .tooManyRequests "Too many paints in one second"
    | .ok () =>
      let painted : Cell := { colour := signals.colour, life := if signals.colour == 0 then 0 else maxLife }
      (state.update fun b => { b with cells := b.cells.setIfInBounds cell painted, paints := b.paints + 1 } : IO Unit)
      logErr s!"cell {cell} painted colour {signals.colour}"
      sseResponse fun _ => pure ()

/-- Every `seconds`, take one step of life from every painted cell. -/
def fade (state : SharedState) (seconds : Nat) : VM Unit := do
  repeat
    VM.sleep state.clock (.ofNat (seconds * 1000))
    monadLift (do
      if (← state.board.get).cells.any (·.life > 0) then
        state.update fun b => { b with cells := b.cells.map fun c => { c with life := c.life - 1 } } : IO Unit)

def app (state : SharedState) (ctx : Task.Ctx) (req : Std.Http.Request Server.BodyReader) : VM Resp := do
  match req.line.method, (toString req.line.uri.path).splitOn "/" with
  | .get, ["", ""] => return respond .ok "text/html; charset=utf-8" indexHtml.toUTF8
  | .get, ["", "datastar.js"] => return respond .ok "text/javascript" datastarJs.toUTF8
  | .get, ["", "updates"] => updates state ctx req
  | .post, ["", "paint", n] =>
    match n.toNat? with
    | some cell => paint state cell req
    | none => return text .notFound "Not found"
  | _, _ => return text .notFound "Not found"

/--
`colouring [port] [fade-seconds] [address] [workers=N]`; a fade of 0 switches the fade off.
`workers` is the number of LeanIn carriers, default 1.
-/
def main (args : List String) : IO UInt32 := do
  let positional := args.filter (!·.startsWith "workers=")
  let port := (positional[0]? >>= String.toNat?).getD 3000
  let fadeSeconds := (positional[1]? >>= String.toNat?).getD 10
  let host := (positional[2]? >>= Std.Net.IPv4Addr.ofString).getD (.ofParts 127 0 0 1)
  let workers := (args.findSome? fun a => (a.dropPrefix? "workers=").bind (·.toNat?)).getD 1

  let hooks ← Hooks.new
  -- Binding starts LeanIn's pump thread, which owns the libuv loop. The `workers` carriers run the
  -- steps; a completion on the loop thread only enqueues its continuation for a carrier to take.
  let listener ← Listener.bind (.v4 ⟨host, port.toUInt16⟩)
  let executor ← Sched.Executor.new Task.Item 256 workers
  let clock := Clock.live hooks
  let state : SharedState := { board := ← IO.mkRef {}, push := sharedPush executor, clock }
  let config : Server.Config := { clock, continuePolicy := .application }
  let handler (ctx : Task.Ctx) : StdHandler := { onRequest := app state ctx }

  IO.println s!"Colouring page on http://{host}:{port}, fade step every {fadeSeconds}s, {workers} workers"
  (← IO.getStdout).flush

  let serve : Task.EAsync IO.Error Unit := do
    if fadeSeconds > 0 then
      monadLift (VM.run (fade state fadeSeconds))
    serveForeverShared hooks executor listener fun conn => do
      let (ctx : Task.Ctx) ← Task.EAsync.ofAsync ⟨fun (k : Task.Ctx → Task.Ctx → IO Unit) (current : Task.Ctx) => k current current⟩
      Server.serveConnStd config conn (handler ctx)

  let (_, outcome) ← Runtime.runCarriersWait executor workers serve
  Runtime.Io.stop
  match outcome with
  | .ok _ => return 0
  | .error err =>
    IO.eprintln s!"The accept loop ended: {err}"
    return 1
