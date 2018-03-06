module Game2048v01 where

import Prelude
import DOM (DOM)
import Control.Monad.Eff.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, translate, rect, clearRect, closePath, fillPath, fillText, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array
import Data.Int
import Data.Foldable as DF
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Data.Tuple (Tuple(..))
import Control.Monad.Eff.Random
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.Node.Types (elementToEventTarget)
import DOM.HTML.Types as DHT
import DOM.HTML.Document (body)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (EventType(..), EventTarget)
import DOM.Event.Event
import DOM.HTML (window)
import DOM.HTML.Window (document)

canvasClean :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Unit
canvasClean ctx = do
  void $ clearRect ctx { x: 0.0, y: 0.0, w: 4.0 * (toNumber width), h: 4.0 * (toNumber width) }

width :: Int
width = 100

type Grid = Array (Array Int)

initialGame :: Grid
initialGame =  [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

checkSpace ::  Grid -> Grid
checkSpace grid = do
  --for_ (0 .. 3) \i -> do
  --  for_ (0 .. 3) \j -> do
      i <- (0 .. 3)
      j <- (0 .. 3)
      let tempar1 = grid !! i
      let ar1 = fromMaybe [] tempar1
      let tempval = ar1 !! j
      let val = fromMaybe 1 tempval
      guard $ val == 0
      pure [i,j]

addNumberUtil :: forall a. Grid -> Grid -> Eff(random :: RANDOM | a) Grid
addNumberUtil availGrid grid = do
  p <- (randomInt 0 (length availGrid))
  let tempar1 = availGrid !! p
  let ar1 = fromMaybe [0,0] tempar1
  let tempi = ar1 !! 0
  let tempj = ar1 !! 1
  let i = fromMaybe 0 tempi
  let j = fromMaybe 0 tempj
  po <-  randomInt 0 10
  let no = if po > 5 then 4 else 2
  let update = [Tuple j no]
  let operatingArray = fromMaybe [] (grid !! i)
  let updated = updateAtIndices update operatingArray
  let update2 = [Tuple i updated]
  let grid2 = updateAtIndices update2 grid
  pure grid2

addNumber :: forall a. Grid -> Eff(random :: RANDOM | a) Grid
addNumber grid =
  let bl = checkSpace grid
  in
  if(length bl) > 0 then (addNumberUtil bl grid) else (addEff grid)

addEff :: forall a. Grid -> Eff(random :: RANDOM | a) Grid
addEff grid =
  pure grid

drawRectangles :: forall e. Context2D -> Grid -> Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
drawRectangles ctx grid = do
  DF.for_ (0 .. 3) \i -> do
    DF.for_ (0 .. 3) \j -> do
      let tempar1 = grid !! i
      let ar1 = fromMaybe [] tempar1
      let tempval = ar1 !! j
      let val = fromMaybe 0 tempval
      drawRectangle ctx i j val
  pure unit

drawRectangle :: forall e. Context2D -> Int -> Int -> Int -> Eff (canvas :: CANVAS | e) Unit
drawRectangle ctx a b val = strokePath ctx $ do
  _ <- setStrokeStyle "#0000FF" ctx
  let v = if val > 0 then (show val) else " "
  _ <- fillText ctx v (toNumber ((b * width) + width/2)) (toNumber ((a * width) + width/2))
  _ <- rect ctx
        { x: toNumber (a * width)
          , y: toNumber (b * width)
          , w: toNumber width
          , h: toNumber width
        }
  pure unit



drawGrid :: forall e. Context2D -> Grid -> Event -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawGrid  ctx g a = do
  --let bl = checkSpace grid
  --ul <- liftEff $ (addNumberUtil bl grid)
  --log (show ul)
  canvasClean ctx
  grid <- liftEff $ (addNumber g)
  log "Up"
  log (show grid)
  drawRectangles ctx grid
  documenttarget <- liftEff $ window >>= document <#> DHT.htmlDocumentToEventTarget
  removeEventListener (EventType "keydown") (eventListener (drawGrid ctx grid)) true (documenttarget)
  drawInitialGrid ctx grid
  pure unit

drawInitialGrid :: forall e. Context2D -> Grid -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawInitialGrid ctx grid = do
  canvasClean ctx
  drawRectangles ctx grid
  log "In initialGame"
  documenttarget <- liftEff $ window >>= document <#> DHT.htmlDocumentToEventTarget
  addEventListener (EventType "keydown") (eventListener (drawGrid ctx grid)) true (documenttarget)

  pure unit


main :: forall e. Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM ,canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  let g = initialGame
  g1 <- liftEff $ (addNumber g)
  grid <-  liftEff $ (addNumber g1)
  drawInitialGrid ctx grid


  pure unit
