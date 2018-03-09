module Game where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Except (runExcept)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (EventType(EventType))
import DOM.HTML (window)
import DOM.HTML.Types as DHT
import DOM.HTML.Window (document)
import Data.Array (concat, filter, length, replicate, reverse, (..))
import Data.Either (Either(..))
import Data.Foldable as DF
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import FFI.Util (property, setProperty)
import Graphics.Canvas (CANVAS, Context2D, clearRect, fillText, getCanvasElementById, getContext2D, rect, setFont, setStrokeStyle, strokePath)
import Partial.Unsafe (unsafePartial)

canvasClean :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Unit
canvasClean ctx = do
  void $ clearRect ctx { x: 0.0, y: 0.0, w: 4.0 * (toNumber width), h: 4.0 * (toNumber width) }

width :: Int
width = 100

type Grid = Array (Array Int)

grid :: Grid
grid =  [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

checkSpace :: Grid -> Grid
checkSpace g = do
      i <- (0 .. 3)
      j <- (0 .. 3)
      guard $ ((getValue g i j) == 0)
      pure [i,j]

getValue :: Grid -> Int -> Int -> Int
getValue g i j = do
  property (property g (show i)) (show j)

addNumberUtil :: forall a. Grid -> Eff(random :: RANDOM, console :: CONSOLE | a) Unit
addNumberUtil availGrid = do
  p <- (randomInt 0 ((length availGrid) - 1))
  let (i::Int) = (property (property availGrid (show p)) "0")
  let (j::Int) = (property (property availGrid (show p)) "1")
  po <-  randomInt 0 10
  let no = if po > 5 then 2 else 4
  let operatingArray = property grid (show i)
  _ <- pure $ setProperty operatingArray (show j) no
  _ <- pure $ setProperty grid (show i) operatingArray
  log (show (grid))
  pure unit

addNumber :: forall a. Grid ->  Eff(random :: RANDOM , console :: CONSOLE | a) Unit
addNumber g = do
  if(length (checkSpace g)) > 0 then
    (addNumberUtil (checkSpace g))
    else
    pure unit

checkWin :: forall a. Grid -> Eff(console :: CONSOLE | a) Unit
checkWin g = do
  let gg = concat g
  let req = filter (\x -> x == 2048) gg
  if ((length req) > 0) then
    log " Won "
    else
    pure unit

drawRectangles :: forall e. Context2D -> Grid -> Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
drawRectangles ctx gr = do
  DF.for_ (0 .. 3) \i -> do
    DF.for_ (0 .. 3) \j -> do
      drawRectangle ctx i j (getValue gr i j)
  pure unit

drawRectangle :: forall e. Context2D -> Int -> Int -> Int -> Eff (canvas :: CANVAS | e) Unit
drawRectangle ctx a b val = strokePath ctx $ do
  _ <- setStrokeStyle "#000000" ctx
  _ <- setFont "30px Arial" ctx
  let v = if val > 0 then (show val) else " "
  _ <- fillText ctx v (toNumber ((b * width) + width/4)) (toNumber ((a * width) + width/2))
  _ <- rect ctx
        { x: toNumber (a * width)
          , y: toNumber (b * width)
          , w: toNumber width
          , h: toNumber width
        }
  pure unit

rightSlide :: Array Int -> Array Int
rightSlide ar =  reverse (leftSlide (reverse ar))

rightCombine :: Array Int -> Array Int
rightCombine ar = do
  let (a :: Int) = (property ar "3")
  let (b :: Int) = (property ar "2")
  let (c :: Int) = (property ar "1")
  let (d :: Int) = (property ar "0")
  let (ar4 :: Int) = if (a == b) then (a + b) else a
  let (ar3 :: Int) = if (a == b ) then 0 else (if (b == c) then (b + c) else b)
  let (ar2 :: Int) = if (b == c && a/= b) then 0 else (if (c == d) then (c + d) else c)
  let (ar1 :: Int) = if (c == d ) then 0 else d
  [ar1, ar2, ar3, ar4]

leftSlide :: Array Int -> Array Int
leftSlide ar = do
  let pr = filter (\x -> x > 0) ar
  concat [pr, (replicate (4 - (length pr)) 0)]

leftCombine :: Array Int -> Array Int
leftCombine arr = reverse (rightCombine (reverse arr))

reverseRotate :: Array (Array Int) ->  (Array (Array Int))
reverseRotate gr = map reverse (rotate (map reverse gr))

rotate :: Array (Array Int) ->  (Array (Array Int))
rotate gr = do
  i <- (0 .. 3)
  pure [
   (property (property gr "3") (show i)) ,
   (property (property gr "2") (show i)) ,
   (property (property gr "1") (show i)) ,
   (property (property gr "0") (show i))
  ]

drawGridRight :: forall e. Context2D -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawGridRight ctx = do
  canvasClean ctx
  _ <- pure $  map (\x -> setProperty grid (show x) (rightSlide (rightCombine (rightSlide (property grid (show x)))))) (0 .. 3)
  _ <- liftEff $ (addNumber grid)
  drawRectangles ctx grid
  checkWin grid
  pure unit

drawGridLeft :: forall e. Context2D -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawGridLeft ctx = do
  canvasClean ctx
  _ <- pure $  map (\x -> setProperty grid (show x) (leftSlide (leftCombine (leftSlide (property grid (show x)))))) (0 .. 3)
  _ <- liftEff $ (addNumber grid)
  drawRectangles ctx grid
  checkWin grid
  pure unit

drawGridUp :: forall e. Context2D -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawGridUp ctx = do
  canvasClean ctx
  let rotated = rotate grid
  _ <- pure $  map (\x -> setProperty rotated (show x) (rightSlide (rightCombine (rightSlide (property rotated (show x)))))) (0 .. 3)
  let req = reverseRotate rotated
  _ <- pure $ map (\x -> setProperty grid (show x) (property req (show x))) (0 .. 3)
  _ <- liftEff $ (addNumber grid)
  drawRectangles ctx grid
  checkWin grid
  pure unit

drawGridDown :: forall e. Context2D -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawGridDown ctx = do
  canvasClean ctx
  let rotated = rotate grid
  _ <- pure $  map (\x -> setProperty rotated (show x) (leftSlide (leftCombine (leftSlide (property rotated (show x)))))) (0 .. 3)
  let req = reverseRotate rotated
  _ <- pure $ map (\x -> setProperty grid (show x) (property req (show x))) (0 .. 3)
  _ <- liftEff $ (addNumber grid)
  drawRectangles ctx grid
  checkWin grid
  pure unit

drawGrid :: forall e. Context2D -> Event -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawGrid ctx a = do
  case runExcept (KE.eventToKeyboardEvent a) of
    Left err ->
      log "Event was not a keyboard event"
    Right ke -> do
      let co = KE.code ke
      log ("Key Pressed : " <> (show co))
      case co of
        "ArrowLeft" -> drawGridLeft ctx
        "ArrowRight" -> drawGridRight ctx
        "ArrowUp" -> drawGridUp ctx
        "ArrowDown" -> drawGridDown ctx
        _ -> log "No Need in 2048!!"

drawInitialGrid :: forall e. Context2D -> Eff (canvas :: CANVAS,random :: RANDOM, dom :: DOM, console :: CONSOLE | e) Unit
drawInitialGrid ctx = do
  canvasClean ctx
  drawRectangles ctx grid
  log "In initialGame"
  documenttarget <- liftEff $ window >>= document <#> DHT.htmlDocumentToEventTarget
  addEventListener (EventType "keydown") (eventListener (drawGrid ctx)) true (documenttarget)
  pure unit

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, random :: RANDOM ,canvas :: CANVAS | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  _ <- liftEff $ (addNumber grid)
  _ <- liftEff $ (addNumber grid)
  drawInitialGrid ctx
  pure unit
