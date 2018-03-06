module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Foldable as DF
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Node.Types (elementToEventTarget)
import DOM.HTML.Types as DHT
import DOM.HTML.Document (body)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (EventType(..), EventTarget)
import DOM.Event.Event
import Data.Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import Data.Foldable as DF
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit)
import Control.Monad.Eff.Ref (REF, newRef, readRef, modifyRef, Ref)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, translate, arc, clearRect, closePath, fillPath, fillText, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)
import FFI.Util (property, setProperty)



test :: forall e. Ref Int -> Eff ( ref :: REF, console :: CONSOLE, dom :: DOM | e) Unit
test tt = do
  --ke <-  KE.eventToKeyboardEvent a
  --co <-  KE.key ke
  let a  = [[1,2,3,4,5],[1,2,3,4],[1,2,3]]
  let ind =  (a !! 2)
  let newind = fromMaybe [] ind
  let t = newind !! 1
  r <- readRef tt
  log (show r)
  pure unit

leftSlide :: Array Int -> Array Int
leftSlide ar = do
  let pr = filter (\x -> x > 0) ar
  let reqtemp = replicate (4 - (length pr)) 0
  let req = concat [pr,reqtemp]
  req

leftCombine :: Array Int -> Array Int
leftCombine arr = do
  let ar = reverse arr
  let (a :: Int) = (property ar "3")
  let (b :: Int) = (property ar "2")
  let (c :: Int) = (property ar "1")
  let (d :: Int) = (property ar "0")
  let (ar4 :: Int) = if (a == b) then (a + b) else a
  let (ar3 :: Int) = if (a == b ) then 0 else (if (b == c) then (b + c) else b)
  let (ar2 :: Int) = if (b == c && a/= b) then 0 else (if (c == d) then (c + d) else c)
  let (ar1 :: Int) = if (c == d ) then 0 else d
  [ar4, ar3, ar2, ar1]

rightSlide :: Array Int -> Array Int
rightSlide ar = do
  let pr = filter (\x -> x > 0) ar
  let reqtemp = replicate (4 - (length pr)) 0
  let req = concat [reqtemp, pr]
  req

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


test2 :: forall e. Ref Int -> Event -> Eff ( ref :: REF, console :: CONSOLE, dom :: DOM | e) Unit
test2 tt a = do
  log "Key Pressed : "
  let pr = [8,2,0,2]
  let rr = [[0,0,0,0],[0,0,4,0],[0,0,0,0],[0,2,0,0]]
  let rotated = rotate rr
  log ("Rotated : " <> (show rotated))
  _ <- pure $ setProperty rotated (show 0) (rightSlide (rightCombine (rightSlide (property rotated "0"))))
  _ <- pure $ setProperty rotated (show 1) (rightSlide (rightCombine (rightSlide (property rotated "1"))))
  _ <- pure $ setProperty rotated (show 2) (rightSlide (rightCombine (rightSlide (property rotated "2"))))
  _ <- pure $ setProperty rotated (show 3) (rightSlide (rightCombine (rightSlide (property rotated "3"))))
  log ("Rotated Combined : " <> (show rotated))
  let req = reverseRotate rotated
  log ("Reverse Rotated : " <> (show req))

--  log (show (rightCombine pr))
--  let rr = (leftSlide (leftCombine (leftSlide pr)))
--  log (show rr)

reverseRotate :: Array (Array Int) ->  (Array (Array Int))
reverseRotate gr = do
  --log (show gr)
  i <- (3 .. 0)
  pure [
   (property (property gr "0") (show i)) ,
   (property (property gr "1") (show i)) ,
   (property (property gr "2") (show i)) ,
   (property (property gr "3") (show i))
  ]

rotate :: Array (Array Int) ->  (Array (Array Int))
rotate gr = do
  --log (show gr)
  i <- (0 .. 3)
  pure [
   (property (property gr "3") (show i)) ,
   (property (property gr "2") (show i)) ,
   (property (property gr "1") (show i)) ,
   (property (property gr "0") (show i))
  ]

{-

      let bl = property blank (show j)
      let km = updateAt i (property (property gr (show i)) (show j)) bl
      let k = fromMaybe [] km
      let blm = updateAt j k blank
      let blank = fromMaybe [] blm
      pure unit
  blank
-}

main :: forall e. Eff (ref :: REF, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  to <- newRef 0
  documenttarget <- liftEff $ window >>= document <#> DHT.htmlDocumentToEventTarget
  addEventListener (EventType "keydown") (eventListener (test2 to)) true (documenttarget)
  test to
