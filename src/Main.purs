module Main where

import Prelude
import Partial.Unsafe (unsafePartial)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)

import Control.Monad.Eff (Eff)
import Graphics.Canvas (getCanvasElementById, getContext2D, CANVAS, Context2D)
import Graphics.Canvas.Free (Graphics, beginPath, fill, rect, restore, runGraphics, save, setFillStyle, translate)

import Signal.Time (every)
import Signal (Signal, foldp, runSignal, (<~))

import Life (Life, glider, evolve)
import Data.Grid (Grid(..))
import Data.Array.Zipper as Zipper

type Cell = { x :: Int, y :: Int, alive :: Boolean }

indexedMap :: forall a b. (a -> Int -> b) -> Array a -> Array b
indexedMap f = fst <<< foldl addCell (Tuple [] 0)
    where
      addCell (Tuple acc n) i = Tuple (Array.snoc acc $ f i n) (n + 1)

getCells :: Life -> Array Cell
getCells (Grid zipper) = Array.concat $ indexedMap mapRow $ Zipper.toArray zipper
    where
      mapRow row y =
          indexedMap (\state x -> { x : x, y : y, alive : state }) $ Zipper.toArray row

at :: Int -> Int -> Graphics Unit -> Graphics Unit
at x y gfx = do
    save
    translate (toNumber x * 10.0) (toNumber y * 10.0)
    gfx
    restore

square :: String -> Number -> Graphics Unit
square color size = do
    beginPath
    setFillStyle color
    rect { x: 0.0, y: 0.0, w: size, h: size }
    fill

drawCalls :: Array Cell -> Array (Graphics Unit)
drawCalls cells = do
    c <- cells
    let gfx = do
          let clr = if c.alive then "#000000" else "#ffffff"
          at c.x c.y $ square clr 10.0
    pure gfx

seqn :: Array (Graphics Unit) -> Graphics Unit
seqn arr =
    case Array.uncons arr of
      Just { head : head, tail : tail } -> seqn tail >>= pure head
      Nothing -> pure unit

life :: forall eff. Context2D -> Life -> Eff ( canvas :: CANVAS | eff ) Unit
life context grid = runGraphics context >>= const $ seqn $ drawCalls $ getCells grid

update :: forall a. Life -> Signal a -> Signal Life
update =
    foldp (const evolve)

lifeEff :: forall eff. Context2D -> Signal (Eff ( canvas :: CANVAS | eff) Unit)
lifeEff cfx = life cfx <~ update glider (every 200.0)

main :: forall eff. Eff ( canvas :: CANVAS | eff ) Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    context <- getContext2D canvas
    runSignal $ lifeEff context
