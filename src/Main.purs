module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Graphics.Canvas (getCanvasElementById, getContext2D, CANVAS, Context2D)
import Graphics.Canvas.Free
import Partial.Unsafe (unsafePartial)
import Data.Maybe
import Data.Tuple
import Data.Array as Array
import Data.Foldable
import Data.Int
import Data.Date

import Life (Life, glider, evolve)
import Data.Grid (Grid(..))
import Data.Array.Zipper

type Cell = { x :: Int, y :: Int, alive :: Boolean }

indexedMap :: forall a b. (a -> Int -> b) -> Array a -> Array b
indexedMap f = fst <<< foldl addCell (Tuple [] 0)
    where
      addCell (Tuple acc n) i = Tuple (Array.snoc acc $ f i n) (n + 1)

getCells :: Life -> Array Cell
getCells (Grid zipper) = Array.concat $ indexedMap mapRow $ toArray zipper
    where
      mapRow row y =
          indexedMap (\state x -> { x : x, y : y, alive : state }) $ toArray row

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

main :: Eff _ Unit
main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "canvas"
    context <- getContext2D canvas

    life context $ evolve $ glider
