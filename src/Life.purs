module Life where

import Prelude
import Data.Grid (Grid(..))
import Data.Grid as Grid
import Data.Array as Array
import Data.Foldable (foldr, foldl)
import Control.Comonad (extract, extend)

type Life = Grid Boolean

aliveNeighbours :: Life -> Int
aliveNeighbours life = f $ extract <$> Grid.neighbours life
    where
        f = Array.length <<< Array.filter (eq true)

rule :: Life -> Boolean
rule life =
    case aliveNeighbours life of
      2 -> extract life
      3 -> true
      _ -> false

evolve :: Life -> Life
evolve = extend rule

glider :: Life
glider =
    Grid.init false 25 25
        # Grid.right
        # Grid.down
        # Grid.right
        # Grid.write true
        # Grid.down
        # Grid.right
        # Grid.write true
        # Grid.down
        # Grid.write true
        # Grid.left
        # Grid.write true
        # Grid.left
        # Grid.write true
        # Grid.up
        # Grid.up
        # Grid.up
