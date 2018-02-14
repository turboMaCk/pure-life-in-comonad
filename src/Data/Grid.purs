module Data.Grid where

import Data.List (List(..), foldr)
import Prelude
import Data.List.Zipper (Zipper(..))
import Data.List.Zipper as Zipper
import Data.Maybe

data Grid a = Grid (Zipper (Zipper a))


up :: forall a. Grid a -> Grid a
up (Grid inner) = Grid $ Zipper.prev inner

down :: forall a. Grid a -> Grid a
down (Grid inner) = Grid $ Zipper.next inner

left :: forall a. Grid a -> Grid a
left (Grid inner) = Grid $ map Zipper.prev inner

right :: forall a. Grid a -> Grid a
right (Grid inner) = Grid $ map Zipper.next inner


-- should be something like free monad or what
