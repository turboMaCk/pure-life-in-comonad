module Data.Grid where

import Prelude
import Data.List (List(..), foldr)
import Data.Array.Zipper (Zipper(..))
import Data.Array.Zipper as Zipper
import Data.Maybe (Maybe(..))
import Control.Extend (class Extend)
import Control.Comonad (class Comonad, extend)
import Data.Traversable (sequence)

newtype Grid a = Grid (Zipper (Zipper a))

getZipper :: forall a. Grid a -> Zipper (Zipper a)
getZipper (Grid zipper) = zipper

upwards :: forall a. Grid a -> Maybe (Grid a)
upwards grid = Grid <$> (Zipper.prev $ getZipper grid)

downwards :: forall a. Grid a -> Maybe (Grid a)
downwards grid = Grid <$> (Zipper.next $ getZipper grid)

prev :: forall a. Grid a -> Maybe (Grid a)
prev grid = Grid <$> (sequence $ Zipper.next <$> getZipper grid)

next :: forall a. Grid a -> Maybe (Grid a)
next grid = Grid <$> (sequence $ Zipper.prev <$> getZipper grid)

up :: forall a. Grid a -> Grid a
up = Grid <<< Zipper.left <<< getZipper

down :: forall a. Grid a -> Grid a
down = Grid <<< Zipper.right <<< getZipper

left :: forall a. Grid a -> Grid a
left = Grid <<< map Zipper.left <<< getZipper

right :: forall a. Grid a -> Grid a
right = Grid <<< map Zipper.right <<< getZipper

read :: forall a. Grid a -> a
read = Zipper.read <<< Zipper.read <<< getZipper

write :: forall a. a -> Grid a -> Grid a
write x (Grid zipper) = Grid $ Zipper.write newLine zipper
    where
      newLine = Zipper.write x oldLine
      oldLine = Zipper.read zipper

instance showGrid :: (Show a) => Show (Grid a) where
    show :: forall a. Show a => Grid a -> String
    show (Grid zipper) = show $ Zipper.toArray $ map showLine zipper
        where
          showLine z =
              show (Zipper.toArray z) <> "\n"

instance eqGrid :: (Eq a) => Eq (Grid a) where
    eq :: forall a. (Eq a) => Grid a -> Grid a -> Boolean
    eq (Grid z1) (Grid z2) = z1 == z2

instance functorGrid :: Functor Grid where
    map :: forall a b. (a -> b) -> Grid a -> Grid b
    map fc = Grid <<< map (map fc) <<< getZipper
