module Data.Array.Zipper where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe', maybe)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Monoid (class Monoid)
import Control.Extend (class Extend)
import Control.Comonad (class Comonad)
import Data.Traversable (class Traversable, traverse)

data Zipper a = Zipper (Array a) a (Array a)

singleton :: forall a. a -> Zipper a
singleton a = Zipper [] a []

cons :: forall a. a -> Zipper a -> Zipper a
cons n (Zipper pre a after) = Zipper (Array.cons n pre) a after

snoc :: forall a. Zipper a -> a -> Zipper a
snoc (Zipper pre a after) n = Zipper pre a $ Array.snoc after n

toArray :: forall a. Zipper a -> Array a
toArray (Zipper pre a after) =
    (Array.snoc pre a) <> after

fromArray :: forall a. Array a -> Maybe (Zipper a)
fromArray =
    map (\ { head : head, tail : tail } -> Zipper [] head tail )
       <<< Array.uncons

repeat :: forall a. a -> Int -> Zipper a
repeat a n = repeat' a (singleton a) (n - 1)

repeat' :: forall a. a -> Zipper a -> Int -> Zipper a
repeat' x acc n =
    if n < 1 then
        acc
    else
        repeat' x (snoc acc x) (n - 1)

read :: forall a. Zipper a -> a
read (Zipper _ a _) = a

write :: forall a. a -> Zipper a -> Zipper a
write a (Zipper pre _ after) = Zipper pre a after

start :: forall a. Zipper a -> Zipper a
start (Zipper pre a after) =
    case Array.uncons pre of
            Just { head : head, tail : tail } ->
                Zipper [] head $ Array.snoc tail a <> after
            Nothing ->
                Zipper pre a after

end :: forall a. Zipper a -> Zipper a
end (Zipper pre a after) =
    case Array.unsnoc after of
      Just { init : init, last : last } ->
          Zipper (Array.snoc pre a <> init) last []
      Nothing ->
          Zipper pre a after

next :: forall a. Zipper a -> Maybe (Zipper a)
next (Zipper pre a after) =
    map (\ { head : head, tail : tail } -> Zipper (Array.snoc pre a) head tail) $ Array.uncons after

prev :: forall a. Zipper a -> Maybe (Zipper a)
prev (Zipper pre a after) =
    map (\ { init : init, last : last } -> Zipper init last $ Array.cons a after) $ Array.unsnoc pre

left :: forall a. Zipper a -> Zipper a
left zipper = maybe' (const $ start zipper) id $ prev zipper

right :: forall a. Zipper a -> Zipper a
right zipper = maybe' (const $ end zipper) id $ next zipper

instance showZipper :: (Show a) => Show (Zipper a) where
    show :: forall a. Show a => Zipper a -> String
    show (Zipper pre a after) = show pre <> " > " <> show a <> " < " <> show after

instance eqZipper :: (Eq a) => Eq (Zipper a) where
    eq :: forall a. Eq a => Zipper a -> Zipper a -> Boolean
    eq z1 z2 = toArray z1 == toArray z2

instance functorZipper :: Functor Zipper where
    map :: forall a b. (a -> b) -> Zipper a -> Zipper b
    map f (Zipper ls c rs) = Zipper (f <$> ls) (f c) (f <$> rs)

instance extendZipper :: Extend Zipper where
    extend :: forall a b. (Zipper a -> b) -> Zipper a -> Zipper b
    extend f = Zipper <$> go f prev <*> f <*> go f next
        where go f d = map f <<< maybeiterate d

instance comonadZipper :: Comonad Zipper where
    extract = read

instance foldableZipper :: Foldable Zipper where
    foldr :: forall a b. (a -> b -> b) -> b -> (Zipper a) -> b
    foldr f acc (Zipper pre a after) = flip (foldr f) pre
                                       $ f a
                                       $ foldr f acc after

    foldl :: forall a b. (b -> a -> b) -> b -> Zipper a -> b
    foldl f acc (Zipper pre a after) = flip (foldl f) after
                                       $ flip f a
                                       $ foldl f acc pre
    foldMap :: forall a m. (Monoid m) => (a -> m) -> (Zipper a) -> m
    foldMap f (Zipper ls c rs) = foldMap f ls <> f c <> foldMap f rs

instance traversableZipper :: Traversable Zipper where
    traverse :: forall a b m. (Applicative m) => (a -> m b) -> (Zipper a) -> m (Zipper b)
    traverse f (Zipper pre a after) = Zipper
                                      <$> traverse f pre
                                      <*> f a
                                      <*> traverse f after
    sequence = traverse id


maybeIterate :: forall a f. (Unfoldable f) => (a -> Maybe a) -> a -> f a
maybeIterate f = unfoldr (map dup <<< f)
    where dup a = Tuple a a
