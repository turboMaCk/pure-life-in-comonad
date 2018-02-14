module Data.Array.CircularZipper where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe', maybe)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Tuple (Tuple(..))
import Control.Extend (class Extend)
import Control.Comonad (class Comonad)

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
left zipper = maybe' (\_ -> start zipper) id $ next zipper

right :: forall a. Zipper a -> Zipper a
right zipper = maybe' (\_ -> end zipper) id $ prev zipper

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
    extend :: forall b a. (Zipper a -> b) -> Zipper a -> Zipper b
    extend f = Zipper <$> go f prev <*> f <*> go f next
        where go f d = map f <<< maybeIterate d

instance comonadZipper :: Comonad Zipper where
    extract = read

maybeIterate :: forall a f. (Unfoldable f) => (a -> Maybe a) -> a -> f a
maybeIterate f = unfoldr (map dup <<< f)
    where dup a = Tuple a a
