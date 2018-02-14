module Data.Array.CircularZipper where

import Prelude
import Data.Array as Array
import Data.Maybe

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

next :: forall a. Zipper a -> Zipper a
next (Zipper pre a after) =
    case Array.uncons after of
      Just { head : head, tail : tail } ->
          Zipper (Array.snoc pre a) head tail
      Nothing ->
          start $ Zipper pre a after

prev :: forall a. Zipper a -> Zipper a
prev (Zipper pre a after) =
    case Array.unsnoc pre of
      Just { init : init, last : last } ->
          Zipper init last $ Array.cons a after
      Nothing ->
          end $ Zipper pre a after

instance showZipper :: (Show a) => Show (Zipper a) where
    show :: forall a. Show a => Zipper a -> String
    show (Zipper pre a after) = show pre <> " > " <> show a <> " < " <> show after

instance eqZipper :: (Eq a) => Eq (Zipper a) where
    eq :: forall a. Eq a => Zipper a -> Zipper a -> Boolean
    eq z1 z2 = toArray z1 == toArray z2

instance mapZipper :: Functor Zipper where
    map :: forall a b. (a -> b) -> Zipper a -> Zipper b
    map f (Zipper ls c rs) = Zipper (f <$> ls) (f c) (f <$> rs)
