module Data.List.Zipper
    ( Zipper
    , prev
    , next
    , cons
    , pure
    ) where

import Prelude
import Data.List
import Data.Maybe
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Tuple (Tuple(..))
import Control.Extend (class Extend)
import Control.Comonad (class Comonad)

data Zipper a = Zipper (List a) a (List a)

prev :: forall a. Zipper a -> Zipper a
prev (Zipper Nil a xs) =
    case reverse xs of
      (Cons last rest) -> Zipper rest last Nil
      Nil -> Zipper Nil a Nil
prev (Zipper xs a after) =
    case reverse xs of
      Cons prev rest -> Zipper rest prev (Cons a after)
      Nil -> Zipper Nil a after

next :: forall a. Zipper a -> Zipper a
next (Zipper Nil a Nil) = Zipper Nil a Nil
next (Zipper prev a (Cons next rest)) = Zipper (Cons a prev) next rest
next (Zipper (Cons first rest) a Nil) = Zipper Nil first rest

cons :: forall a. a -> Zipper a -> Zipper a
cons item (Zipper list a af) = Zipper (Cons item list) a af

-- should be applicative
pure :: forall a. a -> Zipper a
pure a = Zipper Nil a Nil

toList :: forall a. Zipper a -> List a
toList (Zipper pre a after) =
    append pre (Cons a after)

-- should be foldable
foldr' :: forall a b. (a -> b -> b) -> b -> Zipper a -> b
foldr' fc acc = foldr fc acc <<< toList

instance showZipper :: (Show a) => Show (Zipper a) where
    show :: forall a. Show a => Zipper a -> String
    show (Zipper pre a after) = "(" <> show pre <> ") " <> show a <> " (" <> show after <> ")"

instance eqZipper :: (Eq a) => Eq (Zipper a) where
    eq :: forall a. Eq a => Zipper a -> Zipper a -> Boolean
    eq (Zipper ls c rs) (Zipper ls' c' rs') =
        ls == ls' && c == c' && rs == rs'

instance functorZipper :: Functor Zipper where
    map :: forall a b. (a -> b) -> Zipper a -> Zipper b
    map f (Zipper ls c rs) = Zipper (f <$> ls) (f c) (f <$> rs)

-- instance extendZipper :: Extend Zipper where
--     extend :: forall a b. (Zipper a -> b) -> Zipper a -> Zipper b
--     extend f = Zipper <$> go f prev <*> f <*> go f next
--         where go f d = (<$>) f <<< maybeIterate' d

-- instance comonadZipper :: Comonad Zipper where
--     extract :: forall a. Zipper a -> a
--     extract (Zipper _ c _) = c
