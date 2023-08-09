{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Vect where
    import Nat

    data Vect :: (Nat -> * -> *) where
        Empty :: Vect Z a
        Cons  :: a -> Vect m a -> Vect (S m) a

    showVect :: Show a => Vect m a -> String
    showVect Empty = ""
    showVect xs    = "["++(showVectHelper xs)++"]"
        where
            showVectHelper (Cons x Empty) = show x
            showVectHelper v = foldrVect (\x r -> (show x) ++ ","++r) "" v

    foldrVect :: (a -> b -> b) -> b -> Vect m a -> b
    foldrVect f s Empty = s
    foldrVect f s (Cons x xs) = f x (foldrVect f s xs)

    mapVect :: (a -> b) -> Vect m a -> Vect m b
    mapVect f Empty = Empty
    mapVect f (Cons x xs) = Cons (f x) (mapVect f xs)