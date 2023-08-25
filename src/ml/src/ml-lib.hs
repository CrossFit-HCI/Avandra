{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module MLlib where

    import Nat
    import Vect

    ------------------
    -- Tensors      --
    ------------------

    data Tens :: (Nat -> * -> *) where
        Scalar :: a -> Tens Zero a                         -- Tensor 0
        Tensor :: Vect (Suc n) (Tens m a) -> Tens (Suc m) a  -- Tensor (m+1)

    showTensor :: Show a => Tens m a -> String
    showTensor (Scalar d) = show d
    showTensor (Tensor t) = "["++(showVectTensor t)++"]"
        where
            showVectTensor :: Show a => Vect n (Tens m a) -> String
            showVectTensor Empty = ""
            showVectTensor (Cons t Empty) = showTensor t
            showVectTensor (Cons t ts) = (showTensor t) ++ "," ++ (showVectTensor ts)

    instance Show a => Show (Tens m a) where
        show = showTensor

    size :: Tens m a -> Int
    size (Scalar m) = 0
    size (Tensor t) = lengthVect t

    shape :: Tens m a -> [Int]
    shape (Scalar d) = []
    shape (Tensor t) = (lengthVect t) : (shape (headVect t))

    rank :: Tens m a -> Int
    rank = rankHelper 0 
      where
        rankHelper :: Int -> Tens m a -> Int
        rankHelper r (Scalar d) = r
        rankHelper r (Tensor t) = rankHelper (r+1) (headVect t)

    type Scalar = Double
    type Tensor m = Tens m Scalar

    ------------------------
    -- Extended Functions --
    ------------------------
    
    add :: Tensor m -> Tensor m -> Tensor m
    add (Scalar d1) (Scalar d2) = Scalar (d1 + d2)
    add (Tensor t1) (Tensor t2) = Tensor ts
      where
        ts = mapVect (uncurry add) $ zipVect t2 t2

    ------------------
    -- Lines        --
    ------------------

    line :: Double -> (Double, Double) -> Double
    line x (w,b) = (x * w) + b