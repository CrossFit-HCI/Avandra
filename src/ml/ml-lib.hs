{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module MLlib where

    import Nat
    import Vect

    ------------------
    -- Lines        --
    ------------------

    line :: Double -> (Double, Double) -> Double
    line x (w,b) = (x * w) + b

    ------------------
    -- Tensors      --
    ------------------

    data Tens :: (Nat -> * -> *) where
        Scalar :: a -> Tens Z a                           -- Tensor 0
        Tensors :: Vect (S n) (Tens m a) -> Tens (S m) a  -- Tensor (m+1)

    showTensor :: Show a => Tens m a -> String
    showTensor (Scalar d) = show d
    showTensor (Tensors ts) = "["++(showVectTensor ts)++"]"
        where
            showVectTensor :: Show a => Vect n (Tens m a) -> String
            showVectTensor Empty = ""
            showVectTensor (Cons t Empty) = showTensor t
            showVectTensor (Cons t ts) = (showTensor t) ++ "," ++ (showVectTensor ts)

    instance Show a => Show (Tens m a) where
        show = showTensor

    type Scalar = Double
    type Tensor m = Tens m Scalar
