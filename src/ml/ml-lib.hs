{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

import Nat
import Vect

line :: Double -> (Double, Double) -> Double
line x (w,b) = (x * w) + b

--------------
-- Tensors  --
--------------

data Tensor :: (Nat -> * -> *) where
      Scalar :: a -> Tensor Z a                             -- Tensor 0
      Tensors :: Vect (S n) (Tensor m a) -> Tensor (S m) a  -- Tensor (m+1)

showTensor :: Show a => Tensor m a -> String
showTensor (Scalar d) = show d
showTensor (Tensors ts) = "["++(showVectTensor ts)++"]"
  where
    showVectTensor :: Show a => Vect n (Tensor m a) -> String
    showVectTensor Empty = ""
    showVectTensor (Cons t Empty) = showTensor t
    showVectTensor (Cons t ts) = (showTensor t) ++ "," ++ (showVectTensor ts)

instance Show a => Show (Tensor m a) where
  show = showTensor



