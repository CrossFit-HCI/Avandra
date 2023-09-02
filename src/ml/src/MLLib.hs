{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant flip" #-}

module MLLib where

    import Nat
    import Vect

    import Data.Singletons    

    ------------------
    -- Tensors      --
    ------------------

    data Tens :: (Nat -> * -> *) where
        Scalar    :: a -> Tens Zero a                                       -- Tensor 0
        Tensor    :: Vect (Suc l) (Tens m a) -> Tens (Suc m) a              -- Tensor (m+1)

    showTensor :: Show a => Tens m a -> String
    showTensor (Scalar d) = show d
    showTensor (Tensor t) = "["++(showVectTensor t)++"]"
        where
            showVectTensor :: Show a => Vect l (Tens m a) -> String
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

    mapTensor0 :: (a -> b) -> Tens m a -> Tens m b
    mapTensor0 f (Scalar d) = Scalar $ f d
    mapTensor0 f (Tensor ts) = Tensor $ mapVect (mapTensor0 f) ts

    mapTensor1 :: Sing m -> (Tens (Suc Zero) a -> Tens (Suc Zero) b) -> Tens (Suc m) a -> Tens (Suc m) b
    mapTensor1 SZero f ts@(Tensor (Cons (Scalar _) _)) = f ts
    mapTensor1 (SSuc m) f (Tensor ts@(Cons _ _)) = Tensor $ mapVect (mapTensor1 m f) ts

    foldlTensor1 :: (b -> a -> b) -> b -> Tens (Suc Zero) a -> b
    foldlTensor1 f acc (Tensor ts) = foldlVect (\acc (Scalar d) -> f acc d) acc ts

    foldlTensor :: Sing m -> (b -> a -> b) -> b -> Tens (Suc m) a -> Tens m b
    foldlTensor SZero f acc ts@(Tensor (Cons (Scalar _) _)) = Scalar $ foldlTensor1 f acc ts
    foldlTensor (SSuc m) f acc (Tensor ts@(Cons _ _)) = Tensor $ mapVect (foldlTensor m f acc) ts

    type Scalar = Double
    type Tensor m = Tens m Scalar

    ------------------------
    -- Extended Functions --
    ------------------------
    
    addTensor :: Tensor m -> Tensor m -> Tensor m
    addTensor (Scalar d1) (Scalar d2) = Scalar (d1 + d2)
    addTensor (Tensor t1) (Tensor t2) = Tensor ts
      where
        ts = mapVect (uncurry addTensor) $ zipVect t2 t2

    addScalar :: Scalar -> Tensor m -> Tensor m
    addScalar s = mapTensor0 (+ s)    

    addScalarVect :: Vect n Scalar -> Tensor m -> Tensor m
    addScalarVect Empty ts = ts
    addScalarVect (Cons s ss) (Tensor (Cons (Scalar d) ts)) = Tensor $ Cons (Scalar $ s + d) $ mapVect (addScalarVect ss) ts
    addScalarVect ss (Tensor ts) = Tensor $ mapVect (addScalarVect ss) ts

    -- The Hadamard Multiplication:
    multScalar :: Tensor m -> Scalar -> Tensor m
    multScalar ts s = mapTensor0 (\d -> d * s) ts

    sqrtTensor :: Tensor m -> Tensor m
    sqrtTensor = mapTensor0 sqrt

    sum1 :: Tensor (Suc Zero) -> Scalar
    sum1 = foldlTensor1 (+) 0

    sumTensor :: Sing m -> Tensor (Suc m) -> Tensor m
    sumTensor m = foldlTensor m (+) 0

    ------------------
    -- Lines        --
    ------------------

    line :: Tensor m -> (Double, Double) -> Tensor m
    line x (w,b) = (flip addScalar) (x `multScalar` w) b

    ---------------------------
    -- Properties            --
    ---------------------------

    lawOfSum :: Sing m -> Tensor (Suc m) -> Bool
    lawOfSum m t = rank (sumTensor m t) == rank t - 1
