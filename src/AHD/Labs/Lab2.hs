{-# OPTIONS_GHC -Wno-unused-imports #-}

module AHD.Labs.Lab2 where

-- intentionally imported so that one can use the helper function from clashi
import AHD.Util
import Clash.Prelude

-- | A 3-bit adder built using `Vec`
--
-- The adder computes the following sum \(a + b = (a_2 a_1 a_0)_2 + (b_2 b_1 b_0)_2 = (c, s_2, s_1, s_0)\).
--
-- ===Example
--
-- > clashi> threeBitAdder (1 :> 0 :> 0 :> Nil) (0 :> 0 :> 1 :> Nil)
-- > 0 :> 1 :> 0 :> 1 :> Nil
-- >
-- > clashi> threeBitAdder (1 :> 1 :> 1 :> Nil) (0 :> 0 :> 1 :> Nil)
-- > 1 :> 0 :> 0 :> 0 :> Nil
threeBitAdder ::
  -- | The number \(a\)
  Vec 3 Bit ->
  -- | The number \(b\)
  Vec 3 Bit ->
  -- | The sum \(a+b\)
  Vec 4 Bit
threeBitAdder a b = nBitAdder a b

-- | A generic \(n\)-bit adder
--
-- ===Example
--
-- > clashi> nBitAdder (1 :> 0 :> 0 :> Nil) (0 :> 0 :> 1 :> Nil)
-- > 0 :> 1 :> 0 :> 1 :> Nil
-- >
-- > clashi> nBitAdder (1 :> 1 :> 1 :> Nil) (0 :> 0 :> 1 :> Nil)
-- > 1 :> 0 :> 0 :> 0 :> Nil
-- >
-- > clashi> nBitAdder (1 :> 1 :> 1 :> 1 :> Nil) (1 :> 0 :> 0 :> 1 :> Nil)
-- > 1 :> 1 :> 0 :> 0 :> 0 :> Nil
nBitAdder ::
  -- | The number \(a\)
  Vec n Bit ->
  -- | The number \(b\)
  Vec n Bit ->
  -- | The sum \(a+b\)
  Vec (n + 1) Bit
nBitAdder a b = cOut :> s
  where
    tuples = zip a b

    (cOut, s) = mapAccumR helper 0 tuples

    helper cIn (a', b') = fullAdder a' b' cIn

    fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
    fullAdder a' b' cIn' = (cOutFA, sFA)
      where
        sFA = a' `xor` b' `xor` cIn'
        cOutFA = (cIn' .&. (a' `xor` b')) .|. (a' .&. b')

-- | Ensure that values in a vector do not exceed a threshold value \(t\)
--
-- ===Example
--
-- > clashi> capAt 3 (1 :> 2 :> 3 :> 4 :> 5 :> Nil)
-- > 1 :> 2 :> 3 :> 3 :> 3 :> Nil
-- >
-- > clashi> capAt 5 (1 :> 2 :> 3 :> 4 :> 5 :> Nil)
-- > 1 :> 2 :> 3 :> 4 :> 5 :> Nil
-- >
-- > clashi> capAt 0 (1 :> 2 :> 3 :> 4 :> 5 :> Nil)
-- > 0 :> 0 :> 0 :> 0 :> 0 :> Nil
capAt ::
  -- | Threshold \(t\)
  Unsigned 8 ->
  -- | The vector to cap
  Vec n (Unsigned 8) ->
  Vec n (Unsigned 8)
capAt t = map (min t)

-- | Determines the minimal and maximal value in a vector
--
-- ===Example
--
-- > clashi> minMax (1 :> 2 :> 3 :> 4 :> 5 :> Nil)
-- > (1,5)
-- >
-- > clashi> minMax (5 :> 4 :> 3 :> 2 :> 3 :> 4 :> 5 :> Nil)
-- > (2,5)
-- >
-- > clashi> minMax (5 :> 5 :> 5 :> Nil)
-- > (5,5)
-- >
-- > clashi> minMax (5 :> Nil)
-- > (5,5)
minMax ::
  -- | The vector of values \(v\)
  Vec n (Unsigned 8) ->
  -- | The tuple \((\min v, \max v)\)
    (Unsigned 8, Unsigned 8)
minMax = foldr (\val (minAcc, maxAcc) -> (min minAcc val, max maxAcc val)) (255, 0)

-- $setup
-- >>> import Clash.Prelude
