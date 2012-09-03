-- | Helper for writing tests for numeric code
module Test.QuickCheck.Numeric (
    -- * Approximate equality
    eq
  , eqC
    -- * Function monotonicity
  , Monotonicity(..)
  , monotonicFunction
  , monotonicFunctionIEEE
    -- * Inverse function
  , checkInverse
  , checkInverse2
  ) where

import Data.Complex
import qualified Numeric.IEEE    as IEEE




----------------------------------------------------------------
-- Approximate equality
----------------------------------------------------------------

-- | Approximate equality for 'Double'. Doesn't work well for numbers
--   which are almost zero.
eq :: Double                    -- ^ Relative error
   -> Double -> Double -> Bool
eq eps a b 
  | a == 0 && b == 0 = True
  | otherwise        = abs (a - b) <= eps * max (abs a) (abs b)

-- | Approximate equality for 'Complex Double'
eqC :: Double                   -- ^ Relative error
    -> Complex Double
    -> Complex Double
    -> Bool
eqC eps a@(ar :+ ai) b@(br :+ bi)
  | a == 0 && b == 0 = True
  | otherwise        = abs (ar - br) <= eps * d
                    && abs (ai - bi) <= eps * d
  where
    d = max (realPart $ abs a) (realPart $ abs b)



----------------------------------------------------------------
-- Function monotonicity
----------------------------------------------------------------

-- | Function monotonicity type.
data Monotonicity
  = StrictInc   -- ^ Strictly increasing function
  | MonotoneInc -- ^ Monotonically increasing function
  | StrictDec   -- ^ Strictly decreasing function
  | MonotoneDec -- ^ Monotonically decreasing function
  deriving (Show,Eq,Ord)


-- | Check that function is nondecreasing. For floating point number
--   it may give spurious failures so 'monotonicFunction'
--   should be used in this case.
monotonicFunction :: (Ord a, Ord b) => Monotonicity -> (a -> b) -> a -> a -> Bool
monotonicFunction cmp f x1 x2
  = f (min x1 x2) `op` f (max x1 x2)
  where
    op = case cmp of
           StrictInc   -> (< )
           MonotoneInc -> (<=)
           StrictDec   -> (> )
           MonotoneDec -> (>=)

-- | Check that function is nondecreasing taking rounding errors into
--   account. This function makes no distinction between strictly
--   increasing function and monotonically increasing function since
--   distinction is pointless for floating point.
--
--   In fact funstion is allowed to decrease less than one ulp in order
--   to guard againist problems with excess precision. On x86 FPU works
--   with 80-bit numbers but doubles are 64-bit so rounding happens
--   whenever values are moved from registers to memory
monotonicFunctionIEEE :: (Ord a, IEEE.IEEE b) => Monotonicity -> (a -> b) -> a -> a -> Bool
monotonicFunctionIEEE cmp f x1 x2
  =  y1 `op` y2
  || abs (y1 - y2) < abs (y2 * IEEE.epsilon)
  where
    y1 = f (min x1 x2)
    y2 = f (max x1 x2)
    op = case cmp of
           StrictInc   -> (<=)
           MonotoneInc -> (<=)
           StrictDec   -> (>=)
           MonotoneDec -> (>=)


----------------------------------------------------------------
-- Function and its inverse
----------------------------------------------------------------

-- | Check that function is inverse. Breaks down near zero.
checkInverse
  :: (Double -> Double) -- ^ Function @f(x)@
  -> (Double -> Double) -- ^ Inverse function @g@, @g(f(x)) = x@
  -> (Double -> Double) -- ^ Derivative of function @f(x)@
  -> Double             -- ^ Relative error for
                        --   @f(x)@. Usually is machine epsilon.
  -> Double             -- ^ Relative error for inverse function
                        --   @g(x)@. Usually is machine epsilon.
  -> Double -> Bool
checkInverse f invF f' eps eps' x
  = x ~= invF y
  where
    (~=) = eq (eps' + abs (y / f' x * eps))
    y    = f x


-- | Check that function is inverse. Breaks down near zero.
checkInverse2
  :: (Double -> Double) -- ^ Function @f(x)@
  -> (Double -> Double) -- ^ Inverse function @g@, @g(f(x)) = x@
  -> (Double -> Double) -- ^ Derivative of function @g(x)@
  -> Double             -- ^ Relative error for
                        --   @f(x)@. Usually is machine epsilon.
  -> Double             -- ^ Relative error for inverse function
                        --   @g(x)@. Usually is machine epsilon.
  -> Double -> Bool
checkInverse2 f invF invF' eps eps' x
  = x ~= invF y
  where
    (~=) = eq (eps' + abs (y * (invF' y * eps)))
    y    = f x


