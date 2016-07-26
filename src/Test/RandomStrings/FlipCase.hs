


{- | 

module      : Test.RandomStrings.FlipCase
description : Randomly flip case of strings

A helper for randomly changing the case in strings. Useful for generating
test-cases for case-insensitive matching.

-}

module Test.RandomStrings.FlipCase
    ( flipCase
    , flipCaseString
    , randomFlipCase
    , randomFlipCaseString
    )
where


import           Data.Bool ( bool )
import           Data.Char
import           Data.Ratio
import           Control.Monad
import           System.Random

import           Test.RandomStrings.Internal



-- | Toggle the case of a @Char@.
flipCase :: Char -> Char
flipCase c
    | isUpper c = toLower c
    | isLower c = toUpper c
    | otherwise = c


-- | Toggle character case for a @String@.
flipCaseString :: String -> String
flipCaseString = map flipCase


-- | Randomly flip the case of a @Char@.
randomFlipCase
    :: Rational     -- ^ range 0 to 1; chance of flipping case
    -> Char         -- ^ @Char@ to flip
    -> IO Char
randomFlipCase r c = randomBool r >>= bool (return c) (return $ flipCase c)


-- | Randomly flip the case of a @String@.
randomFlipCaseString
    :: Rational     -- ^ range 0 to 1; chance of flipping case
    -> String       -- ^ original @String@
    -> IO String
randomFlipCaseString r = mapM (randomFlipCase r)

