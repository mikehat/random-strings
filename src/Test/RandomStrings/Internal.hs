
{- |
    module:         Test.RandomStrings.Internal

-}

module Test.RandomStrings.Internal
where


import           Data.Ratio
import           System.Random



-- | Flip a 'loaded' coin with a certain chance of being True
randomBool
    :: Rational     -- ^ range 0 to 1; chance of being true
    -> IO Bool      
randomBool r = do
    roll <- getStdRandom $ randomR (1,denominator r)
    return $ roll <= numerator r
