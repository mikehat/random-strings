

{-

These tests are not the most correct tests of randomness and are not intended
to put the random number generator in the spotlight. The hope is that any gross
coding error in this library will be caught.

If there are some more rigorous tests that are easy to implement without adding
too many dependencies, please send suggestions. Even a more mathematical
approach to static numbers for test repetitions and failure limits would be a
welcome improvement. I'm not about to drag out my moldy old textbooks and do
it myself. I'd certainly rather do that than my day job ...

-}

module Main ( main ) where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad
import           Control.Monad.State
import           System.IO
import           System.Exit

import           Data.Char
import           Data.Ord ( max )
import           Data.Ratio ( (%) )
import qualified Data.Map.Strict as M

import           Test.RandomStrings
import           Test.RandomStrings.FlipCase


extraTests = stdArgs { maxSuccess = 1000 }


main = do
    success <- flip execStateT True $ do
        liftTest "randomAlpha'"     stdArgs prop_randomAlpha'
        liftTest "randomString'"    stdArgs prop_randomString'
        liftTest "randomStringsLen" stdArgs prop_randomStringsLen
        liftTest "randomFlipCase"   stdArgs prop_randomFlipCase
    if success then exitSuccess else exitFailure

indent = replicate 4 ' '

liftTest :: (Testable prop) => String -> Args -> prop -> StateT Bool IO ()
liftTest name args prop = do
    liftIO $ putStr $ take 25 $ concat [ indent , name , ":" , repeat ' ' ]
    r <- liftIO $ quickCheckWithResult args prop
    case r of Success {} -> return ()
              _          -> put False


in_range :: (Ord a) => (a,a) -> a -> Bool
in_range (lbound,ubound) a = a >= lbound && a <= ubound



prop_randomAlpha' :: Property
prop_randomAlpha' = monadicIO $ do
    sample <- liftIO $ replicateM 1000 $ onlyAlpha' (1%10) randomChar8

    -- all characters are alphabetic
    assert $ all isAlpha sample

    -- there are not too many upper-case chars
    assert $ (length $ filter isUpper sample) < 200 -- should be around 100

    -- to single char is too frequent
    let freq = foldr (\c -> M.insertWith (+) c 1) M.empty sample
        max_freq = M.foldl max 0 freq
    assert $ max_freq < 60 -- why? made it up. Average max is around 30


prop_randomString' :: Property
prop_randomString' = monadicIO $ do
    sample <- liftIO $ randomString' randomASCII (9%10) (1%10) 1000

    -- there are about the right number of alphabetic chars
    assert $ (length $ filter isAlpha sample) > 800 -- should be around 900

    -- there are the right number of upper-case chars
    assert $ (length $ filter isUpper sample) < 180 -- should be around 90

    -- no single char appears too often
    let freq = foldr (\c -> M.insertWith (+) c 1) M.empty sample
        max_freq = M.foldl max 0 freq
    assert $ max_freq < 80 -- why? made it up. Average max around 40


prop_randomStringsLen :: Property
prop_randomStringsLen = monadicIO $ do
    samples <- liftIO $ randomStringsLen (randomWord randomChar) (5,95) 1000

    -- strings are the right length
    assert $ all (in_range (5,95) . length ) samples

    -- string lengths have a nice distribution
    let freq = foldr (\c -> M.insertWith (+) c 1) M.empty $ map length samples
        max_freq = M.foldl max 0 freq
    assert $ max_freq < 40 -- why? made it up. Average max around 20
    
prop_randomFlipCase :: Property
prop_randomFlipCase = monadicIO $ do
    original <- run $ randomWord (onlyUpper randomASCII) 100
    sample <- run $ randomFlipCaseString (1%2) original

    -- correct length
    assert $ length sample == 100

    -- only case-flipping happened
    assert $ (map toLower original) == (map toLower sample)

    -- about the right number of chars are flipped
    -- liftIO $ putStrLn $ show $ length $ filter isLower sample
    assert $ in_range (30,70) $ length $ filter isLower sample
