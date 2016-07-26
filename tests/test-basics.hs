
{-

These are some simple tests that may seem so obvious they shouldn't be
necessary. There is nothing here to verify proper randomness, just that the
mechanics like string lengths and character classes are working properly.

-}

module Main ( main ) where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad.State
import           System.IO
import           System.Exit

import           Data.Char

import           Test.RandomStrings



extraTests = stdArgs { maxSuccess = 1000 }

main = do
    success <- flip execStateT True $ do
        liftTest "randomASCII"      extraTests prop_randomASCII
        liftTest "randomChar8"      extraTests prop_randomChar8
        liftTest "randomAlphaChar"  extraTests prop_randomAlphaChar
        liftTest "randomAlphaChar8" extraTests prop_randomAlphaChar8
        liftTest "randomAlphaASCII" extraTests prop_randomAlphaASCII
        liftTest "randomUpperASCII" extraTests prop_randomUpperASCII
        liftTest "randomWord"       stdArgs    prop_randomWord
        liftTest "randomStrings"    stdArgs    prop_randomStrings
        liftTest "randomStringsLen" stdArgs    prop_randomStringsLen

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

is_ascii :: Char -> Bool
is_ascii = in_range (0,127) . fromEnum

is_char8 :: Char -> Bool
is_char8 = in_range (0,255) . fromEnum


prop_randomASCII :: Property
prop_randomASCII = monadicIO $ run randomASCII >>= assert . is_ascii

prop_randomChar8 :: Property
prop_randomChar8 = monadicIO $ run randomChar8 >>= assert . is_char8

prop_randomAlphaChar :: Property
prop_randomAlphaChar = monadicIO $ run (onlyAlpha randomChar) >>= assert . isAlpha

prop_randomAlphaChar8 :: Property
prop_randomAlphaChar8 = monadicIO $ run (onlyAlpha randomChar8) >>= assert . isAlpha

prop_randomAlphaASCII :: Property
prop_randomAlphaASCII = monadicIO $ run (onlyAlpha randomASCII) >>= assert . isAlpha

prop_randomUpperASCII :: Property
prop_randomUpperASCII = monadicIO $ run (onlyUpper randomASCII) >>= assert . isUpper

prop_randomWord :: Int -> Property
prop_randomWord len = monadicIO $ run (randomWord randomChar8 $ len') >>= assert . (== len') . length
    where len' = len `mod` 500 -- max string length in 0 to 499, please

prop_randomStrings :: Int -> Int -> Property
prop_randomStrings str_len list_len = monadicIO $ do
    let str_len' = str_len `mod` 100
        list_len' = list_len `mod` 200
    strings <- run $ randomStrings (randomString randomChar str_len') list_len'

    -- all strings are the right length
    assert $ all (== str_len') $ map length strings

    -- the list is the right length
    assert $ length strings == list_len'

prop_randomStringsLen :: (Int,Int) -> Int -> Property
prop_randomStringsLen (min_len,max_len) list_len = monadicIO $ do
    let min_len' = min_len `mod` 100
        max_len' = max_len `mod` 100
        list_len' = list_len `mod` 200
        limits' = (min_len',max_len')
    pre $ min_len' <= max_len'
    strings <- run $ randomStringsLen (randomString randomChar) limits' list_len'

    -- all strings length are within the range
    assert $ all (in_range limits') $ map length strings

    -- the list is the right length
    assert $ length strings == list_len'

