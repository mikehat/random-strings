

module Main ( main ) where

import Test.RandomStrings

iso_alpha = onlyAlpha randomChar8
ascii_alphanum = onlyAlphaNum randomASCII

main = do
    words <- randomStringsLen (randomString ascii_alphanum) (5,25) 30
    mapM_ putStrLn words

