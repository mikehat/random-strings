

## `random-strings-0.1.1.0`

A simple way to generate random strings for testing and benchmarking.

A purely random character string is not always the best test case for testing
and benchmarking. These modules help generate random strings with preferences
for character sets and character properties.

Example:

    module Main ( main ) where

    import Test.RandomStrings

    iso_alpha = onlyAlpha randomChar8
    ascii_alphanum = onlyAlphaNum randomASCII

    -- print a list of 30 random alphanumeric strings between 5 and 25
    -- chars long.

    main = do
        words <- randomStringsLen (randomString ascii_alphanum) (5,25) 30
        mapM_ putStrLn words


Build the example with `cabal build readme-example`.

Functions allow tuning strings for character class and toying with the
distribution of alphabetic and upper/lower-case characters.

