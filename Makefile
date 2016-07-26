

.PHONY : all

all : random-strings tests benchmarks readme-example docs




.PHONY : configure build test

configure : dist/setup-config

dist/setup-config :
	cabal configure --enable-tests

build :
	cabal build

test :
	cabal test



.PHONY : random-strings

random-strings : dist/setup-config dist/build/libHSrandom-strings-*.a

dist/build/libHSrandom-strings-*.a : src/*/*.hs
	cabal build random-strings


.PHONY : tests

tests : dist/build/test-basics/test-basics dist/build/test-randomness/test-randomness

dist/build/test-basics/test-basics : dist/build/libHSrandom-strings-*.a tests/test-basics.hs
	cabal build test-basics

dist/build/test-randomness/test-randomness : dist/build/libHSrandom-strings-*.a tests/test-randomness.hs
	cabal build test-randomness



.PHONY : benchmarks

benchmarks :



.PHONY : readme-example

readme-example : dist/build/readme-example/readme-example

dist/build/readme-example/readme-example : dist/build/libHSrandom-strings-*.a tests/readme-example.hs
	cabal build readme-example


.PHONY : docs

docs : dist/doc/html

dist/doc/html : src/*/*.hs
	cabal haddock






.PHONY : clean clean-all

clean :
	cabal clean

clean-all : clean
