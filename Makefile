#Author: Thorsten Rangwich
#See file LICENSE for details.
#Ugly makefile. This is a start and should be changed to something else later.

.SUFFIXES=
#.PHONY=

HS_SRC=`find . -name \*.hs`

all:
	ghc --make test.hs

test:
	./test.hs

documentation:
	haddock -o doc -h ${HS_SRC}
