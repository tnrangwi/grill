#Author: Thorsten Rangwich
#See file LICENSE for details.
#
#This is a quick and dirty solution. Generation should change to cabal in the future or
#at least will be build using cabal.

#This file magic is for cleanup and documentation, not needed else currently

EXCLUDE_HS=demo_*.hs
#From file1 file2 file3 --> file1 -o -name file2 -o -name file3
EXCLUDE_HS_FIND_PARAM=$(patsubst %,-o -name %,$(EXCLUDE_HS))
HS_SRC=$(shell find . -name \*.hs -a \! \( -name .\* $(EXCLUDE_HS_FIND_PARAM) \))
HS_OBJ=$(HS_SRC:.hs=.o)
HS_HI=$(HS_SRC:.hs=.hi)


.SUFFIXES:
.PHONY: doc test

#Neither correct nor used...
#%.hi:%.hs
#	ghc -xyz $< -o $@


# Just let ghc create one executable depending on all source files. Quick hack, will change in the future.
all:
	ghc --make demo/demo_calc_trees.hs

#Run test suites
test:
	demo/demo_calc_trees.hs
	demo/display_sheet.hs TestData/demo_sheet.gst

#Check syntax of all files using hlint
hlint:
	hlint .

clean:
	rm -f $(HS_OBJ) $(HS_HI)
	rm -f demo/*.hi demo/*.o

doc:
	haddock -o doc -h $(HS_SRC)

clean_doc:
	rm -f doc/*

mrproper: clean clean_doc
	rm -f demo/demo_calc_trees
