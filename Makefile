#Author: Thorsten Rangwich
#See file LICENSE for details.
#
#This is a quick and dirty solution. Generation should change to cabal in the future or
#at least will be build using cabal.

HS_VERSION=$(shell ghc --numeric-version | sed -e 's/^6.*/h98/' -e 's/7.0.*/h98/' -e 's/^7.4.*/h2010/' -e 's/^[0-9].*.*/h0/')


#This file magic is for cleanup and documentation, not needed else currently
DEMO_HS=demo_*.hs grill.hs
DEMO_EXE=$(DEMO_HS:.hs=) $(DEMO_HS:.hs=.exe)
TEST_HS=test_*.hs
TEST_EXE=$(TEST_HS:.hs=) $(TEST_HS:.hs=.exe)
EXCLUDE_HS=$(DEMO_HS) $(TEST_HS)
EXCLUDE_HS_RESTRICTED=$(DEMO_HS) $(TEST_HS) CommandLine.hs
#From file1 file2 file3 --> file1 -o -name file2 -o -name file3
EXCLUDE_HS_FIND_PARAM=$(patsubst %,-o -name %,$(EXCLUDE_HS))
EXCLUDE_HS_RESTRICTED_FIND_PARAM=$(patsubst %,-o -name %,$(EXCLUDE_HS_RESTRICTED))
HS_SRC=$(shell cd src && find . -name \*.hs -a \! \( -name .\* $(EXCLUDE_HS_FIND_PARAM) \))
HS_RESTRICTED_SRC=$(shell cd src && find . -name \*.hs -a \! \( -name .\* $(EXCLUDE_HS_RESTRICTED_FIND_PARAM) \) | grep -v ./Compat/ )

HS_OBJ=$(HS_SRC:.hs=.o)
HS_HI=$(HS_SRC:.hs=.hi)


.SUFFIXES:
.PHONY: doc test demo prerequisites

#Neither correct nor used...
#%.hi:%.hs
#	ghc -xyz $< -o $@


# Just let ghc create one executable depending on all source files. Quick hack, will change in the future.
all: prerequisites
	ghc --make -Wall -isrc -isrc/Compat/$(HS_VERSION) demo/demo_calc_trees.hs
	ghc --make -Wall -isrc -isrc/Compat/$(HS_VERSION) demo/demo_display_sheet.hs
	ghc -Wall -Wall -isrc -isrc/Compat/$(HS_VERSION) -O --make src/Console/grill.hs -o grill

prerequisites:
	ghc --version >/dev/null 2>& 1



# Build via cabal. Thanks to Christian for that.
build:
	runhaskell Setup configure
	runhaskell Setup build

#Run test suites
test:
	runghc -isrc test/functional/test_EvalTrees.hs

#Run demonstrations
demo: prerequisites
#Mac shell does not like she-bang if the script is not compatible to bourne shell. So call runghc directly.
	runghc -isrc -isrc/Compat/$(HS_VERSION) demo/demo_calc_trees.hs
	runghc -isrc -isrc/Compat/$(HS_VERSION) demo/demo_display_sheet.hs TestData/demo_sheet.gst

#Check syntax of all files using hlint
hlint:
	hlint .

clean:
	cd src && rm -f $(HS_OBJ) $(HS_HI)
	rm -f src/Console/grill.hi src/Console/grill.o
	rm -f demo/*.hi demo/*.o
	runhaskell Setup clean

doc:
	cd src && haddock -o ../doc -h $(HS_RESTRICTED_SRC)

clean_doc:
	rm -f doc/*

mrproper: clean clean_doc
	rm -f $(DEMO_EXE)
	rm -f grill
