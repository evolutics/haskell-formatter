MAIN = DependencyCheck
TEMPORARY_FOLDER = temporary

.PHONY : all clean

all : $(MAIN)
	mkdir $(TEMPORARY_FOLDER); cd $(TEMPORARY_FOLDER); \
		../$(MAIN) ../../haskell-formatter.cabal < ../dependencies_to_check | \
		tee --append ../dependencies_checked

$(MAIN) : $(MAIN).hs
	ghc $< -Wall -Werror
	hlint --hint ../HLint.hs $<

clean :
	$(RM) -r $(MAIN) $(MAIN).hi $(MAIN).o $(TEMPORARY_FOLDER)
