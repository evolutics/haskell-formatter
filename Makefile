SOURCE_FILES = Setup.hs src testsuite
GENERATED_FILES = dist

all:
	hlint $(SOURCE_FILES)
	cabal configure --enable-tests
	cabal build
	cabal test

clean:
	rm -fr $(GENERATED_FILES)
