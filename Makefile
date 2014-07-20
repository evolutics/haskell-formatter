CHECKED_FILES = Setup.hs src testsuite
FORMATTED_FILES = Setup.hs src testsuite/src
FORMATTER_UTILITY = dist/build/haskell-formatter/haskell-formatter
FORMATTER_ARGUMENTS = --force --input {} --output {}
GENERATED_FILES = dist

all:
	hlint $(CHECKED_FILES)

	cabal configure --enable-tests
	cabal build

#	Use "xargs" instead of "-exec", since
#	1. the call should fail if any "-exec" fails and
#	2. the behavior of multiple "{}" is undefined for a standard "find".
	find $(FORMATTED_FILES) -type f -name '*.hs' -print0 | \
		xargs -n 1 -0 -I {} $(FORMATTER_UTILITY) $(FORMATTER_ARGUMENTS)

	cabal test

clean:
	rm -fr $(GENERATED_FILES)
