FORMATTED_FILES = Setup.hs src testsuite/src
FORMATTER_UTILITY = dist/build/haskell-formatter/haskell-formatter
FORMATTER_ARGUMENTS = --force --input {} --output {}
GENERATED_FILES = dist README.xhtml

all:
	cabal configure --enable-tests
	cabal build

	cabal test

	cabal haddock --internal
	rst2html --strict README.rst README.xhtml

#	Use "xargs" instead of "-exec", since
#	1. the call should fail if any "-exec" fails and
#	2. the behavior of multiple "{}" is undefined for a standard "find".
	find $(FORMATTED_FILES) -type f -name '*.hs' -print0 | \
		xargs -n 1 -0 -I {} $(FORMATTER_UTILITY) $(FORMATTER_ARGUMENTS)

clean:
	rm -fr $(GENERATED_FILES)
