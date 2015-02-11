FORMATTED_FILES = Setup.hs src testsuite/src
FORMATTER_UTILITY = dist/build/haskell-formatter/haskell-formatter
FORMATTER_ARGUMENTS = --force --input {} --output {}
GENERATED_FILES = dist README.xhtml

.PHONY : all build test document format sandbox clean

all : build test document format

build :
	cabal install --only-dependencies --enable-tests -j
	cabal configure --enable-tests
	cabal build -j

test : build
	cabal test -j

document : build README.xhtml
	cabal haddock --internal

%.xhtml : %.rst
	rst2html --strict $< $@

format : build
#	Use "xargs" instead of "-exec", since
#	1. the call should fail if any "-exec" fails and
#	2. the behavior of multiple "{}" is undefined for a standard "find".
	find $(FORMATTED_FILES) -type f -name '*.hs' -print0 | \
		xargs -n 1 -0 -I {} $(FORMATTER_UTILITY) $(FORMATTER_ARGUMENTS)

sandbox :
	cabal sandbox init
	$(MAKE) build

clean :
	$(RM) -r $(GENERATED_FILES)
	cabal sandbox delete
