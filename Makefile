FORMATTED_FILES = Setup.hs src testsuite/src
FORMATTER_UTILITY = stack run --
FORMATTER_ARGUMENTS = --force --input {} --output {}

.PHONY : format

format :
#	Use "xargs" instead of "-exec", since
#	1. the call should fail if any "-exec" fails and
#	2. the behavior of multiple "{}" is undefined for a standard "find".
	find $(FORMATTED_FILES) -type f -name '*.hs' -print0 | \
		xargs -n 1 -0 -I {} $(FORMATTER_UTILITY) $(FORMATTER_ARGUMENTS)
