.PHONY : format

format :
#	Use "xargs" instead of "-exec", since
#	1. the call should fail if any "-exec" fails and
#	2. the behavior of multiple "{}" is undefined for a standard "find".
	find Setup.hs src testsuite/src -type f -name '*.hs' -print0 | \
		xargs -n 1 -0 -I {} stack run -- --force --input {} --output {}
