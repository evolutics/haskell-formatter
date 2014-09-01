=================
Haskell Formatter
=================

Introduction
============

The Haskell Formatter formats Haskell source code. It is strict in that it fundamentally rearranges code.

By invoking ``make``, the utility is created as `<dist/build/haskell-formatter/haskell-formatter>`_.

Usage
=====

For example, source code is read from ``Input.hs``, formatted, and written to ``Output.hs`` by

::

    $ haskell-formatter --input Input.hs --output Output.hs

If the input or output file is not given, it defaults to the corresponding standard stream.

For more help about the utility, call

::

    $ haskell-formatter --help

License
=======

See the `LICENSE file <LICENSE>`_.
