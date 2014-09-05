=================
Haskell Formatter
=================

Introduction
============

The Haskell Formatter formats Haskell source code. It is strict in that it fundamentally rearranges code.

By invoking ``make``, the utility is created as ``dist/build/haskell-formatter/haskell-formatter``.

Usage
=====

Basics
------

For example, source code is read from ``Input.hs``, formatted, and written to ``Output.hs`` by

::

    $ haskell-formatter --input Input.hs --output Output.hs

If the input or output file is not given, it defaults to the corresponding standard stream.

Style Configuration
-------------------

The formatting style can be configured with a file referred by the ``--style`` option. For instance, the call

::

    $ haskell-formatter --style my_style.yaml --input Input.hs --output Output.hs

uses ``my_style.yaml`` as a style file. Such a file generally follows the `YAML format <http://en.wikipedia.org/wiki/YAML>`_. The following is an `example style file <testsuite/resources/examples/default_style.yaml>`_, which at the same time shows the available keys with their default values.

.. GitHub does currently not allow to include files with the reST directive ``include`` (https://github.com/github/markup/issues/172).

   Thus, the file content is replicated here. There is a test which checks that the strings of both sources are equal.

.. code:: yaml

    # Lines should be no longer than this length in characters.
    line_length_limit: 80
    
    # How much to spread code over multiple lines instead of trying to fill a single
    # line. More precisely, this is the ratio of the ribbon (number of characters
    # on a line without leading and trailing whitespace) to "line_length_limit".
    # Only the lowest value of 1 forces "line_length_limit" to be applied strictly.
    # Reference: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.8777
    ribbons_per_line: 1
    
    # Indentation lengths in characters.
    indentations:
        class: 8     # "class" and "instance" declarations.
        do: 3        # "do" notation. 
        case: 4      # Body of "case" expressions.
        let: 4       # Declarations in "let" expressions.
        where: 6     # Declarations in "where" clauses.
        onside: 2    # Continuation lines which would otherwise be offside.

Help
----

For more help about the usage, call

::

    $ haskell-formatter --help

License
=======

See the `LICENSE file <LICENSE>`_.
