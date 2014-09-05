=================
Haskell Formatter
=================

Introduction
============

The Haskell Formatter formats Haskell source code. It is strict in that it fundamentally rearranges code.

By invoking ``make``, the utility is created as `<dist/build/haskell-formatter/haskell-formatter>`_.

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

    line_length_limit: 80
    ribbons_per_line: 1
    indentations:
        class: 8
        do: 3
        case: 4
        let: 4
        where: 6
        onside: 2

Help
----

For more help about the usage, call

::

    $ haskell-formatter --help

License
=======

See the `LICENSE file <LICENSE>`_.
