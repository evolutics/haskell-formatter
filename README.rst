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

uses ``my_style.yaml`` as a style file. Such a file generally follows the `YAML format <http://en.wikipedia.org/wiki/YAML>`_. The following is an `example style file <default_style.yaml>`_, which at the same time shows the available keys with their default values.

.. include:: default_style.yaml
   :literal:

Help
----

For more help about the usage, call

::

    $ haskell-formatter --help

License
=======

See the `LICENSE file <LICENSE>`_.
