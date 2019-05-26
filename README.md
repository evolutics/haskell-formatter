# Haskell Formatter

The Haskell Formatter formats Haskell source code. It is strict in that it fundamentally rearranges code.

## Installation

Run

```
$ cabal install haskell-formatter
```

to create an executable, which is referred to as `haskell-formatter` in the following. On Linux, for example, you should now be able to call it as

```
$ ~/.cabal/bin/haskell-formatter --help
```

Alternatively, you can use Stack to build from source with

```
$ stack build
```

## Usage

### Basics

Read source code from `Input.hs`, format it, and write it to `Output.hs` by

```
$ haskell-formatter --input Input.hs --output Output.hs
```

If the input or output file is not given, it defaults to the corresponding standard stream. This allows commands like

```
$ haskell-formatter < Input.hs
```

For more help about the usage, call

```
$ haskell-formatter --help
```

### Style Configuration

The formatting style can be configured with a file referred by the `--style` option. For instance, the call

```
$ haskell-formatter --style my_style.yaml --input Input.hs --output Output.hs
```

uses `my_style.yaml` as a style file. Such files are in the [YAML format](http://en.wikipedia.org/wiki/YAML). The following is an [example style file](testsuite/resources/examples/default_style.yaml), which at the same time shows the available keys with their default values.

<!--- GitHub does currently not allow to include files (https://github.com/github/markup/issues/346).

Thus, the file content is replicated here. There is a test which checks that the strings of both sources are equal. --->

```yaml
# Lines should be no longer than this length in characters.
line_length_limit: 80

# How much to spread code over multiple lines instead of trying to fill a single
# line. More precisely, this guides the ratio of "line_length_limit" to the
# ribbon length (the number of characters on a line without leading and trailing
# whitespace). Only the lowest value of 1 forces "line_length_limit" to be
# applied strictly.
# Reference: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.8777
ribbons_per_line: 1

# More than this number of empty lines in succession are merged.
successive_empty_lines_limit: 1

# Indentation lengths in characters.
indentations:
    class:  8    # "class" and "instance" declarations.
    do:     3    # "do" notation. 
    case:   4    # Body of "case" expressions.
    let:    4    # Declarations in "let" expressions.
    where:  6    # Declarations in "where" clauses.
    onside: 2    # Continuation lines which would otherwise be offside.

# Decides which parts of the code to sort.
order:
    # Sequence of import declarations.
    import_declarations: true

    # Entities of import lists.
    import_entities: true
```

## Related Projects

You may like to have a look at the following projects, which aim at formatting Haskell code, too.

- [brittany](https://github.com/lspitzner/brittany)
- [hfmt](https://github.com/danstiner/hfmt)
- [hindent](https://github.com/chrisdone/hindent)
- [stylish-haskell](https://github.com/jaspervdj/stylish-haskell)
