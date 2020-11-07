# Haskell Formatter

[![License](https://img.shields.io/github/license/evolutics/haskell-formatter.svg)](LICENSE)
[![Package](https://img.shields.io/hackage/v/haskell-formatter.svg)](https://hackage.haskell.org/package/haskell-formatter)

This is [**deprecated**](#deprecation-notice).

The Haskell Formatter formats Haskell source code. It is strict in that it fundamentally rearranges code.

## Deprecation notice

The Haskell Formatter is deprecated. If you think it should still be maintained, please let me know.

Take a look at the following projects, which aim at formatting Haskell code, too.

- [brittany](https://github.com/lspitzner/brittany)
- [hfmt](https://github.com/danstiner/hfmt)
- [hindent](https://github.com/mihaimaruseac/hindent)
- [stylish-haskell](https://github.com/jaspervdj/stylish-haskell)

Personally, I like **hindent,** to which you can migrate as follows.

| Use case                       | Haskell Formatter                                      | hindent                    |
| ------------------------------ | ------------------------------------------------------ | -------------------------- |
| Format file in-place           | `haskell-formatter --force --input a.hs --output a.hs` | `hindent a.hs`             |
| Format multiple files in-place | [Not supported out-of-the-box](#formatting-many-files) | `hindent a.hs b.hs`        |
| Format stdin to stdout         | `haskell-formatter`                                    | `hindent`                  |
| Order imports                  | Done by [default](#style-configuration)                | `hindent --sort-imports …` |
| Get help                       | `haskell-formatter --help`                             | `hindent --help`           |

## Installation

Install it by running

```bash
stack install haskell-formatter
```

or

```bash
cabal new-install haskell-formatter
```

You are ready when

```bash
haskell-formatter --help
```

works.

## Usage

### Basics

Read source code from `Input.hs`, format it, and write it to `Output.hs` by

```bash
haskell-formatter --input Input.hs --output Output.hs
```

If the input or output file is not given, it defaults to the corresponding standard stream. This allows commands like

```bash
haskell-formatter < Input.hs
```

To format a file in-place, use the `--force` option as in

```bash
# Warning: this overwrites the file `Code.hs`.
haskell-formatter --force --input Code.hs --output Code.hs
```

For more help about the usage, call

```bash
haskell-formatter --help
```

### Formatting Many Files

For a diff of how code in the current folder would be formatted, without actually changing anything, run

```bash
find . -name '*.hs' -type f -print0 \
  | xargs -0 -n 1 bash -c 'haskell-formatter < "$@" | diff -u "$@" -' --
```

The returned exit status is nonzero if there are unformatted files. This may be useful for continuous integration.

To format any `*.hs` files in a folder `code/` or (recursively) in its subfolders, run

```bash
# Warning: this overwrites files, so better back them up first.
find code/ -name '*.hs' -type f -print0 \
  | xargs -0 -I {} -n 1 haskell-formatter --force --input {} --output {}
```

### Style Configuration

The formatting style can be configured with a file referred by the `--style` option. For instance, the call

```bash
haskell-formatter --style my_style.yaml --input Input.hs --output Output.hs
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
  class: 8 # "class" and "instance" declarations.
  do: 3 # "do" notation.
  case: 4 # Body of "case" expressions.
  let: 4 # Declarations in "let" expressions.
  where: 6 # Declarations in "where" clauses.
  onside: 2 # Continuation lines which would otherwise be offside.

# Decides which parts of the code to sort.
order:
  # Sequence of import declarations.
  import_declarations: true

  # Entities of import lists.
  import_entities: true
```
