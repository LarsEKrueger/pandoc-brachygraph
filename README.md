# Brachygraph - A pandoc filter to replace text

## Introduction

This module provides a filter for [pandoc](https://www.pandoc.org) to replace
text in the input document with other text from a pre-defined list. It can be
used to transliterate Unicode characters from combinations of ASCII characters.

For example, the markdown input

```
<<It's a dog,>> she said.
```

will become

>  «It's a dog,» she said.

if the digraphs (`<<` and `<<` in the example) have been defined for automatic
replacement.

Since the replacement text can be in any form that pandoc can parse, you can
ensure consistent formatting of certain term. Thus, the markdown input

```
A spokesperson of the UN said
```

can be rendered as

```
A spokesperson of the <span class="organisation">United Nations</span> said
```

The module can also be used for writing shorthand, hence the name.

The `+smart` extension of pandoc provides a limited functionality of this filter.

## Defining Replacements

Replacements are defined in the metadata block of your document. It doesn't
matter where you define those, at the top of the file or in a separate YAML
file. The processing, both by `pandoc` and `pandoc-brachygraph`, is the same.

All replacements must be placed in a variable `brachygraph`, which contains a
list of pairs. Each pair consists of the string to be replaced, followed by the
replacement.

Invalid pairs are ignored. A warning is written to standard output.

Characters in patterns that have a special purpose in markdown, e.g. `>`, must
be suitably escaped. In case of `>` and `<`, the HTML entities `&gt;` and
`&lt;` are best suited.

Replacements are searched top to bottom and the first match is taken. Thus,
brachygraphs that contain other brachygraphs need to be placed first.

```{=yaml}
---
brachygraph:
  - [ "&gt;&gt;&gt;", "›" ]
  - [ "&lt;&lt;&lt;", "‹" ]
  - [ "&gt;&gt;", "»"]
  - [ "&lt;&lt;", "«" ]
  - [ "UN", "[United Nations]{.organisation}"]
  - fail
---
```

## Installation

### Prerequisistes

* A working [Haskell installation](https://www.haskell.org/), either via
  [cabal](https://www.haskell.org/downloads/#platform) or
  [stack](https://www.haskell.org/downloads/#stack)

### Build and install

* Get the source code. Either
  * clone this repository or
  * download and extract the ZIP
* Go to the source code directory
* Run either
  * `cabal install` or
  * `stack install`

The installation might take some time due to the size and number of
dependencies required for the pandoc parser.

## Running the example(s)

Currently, one example is provided in `example/example1.md`.

The suggested way to run it is

```
pandoc -F pandoc-brachygraph --from markdown --to html example/example1.md
```

If everything worked as expected, it should display the output

```
<p>A spokesperson for the <span class="organisation">United Nations</span> — who wants to remain anonymous — said: «It’s a dog».</p>
<p>‹It isn’t,› they thought.</p>
```

on stdout, and

```
Cannot interpret brachygraph meta data: MetaInlines [Str "fail"]
```

on stderr.
