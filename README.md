# pandoc-symreg: a pandoc-like tool to convert symbolic regression expressions to convenient formats.

[![github
release](https://img.shields.io/github/release/folivetti/pandoc-symreg.svg?label=current+release)](https://github.com/folivetti/pandoc-symreg/releases)
[![hackage
release](https://img.shields.io/hackage/v/pandoc-symreg.svg?label=hackage)](https://hackage.haskell.org/package/pandoc-symreg)
[![stackage LTS
package](https://stackage.org/package/pandoc-symreg/badge/lts)](https://www.stackage.org/lts/package/pandoc-symreg-types)
[![CI
tests](https://github.com/folivetti/pandoc-symreg/workflows/CI%20tests/badge.svg)](https://github.com/folivetti/pandoc-symreg/actions)
[![license](https://img.shields.io/badge/license-GPLv3+-lightgray.svg)](https://www.gnu.org/licenses/gpl.html)

## Conversion tool for Symbolic Regression tools

Pandoc-Symreg is a [Haskell](https://haskell.org) library and CLI inspired by [Pandoc](https://github.com/jgm/pandoc) for converting the output of Symbolic Regression tools to convenient formats for post analysis and documentation. It currently supports converting the output *from*

- `TIR` ([Transformation-Interaction-Rational Symbolic Regression](https://github.com/folivetti/tir))
- `HL` ([HeuristicLab](https://github.com/heal-research/HeuristicLab))
- `Operon` ([Operon](https://github.com/heal-research/operon))
- `Bingo` ([Bingo](https://github.com/nasa/bingo/tree/master/bingo))

And it can convert *to*

- `python` ([Numpy](https://numpy.org/doc/stable/index.html) expression)
- `math` (Plain math expression)
- `tikz` ([TikZ](https://tikz.net/) code to print a tree)
- `latex` ([LaTeX](https://www.latex-project.org/) equation)

## Installing

This tool can be installed via [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/). The easiest way to install is via Haskell stack:

- Install [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)
- Clone this repository
- Inside the project directory run the command `stack install`

## Usage

```bash
Usage: pandoc-symreg (-f|--from ['tir'|'hl'|'operon'|'bingo'])
                     (-t|--to ['python'|'math'|'tikz']) [-i|--input INPUT] 
                     [-o|--output OUTPUT] [-v|--varnames VARNAMES]

  Convert different symbolic expressions format to common formats()().

Available options:
  -f,--from ['tir'|'hl'|'operon'|'bingo']
                           Input expression format
  -t,--to ['python'|'math'|'tikz']
                           Output expression format
  -i,--input INPUT         Input file containing expressions. Empty string gets
                           expression from stdin. (default: "")
  -o,--output OUTPUT       Output file to store expressions. Empty string prints
                           expressions to stdout. (default: "")
  -v,--varnames VARNAMES   Comma separated list of variables names. Empty list
                           assumes the default of each algorithm (e.g, "x,y,epsilon").
                           (default: "")
  -h,--help                Show this help text
```

## Contributing

If you want to add support to your SR algorithm, have a look at the file [`src/PandocSR.hs`](src/PandocSR.hs) at the current parsers. You can either modify that file and make a Pull request or open an issue with the following informations:

- The name of your algorithm
- A list of supported univariate functions and their string representations
- A list of supported bivariate functions and their string representations
- A list of supported binary operators and their string representations. The string representation is sensitive to whether the operator is surrounded by space or not. See the source code for some examples.

Notice that we currently support a limited set of math functions and operators. See [SRTree](https://github.com/folivetti/srtree/blob/main/src/Data/SRTree/Internal.hs) for the current list. Please open an issue describing any other function that you want to be supported.

If you want to add support to other output formats. Please open an issue with a description of the format and a link to the official project of the format, if any.

Bug reports and feature requests are welcome.

## License

© 2023-2023 Fabricio Olivetti de Franca (fabricio.olivetti@gmail.com). Released under the
[GPL](https://www.gnu.org/licenses/old-licenses/gpl-3.0.html "GNU General Public License"),
version 3 or greater. This software carries no warranty of any kind.
(See COPYRIGHT for full copyright and warranty notices.)
