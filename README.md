# jigsaw-sudoku

Jigsaw Sudoku written in Haskell

# Installation

## Prerequisites

Make sure [OpenGL](https://www.opengl.org/) and [GLUT](http://freeglut.sourceforge.net/) are installed.

- Windows & Mac: no installation required
- Linux: `sudo apt-get install build-essential libglu1-mesa-dev freeglut3-dev mesa-common-dev`

## Precompiled binaries

You are recommended to download binaries for 64-bit Windows, Mac, Linux in [Releases](https://github.com/patrickpang/jigsaw-sudoku/releases)

## Build from source

You can also build the game from source with a proper [stack](https://docs.haskellstack.org/) installation. The compilation process can take a long time.

1. `git clone https://github.com/patrickpang/jigsaw-sudoku.git`
2. `cd jigsaw-sudoku`
3. `stack setup`
4. `stack build`
5. `stack exec jigsaw-sudoku-exe`

# Metrics

`scc . --exclude-dir maps`

```
───────────────────────────────────────────────────────────────────────────────
Language                 Files     Lines   Blanks  Comments     Code Complexity
───────────────────────────────────────────────────────────────────────────────
Haskell                      9       425       80        45      300         28
Markdown                     2         4        1         0        3          0
YAML                         2       123       12        62       49          0
License                      1        30        6         0       24          0
gitignore                    1         3        0         0        3          0
───────────────────────────────────────────────────────────────────────────────
Total                       15       585       99       107      379         28
───────────────────────────────────────────────────────────────────────────────
Estimated Cost to Develop $9,753
Estimated Schedule Effort 2.641034 months
Estimated People Required 0.437469
───────────────────────────────────────────────────────────────────────────────
```
