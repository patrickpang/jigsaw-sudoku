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
Haskell                     14       654      123        31      500         54
Plain Text                   3        38        8         0       30          0
Markdown                     2        52       14         0       38          0
YAML                         2       123       11        61       51          0
License                      1        30        6         0       24          0
gitignore                    1         3        0         0        3          0
───────────────────────────────────────────────────────────────────────────────
Total                       23       900      162        92      646         54
───────────────────────────────────────────────────────────────────────────────
Estimated Cost to Develop $17,074
Estimated Schedule Effort 3.267231 months
Estimated People Required 0.619034
───────────────────────────────────────────────────────────────────────────────
```
