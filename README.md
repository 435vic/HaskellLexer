# HaskellLexer - Lexer for arithmetic operations written in Haskell

HaskellLexer is a proof-of-concept lexer that uses DFAs to parse and tokenize a string.

## Installation

First, make sure that GHCup is installed on your system. Follow the instructions at https://www.haskell.org/ghcup/install/ for more information.

While testing installation and building in my WSL Ubuntu system, I ran into an error that was only fixed by installing `libgmp3-dev`. If you get a linker error, I suggest installing this package through `apt`.

Next, clone this repository:
```
git clone https://github.com/435vic/HaskellLexer && cd HaskellLexer
```

Then, download and install the required Haskell dependencies:
```
cabal update
```

Finally, you can build and run the project, specifying a file to parse into AST:

```
cabal run lexer -- -f test/program1.txt
```
