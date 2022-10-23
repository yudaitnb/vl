# VL

## Test
```bash
% stack run "Main.hs"
```

## Build essential
Haskell build tools need libGMP and gcc.
```bash
% sudo apt update
% sudo apt install libgmp-dev build-essential
```
Install GHC, stack, and cabal via [GHCup](https://www.haskell.org/ghcup/#).
```bash
% curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
...
% ghcup tui
- GHC 9.0.2
- stack 2.7.5
```

Finally install z3 solver.
```bash
% sudo apt install z3
```

If you need hspec to run the unittests, install libtinfo-dev.
```
sudo apt install libtinfo-dev
```

## Installation
TBD

## Compiler
TBD

