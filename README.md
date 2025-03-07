# Minimal skeleton for developing a new Agda backend 

- The backend is defined in `src/Main.hs`.
- The `test/` directory contains an example compilation of `Test.agda` to `Test.txt`:
    + `$ cabal run agda2min -- -i test/ -o test/ test/Test.agda`

