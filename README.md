# plutonk
A plutus plonk verifier. 

This repository will follow [this](https://github.com/iquerejeta/dummy_plonk) repository (actually [this](https://github.com/perturbing/dummy_plonk) particular fork). In light of this, this repository will only contain an onchain POC verifier (this is experimental work), no proofs are generated in this repository.

# To run
This repository can be run via nix, to enter a dev shell use `nix develop`. In particular, these executables are of particular interest:

1. `nix run .#plutus-plonk:test:run-vector-test`  to check the implementations against the test vectors.
2. `nix run .#plutus-plonk:bench:run-bench` to compile the fast implementation to UPLC (`.flat`) for further processing.
3. `nix run .#plutus-benchmark:exe:bench-exe` to calculate the cpu/mem units of the tests in the plutus-benchmark package.

# Benchmark
```bash
Run fast vanilla plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    3263  (19.9%)      3201547262  (32.0%)          228031   (1.6%)
```
