# plutonk
A plutus plonk verifier. 

This repository will follow [this](https://github.com/iquerejeta/dummy_plonk) repository (actually [this](https://github.com/perturbing/dummy_plonk) particular fork). In light of this, this repository will only contain an onchain POC verifier (this is experimental work), no proofs are generated in this repository.

# To run
This repository can be run via nix, to enter a dev shell use `nix develop`. In particular, these executables are of particular interest:

1. `nix run .#plutus-plonk:test:run-vector-test`  to check the implementations against the test vectors.
2. `nix run .#plutus-plonk:bench:run-bench` to compile the fast implementation to UPLC (`.flat`) for further processing.
3. `nix run .#plutus-benchmark:exe:bench-exe` to calculate the cpu/mem units of the tests in the plutus-benchmark package.

# Benchmark of plonk
```bash
Run fast vanilla plonk verifier

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    -    3304  (20.2%)      3490871390  (34.9%)          226107   (1.6%) 
```
# Benchmark of field implementation
These operations are over the scalar field of bls12-381 (255 bit prime field)
```bash
n scalars additions (size 31 bytes)

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    0      97   (0.6%)          897100   (0.0%)            4000   (0.0%) 
  400   16191  (98.8%)       570766300   (5.7%)         1326400   (9.5%) 
  800   32281 (197.0%)      1140635500  (11.4%)         2648800  (18.9%) 
  1200   48363 (295.2%)      1710504700  (17.1%)         3971200  (28.4%) 
  1600   64452 (393.4%)      2280373900  (22.8%)         5293600  (37.8%) 
  2000   80537 (491.6%)      2850243100  (28.5%)         6616000  (47.3%) 
  2400   96632 (589.8%)      3420112300  (34.2%)         7938400  (56.7%) 
  2800  112731 (688.1%)      3989981500  (39.9%)         9260800  (66.1%) 
  3200  128795 (786.1%)      4559850700  (45.6%)        10583200  (75.6%) 
  3600  144897 (884.4%)      5129719900  (51.3%)        11905600  (85.0%) 
  4000  160977 (982.5%)      5699589100  (57.0%)        13228000  (94.5%) 


n scalars multiplications (size 31 bytes)

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    0      97   (0.6%)          897100   (0.0%)            4000   (0.0%) 
  400   16182  (98.8%)       553777719   (5.5%)         1328794   (9.5%) 
  800   32263 (196.9%)      1106696919  (11.1%)         2653594  (19.0%) 
  1200   48359 (295.2%)      1659616119  (16.6%)         3978394  (28.4%) 
  1600   64438 (393.3%)      2212535319  (22.1%)         5303194  (37.9%) 
  2000   80551 (491.6%)      2765454519  (27.7%)         6627994  (47.3%) 
  2400   96616 (589.7%)      3318373719  (33.2%)         7952794  (56.8%) 
  2800  112716 (688.0%)      3871292919  (38.7%)         9277594  (66.3%) 
  3200  128807 (786.2%)      4424212119  (44.2%)        10602394  (75.7%) 
  3600  144886 (884.3%)      4977131319  (49.8%)        11927194  (85.2%) 
  4000  160976 (982.5%)      5530050519  (55.3%)        13251994  (94.7%) 


n scalar exponentiation with exponent of size 32 bytes 

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    0     262   (1.6%)         1863100   (0.0%)            8200   (0.1%) 
    1     303   (1.8%)       963635367   (9.6%)         1942711  (13.9%) 
    2     343   (2.1%)      1925407634  (19.3%)         3877222  (27.7%) 
    3     383   (2.3%)      2887179901  (28.9%)         5811733  (41.5%) 
    4     423   (2.6%)      3848952168  (38.5%)         7746244  (55.3%) 
    5     464   (2.8%)      4810724435  (48.1%)         9680755  (69.1%) 
    6     503   (3.1%)      5772496702  (57.7%)        11615266  (83.0%) 
    7     544   (3.3%)      6734268969  (67.3%)        13549777  (96.8%) 
    8     584   (3.6%)      7696041236  (77.0%)        15484288 (110.6%) 


scalar exponent for a^e if e = 2^n 

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    0     134   (0.8%)         1614552   (0.0%)            4604   (0.0%) 
    4     134   (0.8%)        10849754   (0.1%)           24649   (0.2%) 
    8     134   (0.8%)        20367710   (0.2%)           44695   (0.3%) 
   12     134   (0.8%)        30604618   (0.3%)           64767   (0.5%) 
   16     134   (0.8%)        40841526   (0.4%)           84839   (0.6%) 
   20     134   (0.8%)        51078434   (0.5%)          104911   (0.7%) 
   24     134   (0.8%)        61315342   (0.6%)          124983   (0.9%) 
   28     134   (0.8%)        71552250   (0.7%)          145055   (1.0%) 
   32     134   (0.8%)        81789158   (0.8%)          165127   (1.2%) 


n scalar inversion 

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    0     257   (1.6%)         1932100   (0.0%)            8500   (0.1%) 
    1     297   (1.8%)       966204740   (9.7%)         1946136  (13.9%) 
    2     337   (2.1%)      1930477380  (19.3%)         3883772  (27.7%) 
    3     377   (2.3%)      2894750020  (28.9%)         5821408  (41.6%) 
    4     418   (2.6%)      3859022660  (38.6%)         7759044  (55.4%) 
    5     458   (2.8%)      4823295300  (48.2%)         9696680  (69.3%) 
    6     498   (3.0%)      5787567940  (57.9%)        11634316  (83.1%) 
    7     538   (3.3%)      6751840580  (67.5%)        13571952  (96.9%) 
    8     578   (3.5%)      7716113220  (77.2%)        15509588 (110.8%) 

```
