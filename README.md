# TP Union Find:

Union-Find with a benchmark and a maze generator, school assignement.

## Build:

Prerequisites:
 * Ocaml installation
 * Dune

Simply run:
> dune build

## Run

### Run tests

> dune test

### Run random maze generator program: 

> dune exec _build/default/bin/main.exe

### Run Benchmark

> dune exec benchmark <n_min> <n_max>

where `<n_min>` is the smallest size of union-find tested, and `<n_max>` is the biggest size of union-find tested. Outputs csv results to stdout.

## Benchmark Results

![Graph of benchmark results](https://github.com/Dr-McFish/TP-Union-Find/blob/master/Resultats_Benchmark.png?raw=true "Benchmark Results")

The spikes visible are strange, maybe caused by garbage collection.
