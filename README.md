# SymLearn

[![Build Status](https://travis-ci.com/PhillipVH/symlearn.svg?token=v9TxturJsxPzarfynxN2&branch=master)](https://travis-ci.com/PhillipVH/symlearn)

## Getting Started
The `symlearn.py` script provides facilities for building the Docker images, running benchmarks, and aggregating results.
Python 3 is required.
```
$ git clone https://github.com/PhillipVH/symlearn.git
$ ./symlearn.py init
```

## Running Experiments
To start the evaluation, run the following:
```
$ ./symlearn.py evaluate -h
usage: symlearn.py deploy [-h] [--dry] name benchmark_file depth_limit timeout

$ ./symlearn.py evaluate foo regexlib-clean-single.re 10 10
```
This will begin an evaluation of the `regexlib-clean-single.re` file, with a equivalence check depth limit of 10, and a
timeout of 10 minutes.

Results from a run are stored in a `results.edn` file in the `results/[name]/` folder in the root of the repository.
