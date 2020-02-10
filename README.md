# SymLearn

[![Build Status](https://travis-ci.com/PhillipVH/symlearn.svg?token=v9TxturJsxPzarfynxN2&branch=master)](https://travis-ci.com/PhillipVH/symlearn)

## Getting Started
The `symlearn.py` script provides facilities for building the Docker images, running benchmarks, and aggregating results.
Python 3 is required.
```
$ git clone https://github.com/PhillipVH/symlearn.git
$ ./symlearn.py init
```

For more information, see `./symlearn.py -h`.

```
$ ./symlearn.py evaluate -h
usage: symlearn.py evaluate [-h] --name NAME --benchmark-file FILE --timeout N
                            [--max-string-length N] [--dry] [--parallel N]

optional arguments:
  -h, --help            show this help message and exit
  --name NAME           human-readable label for the evaluation
  --benchmark-file FILE
                        file containing newline delimited regular expressions
  --timeout N           timeout for each symbolic equivalence query in minutes
  --max-string-length N
                        maximum string length used in equivalence queries
                        (default 30)
  --dry                 do not start the experiments
  --parallel N          start N experiments in parallel, dividing the
                        benchmark file between them


```
## Running Experiments
To start the evaluation, run the following:
```
$ ./symlearn.py evaluate --name foo --benchmark-file regexlib-stratified.re --timeout 10
```
This will begin an evaluation of the `regexlib-stratified.re` file, with a timeout of 10 minutes.

Results from a run are stored in a `results.edn` file in the `results/[name]/` folder in the root of the repository.

To check in on a set of running experiments, you can get the logs with `./symlearn.py logs [name]`.
