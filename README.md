# SymLearn

[![Build Status](https://travis-ci.com/PhillipVH/symlearn.svg?token=v9TxturJsxPzarfynxN2&branch=master)](https://travis-ci.com/PhillipVH/symlearn)

## Installation (Docker)
To clone the repository and build the Docker image, run the following:
```
git clone https://github.com/PhillipVH/symlearn.git
git submodule init
git submodule update
bash docker_build.sh
```

## Running Experiments
To start the evaluation, run the following:
```
bash docker_startall.sh
```

Results from a run are stored in a `results.edn` file in the `results/` folder in the root of the repository.
The `symlearn-learner` container has access to this folder through a bind mount.

Once the experiments are completed, you can clean up the Docker containers by running `bash docker_killall.sh`.
