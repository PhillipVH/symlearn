# SymLearn

[![Build Status](https://travis-ci.com/PhillipVH/symlearn.svg?token=v9TxturJsxPzarfynxN2&branch=master)](https://travis-ci.com/PhillipVH/symlearn)

## Installation (Docker)

```
git clone https://github.com/PhillipVH/symlearn.git
git submodule init
git submodule update
bash docker_build.sh
bash docker_startall.sh
```

Once the Docker container has started, you can start the evaluation with `lein run`.
