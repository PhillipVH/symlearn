#!/bin/bash

# Containers were launched with --rm, so they get deleted automatically
docker stop "symlearn-redis-$1"
docker stop "symlearn-learner-$1"
