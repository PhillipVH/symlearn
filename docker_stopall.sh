#!/bin/bash

# Containers were launched with --rm, so they get deleted automatically
docker stop symlearn-redis
docker stop symlearn-learner
