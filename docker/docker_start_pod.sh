#!/bin/bash

bash docker/docker_start_redis.sh "$1"
bash docker/docker_start_learner.sh "$1"
