#!/bin/bash

bash docker_start_redis.sh "$1"
bash docker_start_learner.sh "$1"
