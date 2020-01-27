#!/bin/bash
docker run --rm --name symlearn-redis-"$1" -d redis
