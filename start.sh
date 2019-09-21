#!/bin/bash
docker run --name symlearn-redis -d redis
docker run -it \
           --rm \
           --name symlearn-learner \
           --link symlearn-redis:redis \
           symlearn:demo-21092019 \
           bash

# Clean up
docker stop $(docker ps -aq)
docker rm $(docker ps -aq)
