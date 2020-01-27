#!/bin/bash

# Create the results folder, unless it already exists
mkdir -p "results/$1/"

# Run the learner, binding the results folder, and connecting to redis over a link
docker run -d \
		   --name "symlearn-learner-$1" \
           --link "symlearn-redis-$1":redis \
           --mount type=bind,source="$(pwd)/results/$1",target=/usr/src/symlearn/results \
           symlearn
