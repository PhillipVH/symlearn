#!/bin/bash
docker run -it \
           --rm \
           --name symlearn-learner \
           --link symlearn-redis:redis \
           --mount type=bind,source="$(pwd)"/results,target=/usr/src/symlearn/results \
           -p 3467:3467 \
           symlearn \
           bash
           #lein repl :headless :host 0.0.0.0 :port 3467
