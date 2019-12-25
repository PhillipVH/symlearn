#!/bin/bash
docker run -it \
           --rm \
           --name symlearn-learner \
           --link symlearn-redis:redis \
           -p 3467:3467 \
           18962378/symlearn \
           bash
           #lein repl :headless :host 0.0.0.0 :port 3467
