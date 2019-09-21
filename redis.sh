#!/usr/bin
docker run -it \
           --rm \
           --link symlearn-redis:redis \
           redis \
           redis-cli -h redis
