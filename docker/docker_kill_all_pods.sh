#!/bin/bash 
docker container ls -aq --filter name=symlearn-* | xargs docker stop | xargs docker rm
