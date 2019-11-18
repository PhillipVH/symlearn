FROM clojure

# Install z3 + redis + dot
RUN apt update -y
RUN apt install z3 redis-tools graphviz -y

# Install Maven + Vim
RUN apt install maven vim -y

# Copy the project definition and download dependencies
RUN mkdir -p /usr/src/symlearn
WORKDIR /usr/src/symlearn
COPY . /usr/src/symlearn


# Install symbolicautomata into Maven
RUN mvn deploy:deploy-file -Dfile=symbolicautomata/SVPAlib-1.0.jar -DartifactId=SVPAlib -Dversion=1.0 -DgroupId=cs.wisc.edu -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/org.ow2.sat4j.core-2.3.4.jar -DartifactId=org.ow2.sat4j.core -Dversion=1.0 -DgroupId=org.ow2.sat4j -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/SVPABenchmark-0.0.1-SNAPSHOT.jar -DartifactId=SVPABenchmark -Dversion=0.0.1-SNAPSHOT -DgroupId=SVPABenchmark -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/Parsers-1.0.jar -DartifactId=Parsers -Dversion=1.0 -DgroupId=cs.wisc.edu -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -DgroupId=javacup -DartifactId=cup -Dversion=11 -Dpackaging=jar -Dfile=symbolicautomata/javacup-11c.jar -Durl=file:lib

RUN lein deps


# Point to Redis
ENV REDIS_HOST redis





# Compile the learner
COPY . /usr/src/symlearn
WORKDIR /usr/src/symlearn


# Drop into a shell
CMD ["bash"]
