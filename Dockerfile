FROM clojure:openjdk-8-lein

# Install z3 + redis + dot + maven + vim
RUN mkdir -p /usr/share/man/man1
RUN apt update -y && apt upgrade -y && apt install z3 redis-tools graphviz maven vim -y

# Move z3 into the position that coastal expects it to be in
RUN cp /usr/bin/z3 /usr/local/bin/

# Point to Redis
ENV REDIS_HOST redis

# Install membership-coastal
RUN mkdir -p /usr/src/symlearn
COPY coastal /usr/src/symlearn/coastal
WORKDIR /usr/src/symlearn/coastal
RUN ./gradlew build -x test --no-daemon

# Install equivalence-coastal
COPY eqv-coastal /usr/src/symlearn/eqv-coastal
WORKDIR /usr/src/symlearn/eqv-coastal
RUN ./gradlew build installDist -x test --no-daemon

# Install symbolicautomata into Maven
WORKDIR /usr/src/symlearn
COPY symbolicautomata /usr/src/symlearn/symbolicautomata

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/SVPAlib-1.0.jar -DartifactId=SVPAlib -Dversion=1.0 -DgroupId=cs.wisc.edu -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/org.ow2.sat4j.core-2.3.4.jar -DartifactId=org.ow2.sat4j.core -Dversion=1.0 -DgroupId=org.ow2.sat4j -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/SVPABenchmark-0.0.1-SNAPSHOT.jar -DartifactId=SVPABenchmark -Dversion=0.0.1-SNAPSHOT -DgroupId=SVPABenchmark -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -Dfile=symbolicautomata/Parsers-1.0.jar -DartifactId=Parsers -Dversion=1.0 -DgroupId=cs.wisc.edu -Dpackaging=jar -Durl=file:lib

RUN mvn deploy:deploy-file -DgroupId=javacup -DartifactId=cup -Dversion=11 -Dpackaging=jar -Dfile=symbolicautomata/javacup-11c.jar -Durl=file:lib

# Install Clojure deps
COPY project.clj /usr/src/symlearn/
RUN lein deps

# Install the rest of the learner
COPY src /usr/src/symlearn/src
COPY lib /usr/src/symlearn/lib
COPY resources /usr/src/symlearn/resources

RUN lein uberjar

# Compile the learner
WORKDIR /usr/src/symlearn

# Drop into a shell
CMD ["bash"]
