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
COPY eqv-coastal-new /usr/src/symlearn/eqv-coastal-new
WORKDIR /usr/src/symlearn/eqv-coastal-new
RUN ./gradlew build installDist -x test

# Install symbolicautomata into Maven
WORKDIR /usr/src/symlearn
COPY symbolicautomata /usr/src/symlearn/symbolicautomata
COPY install_symbolicautomata.sh /usr/src/symlearn
RUN bash install_symbolicautomata.sh

# Install Clojure deps
COPY project.clj /usr/src/symlearn/
RUN lein deps

# Install the rest of the learner
COPY src /usr/src/symlearn/src
COPY resources /usr/src/symlearn/resources

# Install the benchmark files
COPY regexlib-clean-10.re /usr/src/symlearn/
COPY regexlib-clean-20.re /usr/src/symlearn/
COPY regexlib-clean-100.re /usr/src/symlearn/

RUN lein uberjar

# Fix path to file when the jar is run
ENV MEMBERSHIP_CONFIG_PATH /usr/src/symlearn/resources/Regex.xml

# Compile the learner
WORKDIR /usr/src/symlearn

# Drop into a shell
CMD ["bash"]
