FROM clojure

# Install z3 + redis + dot
RUN apt update -y
RUN apt install z3 redis-tools graphviz -y

# Copy the project definition and download dependencies
RUN mkdir -p /usr/src/symlearn
WORKDIR /usr/src/symlearn
COPY project.clj /usr/src/symlearn
RUN lein deps

# Compile the learner
COPY . /usr/src/symlearn
WORKDIR /usr/src/symlearn

# Drop into a shell
CMD ["bash"]