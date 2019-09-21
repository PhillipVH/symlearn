FROM clojure

# Install z3 + redis + vim
RUN apt update -y
RUN apt install z3 redis-tools -y

# Copy the project definition and download dependencies
RUN mkdir -p /usr/src/symlearn
WORKDIR /usr/src/symlearn
COPY project.clj /usr/src/symlearn
RUN lein deps

# Compile coastal
RUN mkdir -p /usr/src/symlearn/coastal
COPY ./coastal /usr/src/symlearn/coastal
WORKDIR /usr/src/symlearn/coastal
RUN bash gradlew # Cache Gralde
RUN bash gradlew compileJava

# Compile the learner
COPY . /usr/src/symlearn
WORKDIR /usr/src/symlearn
RUN mv "$(lein uberjar | sed -n 's/^Created \(.*standalone\.jar\)/\1/p')" symlearn-standalone.jar

# Entry point runs the learner on PaperExample
WORKDIR /usr/src/symlearn
CMD ["java", "-jar", "symlearn-standalone.jar"]
