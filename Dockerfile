FROM clojure

# Install z3
RUN apt update -y
RUN apt install z3 -y

# Copy the project definition and download dependencies
RUN mkdir -p /usr/src/symlearn
WORKDIR /usr/src/symlearn
COPY project.clj /usr/src/symlearn
RUN lein deps

# Copy the code
COPY . /usr/src/symlearn

# Compile coastal
WORKDIR /usr/src/symlearn/coastal
RUN bash gradlew compileJava

# Compile the learner
WORKDIR /usr/src/symlearn
RUN mv "$(lein uberjar | sed -n 's/^Created \(.*standalone\.jar\)/\1/p')" symlearn-standalone.jar

# Entry point runs the learner on PaperExample
WORKDIR /usr/src/symlearn
CMD ["java", "-jar", "symlearn-standalone.jar"]
