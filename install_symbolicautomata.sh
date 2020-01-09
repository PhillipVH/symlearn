#!/bin/bash
mvn deploy:deploy-file -Dfile=symbolicautomata/SVPAlib-1.0.jar -DartifactId=SVPAlib -Dversion=1.0 -DgroupId=cs.wisc.edu -Dpackaging=jar -Durl=file:lib
mvn deploy:deploy-file -Dfile=symbolicautomata/org.ow2.sat4j.core-2.3.4.jar -DartifactId=org.ow2.sat4j.core -Dversion=1.0 -DgroupId=org.ow2.sat4j -Dpackaging=jar -Durl=file:lib
mvn deploy:deploy-file -Dfile=symbolicautomata/SVPABenchmark-0.0.1-SNAPSHOT.jar -DartifactId=SVPABenchmark -Dversion=0.0.1-SNAPSHOT -DgroupId=SVPABenchmark -Dpackaging=jar -Durl=file:lib
mvn deploy:deploy-file -Dfile=symbolicautomata/Parsers-1.0.jar -DartifactId=Parsers -Dversion=1.0 -DgroupId=cs.wisc.edu -Dpackaging=jar -Durl=file:lib
mvn deploy:deploy-file -DgroupId=javacup -DartifactId=cup -Dversion=11 -Dpackaging=jar -Dfile=symbolicautomata/javacup-11c.jar -Durl=file:lib

# Pending investigation
cp lib/SVPABenchmark/SVPABenchmark/0.0.1-SNAPSHOT/SVPABenchmark-0.0.1-20200109.073211-2.jar lib/SVPABenchmark/SVPABenchmark/0.0.1-SNAPSHOT/SVPABenchmark-0.0.1-20191120.122110-1.jar
