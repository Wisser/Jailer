#!/bin/sh

instdir=`dirname $0`
cd $instdir
 
LIB=jailer-engine/target/lib

# configuration files in the config directory
CLASSPATH="${CLASSPATH}:config"

# Add all .jar files from the lib directory to the CLASSPATH
for jar in $LIB/*.jar; do
  CLASSPATH="${CLASSPATH}:$jar"
done

CLASSPATH="${CLASSPATH}:jailer-engine/target/jailer-engine-1.0.0-SNAPSHOT.jar"

# echo "${CLASSPATH}"

java -Xmx1200M -cp "${CLASSPATH}" net.sf.jailer.Jailer "$@"
