#!/bin/sh

instdir=`dirname $0`
cd $instdir
 
LIB=target/lib

# JDBC-driver
# CP=$CP:<driver-jar>

# configuration files in the config directory
CLASSPATH=$CLASSPATH:config

# Add all .jar files from the lib directory to the CLASSPATH
for jar in $LIB/*.jar; do
  CLASSPATH="$CLASSPATH:$jar"
done

# the libraries
CLASSPATH=$CLASSPATH:target/jailer-gui-1.0.0-SNAPSHOT.jar

# echo $CP

java -Xmx1200M -Djava.util.Arrays.useLegacyMergeSort=true -cp $CLASSPATH net.sf.jailer.ui.JailerUI "$@"
