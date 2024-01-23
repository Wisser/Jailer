#!/bin/sh

instdir=`dirname $0`
cd $instdir
 
LIB=lib

# JDBC-driver
# CP=$CP:<jdbc.jar>

# configuration files in the config directory
CP=$CP:config

# the libraries
CP=$CP:$LIB/junit.jar
CP=$CP:$LIB/log4j-api-2.17.2.jar
CP=$CP:$LIB/log4j-core-2.17.2.jar
CP=$CP:$LIB/log4j-slf4j-impl-2.17.2.jar
CP=$CP:$LIB/slf4j-api-1.7.25.jar
CP=$CP:$LIB/flatlaf-3.3.jar
CP=$CP:$LIB/args4j.jar
CP=$CP:$LIB/sdoc-0.5.0-beta.jar
CP=$CP:$LIB/activation-1.1.1.jar
CP=$CP:$LIB/FastInfoset-1.2.13.jar
CP=$CP:$LIB/istack-commons-runtime-3.0.5.jar
CP=$CP:$LIB/axb-api-2.3.0.jar
CP=$CP:$LIB/jaxb-core-2.3.0.jar
CP=$CP:$LIB/jaxb-impl-2.3.0.jar
CP=$CP:$LIB/jaxb-runtime-2.3.0.jar
CP=$CP:$LIB/stax-ex-1.7.8.jar
CP=$CP:$LIB/txw2-2.3.0.jar
CP=$CP:jailer.jar

java -Xmx1200M -cp $CP net.sf.jailer.Jailer "$@"
