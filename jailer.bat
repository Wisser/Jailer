@echo off
set LIB=lib

rem JDBC-driver
rem set CP=%CP%;<jdbc-driver>.jar

rem configuration files in the config directory
set CP=%CP%;config

rem the libraries
set CP=%CP%;%LIB%\junit.jar
set CP=%CP%;%LIB%\log4j-api-2.17.2.jar
set CP=%CP%;%LIB%\log4j-core-2.17.2.jar
set CP=%CP%;%LIB%\log4j-slf4j-impl-2.17.2.jar
set CP=%CP%;%LIB%\slf4j-api-1.7.25.jar
set CP=%CP%;%LIB%\flatlaf-3.3.jar
set CP=%CP%;%LIB%\args4j.jar
set CP=%CP%;%LIB%\sdoc-0.5.0-beta.jar
set CP=%CP%;%LIB%\angus-activation-2.0.1.jar
set CP=%CP%;%LIB%\istack-commons-runtime-4.1.2.jar
set CP=%CP%;%LIB%\jakarta.activation-api-2.1.2.jar
set CP=%CP%;%LIB%\jakarta.xml.bind-api-4.0.1.jar
set CP=%CP%;%LIB%\jaxb-core-4.0.4.jar
set CP=%CP%;%LIB%\jaxb-runtime-4.0.4.jar
set CP=%CP%;%LIB%\txw2-4.0.4.jar
set CP=%CP%;jailer.jar

java -Xmx1200M -cp %CP% net.sf.jailer.Jailer %*
