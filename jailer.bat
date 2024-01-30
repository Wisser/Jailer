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
set CP=%CP%;%LIB%\jackson-core-2.16.1.jar
set CP=%CP%;%LIB%\jackson-annotations-2.16.1.jar
set CP=%CP%;%LIB%\jackson-databind-2.16.1.jar
set CP=%CP%;jailer.jar

java -Xmx1200M -cp %CP% net.sf.jailer.Jailer %*
