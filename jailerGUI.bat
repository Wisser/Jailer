@echo off
set LIB=lib

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
set CP=%CP%;%LIB%\prefuse.jar
set CP=%CP%;%LIB%\sdoc-0.5.0-beta.jar
set CP=%CP%;%LIB%\liquibase-core-4.27.0.jar
set CP=%CP%;%LIB%\commons-collections4-4.4.jar
set CP=%CP%;%LIB%\commons-lang3-3.14.0.jar
set CP=%CP%;%LIB%\commons-text-1.11.0.jar
set CP=%CP%;%LIB%\opencsv-5.9.jar
set CP=%CP%;%LIB%\snakeyaml-2.2.jar
set CP=%CP%;%LIB%\jackson-core-2.16.1.jar
set CP=%CP%;%LIB%\jackson-annotations-2.16.1.jar
set CP=%CP%;%LIB%\jackson-databind-2.16.1.jar
set CP=%CP%;%LIB%\jsqlparser-3.2.jar
set CP=%CP%;%LIB%\tablefilter-swing-5.3.1.jar
set CP=%CP%;jailer.jar

start javaw -Xmx1200M -Djava.util.Arrays.useLegacyMergeSort=true -cp %CP% net.sf.jailer.ui.JailerUI %*
