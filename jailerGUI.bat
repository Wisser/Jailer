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
set CP=%CP%;%LIB%\activation-1.1.1.jar
set CP=%CP%;%LIB%\FastInfoset-1.2.13.jar
set CP=%CP%;%LIB%\istack-commons-runtime-3.0.5.jar
set CP=%CP%;%LIB%\axb-api-2.3.0.jar
set CP=%CP%;%LIB%\jaxb-core-2.3.0.jar
set CP=%CP%;%LIB%\jaxb-impl-2.3.0.jar
set CP=%CP%;%LIB%\jaxb-runtime-2.3.0.jar
set CP=%CP%;%LIB%\stax-ex-1.7.8.jar
set CP=%CP%;%LIB%\txw2-2.3.0.jar
set CP=%CP%;%LIB%\jsqlparser-3.2.jar
set CP=%CP%;%LIB%\tablefilter-swing-5.3.1.jar
set CP=%CP%;jailer.jar

start javaw -Xmx1200M -Djava.util.Arrays.useLegacyMergeSort=true -cp %CP% net.sf.jailer.ui.JailerUI %*
