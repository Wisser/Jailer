@echo off
set LIB=lib

rem JDBC-driver
rem set CP=%CP%;<jdbc-driver>.jar

rem configuration files in the config directory
set CP=%CP%;config

rem the libraries
set CP=%CP%;%LIB%\junit.jar
set CP=%CP%;%LIB%\commons-logging.jar
set CP=%CP%;%LIB%\log4j.jar
set CP=%CP%;%LIB%\args4j.jar
set CP=%CP%;%LIB%\spring.jar
set CP=%CP%;%LIB%\prefuse.jar
set CP=%CP%;%LIB%\sdoc-0.5.0-beta.jar
set CP=%CP%;%LIB%\activation-1.0.2.jar
set CP=%CP%;%LIB%\jaxb-core-2.3.0-b170127.1453.jar
set CP=%CP%;%LIB%\jaxb-impl-2.3.0-b170127.1453.jar
set CP=%CP%;%LIB%\jaxb-api-2.3.0-b170201.1204.jar
set CP=%CP%;%LIB%\jsqlparser-3.2.jar
set CP=%CP%;%LIB%\tablefilter-swing-5.3.1.jar
set CP=%CP%;jailer.jar

start javaw -Xmx1024M -Djava.util.Arrays.useLegacyMergeSort=true -cp %CP% net.sf.jailer.ui.databrowser.DataBrowser %*
