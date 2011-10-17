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
set CP=%CP%;%LIB%\h2-1.3.160.jar
set CP=%CP%;jailer.jar

echo %CP%

start javaw -Xmx1024M -cp %CP% net.sf.jailer.ui.databrowser.DataBrowser %*
