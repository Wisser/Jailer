set LIB=lib
set CP=.;out;out\classes;out\jailer.jar

rem JDBC-driver
rem set CP=%CP%;%JDBCLIB%\<jdbc-driver>.jar

# configuration files in the config directory

set CP=%CP%;config

rem the libraries
set CP=%CP%;%LIB%\junit.jar
set CP=%CP%;%LIB%\commons-logging.jar
set CP=%CP%;%LIB%\log4j.jar
set CP=%CP%;%LIB%\args4j.jar
set CP=%CP%;%LIB%\spring.jar
set CP=%CP%;%LIB%\jailer.jar

java -cp %CP% org.jailer.Jailer %*
