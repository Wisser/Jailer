JDBCLIB=/opt/IBM/db2/V8.1/java
LIB=lib
CP=.:out:out/jailer.jar

# JDBC-driver
CP=$CP:$JDBCLIB/db2jcc.jar
CP=$CP:$JDBCLIB/db2jcc_license_cu.jar

# configuration files in the config directory
CP=$CP:config

# the libraries
CP=$CP:$LIB/junit.jar
CP=$CP:$LIB/commons-logging.jar
CP=$CP:$LIB/log4j.jar
CP=$CP:$LIB/args4j.jar
CP=$CP:$LIB/spring.jar
CP=$CP:$LIB/jailer.jar

java -cp $CP org.jailer.Jailer $@
