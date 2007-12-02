#JDBCLIB=...
LIB=lib
CP=.:out:out/classes:out/jailer.jar

# JDBC-driver
# CP=$CP:$JDBCLIB/<driver-jar>

# configuration files in the config directory
CP=$CP:config

# the libraries
CP=$CP:$LIB/junit.jar
CP=$CP:$LIB/commons-logging.jar
CP=$CP:$LIB/log4j.jar
CP=$CP:$LIB/args4j.jar
CP=$CP:$LIB/spring.jar
CP=$CP:$LIB/jailer.jar

java -cp $CP net.sf.jailer.ui.ExtractionModelFrame $@
