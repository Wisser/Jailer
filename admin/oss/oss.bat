gpg -ab jailer-engine-VERSION.jar
gpg -ab jailer-engine-VERSION-sources.jar
gpg -ab jailer-engine-VERSION-javadoc.jar
gpg -ab jailer-engine-VERSION.pom
zip oss.zip *.jar *.pom *.asc
