gpg -ab jailer-engine-X.jar
gpg -ab jailer-engine-X-sources.jar
gpg -ab jailer-engine-X-javadoc.jar
gpg -ab jailer-engine-X.pom
jar -cvf bundle.jar *.jar *.pom *.asc
