gpg -ab jailer-engine-V.jar
gpg -ab jailer-engine-V-sources.jar
gpg -ab jailer-engine-V-javadoc.jar
gpg -ab jailer-engine-V.pom
jar -cvf bundle.jar *.jar *.pom *.asc
