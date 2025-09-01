gpg -ab jailer-engine-VERSION.jar
gpg -ab jailer-engine-VERSION-sources.jar
gpg -ab jailer-engine-VERSION-javadoc.jar
gpg -ab jailer-engine-VERSION.pom
forfiles /s /m *.jar /c "cmd /c CertUtil -hashfile @path MD5 | grep -v CertUtil | grep -v Hash > @path.md5"
forfiles /s /m *.jar /c "cmd /c CertUtil -hashfile @path sha1 | grep -v CertUtil | grep -v Hash > @path.sha1"
forfiles /s /m *.pom /c "cmd /c CertUtil -hashfile @path MD5 | grep -v CertUtil | grep -v Hash > @path.md5"
forfiles /s /m *.pom /c "cmd /c CertUtil -hashfile @path sha1 | grep -v CertUtil | grep -v Hash > @path.sha1"
mkdir io\github\wisser\jailer-engine\VERSION
cp *.jar *.pom *.asc *.sha1 *.md5 io\github\wisser\jailer-engine\VERSION
zip -r oss.zip io
