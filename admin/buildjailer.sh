JAVA_HOME="C:\Program Files\Java\jdk1.8.0_361"
export JAVA_HOME

pwd

cd git/Jailer/admin
dos2unix *.sh
/c/cygwin64/bin/bash jbuild.sh $1 > buildprotokoll.txt
