JAVA_HOME="C:\Program Files\Java\jdk1.8.0_202"
export JAVA_HOME

cd git/Jailer/admin
dos2unix *.sh
sh jbuild.sh $1 > buildprotokoll.txt
