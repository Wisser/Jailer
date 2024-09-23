JAVA_HOME="C:\Program Files\Java\jdk1.8.0_361"
export JAVA_HOME

pwd

cd git/Jailer/admin
dos2unix *.sh
sh jbuild.sh $1 > buildprotokoll.txt

read -n 1 -s