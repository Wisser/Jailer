echo on

echo $JAVA_HOME

echo "$JAVA_HOME/bin/java" -version 

VER=`"$JAVA_HOME/bin/java" -version 2>&1 | grep "java version" | awk '{print $3}' | tr -d \" | awk '{split($0, array, ".")} END{print array[2]}'`
echo $VER
if [ $VER = 8 ]; then
    echo "Java version is 8"
else
    echo "Java version not 8"
    exit 1
fi



cd
cd git/Jailer
git pull
sh admin/release.sh $1

# make *.sh executable
cd
cd tmp
rm -r _2$1
mkdir _2$1
echo wsl rm -rf /home/ralf/jailer > _.bat
./_.bat
echo wsl unzip /mnt/c/Users/ralfw/tmp/jailer_$1.zip -d /home/ralf/ > _.bat
./_.bat
echo wsl chmod a+x /home/ralf/jailer/*.sh > _.bat
./_.bat
rm jailer_$1.zip
echo "wsl cd; zip -r /mnt/c/Users/ralfw/tmp/jailer_$1.zip jailer" > _.bat
./_.bat

cd
cd tmp
rm -r _$1
mkdir _$1
cd _$1
unzip ../jailer_$1.zip 
cd jailer/

unzip docs/admin.zip
echo rm -rf ../../oss
rm -rf ../../oss
mkdir ../../oss
sed s/VERSION/$1/g admin/oss/jailer-engine.pom > ../../oss/jailer-engine-$1.pom
sed s/VERSION/$1/g admin/oss/oss.bat > ../../oss/oss.bat
cp maven-artifacts/* ../../oss/

cd admin
./j14pack.bat $1

cd
cp /mnt/c/tmp/*.msi .

cd
cd tmp
rm -r _$1
mkdir _$1
cd _$1
unzip ../jailer_$1.zip 
cd jailer/

unzip docs/admin.zip
cd admin
dos2unix *.sh
echo "wsl sh j14pack.sh $1" > _.bat
./_.bat

cd
cd tmp
cd _$1
cd jailer/
head releasenotes.txt 
LIB=lib
CP="$CP;config"

# the libraries
CP="$CP;$LIB/junit.jar"
CP="$CP;$LIB/log4j-api-2.17.2.jar"
CP="$CP;$LIB/log4j-core-2.17.2.jar"
CP="$CP;$LIB/log4j-slf4j-impl-2.17.2.jar"
CP="$CP;$LIB/slf4j-api-1.7.25.jar"
CP="$CP;$LIB/flatlaf-3.3.jar"
CP="$CP;$LIB/args4j.jar"
CP="$CP;$LIB/prefuse.jar"
CP="$CP;$LIB/sdoc-0.5.0-beta.jar"
CP="$CP;$LIB/angus-activation-2.0.1.jar"
CP="$CP;$LIB/istack-commons-runtime-4.1.2.jar"
CP="$CP;$LIB/jakarta.activation-api-2.1.2.jar"
CP="$CP;$LIB/jakarta.xml.bind-api-4.0.1.jar"
CP="$CP;$LIB/jaxb-core-4.0.4.jar"
CP="$CP;$LIB/jaxb-runtime-4.0.4.jar"
CP="$CP;$LIB/txw2-4.0.4.jar"
CP="$CP;$LIB/jsqlparser-3.2.jar"
CP="$CP;$LIB/tablefilter-swing-5.3.1.jar"
CP="$CP;jailer.jar"

echo $CP

"$JAVA_HOME/bin/java" -Xmx1200M -cp $CP net.sf.jailer.ui.ExtractionModelFrame
