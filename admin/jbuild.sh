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

cd
cd tmp
rm -r _$1
mkdir _$1
cd _$1
unzip ../jailer_$1.zip 
cd jailer/

cd
cd tmp
cd _$1
cd jailer/
head releasenotes.txt 
LIB=lib
CP="$CP;config"

# the libraries
CP="$CP;$LIB/junit.jar"
CP="$CP;$LIB/log4j.jar"
CP="$CP;$LIB/args4j.jar"
CP="$CP;$LIB/prefuse.jar"
CP="$CP;$LIB/sdoc-0.5.0-beta.jar"
CP="$CP;$LIB/activation-1.0.2.jar"
CP="$CP;$LIB/jaxb-core-2.3.0-b170127.1453.jar"
CP="$CP;$LIB/jaxb-impl-2.3.0-b170127.1453.jar"
CP="$CP;$LIB/jaxb-api-2.3.0-b170201.1204.jar"
CP="$CP;$LIB/jsqlparser-3.2.jar"
CP="$CP;$LIB/tablefilter-swing-5.3.1.jar"
CP="$CP;jailer.jar"

echo $CP

"$JAVA_HOME/bin/java" -Xmx1200M -cp $CP net.sf.jailer.ui.ExtractionModelFrame
