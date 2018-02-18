VER=`java -version 2>&1 | grep "java version" | awk '{print $3}' | tr -d \" | awk '{split($0, array, ".")} END{print array[2]}'`
echo $VER
if [ $VER = 7 ]; then
    echo "Java version is 7"
else
    echo "Java version not 7"
    exit 1
fi



cd
cd workspace/jailer/jailer-code/
svn up
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
sh jailerGUI.sh &
