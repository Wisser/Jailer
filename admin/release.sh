cd jailergit/
cd trunk/
svn up
sh admin/release.sh jailer_$1
sh admin/release-dbeauty.sh dbeauty_$2
cd
cd tmp
rm -r _$1
mkdir _$1
cd _$1
unzip ../jailer_$1.zip 
cd jailer/
head releasenotes.txt 

cd ..
cd ..
rm -r _$2
mkdir _$2
cd _$2
cd dbeauty
unzip ../dbeauty_$2.zip 

echo
echo Jailer Releasenotes
echo

cd
cd tmp
cd _$1
cd jailer/
head releasenotes.txt

echo
echo DBeauty Releasenotes
echo

cd
cd tmp
cd _$2
cd dbeauty
head releasenotes.txt

cd
cd tmp
cd _$1
cd jailer/
sh jailerGUI.sh &

cd
cd tmp
cd _$2
cd dbeauty/
sh dbeauty.sh &


