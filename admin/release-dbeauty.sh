rm -rf ~/tmp/dbeauty
rm -rf ~/tmp/$1
rm -rf ~/tmp/$1.co
mkdir ~/tmp/dbeauty
mkdir ~/tmp/$1
mkdir ~/tmp/$1.co
cd ~/tmp/$1.co
svn checkout --username=rwisser https://svn.code.sf.net/p/jailer/code/trunk
cd ..
mv $1.co/trunk/* dbeauty
cd dbeauty
sed "s/new javax.swing.JComboBox()/new net.sf.jailer.ui.JComboBox()/g" src/main/gui/net/sf/jailer/ui/*.java --in-place
sed "s/new javax.swing.JComboBox()/new net.sf.jailer.ui.JComboBox()/g" src/main/gui/net/sf/jailer/ui/databrowser/*.java --in-place
find -iname ".svn" -exec rm -rf '{}' \;

ant package

chmod a+x *.sh

rm -rf admin
rm -rf docs
rm -rf out
rm svn-commit.tmp
mv datamodel.def datamodel
rm -rf datamodel.scr
ls jailer.jar

mv jailerDataBrowser.sh dbeauty.sh
mv jailerDataBrowser.bat dbeauty.bat
mv JailerDataBrowser.exe dbeauty.exe
rm -rf jailerGUI.*
rm -rf jailer.sh jailer.bat Demo.csv domainmodel example Jailer.exe README releasenotes.txt Jailer.html
mv README-dbeauty README
rm README.md
mv releasenotes-dbeauty.txt releasenotes.txt
echo "DBeauty" > .standalone

cd ..
rm $1.zip
zip -r $1.zip dbeauty 
