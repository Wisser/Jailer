rm -rf ~/tmp/jailer
rm -rf ~/tmp/$1
rm -rf ~/tmp/$1.co
mkdir ~/tmp/jailer
mkdir ~/tmp/$1
mkdir ~/tmp/$1.co
cd ~/tmp/$1.co
svn checkout --username=rwisser https://svn.code.sf.net/p/jailer/code/trunk jailer-code
cd ..
mv $1.co/trunk/* jailer
cd jailer
sed "s/new javax.swing.JComboBox()/new net.sf.jailer.ui.JComboBox()/g" src/main/net/sf/jailer/ui/*.java --in-place
sed "s/new javax.swing.JComboBox()/new net.sf.jailer.ui.JComboBox()/g" src/main/net/sf/jailer/ui/databrowser/*.java --in-place
cp doc/web/home.htm doc/web/index.html
rm -rf doc/htdocs
find -iname ".svn" -exec rm -rf '{}' \;
find -iname ".cvs*" -exec rm -rf '{}' \;
ant package
rm -rf admin
rm -rf doc
rm -rf out
rm -rf beta
rm svn-commit.tmp
mv datamodel.def datamodel
rm -rf datamodel.scr
ls jailer.jar

rm -rf README-dbeauty releasenotes-dbeauty.txt

cd ..
rm $1.zip
zip -r $1.zip jailer 
