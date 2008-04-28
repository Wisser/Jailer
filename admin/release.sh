rm -rf ~/tmp/$1
mkdir ~/tmp/$1
cd ~/tmp/$1
svn co https://jailer.svn.sf.net/svnroot/jailer/trunk_ui
find -iname ".svn" -exec rm -rf '{}' \;
find -iname ".cvs*" -exec rm -rf '{}' \;
ant package
rm -rf admin
rm -rf out
rm svn-commit.tmp
mv datamodel.def datamodel
ls jailer.jar
cd ..
rm $1.zip
zip -r $1.zip $1 
