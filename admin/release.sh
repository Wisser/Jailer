rm -rf ~/tmp/$1
rm -rf ~/tmp/$1.co
mkdir ~/tmp/$1
mkdir ~/tmp/$1.co
cd ~/tmp/$1.co
svn co https://jailer.svn.sf.net/svnroot/jailer/trunk
cd ..
mv $1.co/trunk/* $1
cd $1
cp doc/web/home.htm doc/web/index.html
rm -rf doc/htdocs
find -iname ".svn" -exec rm -rf '{}' \;
find -iname ".cvs*" -exec rm -rf '{}' \;
ant package
rm -rf admin
rm -rf out
rm svn-commit.tmp
mv datamodel.def datamodel
rm -rf datamodel.scr
ls jailer.jar
cd ..
rm $1.zip
zip -r $1.zip $1 
