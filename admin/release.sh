rm -rf ~/tmp/jailer
rm -rf ~/tmp/$1
rm -rf ~/tmp/$1.co
mkdir ~/tmp/jailer
mkdir ~/tmp/$1
mkdir ~/tmp/$1.co
cd ~/tmp/$1.co
svn checkout --username=rwisser https://svn.code.sf.net/p/jailer/code/trunk
cd ..
mv $1.co/trunk/* jailer
cd jailer
find -iname ".svn" -exec rm -rf '{}' \;

ant package

chmod a+x *.sh

rm -rf admin
rm -rf docs
rm -rf out

cd ..
rm $1.zip
zip -r $1.zip jailer 

