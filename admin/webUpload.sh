rm -rf ~/tmp/jailer
rm -rf ~/tmp/1
rm -rf ~/tmp/1.co
mkdir ~/tmp/jailer
mkdir ~/tmp/1
mkdir ~/tmp/1.co
cd ~/tmp/1.co
svn checkout --username=rwisser https://svn.code.sf.net/p/jailer/code/trunk/docs jailer-code
cd ..
mv 1.co/jailer-code/* jailer
cd jailer

cp home.htm index.html
find -iname ".svn" -exec rm -rf '{}' \;
find -iname ".cvs*" -exec rm -rf '{}' \;

scp -r * rwisser,jailer@web.sf.net:/home/groups/j/ja/jailer/htdocs/
scp -r * rwisser,jailer@web.sf.net:/home/groups/j/ja/jailer/htdocs/doc/

