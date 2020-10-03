rm -rf ~/tmp/jailer*
rm -rf ~/tmp/$1
rm -rf ~/tmp/$1.co
mkdir C:/tmp/jailer
mkdir ~/tmp/jailer
mkdir ~/tmp/$1
mkdir ~/tmp/$1.co
cd ~/tmp/$1.co
git clone --depth 1 https://github.com/Wisser/Jailer.git
cd ..

mv $1.co/Jailer/* jailer
cd jailer

sed "s/stateOffset = 100/stateOffset = 0/g" src/main/gui/net/sf/jailer/ui/Environment.java --in-place
ant all
sed "s/stateOffset = 0/stateOffset = 100/g" src/main/gui/net/sf/jailer/ui/Environment.java --in-place

rm maven-artifacts/dummy
mv jailer-engine* maven-artifacts

rm -rf docs/api
rm -rf out

rm -rf C:/tmp/jailer
cp -r . C:/tmp/jailer/
rm -rf C:/tmp/jailer/admin

sed s/%VERSION%/$1/g admin/Jailer.nsi > admin/tmp.nsi
cd admin
makensis tmp.nsi
cd ..
rm admin/tmp.nsi

mv admin/*nstall* ..

dos2unix *.sh
chmod a+x *.sh

zip -r docs/admin.zip admin
rm -rf admin

cd ..
rm $1.zip
zip -r jailer_$1.zip jailer

# Web upload
# cd docs
# scp -r * rwisser,jailer@web.sf.net:/home/groups/j/ja/jailer/htdocs/

