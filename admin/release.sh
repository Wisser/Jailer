rm -rf ~/tmp/jailer*
rm -rf ~/tmp/dbeauty*
rm -rf ~/tmp/$1
rm -rf ~/tmp/$1.co
mkdir ~/tmp/jailer
mkdir ~/tmp/$1
mkdir ~/tmp/$1.co
cd ~/tmp/$1.co
svn checkout --username=rwisser http://svn.code.sf.net/p/jailer/code/trunk
cd ..
mv $1.co/trunk/* jailer
cd jailer
find -iname ".svn" -exec rm -rf '{}' \;

ant package

rm -rf docs/api
rm -rf out

rm -rf ~/.wine/drive_c/tmp/jailer
cp -r . ~/.wine/drive_c/tmp/jailer/
rm -rf ~/.wine/drive_c/tmp/jailer/admin

sed s/%VERSION%/$1/g admin/Jailer.nsi > admin/tmp.nsi
cd admin
wine ~/nsis-3.03/makensis.exe tmp.nsi
cd ..
rm admin/tmp.nsi

rm -rf ~/.wine/drive_c/tmp/dbeauty
cp -r . ~/.wine/drive_c/tmp/dbeauty/
rm -rf ~/.wine/drive_c/tmp/dbeauty/admin

sed s/%VERSION%/$1/g admin/dbeauty.nsi > admin/tmp.nsi
cd admin
wine ~/nsis-3.03/makensis.exe tmp.nsi
cd ..
rm admin/tmp.nsi

sed s/%VERSION%/$1/g admin/Jailer.nsi > admin/tmp.nsi
wine ~/nsis-3.03/makensis.exe admin/tmp.nsi
rm admin/tmp.nsi

cp admin/*nstall* ..

chmod a+x *.sh

zip -r docs/admin.zip admin
rm -rf admin

cd ..
rm $1.zip
zip -r jailer_$1.zip jailer
cp -r jailer dbeauty
zip -r dbeauty_$1.zip dbeauty 
