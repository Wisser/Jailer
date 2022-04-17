 #!/bin/bash

rm -rf ~/tmp/_ 
mkdir ~/tmp/_ 
rm -rf ~/tmp/jre$1  
cd .. 
 
cp -r maven-artifacts ~/tmp/_ 
cp -r bookmark ~/tmp/_ 
cp -r template ~/tmp/_ 
cp -r build.xml ~/tmp/_ 
cp -r config ~/tmp/_ 
cp -r datamodel ~/tmp/_ 
cp -r demo-sakila-1.4.mv.db ~/tmp/_ 
cp -r demo-scott-1.4.mv.db ~/tmp/_ 
cp -r demo-scott-subset-1.4.mv.db ~/tmp/_ 
cp -r driverlist.csv ~/tmp/_ 
cp -r extractionmodel ~/tmp/_ 
cp -r jailer.bat ~/tmp/_ 
cp -r Jailer.exe ~/tmp/_ 
cp -r jailer.jar ~/tmp/_ 
cp -r jailer.sh ~/tmp/_ 
cp -r jailer.xml ~/tmp/_ 
cp -r jailerDataBrowser.bat ~/tmp/_ 
cp -r JailerDataBrowser.exe ~/tmp/_ 
cp -r jailerDataBrowser.sh ~/tmp/_ 
cp -r jailerGUI.bat ~/tmp/_ 
cp -r jailerGUI.sh ~/tmp/_ 
cp -r layout ~/tmp/_ 
cp -r lib ~/tmp/_ 
cp -r license-prefuse.txt ~/tmp/_ 
cp -r license.txt ~/tmp/_ 
cp -r manifest.mf ~/tmp/_ 
cp -r README.md ~/tmp/_ 
cp -r releasenotes.txt ~/tmp/_ 
cp -r render ~/tmp/_ 
cp admin/jailer.png ~/tmp/_ 
cp admin/databrowserlauncher.properties ~/tmp/_ 
cp admin/databrowserlauncher-linux.properties ~/tmp/_ 
 
cp -r ~/tmp/_/lib ~/tmp/_/jdbc_lib 
rm ~/tmp/_/lib/* 
rm ~/tmp/_/*.bat 
rm ~/tmp/_/*.exe 

chmod a+x ~/tmp/_/*.sh 
 
mv ~/tmp/_/jdbc_lib/activation-1.0.2.jar ~/tmp/_/lib/activation-1.0.2.jar 
mv ~/tmp/_/jdbc_lib/args4j.jar ~/tmp/_/lib/args4j.jar 
mv ~/tmp/_/jdbc_lib/jaxb-api-2.3.0-b170201.1204.jar ~/tmp/_/lib/jaxb-api-2.3.0-b170201.1204.jar 
mv ~/tmp/_/jdbc_lib/jaxb-core-2.3.0-b170127.1453.jar ~/tmp/_/lib/jaxb-core-2.3.0-b170127.1453.jar 
mv ~/tmp/_/jdbc_lib/jaxb-impl-2.3.0-b170127.1453.jar ~/tmp/_/lib/jaxb-impl-2.3.0-b170127.1453.jar 
mv ~/tmp/_/jdbc_lib/jsqlparser-3.2.jar ~/tmp/_/lib/jsqlparser-3.2.jar 
mv ~/tmp/_/jdbc_lib/log4j-api-2.17.2.jar ~/tmp/_/lib/log4j-api-2.17.2.jar 
mv ~/tmp/_/jdbc_lib/log4j-core-2.17.2.jar ~/tmp/_/lib/log4j-core-2.17.2.jar 
mv ~/tmp/_/jdbc_lib/log4j-slf4j-impl-2.17.2.jar ~/tmp/_/lib/log4j-slf4j-impl-2.17.2.jar 
mv ~/tmp/_/jdbc_lib/slf4j-api-1.7.25.jar ~/tmp/_/lib/slf4j-api-1.7.25.jar 
mv ~/tmp/_/jdbc_lib/flatlaf-2.2.jar ~/tmp/_/lib/flatlaf-2.2.jar 
mv ~/tmp/_/jdbc_lib/prefuse.jar ~/tmp/_/lib/prefuse.jar 
mv ~/tmp/_/jdbc_lib/sdoc-0.5.0-beta.jar ~/tmp/_/lib/sdoc-0.5.0-beta.jar 
mv ~/tmp/_/jdbc_lib/tablefilter-swing-5.3.1.jar ~/tmp/_/lib/tablefilter-swing-5.3.1.jar 
cp ~/tmp/_/jdbc_lib/h2-1.4.199.jar ~/tmp/_/lib/h2-1.4.199.jar 
 
rm ~/tmp/_/jdbc_lib/dbunit-2.4.4.jar 
rm ~/tmp/_/jdbc_lib/h2-1.3.160.jar 
rm ~/tmp/_/jdbc_lib/h2-1.3.175.jar 
rm ~/tmp/_/jdbc_lib/jsqlparser-1.1.jar 
rm ~/tmp/_/jdbc_lib/junit-4.4.jar 
rm ~/tmp/_/jdbc_lib/postgresql-42.2.0.jre7.jar 

cd  
for file in ~/tmp/_/jdbc_lib/*; do
  mv $file $file.x
done

cd ~/tmp/_ 
echo "" > .singleuser 

/home/ralf/jdk-15.0.1/bin/jlink --add-modules java.se --output ../jre$1  
/home/ralf/jdk-15.0.1/bin/jpackage --name "Jailer" --linux-package-name jailer-database-tools --arguments "-jpack" --input . --main-jar jailer.jar --type deb --icon jailer.png --vendor Wisser --app-version "$1" --add-launcher "Jailer Data Browser"=databrowserlauncher-linux.properties --runtime-image ../jre$1 

cp *.deb /mnt/c/Users/ralfw/tmp/jailer-database-tools_$1-x64.deb
