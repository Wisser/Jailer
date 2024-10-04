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
cp -r demo-sakila.mv.db ~/tmp/_ 
cp -r demo-scott.mv.db ~/tmp/_ 
cp -r demo-scott-subset.mv.db ~/tmp/_ 
cp -r driverlist.csv ~/tmp/_ 
cp -r extractionmodel ~/tmp/_ 
cp -r jailer.bat ~/tmp/_ 
cp -r Jailer.exe ~/tmp/_ 
cp -r jailer.jar ~/tmp/_ 
cp -r jailer.sh ~/tmp/_ 
cp -r jailer.json ~/tmp/_ 
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
 
mv ~/tmp/_/jdbc_lib/args4j.jar ~/tmp/_/lib/args4j.jar 
mv ~/tmp/_/jdbc_lib/liquibase-core-4.27.0.jar ~/tmp/_/lib/liquibase-core-4.27.0.jar
mv ~/tmp/_/jdbc_lib/commons-collections4-4.4.jar ~/tmp/_/lib/commons-collections4-4.4.jar
mv ~/tmp/_/jdbc_lib/commons-lang3-3.14.0.jar ~/tmp/_/lib/commons-lang3-3.14.0.jar
mv ~/tmp/_/jdbc_lib/commons-text-1.11.0.jar ~/tmp/_/lib/commons-text-1.11.0.jar
mv ~/tmp/_/jdbc_lib/opencsv-5.9.jar ~/tmp/_/lib/opencsv-5.9.jar
mv ~/tmp/_/jdbc_lib/snakeyaml-2.2.jar ~/tmp/_/lib/snakeyaml-2.2.jar
mv ~/tmp/_/jdbc_lib/jackson-core-2.16.1.jar ~/tmp/_/lib/jackson-core-2.16.1.jar
mv ~/tmp/_/jdbc_lib/jackson-annotations-2.16.1.jar ~/tmp/_/lib/jackson-annotations-2.16.1.jar
mv ~/tmp/_/jdbc_lib/jackson-databind-2.16.1.jar ~/tmp/_/lib/jackson-databind-2.16.1.jar
mv ~/tmp/_/jdbc_lib/jsqlparser-3.2.jar ~/tmp/_/lib/jsqlparser-3.2.jar 
mv ~/tmp/_/jdbc_lib/log4j-api-2.17.2.jar ~/tmp/_/lib/log4j-api-2.17.2.jar 
mv ~/tmp/_/jdbc_lib/log4j-core-2.17.2.jar ~/tmp/_/lib/log4j-core-2.17.2.jar 
mv ~/tmp/_/jdbc_lib/log4j-slf4j-impl-2.17.2.jar ~/tmp/_/lib/log4j-slf4j-impl-2.17.2.jar 
mv ~/tmp/_/jdbc_lib/slf4j-api-1.7.25.jar ~/tmp/_/lib/slf4j-api-1.7.25.jar 
mv ~/tmp/_/jdbc_lib/flatlaf-3.3.jar ~/tmp/_/lib/flatlaf-3.3.jar 
mv ~/tmp/_/jdbc_lib/prefuse.jar ~/tmp/_/lib/prefuse.jar 
mv ~/tmp/_/jdbc_lib/sdoc-0.5.0-beta.jar ~/tmp/_/lib/sdoc-0.5.0-beta.jar 
mv ~/tmp/_/jdbc_lib/tablefilter-swing-5.3.1.jar ~/tmp/_/lib/tablefilter-swing-5.3.1.jar 

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

jlink --add-options="-Xmx4000m" --add-modules java.se --output ../jre$1  

echo "" > .singleuser 
jpackage --name "Jailer Database Tools" --linux-package-name jailer-database-tools --arguments "-jpackrpm" --type rpm --input . --main-jar jailer.jar  --icon jailer.png --vendor Wisser --app-version "$1" --runtime-image ../jre$1 
mv *.rpm /mnt/c/Users/ralfw/tmp/jailer-database-tools_$1-x64.rpm
jpackage --name "Jailer Database Tools" --linux-package-name jailer-database-tools --arguments "-jpackrpm" --type deb --input . --main-jar jailer.jar  --icon jailer.png --vendor Wisser --app-version "$1" --runtime-image ../jre$1 
mv *.deb /mnt/c/Users/ralfw/tmp/jailer-database-tools_$1-x64.deb
