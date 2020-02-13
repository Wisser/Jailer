
rm -r ~/tmp/_
mkdir ~/tmp/_
rm -r ~/tmp/myjre
cd ..

cp -r bookmark ~/tmp/_
cp -r build.xml ~/tmp/_
cp -r config ~/tmp/_
cp -r datamodel ~/tmp/_
cp -r dbeauty.bat ~/tmp/_
cp -r dbeauty.exe ~/tmp/_
cp -r dbeauty.sh ~/tmp/_
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

cd ~/tmp/_
echo "" > .singleuser

~/jdk-14/bin/jlink --add-modules java.base,java.datatransfer,java.desktop,java.logging,java.management,java.scripting,java.sql,java.xml,java.rmi,java.scripting,java.xml.crypto --output myjre 
~/jdk-14/bin/jpackage --name Jailer --input . --main-jar jailer.jar --type deb --icon jailer.png --vendor Wisser --app-version 2.8 --add-launcher "Jailer Data Browser"=databrowserlauncher.properties --runtime-image myjre
