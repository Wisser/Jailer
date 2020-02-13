set PATH=d:\jdk-14\bin;%PATH%

rm -r c:\tmp\_
rm -r c:\tmp\myjre
mkdir c:\tmp\_
cd ..

cp -r bookmark c:\tmp\_
cp -r build.xml c:\tmp\_
cp -r config c:\tmp\_
cp -r datamodel c:\tmp\_
cp -r dbeauty.bat c:\tmp\_
cp -r dbeauty.exe c:\tmp\_
cp -r dbeauty.sh c:\tmp\_
cp -r demo-sakila-1.4.mv.db c:\tmp\_
cp -r demo-scott-1.4.mv.db c:\tmp\_
cp -r demo-scott-subset-1.4.mv.db c:\tmp\_
cp -r driverlist.csv c:\tmp\_
cp -r extractionmodel c:\tmp\_
cp -r jailer.bat c:\tmp\_
cp -r Jailer.exe c:\tmp\_
cp -r jailer.jar c:\tmp\_
cp -r jailer.sh c:\tmp\_
cp -r jailer.xml c:\tmp\_
cp -r jailerDataBrowser.bat c:\tmp\_
cp -r JailerDataBrowser.exe c:\tmp\_
cp -r jailerDataBrowser.sh c:\tmp\_
cp -r jailerGUI.bat c:\tmp\_
cp -r jailerGUI.sh c:\tmp\_
cp -r layout c:\tmp\_
cp -r lib c:\tmp\_
cp -r license-prefuse.txt c:\tmp\_
cp -r license.txt c:\tmp\_
cp -r manifest.mf c:\tmp\_
cp -r README.md c:\tmp\_
cp -r releasenotes.txt c:\tmp\_
cp -r render c:\tmp\_
cp admin\jailer.ico c:\tmp\_
cp admin\databrowserlauncher.properties c:\tmp\_

cd c:\tmp\_
echo "" > .singleuser

jlink --add-modules java.base,java.datatransfer,java.desktop,java.logging,java.management,java.scripting,java.sql,java.xml,java.rmi,java.scripting,java.xml.crypto --output myjre 
jpackage --name myapp --input . --main-jar jailer.jar --type msi --icon jailer.ico --win-menu --win-menu-group JailerJ14 --vendor Wisser --app-version 2.12 --win-upgrade-uuid d636b4ee-6f10-451e-bf57-c89656780e22 --add-launcher "Jailer Data Browser"=databrowserlauncher.properties --runtime-image myjre


pause
