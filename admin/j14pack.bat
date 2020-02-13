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

cp -r c:\tmp\_\lib c:\tmp\_\jdbc_lib
rm c:\tmp\_\lib\*
rm c:\tmp\_\*.bat
rm c:\tmp\_\*.exe
rm c:\tmp\_\*.sh

mv c:\tmp\_\jdbc_lib\activation-1.0.2.jar c:\tmp\_\lib\activation-1.0.2.jar
mv c:\tmp\_\jdbc_lib\args4j.jar c:\tmp\_\lib\args4j.jar
mv c:\tmp\_\jdbc_lib\jaxb-api-2.3.0-b170201.1204.jar c:\tmp\_\lib\jaxb-api-2.3.0-b170201.1204.jar
mv c:\tmp\_\jdbc_lib\jaxb-core-2.3.0-b170127.1453.jar c:\tmp\_\lib\jaxb-core-2.3.0-b170127.1453.jar
mv c:\tmp\_\jdbc_lib\jaxb-impl-2.3.0-b170127.1453.jar c:\tmp\_\lib\jaxb-impl-2.3.0-b170127.1453.jar
mv c:\tmp\_\jdbc_lib\jsqlparser-1.3.jar c:\tmp\_\lib\jsqlparser-1.3.jar
mv c:\tmp\_\jdbc_lib\log4j.jar c:\tmp\_\lib\log4j.jar
mv c:\tmp\_\jdbc_lib\prefuse.jar c:\tmp\_\lib\prefuse.jar
mv c:\tmp\_\jdbc_lib\sdoc-0.5.0-beta.jar c:\tmp\_\lib\sdoc-0.5.0-beta.jar
mv c:\tmp\_\jdbc_lib\tablefilter-swing-5.3.1.jar c:\tmp\_\lib\tablefilter-swing-5.3.1.jar

rm c:\tmp\_\jdbc_lib\dbunit-2.4.4.jar
rm c:\tmp\_\jdbc_lib\h2-1.3.160.jar
rm c:\tmp\_\jdbc_lib\h2-1.3.175.jar
rm c:\tmp\_\jdbc_lib\jsqlparser-1.1.jar
rm c:\tmp\_\jdbc_lib\junit-4.4.jar
rm c:\tmp\_\jdbc_lib\postgresql-42.2.0.jre7.jar

cd 
SETLOCAL ENABLEDELAYEDEXPANSION
for /f "tokens=*" %%f in ('dir /b c:\tmp\_\jdbc_lib\*') do (
  move c:\tmp\_\jdbc_lib\"%%f" c:\tmp\_\jdbc_lib\"%%f.x"
)

cd c:\tmp\_
echo "" > .singleuser

jlink --add-modules java.base,java.datatransfer,java.desktop,java.logging,java.management,java.scripting,java.sql,java.xml,java.rmi,java.scripting,java.xml.crypto --output myjre 
jpackage --name myapp --input . --main-jar jailer.jar --type msi --icon jailer.ico --win-menu --win-menu-group JailerJ14 --vendor Wisser --app-version 2.26 --win-upgrade-uuid d636b4ee-6f10-451e-bf57-c89656780e22 --add-launcher "Jailer Data Browser"=databrowserlauncher.properties --runtime-image myjre


pause
