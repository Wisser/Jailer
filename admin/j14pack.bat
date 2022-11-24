set version=%1

set PATH=C:\Program Files\Java\jdk-15\bin;%PATH%

del /S /Q c:\tmp\_
del /S /Q c:\tmp\jre%version%
mkdir c:\tmp\_
cd ..

xcopy /S /E maven-artifacts c:\tmp\_\maven-artifacts\
xcopy /S /E bookmark c:\tmp\_\bookmark\
xcopy /S /E config c:\tmp\_\config\
xcopy /S /E datamodel c:\tmp\_\datamodel\
xcopy /S /E template c:\tmp\_\template\
xcopy demo-sakila.mv.db c:\tmp\_
xcopy demo-scott.mv.db c:\tmp\_
xcopy demo-scott-subset.mv.db c:\tmp\_
xcopy driverlist.csv c:\tmp\_
xcopy /S /E extractionmodel c:\tmp\_\extractionmodel\
xcopy jailer.bat c:\tmp\_
xcopy Jailer.exe c:\tmp\_
xcopy jailer.jar c:\tmp\_
xcopy jailer.sh c:\tmp\_
xcopy jailer.xml c:\tmp\_
xcopy jailerDataBrowser.bat c:\tmp\_
xcopy JailerDataBrowser.exe c:\tmp\_
xcopy jailerDataBrowser.sh c:\tmp\_
xcopy jailerGUI.bat c:\tmp\_
xcopy jailerGUI.sh c:\tmp\_
xcopy /S /E layout c:\tmp\_\layout\
xcopy /S /E lib c:\tmp\_\lib\
xcopy license-prefuse.txt c:\tmp\_
xcopy license.txt c:\tmp\_
xcopy manifest.mf c:\tmp\_
xcopy README.md c:\tmp\_
xcopy releasenotes.txt c:\tmp\_
xcopy /S /E render c:\tmp\_\render\
xcopy admin\jailer.ico c:\tmp\_
xcopy admin\databrowserlauncher.properties c:\tmp\_

xcopy /S /E c:\tmp\_\lib c:\tmp\_\jdbc_lib\
del /Q c:\tmp\_\lib\*
del /Q c:\tmp\_\*.sh

move c:\tmp\_\jdbc_lib\activation-1.0.2.jar c:\tmp\_\lib\activation-1.0.2.jar
move c:\tmp\_\jdbc_lib\args4j.jar c:\tmp\_\lib\args4j.jar
move c:\tmp\_\jdbc_lib\jaxb-api-2.3.0-b170201.1204.jar c:\tmp\_\lib\jaxb-api-2.3.0-b170201.1204.jar
move c:\tmp\_\jdbc_lib\jaxb-core-2.3.0-b170127.1453.jar c:\tmp\_\lib\jaxb-core-2.3.0-b170127.1453.jar
move c:\tmp\_\jdbc_lib\jaxb-impl-2.3.0-b170127.1453.jar c:\tmp\_\lib\jaxb-impl-2.3.0-b170127.1453.jar
move c:\tmp\_\jdbc_lib\jsqlparser-3.2.jar c:\tmp\_\lib\jsqlparser-3.2.jar
move c:\tmp\_\jdbc_lib\log4j-api-2.17.2.jar c:\tmp\_\lib\log4j-api-2.17.2.jar
move c:\tmp\_\jdbc_lib\log4j-core-2.17.2.jar c:\tmp\_\lib\log4j-core-2.17.2.jar
move c:\tmp\_\jdbc_lib\log4j-slf4j-impl-2.17.2.jar c:\tmp\_\lib\log4j-slf4j-impl-2.17.2.jar
move c:\tmp\_\jdbc_lib\slf4j-api-1.7.25.jar c:\tmp\_\lib\slf4j-api-1.7.25.jar
move c:\tmp\_\jdbc_lib\flatlaf-2.2.jar c:\tmp\_\lib\flatlaf-2.2.jar
move c:\tmp\_\jdbc_lib\prefuse.jar c:\tmp\_\lib\prefuse.jar
move c:\tmp\_\jdbc_lib\sdoc-0.5.0-beta.jar c:\tmp\_\lib\sdoc-0.5.0-beta.jar
move c:\tmp\_\jdbc_lib\tablefilter-swing-5.3.1.jar c:\tmp\_\lib\tablefilter-swing-5.3.1.jar

del c:\tmp\_\jdbc_lib\dbunit-2.4.4.jar
del c:\tmp\_\jdbc_lib\h2-1.3.160.jar
del c:\tmp\_\jdbc_lib\h2-1.3.175.jar
del c:\tmp\_\jdbc_lib\jsqlparser-1.1.jar
del c:\tmp\_\jdbc_lib\junit-4.4.jar
del c:\tmp\_\jdbc_lib\postgresql-42.2.0.jre7.jar

cd 
SETLOCAL ENABLEDELAYEDEXPANSION
for /f "tokens=*" %%f in ('dir /b c:\tmp\_\jdbc_lib\*') do (
  move c:\tmp\_\jdbc_lib\"%%f" c:\tmp\_\jdbc_lib\"%%f.x"
)

cd c:\tmp\_
echo "" > .singleuser

jlink --add-modules ALL-MODULE-PATH --output jre%version% 
jpackage --name "Jailer Database Tools" --input . --main-jar jailer.jar --type msi --icon jailer.ico --win-menu --win-menu-group Jailer --vendor Wisser --app-version %version% --win-upgrade-uuid d636b4ee-6f10-451e-bf57-c89656780e22 --runtime-image jre%version%

move *.msi C:\Users\ralfw\tmp\"Jailer-database-tools-%version%.msi"
move *.msi c:\tmp\"Jailer-database-tools-%version%.msi"
