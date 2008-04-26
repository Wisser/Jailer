mv .svn ~/
find . -name "styles.css" -exec cp style.css '{}' \;
find . -name "logo.gif" -exec cp logo.gif '{}' \;
find . -name "*.htm*" -exec sed "s|file:///C:/Users/ralf/Documents/Open%20Jail.%20The%20Jailer%20Project%20Web%20Site/local/preview/pages/||g" '{}' --in-place \;
find . -iname "*-Dateien" -exec cp vgradp.gif '{}'/  \;
find . -iname "*-Dateien" -exec cp bg1.JPG '{}'/  \;
mv ~/.svn .
find . -name "*.htm*" -exec sed "s|<meta name=\"GENERATOR\" content=.*>||g" '{}' --in-place \;
find . -name "*.htm*" -exec sed "s|<a href=\"http://www.ewisoft.com/\".*</a>|</title>|g" '{}' --in-place \;

