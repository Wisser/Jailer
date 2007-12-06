rm -rf ~/tmp/$1
mkdir ~/tmp/$1
cp -r . ~/tmp/$1
cd ~/tmp/$1
find -iname ".svn" -exec rm -rf '{}' \;
find -iname ".cvs*" -exec rm -rf '{}' \;
rm -rf *.log
rm -rf _*
rm -rf admin
rm jailer_ddl.sql
rm *.csv
rm render/*.html
rm -rf nbproject
rm -rf out
rm upload.sh
rm release.sh
rm *.sql
rm svn-commit.tmp
rm -rf .connect.ui .buildmodel.ui.original .classpath .export.ui .project .buildmodel.ui.r133 .cp.ui .plaf.ui .buildmodel.ui.r135 .exportdata.ui .printmodel.ui
rm datamodel/model-builder-association.csv datamodel/model-builder-table.csv
ls jailer.jar
cd ..
rm $1.zip
zip -r $1.zip $1 
