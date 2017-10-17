# Jailer Database Tool

Jailer is a tool for database subsetting and relational data browsing. 

 - The Subsetter exports consistent, referentially intact row-sets from relational databases,
   generates topologically sorted SQL-DML, DbUnit datasets and hierarchically structured XML.

 - The Data Browser allows bidirectional navigation through the database 
   by following foreign-key-based or user-defined relationships.


<img src="/docs/screenshot.png" width="850" />

## Features

 - Exports consistent and referentially intact row-sets from your productive database
   and imports the data into your development and test environment.
 - Improves database performance by removing and archiving obsolete data without violating integrity.
 - Generates hierarchically structured XML, topologically sorted SQL-DML and DbUnit datasets.
 - Data Browsing. Navigate bidirectionally through the database by following foreign-key-based or user-defined relationships.
 - Open Source. Entirely written in Java. Platform independent. DBMS agnostic.

## News

 - 2017-10-15   SQL Console with code completion, syntax highlighting and database metadata visualization.
 - 2017-05-10   New API provides programmatic access to the data export and import functionality. http://jailer.sourceforge.net/api.html
 - 2017-03-30 	Improved filter management. Support for import-filters and literal-filters.
 - 2017-01-27 	Referential cycles can now be exported by deferring the insert of nullable foreign keys.
 - 2016-21-10 	Filter Templates allows you to define rules for assigning filters to columns.Filters on primary key columns will automatically be propagated to the corresponding foreign key columns.
 - 2015-12-04 	Support for oracle's ROWID pseudo-column.
 - 2016-09-08 	New "Export To" mode allows to export rows directly into a different schema in the same database.
 - 2015-12-04 	Support for oracle's ROWID pseudo-column.
 - 2015-10-23 	Release 5.0 introduces the ability to collect rows in a separate embedded database. This allows it to export data from read-only databases.
 - 2011-07-20 	Implemented the "Subset by Example" feature: Use the Data Browser to collect all the rows to be extracted and let Jailer create a model for that subset.
 - 2010-04-15 	A Data Browser has been introduced. Navigate bidirectionally through the database by following foreign-key-based or user-defined relationships.
 - 2008-12-23 	Jailer now supports the DbUnit flat XML dataset file format, thus allowing the users of the famous JUnit extension DbUnit to use the extracted data for unit testing.
 - 2007-12-05 	Version 2.0 comes with new graphical user interface.
 - 2007-06-05 	Tutorial for Jailer now available.


# Installation

Unzip the file "jailer_<N>.zip".

- Database Subsetter

On windows platform execute "Jailer.exe". You can also start "jailerGUI.bat".
On Unix/Linux platform execute the script "jailerGUI.sh" or use "java -jar jailer.jar"


- Data Browser

On windows platform execute "jailerDataBrowser.exe", or "jailerDataBrowser.bat"
On Unix/Linux platform execute the script "jailerDataBrowser.sh"




- Home:    http://jailer.sourceforge.net/
- Forum:   https://sourceforge.net/p/jailer/discussion/
- Support: rwisser@users.sourceforge.net
