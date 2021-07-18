# Jailer Database Tool

Jailer is a tool for database subsetting and relational data browsing.

 - The Subsetter exports consistent, referentially intact row-sets from relational databases,
   generates topologically sorted SQL-DML, DbUnit datasets and XML.
 - The Data Browser allows bidirectional navigation through the database
   by following foreign-key-based or user-defined relationships.


<img src="/docs/screenshot.png" width="850" />


## Features

 - Exports consistent and referentially intact row-sets from your productive database
   and imports the data into your development and test environment.
 - Improves database performance by removing and archiving obsolete data without violating integrity.
 - Generates topologically sorted SQL-DML, hierarchically structured XML and DbUnit datasets.
 - Data Browsing. Navigate bidirectionally through the database by following foreign-key-based or user-defined relationships.
 - SQL Console with code completion, syntax highlighting and database metadata visualization.
 - A demo database is included with which you can get a first impression without any configuration effort.


## Prerequisites

 - Java JRE 8 (or above)
    - Windows and Linux installation programs (\*.msi and \*.deb) already contain a JRE. If you use them, no further installation is needed.
    - Important: due to HiDPI graphics support, Java JRE 11 (or above) is strongly recommended.
 - JDBC-driver for your RDBMS
     - For most  popular database systems a driver is already included.

## News

 - 2021-02-04    Cycles in parent-child relationships will be detected and broken. Thus, such data can be exported by deferring the insertion of nullable foreign keys.
 - 2020-01-01    The Jailer engine is published in Maven repository. https://mvnrepository.com/artifact/io.github.wisser/jailer-engine
 - 2019-02-01    The new "Model Migration Tool" allows you to easily find and edit the newly added associations if the data model has been extended after the last change to this extraction model.
 - 2018-04-26    The new feature "Analyze SQL" analyzes SQL statements and proposes association definitions. This allows to reverse-engineer the data model based on existing SQL queries.
 - 2018-03-06    SQL Console with code completion, syntax highlighting and database metadata visualization.
 - 2017-05-10    New API provides programmatic access to the data export and import functionality. http://jailer.sourceforge.net/api.html
 - 2017-03-30    Improved filter management. Templates allows you to define rules for assigning filters to columns. Filters on primary key columns will automatically be propagated to the corresponding foreign key columns. http://jailer.sourceforge.net/filters.html
 - 2015-12-04    Data can now also be exported directly to a schema of the same database. This ensures optimal performance.
 - 2015-10-23    Rows can alternatively be collected in a separate embedded database. This allows exporting data from read-only databases.
 - 2014-07-20    Implemented the "Subset by Example" feature: Use the Data Browser to collect all the rows to be extracted and let Jailer create a model for that subset. http://jailer.sourceforge.net/subset-by-example.html
 - 2014-04-15    A Data Browser has been introduced. Navigate bidirectionally through the database by following foreign-key-based or user-defined relationships.



## Installation

If you do not want to install Java yourself, use the installation file "Jailer-n.n.n-with-java-JRE.msi" (for Windows) or "jailer-database-tools_10.2.2-x64-with-java-JRE.deb" (for Linux).

Otherwise use the installer "Jailer-Install-n.n.n.exe" or unzip the file "jailer_n.n.n.zip".
See also <a href="http://jailer.sourceforge.net/faq.html#multiuser">http://jailer.sourceforge.net/faq.html#multiuser</a>

To start the tool from the unpacked zip:

 - Database Subsetter
    - On windows platform execute "Jailer.exe". You can also start "jailerGUI.bat".
    - On Unix/Linux platform execute the script "jailerGUI.sh" or use "java -jar jailer.jar"

 - Data Browser
    - On windows platform execute "jailerDataBrowser.exe", or "jailerDataBrowser.bat"
    - On Unix/Linux platform execute the script "jailerDataBrowser.sh"


## Building

Clone the git repository:

* `git clone https://github.com/Wisser/Jailer.git`

To build the tool you can just use ant: ( https://ant.apache.org )

* `cd Jailer`
* `ant`


## Contact
 - Home:    http://jailer.sourceforge.net/ or https://github.com/Wisser/Jailer
 - Forum:   https://sourceforge.net/p/jailer/discussion/
 - Support: rwisser@users.sourceforge.net


## Contributors

### Code Contributors

This project exists thanks to all the people who contribute.
<a href="https://github.com/Wisser/Jailer/graphs/contributors"><img src="https://opencollective.com/Jailer/contributors.svg?width=890&button=false" /></a>

### Financial Contributors

Become a financial contributor and help us sustain our community. [[Contribute](https://opencollective.com/Jailer/contribute)]

#### Individuals

<a href="https://opencollective.com/Jailer"><img src="https://opencollective.com/Jailer/individuals.svg?width=890"></a>

#### Organizations

Support this project with your organization. Your logo will show up here with a link to your website. [[Contribute](https://opencollective.com/Jailer/contribute)]

<a href="https://opencollective.com/Jailer/organization/0/website"><img src="https://opencollective.com/Jailer/organization/0/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/1/website"><img src="https://opencollective.com/Jailer/organization/1/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/2/website"><img src="https://opencollective.com/Jailer/organization/2/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/3/website"><img src="https://opencollective.com/Jailer/organization/3/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/4/website"><img src="https://opencollective.com/Jailer/organization/4/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/5/website"><img src="https://opencollective.com/Jailer/organization/5/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/6/website"><img src="https://opencollective.com/Jailer/organization/6/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/7/website"><img src="https://opencollective.com/Jailer/organization/7/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/8/website"><img src="https://opencollective.com/Jailer/organization/8/avatar.svg"></a>
<a href="https://opencollective.com/Jailer/organization/9/website"><img src="https://opencollective.com/Jailer/organization/9/avatar.svg"></a>
