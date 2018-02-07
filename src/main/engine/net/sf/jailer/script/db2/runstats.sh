db2 connect to $1 user $2 using $3
db2 runstats on table $2.JAILER_GRAPH and indexes all
db2 runstats on table $2.JAILER_ENTITY and indexes all
db2 runstats on table $2.JAILER_DEPENDENCY and indexes all
db2 runstats on table $2.JAILER_SET and indexes all
db2 disconnect $1

