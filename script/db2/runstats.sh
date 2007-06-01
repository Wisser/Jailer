db2 connect to $1 user $2 using $3
db2 runstats on table xbcsetup.entity_graph and indexes all
db2 runstats on table xbcsetup.entity and indexes all
db2 runstats on table xbcsetup.dependency and indexes all
db2 runstats on table xbcsetup.ENTITY_SET_ELEMENT and indexes all
db2 disconnect $1

