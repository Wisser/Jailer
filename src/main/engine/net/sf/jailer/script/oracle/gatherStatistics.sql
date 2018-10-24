{call dbms_stats.gather_table_stats('${SCHEMA_JAILER_GRAPH}', '${TABLE_JAILER_GRAPH}', cascade => TRUE)};
{call dbms_stats.gather_table_stats('${SCHEMA_JAILER_ENTITY}', '${TABLE_JAILER_ENTITY}', cascade => TRUE)};
{call dbms_stats.gather_table_stats('${SCHEMA_JAILER_SET}', '${TABLE_JAILER_SET}', cascade => TRUE)};
{call dbms_stats.gather_table_stats('${SCHEMA_JAILER_DEPENDENCY}', '${TABLE_JAILER_DEPENDENCY}', cascade => TRUE)};
