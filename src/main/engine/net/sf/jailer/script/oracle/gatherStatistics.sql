{call dbms_stats.gather_table_stats(USER, '${JAILER_GRAPH}', cascade => TRUE)};
{call dbms_stats.gather_table_stats(USER, '${JAILER_ENTITY}', cascade => TRUE)};
{call dbms_stats.gather_table_stats(USER, '${JAILER_SET}', cascade => TRUE)};
{call dbms_stats.gather_table_stats(USER, '${JAILER_DEPENDENCY}', cascade => TRUE)};
