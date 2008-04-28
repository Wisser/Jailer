{call dbms_stats.gather_table_stats(USER, 'JL_GRAPH', cascade => TRUE)};
{call dbms_stats.gather_table_stats(USER, 'JL_ENTITY', cascade => TRUE)};
{call dbms_stats.gather_table_stats(USER, 'JL_SET', cascade => TRUE)};
{call dbms_stats.gather_table_stats(USER, 'JL_DEPENDENCY', cascade => TRUE)};
