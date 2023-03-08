${drop-table}${schema}JAILER_ENTITY${table-suffix};
${drop-table}${schema}JAILER_DEPENDENCY${table-suffix};
${drop-table}${schema}JAILER_SET${table-suffix};
${drop-table}${schema}JAILER_GRAPH${table-suffix};
${drop-table}${schema}JAILER_CONFIG${table-suffix};
${drop-table}${schema}JAILER_TMP${table-suffix};

${create-table}${schema}JAILER_CONFIG${table-suffix}
(
   jversion        VARCHAR(20),
   jkey            VARCHAR(200),
   jvalue          VARCHAR(200)
) ${create-table-suffix};

INSERT INTO ${schema}${config-dml-reference}${table-suffix}(jversion, jkey, jvalue) values('${version}', 'magic', '837065098274756382534403654245288');

${create-table}${schema}JAILER_GRAPH${table-suffix}
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL
      
--   ,CONSTRAINT jlr_pk_graph PRIMARY KEY(id)
) ${create-table-suffix};

${create-table}${schema}JAILER_ENTITY${table-suffix}
(
   r_entitygraph   INTEGER NOT NULL,

   ${upk},
   birthday        INTEGER NOT NULL,
   type            INTEGER NOT NULL,

   orig_birthday   INTEGER${constraint},
   association     INTEGER${constraint}

-- ,  CONSTRAINT jlr_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES ${schema}JAILER_GRAPH${table-suffix}(id)
) ${create-table-suffix};

${create-index}${index-schema}jlr_enty_brthdy${table-suffix} ON ${index-table-prefix}${schema}JAILER_ENTITY${table-suffix} (r_entitygraph, type, birthday) ${create-index-suffix};
${for-each:column-list}${create-unique-index}${index-schema}jlr_enty_upk${table-suffix}$i ON ${index-table-prefix}${schema}JAILER_ENTITY${table-suffix} (r_entitygraph $, type, birthday) ${create-index-suffix};
${end}

${create-table}${schema}JAILER_SET${table-suffix}
(
   set_id          INTEGER NOT NULL,
   type            INTEGER NOT NULL,
   ${upk}
) ${create-table-suffix};

${for-each:column-list}${create-index}${index-schema}jlr_pk_set${table-suffix}$i ON ${index-table-prefix}${schema}JAILER_SET${table-suffix} (set_id $, type) ${create-index-suffix};
${end}

${create-table}${schema}JAILER_DEPENDENCY${table-suffix}
(
   r_entitygraph   INTEGER NOT NULL,
   assoc           INTEGER NOT NULL,
   depend_id       INTEGER NOT NULL,
   traversed       INTEGER${constraint},
   from_type       INTEGER NOT NULL,
   to_type         INTEGER NOT NULL,
   ${from},
   ${to}   

-- ,  CONSTRAINT jlr_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES ${schema}JAILER_GRAPH${table-suffix}(id)
) ${create-table-suffix};

${for-each:column-list-from}${create-index}${index-schema}jlr_dep_from${table-suffix}$i ON ${index-table-prefix}${schema}JAILER_DEPENDENCY${table-suffix} (r_entitygraph, assoc $) ${create-index-suffix};
${end}
${for-each:column-list-to}${create-index}${index-schema}jlr_dep_to${table-suffix}$i ON ${index-table-prefix}${schema}JAILER_DEPENDENCY${table-suffix} (r_entitygraph $) ${create-index-suffix};
${end}

${create-table}${schema}JAILER_TMP${table-suffix} 
(
    c1 INTEGER, 
    c2 INTEGER
) ${create-table-suffix};

INSERT INTO ${schema}${config-dml-reference}${table-suffix}(jversion, jkey, jvalue) values('${version}', 'upk', '${upk-hash}');
