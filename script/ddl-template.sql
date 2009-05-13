${drop-table}JAILER_ENTITY${table-suffix};
${drop-table}JAILER_DEPENDENCY${table-suffix};
${drop-table}JAILER_SET${table-suffix};
${drop-table}JAILER_GRAPH${table-suffix};
${drop-table}JAILER_CONFIG${table-suffix};
${drop-table}JAILER_TMP${table-suffix};

${create-table}JAILER_CONFIG${table-suffix}
(
   jversion        VARCHAR(20),
   jkey            VARCHAR(200),
   jvalue          VARCHAR(200)
) ${create-table-suffix};

INSERT INTO ${config-dml-reference}(jversion, jkey, jvalue) values('${version}', 'magic', '837065098274756382534403654245288');

${create-table}JAILER_GRAPH${table-suffix}
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL
      
--   ,CONSTRAINT jlr_pk_graph PRIMARY KEY(id)
) ${create-table-suffix};

${create-table}JAILER_ENTITY${table-suffix}
(
   r_entitygraph   INTEGER NOT NULL,

   ${upk},
   birthday        INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,

   ${pre},
   PRE_TYPE        VARCHAR(60)${constraint},
   orig_birthday   INTEGER,
   association     INTEGER${constraint}

-- ,  CONSTRAINT jlr_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES JAILER_GRAPH${table-suffix}(id)
) ${create-table-suffix};

${create-index}jlr_enty_brthdy${table-suffix} ON ${index-table-prefix}JAILER_ENTITY${table-suffix} (r_entitygraph, birthday, type) ${create-index-suffix};
${for-each:column-list}${create-index}jlr_enty_upk${table-suffix}$i ON ${index-table-prefix}JAILER_ENTITY${table-suffix} (r_entitygraph $, type) ${create-index-suffix};
${end}

${create-table}JAILER_SET${table-suffix}
(
   set_id          INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,
   ${upk}
) ${create-table-suffix};

${for-each:column-list}${create-index}jlr_pk_set${table-suffix}$i ON ${index-table-prefix}JAILER_SET${table-suffix} (set_id $, type) ${create-index-suffix};
${end}

${create-table}JAILER_DEPENDENCY${table-suffix}
(
   r_entitygraph   INTEGER NOT NULL,
   assoc           INTEGER NOT NULL,
   depend_id       INTEGER NOT NULL,
   traversed       INTEGER${constraint},
   from_type       VARCHAR(60) NOT NULL,
   to_type         VARCHAR(60) NOT NULL,
   ${from},
   ${to}   

-- ,  CONSTRAINT jlr_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES JAILER_GRAPH${table-suffix}(id)
) ${create-table-suffix};

${for-each:column-list-from}${create-index}jlr_dep_from${table-suffix}$i ON ${index-table-prefix}JAILER_DEPENDENCY${table-suffix} (r_entitygraph, assoc $) ${create-index-suffix};
${end}
${for-each:column-list-to}${create-index}jlr_dep_to${table-suffix}$i ON ${index-table-prefix}JAILER_DEPENDENCY${table-suffix} (r_entitygraph $) ${create-index-suffix};
${end}

${create-table}JAILER_TMP${table-suffix} 
(
    c1 INTEGER, 
    c2 INTEGER
) ${create-table-suffix};

INSERT INTO ${config-dml-reference}(jversion, jkey, jvalue) values('${version}', 'upk', '${upk-hash}');
