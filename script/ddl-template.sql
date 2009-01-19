DROP TABLE JAILER_ENTITY${table-suffix};
DROP TABLE JAILER_DEPENDENCY${table-suffix};
DROP TABLE JAILER_SET${table-suffix};
DROP TABLE JAILER_GRAPH${table-suffix};
DROP TABLE JAILER_CONFIG${table-suffix};
DROP TABLE JAILER_TMP${table-suffix};

CREATE TABLE JAILER_CONFIG${table-suffix}
(
   jversion        VARCHAR(20),
   jkey            VARCHAR(200),
   jvalue          VARCHAR(2000)
);

INSERT INTO ${config-dml-reference}(jversion, jkey, jvalue) values('${version}', 'magic', '837065098274756382534403654245288');

CREATE TABLE JAILER_GRAPH${table-suffix}
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL,
      
   CONSTRAINT jlr_pk_graph PRIMARY KEY(id)
);

CREATE TABLE JAILER_ENTITY${table-suffix}
(
   r_entitygraph   INTEGER NOT NULL,

   ${upk},
   birthday        INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,

   ${pre},
   PRE_TYPE        VARCHAR(60)${constraint},
   association     INTEGER${constraint},

   CONSTRAINT jlr_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES JAILER_GRAPH(id)
);

CREATE INDEX jlr_enty_upk ON JAILER_ENTITY${table-suffix} (r_entitygraph, ${column-list}, type);
CREATE INDEX jlr_enty_brthdy ON JAILER_ENTITY${table-suffix} (r_entitygraph, birthday, type);

CREATE TABLE JAILER_SET${table-suffix}
(
   set_id          INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,
   ${upk}
);

CREATE INDEX jlr_pk_set ON JAILER_SET${table-suffix} (set_id, ${column-list}, type);

CREATE TABLE JAILER_DEPENDENCY${table-suffix}
(
   r_entitygraph   INTEGER NOT NULL,
   assoc           INTEGER NOT NULL,
   depend_id       INTEGER NOT NULL,
   traversed       INTEGER${constraint},
   from_type       VARCHAR(60) NOT NULL,
   to_type         VARCHAR(60) NOT NULL,
   ${from},
   ${to},   

   CONSTRAINT jlr_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES JAILER_GRAPH(id)
);

CREATE INDEX jlr_dep_from ON JAILER_DEPENDENCY${table-suffix} (r_entitygraph, assoc, ${column-list-from});
CREATE INDEX jlr_dep_to ON JAILER_DEPENDENCY${table-suffix} (r_entitygraph, ${column-list-to});

CREATE TABLE JAILER_TMP${table-suffix} (c1 INTEGER, c2 INTEGER);

INSERT INTO ${config-dml-reference}(jversion, jkey, jvalue) values('${version}', 'upk', '${upk}');
