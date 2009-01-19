DROP TABLE JAILER_ENTITY;
DROP TABLE JAILER_DEPENDENCY;
DROP TABLE JAILER_SET;
DROP TABLE JAILER_GRAPH;
DROP TABLE JAILER_CONFIG;
DROP TABLE JAILER_TMP;

CREATE TABLE JAILER_CONFIG
(
   jversion        VARCHAR(20),
   jkey            VARCHAR(200),
   jvalue          VARCHAR(2000)
);

INSERT INTO JAILER_CONFIG(jversion, jkey, jvalue) values('${version}', 'magic', '837065098274756382534403654245288');

CREATE TABLE JAILER_GRAPH
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL,
      
   CONSTRAINT jlr_pk_graph PRIMARY KEY(id)
);

CREATE TABLE JAILER_ENTITY
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

CREATE INDEX jlr_enty_upk ON JAILER_ENTITY (r_entitygraph, ${column-list}, type);
CREATE INDEX jlr_enty_brthdy ON JAILER_ENTITY (r_entitygraph, birthday, type);

CREATE TABLE JAILER_SET
(
   set_id          INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,
   ${upk}
);

CREATE INDEX jlr_pk_set ON JAILER_SET (set_id, ${column-list}, type);

CREATE TABLE JAILER_DEPENDENCY
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

CREATE INDEX jlr_dep_from ON JAILER_DEPENDENCY (r_entitygraph, assoc, ${column-list-from});
CREATE INDEX jlr_dep_to ON JAILER_DEPENDENCY (r_entitygraph, ${column-list-to});

CREATE TABLE JAILER_TMP (c1 INTEGER, c2 INTEGER);

INSERT INTO JAILER_CONFIG(jversion, jkey, jvalue) values('${version}', 'upk', '${upk}');
