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

INSERT INTO JAILER_CONFIG(jversion, jkey, jvalue) values(''{7}'', ''magic'', ''837065098274756382534403654245288'');

CREATE TABLE JAILER_GRAPH
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL,
      
   CONSTRAINT jlr_pk_graph PRIMARY KEY(id)
);

CREATE TABLE JAILER_ENTITY
(
   r_entitygraph   INTEGER NOT NULL,

   {0},
   birthday        INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,

   {1},
   PRE_TYPE        VARCHAR(60){8},
   association     INTEGER{8},

   CONSTRAINT jlr_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES JAILER_GRAPH(id)
);

CREATE INDEX jlr_enty_upk ON JAILER_ENTITY (r_entitygraph, {4}, type);
CREATE INDEX jlr_enty_brthdy ON JAILER_ENTITY (r_entitygraph, birthday, type);

CREATE TABLE JAILER_SET
(
   set_id          INTEGER NOT NULL,
   type            VARCHAR(60) NOT NULL,
   {0}
);

CREATE INDEX jlr_pk_set ON JAILER_SET (set_id, {4}, type);

CREATE TABLE JAILER_DEPENDENCY
(
   r_entitygraph   INTEGER NOT NULL,
   assoc           INTEGER NOT NULL,
   depend_id       INTEGER NOT NULL,
   traversed       INTEGER{8},
   from_type       VARCHAR(60) NOT NULL,
   to_type         VARCHAR(60) NOT NULL,
   {2},
   {3},   

   CONSTRAINT jlr_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES JAILER_GRAPH(id)
);

CREATE INDEX jlr_dep_from ON JAILER_DEPENDENCY (r_entitygraph, assoc, {5});
CREATE INDEX jlr_dep_to ON JAILER_DEPENDENCY (r_entitygraph, {6});

CREATE TABLE JAILER_TMP (c1 INTEGER, c2 INTEGER);

INSERT INTO JAILER_CONFIG(jversion, jkey, jvalue) values(''{7}'', ''upk'', ''{0}'');
