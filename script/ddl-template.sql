-- Jailer

DROP TABLE ENTITY;
DROP TABLE DEPENDENCY;
DROP TABLE ENTITY_SET_ELEMENT;
DROP TABLE ENTITY_GRAPH;

CREATE TABLE ENTITY_GRAPH
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL DEFAULT 1,

   CONSTRAINT j_pk_graph PRIMARY KEY(id)
);

CREATE TABLE ENTITY
(
   r_entitygraph   INTEGER NOT NULL,
   {0},
   birthday        INTEGER NOT NULL,
   type            varchar(50) NOT NULL,

   {1},
   PRE_TYPE        varchar(50),
   association     INTEGER,

   CONSTRAINT j_pk_entity PRIMARY KEY(r_entitygraph, {4}),
   CONSTRAINT j_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES entity_graph(id)
);

CREATE INDEX ix_entity_brthdy ON entity (r_entitygraph, birthday, type);

CREATE TABLE ENTITY_SET_ELEMENT
(
   set_id          INTEGER NOT NULL,
   {0},

   CONSTRAINT j_pk_entityset PRIMARY KEY(set_id, {4})
);

CREATE TABLE DEPENDENCY
(
   r_entitygraph   INTEGER NOT NULL,
   {2},
   {3},   

   CONSTRAINT j_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES entity_graph(id)
);

CREATE INDEX ix_dependency_from ON dependency (r_entitygraph, {5});
CREATE INDEX ix_dependency_to ON dependency (r_entitygraph, {6});

