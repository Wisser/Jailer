-- SQL Export Tool

drop table entity;
drop table dependency;
drop table entity_set_element;
drop table entity_graph;

CREATE TABLE entity_graph
(
   id              INTEGER NOT NULL,
   created         TIMESTAMP NOT NULL WITH DEFAULT CURRENT TIMESTAMP,
   age             INTEGER NOT NULL WITH DEFAULT 1,

   CONSTRAINT pk_graph PRIMARY KEY(id)
);

CREATE TABLE entity
(
   r_entitygraph   INTEGER NOT NULL,
   {0},
   birthday        INTEGER NOT NULL,
   type            varchar(50) NOT NULL,

   {1},
   PRE_TYPE        varchar(50),
   association     INTEGER,

   CONSTRAINT pk_entity PRIMARY KEY(r_entitygraph, {4}),
   CONSTRAINT fk_graph FOREIGN KEY (r_entitygraph) REFERENCES entity_graph(id)
);

CREATE INDEX ix_entity_brthdy ON entity (r_entitygraph, birthday, type);

CREATE TABLE entity_set_element
(
   set_id          INTEGER NOT NULL,
   {0},

   CONSTRAINT pk_entity PRIMARY KEY(set_id, {4})
);

CREATE TABLE dependency
(
   r_entitygraph   INTEGER NOT NULL,
   {2},
   {3},   

   CONSTRAINT fk_graph FOREIGN KEY (r_entitygraph) REFERENCES entity_graph(id)
);

CREATE INDEX ix_dependency_from ON dependency (r_entitygraph, {5});
CREATE INDEX ix_dependency_to ON dependency (r_entitygraph, {6});

