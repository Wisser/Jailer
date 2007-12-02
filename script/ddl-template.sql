-- Jailer

DROP TABLE JL_ENTITY;
DROP TABLE JL_DEPENDENCY;
DROP TABLE JL_SET;
DROP TABLE JL_GRAPH;

CREATE TABLE JL_GRAPH
(
   id              INTEGER NOT NULL,
   age             INTEGER NOT NULL,

   CONSTRAINT jl_pk_graph PRIMARY KEY(id)
);

CREATE TABLE JL_ENTITY
(
   r_entitygraph   INTEGER NOT NULL,
   {0},
   birthday        INTEGER NOT NULL,
   type            varchar(50) NOT NULL,

   {1},
   PRE_TYPE        varchar(50),
   association     INTEGER,

   CONSTRAINT jl_pk_entity PRIMARY KEY(r_entitygraph, {4}),
   CONSTRAINT jl_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES JL_GRAPH(id)
);

CREATE INDEX jl_enty_brthdy ON JL_ENTITY (r_entitygraph, birthday, type);

CREATE TABLE JL_SET
(
   set_id          INTEGER NOT NULL,
   {0},

   CONSTRAINT jl_pk_set PRIMARY KEY(set_id, {4})
);

CREATE TABLE JL_DEPENDENCY
(
   r_entitygraph   INTEGER NOT NULL,
   {2},
   {3},   

   CONSTRAINT jl_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES JL_GRAPH(id)
);

CREATE INDEX jl_dep_from ON JL_DEPENDENCY (r_entitygraph, {5});
CREATE INDEX jl_dep_to ON JL_DEPENDENCY (r_entitygraph, {6});

