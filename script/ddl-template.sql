DROP TABLE JL_ENTITY;
DROP TABLE JL_DEPENDENCY;
DROP TABLE JL_SET;
DROP TABLE JL_GRAPH;
DROP TABLE JL_CONFIG;

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

   CONSTRAINT jl_pk_entity PRIMARY KEY(r_entitygraph, {4}, type),
   CONSTRAINT jl_fk_graph_e FOREIGN KEY (r_entitygraph) REFERENCES JL_GRAPH(id)
);

CREATE INDEX jl_enty_brthdy ON JL_ENTITY (r_entitygraph, birthday, type);

CREATE TABLE JL_SET
(
   set_id          INTEGER NOT NULL,
   type            varchar(50) NOT NULL,
   {0},

   CONSTRAINT jl_pk_set PRIMARY KEY(set_id, {4}, type)
);

CREATE TABLE JL_DEPENDENCY
(
   r_entitygraph   INTEGER NOT NULL,
   assoc           INTEGER NOT NULL,
   from_type       varchar(50) NOT NULL,
   to_type         varchar(50) NOT NULL,
   {2},
   {3},   

   CONSTRAINT jl_fk_graph_d FOREIGN KEY (r_entitygraph) REFERENCES JL_GRAPH(id)
);

CREATE INDEX jl_dep_from ON JL_DEPENDENCY (r_entitygraph, assoc, {5});
CREATE INDEX jl_dep_to ON JL_DEPENDENCY (r_entitygraph, {6});

CREATE TABLE JL_CONFIG
(
   jversion        varchar(10) NOT NULL,
   jkey            varchar(200),
   jvalue          varchar(500)
);

INSERT INTO JL_CONFIG(jversion, jkey, jvalue) values(''{7}'', ''upk'', ''{0}'');

