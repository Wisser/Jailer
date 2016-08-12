/*
 * Copyright 2007 - 2016 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.sf.jailer.entitygraph.intradatabase;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.Configuration;
import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.StatementBuilder;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.entitygraph.remote.RemoteEntityGraph;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.progress.ProgressListenerRegistry;
import net.sf.jailer.util.Quoting;


/**
 * Specialized {@link RemoteEntityGraph} for exporting data into a different
 * schema within the same database.
 * 
 * @author Ralf Wisser
 */
public class IntraDatabaseEntityGraph extends RemoteEntityGraph {
	
	private boolean upsertOnly;
	
	/**
	 * Constructor.
	 * 
	 * @param graphID
	 *            the unique ID of the graph
	 * @param session
	 *            for executing SQL-Statements
	 * @param universalPrimaryKey
	 *            the universal primary key
	 * @throws SQLException 
	 */
	private IntraDatabaseEntityGraph(DataModel dataModel, int graphID,
			Session session, PrimaryKey universalPrimaryKey) throws SQLException {
		super(dataModel, graphID, session, universalPrimaryKey);
		upsertOnly = CommandLineParser.getInstance().upsertOnly;
		synchronized (this) {
			upsertStrategy = null;
			upsertStrategies = new ArrayList<UpsertStrategy>();
			
			upsertStrategies.add(new MergeUS(false));
			upsertStrategies.add(new MergeUS(true));
			upsertStrategies.add(new UpsertMYSQLUS());
			upsertStrategies.add(new UpsertPGUS());
			upsertStrategies.add(new UpsertStandardUS());
		}
		quoting = new Quoting(session);
	}

	private String defaultSchema = null;
	private Quoting quoting = null;
	private Map<Table, Boolean> hasPKPerTable = new HashMap<Table, Boolean>();

	private synchronized boolean hasPrimarykey(Session session, Table table) {
		try {
			Boolean hasPrimarykey = hasPKPerTable.get(table);
			if (hasPrimarykey != null) {
				return hasPrimarykey;
			}
			
			if (defaultSchema == null) {
				defaultSchema = JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.dbUser);
			}
	
			String schema = table.getOriginalSchema("");
			String mappedSchema = CommandLineParser.getInstance()
					.getSchemaMapping().get(schema);
			if (mappedSchema != null) {
				schema = mappedSchema;
			}
			if (schema.length() == 0) {
				schema = defaultSchema;
			}
			
			schema = quoting.unquote(schema);
			String tableName = quoting.unquote(table.getUnqualifiedName());
			ResultSet resultSet = getPrimaryKeys(session, session.getMetaData(), schema, tableName);
			hasPrimarykey = resultSet.next();
			resultSet.close();
			if (!hasPrimarykey) {
				if (session.getMetaData().storesUpperCaseIdentifiers()) {
	        		schema = schema.toUpperCase();
	        		tableName = tableName.toUpperCase();
	        	} else {
	        		schema = schema.toLowerCase();
	        		tableName = tableName.toLowerCase();
	        	}
				resultSet = getPrimaryKeys(session, session.getMetaData(), schema, tableName);
				hasPrimarykey = resultSet.next();
				resultSet.close();
			}
			hasPKPerTable.put(table, hasPrimarykey);
			return hasPrimarykey;
		} catch (SQLException e) {
			return true;
		}
	}

	private ResultSet getPrimaryKeys(Session session,
			DatabaseMetaData metaData, String schema, String table)
			throws SQLException {
		if (session.dbms == DBMS.MySQL) {
			return metaData.getPrimaryKeys(schema, null, table);
		}
		return metaData.getPrimaryKeys(null, schema, table);
	}

	/**
	 * Creates a new entity-graph.
	 * 
	 * @param graphID
	 *            the unique ID of the graph
	 * @param session
	 *            for executing SQL-Statements
	 * @param universalPrimaryKey
	 *            the universal primary key
	 * @return the newly created entity-graph
	 */
	public static IntraDatabaseEntityGraph create(DataModel dataModel,
			int graphID, Session session, PrimaryKey universalPrimaryKey) throws SQLException {
		IntraDatabaseEntityGraph entityGraph = new IntraDatabaseEntityGraph(
				dataModel, graphID, session, universalPrimaryKey);
		init(graphID, session);
		return entityGraph;
	}

	/**
	 * Copies an entity-graph.
	 * 
	 * @param newGraphID
	 *            the unique ID of the new graph
	 * @param session
	 *            for executing SQL-Statements
	 * @return the newly created entity-graph
	 */
	public EntityGraph copy(int newGraphID, Session session)
			throws SQLException {
		IntraDatabaseEntityGraph entityGraph = create(dataModel, newGraphID,
				session, universalPrimaryKey);
		entityGraph.setBirthdayOfSubject(birthdayOfSubject);
		session.executeUpdate("Insert into "
				+ SQLDialect.dmlTableReference(ENTITY, session)
				+ "(r_entitygraph, " + universalPrimaryKey.columnList(null)
				+ ", birthday, orig_birthday, type) " + "Select " + newGraphID
				+ ", " + universalPrimaryKey.columnList(null)
				+ ", birthday, birthday, type From "
				+ SQLDialect.dmlTableReference(ENTITY, session)
				+ " Where r_entitygraph=" + graphID + "");
		return entityGraph;
	}

	/**
	 * Finds an entity-graph.
	 * 
	 * @param graphID
	 *            the unique ID of the graph
	 * @param universalPrimaryKey
	 *            the universal primary key
	 * @param session
	 *            for executing SQL-Statements
	 * @return the entity-graph
	 */
	public EntityGraph find(int graphID, Session session,
			PrimaryKey universalPrimaryKey) throws SQLException {
		IntraDatabaseEntityGraph entityGraph = new IntraDatabaseEntityGraph(
				dataModel, graphID, session, universalPrimaryKey);
		final boolean[] found = new boolean[1];
		found[0] = false;
		session.executeQuery(
				"Select * From "
						+ SQLDialect.dmlTableReference(ENTITY_GRAPH, session)
						+ "Where id=" + graphID + "",
				new Session.ResultSetReader() {
					public void readCurrentRow(ResultSet resultSet)
							throws SQLException {
						found[0] = true;
					}

					public void close() {
					}
				});
		if (!found[0]) {
			throw new RuntimeException("entity-graph " + graphID + " not found");
		}
		return entityGraph;
	}

	private final String COLUMN_PREFIX = "JALR_";
	
	/**
	 * Reads all entities of a given table which are marked as independent or as
	 * roots.
	 * 
	 * @param table
	 *            the table
	 * @param orderByPK
	 *            not used
	 */
	public void readMarkedEntities(Table table, boolean orderByPK)
			throws SQLException {
		String selectionSchema = filteredSelectionClause(table, COLUMN_PREFIX, quoting);
		readEntitiesByQuery(table, "Select " + selectionSchema + " From "
				+ SQLDialect.dmlTableReference(ENTITY, session) + " E join "
				+ quoting.quote(table.getName()) + " T on "
				+ pkEqualsEntityID(table, "T", "E")
				+ " Where (E.birthday=0 and E.r_entitygraph=" + graphID
				+ " and E.type=" + typeName(table) + ")");
	}
	
	/**
	 * Reads all entities of a given table.
	 * 
	 * @param table
	 *            the table
	 * @param orderByPK
	 *            not used
	 */
	public void readEntities(Table table, boolean orderByPK)
			throws SQLException {
		readEntitiesByQuery(table, "Select " + filteredSelectionClause(table, COLUMN_PREFIX, quoting) + " From "
				+ SQLDialect.dmlTableReference(ENTITY, session) + " E join "
				+ quoting.quote(table.getName()) + " T on "
				+ pkEqualsEntityID(table, "T", "E")
				+ " Where (E.birthday>=0 and E.r_entitygraph=" + graphID
				+ " and E.type=" + typeName(table) + ")");
	}

	/**
	 * Reads all entities of a given table.
	 * 
	 * @param table
	 *            the table
	 * @param sql retrieves the entities
	 */
	private void readEntitiesByQuery(Table table, String sql) throws SQLException {
		boolean tableHasIdentityColumn = false;
        if (Configuration.forDbms(session).isIdentityInserts()) {
        	for (Column c: table.getColumns()) {
        		if (c.isIdentityColumn) {
        			tableHasIdentityColumn = true;
        			break;
        		}
        	}
        }
		long rc;
		if (tableHasIdentityColumn) {
			synchronized (session.getConnection()) {
				session.executeUpdate("SET IDENTITY_INSERT " + qualifiedTableName(table) + " ON");
				if (table.getUpsert() || upsertOnly) {
					rc = upsertRows(table, sql, true);
				} else {
					rc = insertRows(table, sql);
				}
				session.executeUpdate("SET IDENTITY_INSERT " + qualifiedTableName(table) + " OFF");
			}
		} else {
			if (table.getUpsert() || upsertOnly) {
				rc = upsertRows(table, sql, true);
			} else {
				rc = insertRows(table, sql);
			}
		}
		ProgressListenerRegistry.getProgressListener().exported(table, rc);
	}

	/**
	 * Gets qualified table name.
	 * 
	 * @param t
	 *            the table
	 * @return qualified name of t
	 */
	private String qualifiedTableName(Table t) {
		String schema = t.getOriginalSchema("");
		String mappedSchema = CommandLineParser.getInstance()
				.getSchemaMapping().get(schema);
		if (mappedSchema != null) {
			schema = mappedSchema;
		}
		if (schema.length() == 0) {
			return quoting.quote(t.getUnqualifiedName());
		}
		return quoting.quote(schema) + "." + quoting.quote(t.getUnqualifiedName());
	}

	/**
	 * Checks if columns is part of primary key.
	 * 
	 * @param column
	 *            the column
	 * @return <code>true</code> if column is part of primary key
	 */
	private boolean isPrimaryKeyColumn(Table table, String column) {
		for (Column c : table.primaryKey.getColumns()) {
			if (c.name.equalsIgnoreCase(column)) {
				return true;
			}
		}
		return false;
	}

	private long insertRows(Table table, String sqlSelect) throws SQLException {
		StringBuilder sb = new StringBuilder();
		String labelCSL = insertClause(table, null, null);
		sb.append("Insert into " + qualifiedTableName(table) + "(" + labelCSL
				+ ") " + sqlSelect);
		
		if (!table.primaryKey.getColumns().isEmpty() && !hasPrimarykey(session, table)) {
			// dont insert if PK constraint is not enforced
			return upsertRows(table, sqlSelect, true);
		}
		
		boolean silent = session.getSilent();
		session.setSilent(true);
		try {
			return session.executeUpdate(sb.toString());
		} catch (SQLException e) {
			try {
				// try upsert
				return upsertRows(table, sqlSelect, true);
			} catch (SQLException e2) {
				// throw original exception
				throw e;
			}
		} finally {
			session.setSilent(silent);
		}
	}

	private long upsertRows(Table table, String sqlSelect, boolean retry) throws SQLException {
		if (table.primaryKey.getColumns().isEmpty()) {
			throw new RuntimeException("Unable to merge/upsert into table \""
					+ table.getName() + "\".\n" + "No primary key.");
		}

		UpsertStrategy us;
		boolean done = false;
		long rc = 0;
		synchronized (this) {
			us = upsertStrategy;
		}
		if (us == null) {
			for (UpsertStrategy strategy: upsertStrategies) {
				boolean silent = session.getSilent();
				session.setSilent(true);
				try {
					rc = strategy.upsert(table, sqlSelect);
					synchronized (this) {
						upsertStrategy = strategy;
						us = upsertStrategy;
					}
					done = true;
					break;
				} catch (SQLException e) {
					// try another strategy
				} finally {
					session.setSilent(silent);
				}
			}
			if (!done) {
				synchronized (this) {
					upsertStrategy = upsertStrategies.get(0);
					us = upsertStrategy;
				}
			}
		}
		
		if (!done) {
			if (retry) {
				boolean silent = session.getSilent();
				session.setSilent(true);
				try {
					rc = us.upsert(table, sqlSelect);
				} catch (SQLException e) {
					synchronized (this) {
						upsertStrategy = null;
					}
					return upsertRows(table, sqlSelect, false);
				} finally {
					session.setSilent(silent);
				}
			} else {
				rc = us.upsert(table, sqlSelect);
			}
		}
		
		return rc;
	}

	/**
	 * Gets insert clause for inserting rows of given type with respect of the
	 * column filters.
	 * 
	 * @param table
	 *            the table to read rows from
	 * @return insert clause
	 */
	private String insertClause(Table table, String tableAlias, String columnPrefix) {
		StringBuilder sb = new StringBuilder();
		boolean first = true;

		for (Column c : table.getColumns()) {
			if (Configuration.forDbms(session).exportBlocks.contains(c.type)) {
				continue;
			}
			if (!first) {
				sb.append(", ");
			}
			if (tableAlias != null) {
				sb.append(tableAlias + ".");
			}
			sb.append(prefixColumnName(columnPrefix, quoting, c));
			first = false;
		}

		return sb.toString();
	}

	private long insertWhereNotExists(Table table, String sqlSelect) throws SQLException {
		StatementBuilder upsertInsertStatementBuilder = new StatementBuilder(1);
		
		String insertHead = "Insert into " + qualifiedTableName(table)
				+ "(" + insertClause(table, null, null) + ") ";
		StringBuffer whereForTerminator = new StringBuffer("");

		// assemble 'where'
		boolean f = true;
		for (Column pk : table.primaryKey.getColumns()) {
			if (!f) {
				whereForTerminator.append(" and ");
			}
			f = false;
			whereForTerminator.append("T." + quoting.quote(pk.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, pk));
		}

		insertHead += "Select " + insertClause(table, "Q", COLUMN_PREFIX) + " From (";
		StringBuffer terminator = new StringBuffer(") as Q"
				+ " Where not exists (Select * from "
				+ qualifiedTableName(table) + " T " + "Where ");
		terminator.append(whereForTerminator + ")");

		upsertInsertStatementBuilder.append(insertHead, sqlSelect, "",
				terminator.toString());

		String sql = upsertInsertStatementBuilder.build();
		return session.executeUpdate(sql);
	}

	private interface UpsertStrategy {
		long upsert(Table table, String sqlSelect)
				throws SQLException;
	}

	private class MergeUS implements UpsertStrategy {
		private final boolean withSemicolon;

		public MergeUS(boolean withSemicolon) {
			this.withSemicolon = withSemicolon;
		}
		
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			String labelCSL = insertClause(table, null, null);
			StatementBuilder upsertInsertStatementBuilder = new StatementBuilder(1);

			String insertHead = "Insert into " + qualifiedTableName(table)
					+ "(" + labelCSL + ") ";
			StringBuffer whereForTerminator = new StringBuffer("");

			// assemble 'where'
			boolean f = true;
			for (Column pk : table.primaryKey.getColumns()) {
				if (!f) {
					whereForTerminator.append(" and ");
				}
				f = false;
				whereForTerminator.append("T." + quoting.quote(pk.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, pk));
			}

			insertHead = "MERGE INTO " + qualifiedTableName(table)
					+ " T USING(";
			StringBuffer terminator = new StringBuffer(") Q ON("
					+ whereForTerminator + ") ");

			StringBuffer sets = new StringBuffer();
			StringBuffer tSchema = new StringBuffer();
			StringBuffer iSchema = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (sets.length() > 0) {
						sets.append(", ");
					}
					sets.append("T." + quoting.quote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				}
				if (tSchema.length() > 0) {
					tSchema.append(", ");
				}
				tSchema.append(quoting.quote(column.name));
				if (iSchema.length() > 0) {
					iSchema.append(", ");
				}
				iSchema.append("Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
			}
			if (sets.length() > 0) {
				terminator.append("WHEN MATCHED THEN UPDATE SET " + sets + " ");
			}
			terminator.append("WHEN NOT MATCHED THEN INSERT (" + tSchema
					+ ") VALUES(" + iSchema + ")");

			upsertInsertStatementBuilder.append(insertHead, sqlSelect, "",
					terminator.toString());

			String sql = upsertInsertStatementBuilder.build();
			return session.executeUpdate(sql + (withSemicolon? ";" : ""));
		}
	};

	private class UpsertPGUS implements UpsertStrategy {
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			StringBuffer sets = new StringBuffer();
			StringBuffer where = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (sets.length() > 0) {
						sets.append(", ");
					}
					sets.append(quoting.quote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				} else {
					if (where.length() > 0) {
						where.append(" and ");
					}
					where.append("S." + quoting.quote(column.name) + "=T." + quoting.quote(column.name));
				}
			}
			
			if (sets.length() == 0) {
				// nothing to do
				return 0;
			}
			
			String sql = "Update " + qualifiedTableName(table) + " S set " + sets + " from (" + sqlSelect + " and (" + where + ")) Q ";
			
			long rc = session.executeUpdate(sql);
			return rc + insertWhereNotExists(table, sqlSelect);
		}
	};

	private class UpsertMYSQLUS implements UpsertStrategy {
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			StringBuffer sets = new StringBuffer();
			StringBuffer where = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (sets.length() > 0) {
						sets.append(", ");
					}
					sets.append("S." + quoting.quote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				} else {
					if (where.length() > 0) {
						where.append(" and ");
					}
					where.append("S." + quoting.quote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				}
			}

			if (sets.length() == 0) {
				// nothing to do
				return 0;
			}

			String sql = "Update (" + sqlSelect + ") Q join " + qualifiedTableName(table) + " S on " + where + " set " + sets;
			
			long rc = session.executeUpdate(sql);
			return rc + insertWhereNotExists(table, sqlSelect);
		}
	};

	private class UpsertStandardUS implements UpsertStrategy {
		@Override
		public long upsert(Table table, String sqlSelect)
				throws SQLException {
			
			StringBuffer nonPKList = new StringBuffer();
			StringBuffer nonPKListQ = new StringBuffer();
			StringBuffer where = new StringBuffer();
			StringBuffer whereT = new StringBuffer();
			for (Column column : table.getColumns()) {
				if (!isPrimaryKeyColumn(table, column.name)) {
					if (nonPKList.length() > 0) {
						nonPKList.append(", ");
					}
					nonPKList.append(quoting.quote(column.name));
					if (nonPKListQ.length() > 0) {
						nonPKListQ.append(", ");
					}
					nonPKListQ.append("Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
				} else {
					if (where.length() > 0) {
						where.append(" and ");
					}
					where.append("S." + quoting.quote(column.name) + "=Q." + prefixColumnName(COLUMN_PREFIX, quoting, column));
					if (whereT.length() > 0) {
						whereT.append(" and ");
					}
					whereT.append("S." + quoting.quote(column.name) + "=T." + quoting.quote(column.name));
				}
			}
			
			if (nonPKList.length() == 0) {
				// nothing to do
				return 0;
			}
			
			String sql = 
					"Update " + qualifiedTableName(table) + " S set (" + nonPKList + ")" +
					" = (Select " + nonPKListQ + " From (" + sqlSelect + ") Q Where " + where + ") " +
					"Where exists (" + sqlSelect + " and (" + whereT + "))";
			
			long rc = session.executeUpdate(sql);
			return rc + insertWhereNotExists(table, sqlSelect);
		}
	};

	private UpsertStrategy upsertStrategy;
	private List<UpsertStrategy> upsertStrategies;

}
