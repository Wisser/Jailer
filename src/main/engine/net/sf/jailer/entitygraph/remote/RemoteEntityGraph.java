/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
package net.sf.jailer.entitygraph.remote;

import java.io.OutputStreamWriter;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.configuration.LimitTransactionSizeInfo;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.database.UpdateTransformer;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.extractionmodel.SubjectLimitDefinition;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Persistent graph of entities in the remote database.
 *
 * @author Ralf Wisser
 */
public class RemoteEntityGraph extends EntityGraph {

	/**
	 * For executing SQL-Statements.
	 */
	public final Session session;

	/**
	 * The universal primary key.
	 */
	protected final PrimaryKey universalPrimaryKey;

	/**
	 * Birthday of subject rows.
	 */
	protected int birthdayOfSubject = 0;

	/**
	 * {@link RowIdSupport}.
	 */
	protected final RowIdSupport rowIdSupport;

	/**
	 * Updates statistics (optional).
	 */
	private final Runnable updateStatistics;

	/**
	 * Constructor.
	 *
	 * @param graphID the unique ID of the graph
	 * @param session for executing SQL-Statements
	 * @param universalPrimaryKey the universal primary key
	 */
	protected RemoteEntityGraph(DataModel dataModel, int graphID, Session session, PrimaryKey universalPrimaryKey, Runnable updateStatistics, ExecutionContext executionContext) throws SQLException {
		super(graphID, dataModel, executionContext);
		this.session = session;
		this.quoting = Quoting.getQuoting(session);
		this.universalPrimaryKey = universalPrimaryKey;
		this.updateStatistics = updateStatistics;
		this.rowIdSupport = new RowIdSupport(dataModel, session.dbms, executionContext);
	}

	private final Quoting quoting;

	/**
	 * Sets birthday of subject rows.
	 *
	 * @param birthdayOfSubject birthday of subject rows
	 */
	@Override
	public void setBirthdayOfSubject(int birthdayOfSubject) {
		this.birthdayOfSubject = birthdayOfSubject;
	}

	/**
	 * Creates a new entity-graph.
	 *
	 * @param graphID the unique ID of the graph
	 * @param session for executing SQL-Statements
	 * @param universalPrimaryKey the universal primary key
	 * @return the newly created entity-graph
	 */
	public static RemoteEntityGraph create(DataModel dataModel, int graphID, Session session, PrimaryKey universalPrimaryKey, Runnable updateStatistics, ExecutionContext executionContext) throws SQLException {
		RemoteEntityGraph entityGraph = new RemoteEntityGraph(dataModel, graphID, session, universalPrimaryKey, updateStatistics, executionContext);
		init(graphID, session, executionContext);
		return entityGraph;
	}

	/**
	 * Initializes a new entity-graph.
	 *
	 * @param graphID the unique ID of the graph
	 * @param session for executing SQL-Statements
	 */
	protected static void init(int graphID, Session session, ExecutionContext executionContext) {
		synchronized (EntityGraph.class) {
			try {
				session.executeUpdate("Insert into " + SQLDialect.dmlTableReference(ENTITY_GRAPH, session, executionContext) + "(id, age) values (" + graphID + ", 1)");
			} catch (SQLException e) {
				throw new RuntimeException("Can't find working tables! " +
						"Run 'bin/jailer.sh create-ddl' " +
						"and execute the DDL-script first!", e);
			}
		}
	}

	/**
	 * Copies an entity-graph.
	 *
	 * @param newGraphID the unique ID of the new graph
	 * @param session for executing SQL-Statements
	 * @return the newly created entity-graph
	 */
	@Override
	public EntityGraph copy(int newGraphID, Session session) throws SQLException {
		RemoteEntityGraph entityGraph = create(dataModel, newGraphID, session, universalPrimaryKey, null, executionContext);
		entityGraph.setBirthdayOfSubject(birthdayOfSubject);
		session.executeUpdate(
				"Insert into " + dmlTableReference(ENTITY, session) + "(r_entitygraph, " + universalPrimaryKey.columnList(null) + ", birthday, orig_birthday, type) " +
					"Select " + newGraphID + ", " + universalPrimaryKey.columnList(null) + ", birthday, birthday, type From " + dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + "");
		entityGraph.setTransformerFactory(getTransformerFactory());
		return entityGraph;
	}

	/**
	 * Creates a new entity-graph of same type and session.
	 */
	public EntityGraph createNewGraph() throws SQLException {
		RemoteEntityGraph entityGraph = create(dataModel, createUniqueGraphID(), session, universalPrimaryKey, null, executionContext);
		entityGraph.setBirthdayOfSubject(birthdayOfSubject);
		return entityGraph;
	}

	/**
	 * Gets the age of the graph.
	 *
	 * @return the age of the graph
	 */
	@Override
	public int getAge() throws SQLException {
		final int[] age = new int[1];
		age[0] = -1;
		session.executeQuery("Select age From " + dmlTableReference(ENTITY_GRAPH, session) + " Where id=" + graphID + "", new Session.ResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				age[0] = resultSet.getInt(1);
			}
			@Override
			public void close() {
			}
		});
		return age[0];
	}

	/**
	 * Sets the age of the graph.
	 *
	 * @param age the age of the graph
	 */
	@Override
	public void setAge(int age) throws SQLException {
		session.executeUpdate("Update " + dmlTableReference(ENTITY_GRAPH, session) + " Set age=" + age + " Where id=" + graphID + "");
	}

	/**
	 * Gets the number of entities in the graph.
	 *
	 * @return the number of entities in the graph
	 */
	@Override
	public long getSize() throws SQLException {
		final int[] size = new int[1];
		size[0] = -1;
		session.executeQuery("Select count(*) From " + dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + " and birthday >= 0", new Session.ResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				size[0] = resultSet.getInt(1);
			}
			@Override
			public void close() {
			}
		});
		return size[0];
	}


	/**
	 * Gets the number of entities from given tables in the graph.
	 *
	 * @return the number of entities in the graph
	 */
	@Override
	public long getSize(final Set<Table> tables) throws SQLException {
		final long[] total = new long[1];
		total[0] = 0;
		if (!tables.isEmpty()) {
			session.executeQuery("Select type, count(*) From " + dmlTableReference(ENTITY, session) + " Where r_entitygraph=" + graphID + " and birthday>=0 group by type", new Session.AbstractResultSetReader() {
				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					Table table = dataModel.getTableByOrdinal(resultSet.getInt(1));
					if (tables.contains(table)) {
						long count = resultSet.getLong(2);
						total[0] += count;
					}
				}
			});
		}
		return total[0];
	}

	/**
	 * Deletes the graph.
	 */
	@Override
	public void delete(boolean force) {
		if (!isTruncated) {
			try {
				deleteRows(session, dmlTableReference(DEPENDENCY, session), "r_entitygraph=" + graphID + "");
				deleteRows(session, dmlTableReference(ENTITY, session), "r_entitygraph=" + graphID + "");
				deleteRows(session, dmlTableReference(ENTITY_GRAPH, session), "id=" + graphID + "");
			} catch (SQLException e) {
				// ignore
			}
		}
	}

	/**
	 * Adds entities to the graph.
	 *
	 * @param table the table
	 * @param condition the condition in SQL that the entities must fulfill
	 * @param today the birthday of the new entities
	 *
	 * @return row-count
	 */
	@Override
	public long addEntities(Table table, String condition, int today) throws SQLException {
		return addEntities(table, "T", condition, null, null, null, null, false, today, true);
	}

	/**
	 * Adds limited number of entities to the graph.
	 *
	 * @param table the table
	 * @param condition the condition in SQL that the entities must fulfill
	 * @param today the birthday of the new entities
	 * @param limitDefinition limit
	 *
	 * @return row-count
	 */
	@Override
	public long addEntities(Table table, String condition, int today, SubjectLimitDefinition limitDefinition, boolean joinWithEntity) throws SQLException {
		String select;
		String alias = "T";

		select =
				"Select " + pkList(table, alias) +
				" From " + quoting.requote(table.getName()) + " " + alias;
		if (condition != null && !SqlUtil.SQL_TRUE.equals(condition)) {
			select += " Where (" + condition + ")";
		}
		if (limitDefinition.limit != null && limitDefinition.orderBy != null && limitDefinition.orderBy.length() > 0) {
			select += " order by " + limitDefinition.orderBy;
		}

		final Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		final int MAX_BATCH_SIZE = 200;

		String insSQL =
				"Insert into " + dmlTableReference(ENTITY, session) + " (r_entitygraph, birthday, type, " + upkColumnList(table, null) + ") " +
				"values (" + graphID + ", " + today + ", " + typeName(table);
		for (Column column: universalPrimaryKey.getColumns()) {
			Column tableColumn = match.get(column);
			if (tableColumn != null) {
				insSQL += " ,?";
			}
		}
		insSQL += ")";

		String delSQL = null;
		if (joinWithEntity) {
			StringBuffer sb = new StringBuffer();
			for (Column column: universalPrimaryKey.getColumns()) {
				if (sb.length() > 0) {
					sb.append(" and ");
				}
				Column tableColumn = match.get(column);
				if (tableColumn != null) {
					if (tableColumn.isNullable) {
						sb.append("(");
					}
					sb.append(column.name + " = ?");
					if (tableColumn.isNullable) {
						sb.append(" or (");
						sb.append(column.name + " is null and ? is null)");
					}
					if (tableColumn.isNullable) {
						sb.append(")");
					}
				} else {
					sb.append(column.name);
					sb.append(" is null");
				}
			}
			delSQL =
				"Delete from " + dmlTableReference(ENTITY, session) +
				" Where r_entitygraph=" + graphID + " and type=" + typeName(table) +
				" and (" + sb + ")";
		}

		final String insert = insSQL;
		final String delete = delSQL;
		final String finalSelect = select;

		final PreparedStatement ins = session.getConnection().prepareStatement(insert);
		final PreparedStatement del = delete != null? session.getConnection().prepareStatement(delete) : null;
		final long[] rc = new long[] { 0 };
		final boolean[] rcValid = new boolean[] { true };

		session.executeQuery(select + " /*!*/", new Session.AbstractResultSetReader() {
			int batchSize = 0;
			Map<Integer, Integer> columnType = null;
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
                try {
                	if (columnType == null) {
                		columnType = new HashMap<Integer, Integer>();
                		ResultSetMetaData md = getMetaData(resultSet);
                		int ci = 1;
    					for (Column column: universalPrimaryKey.getColumns()) {
    						Column tableColumn = match.get(column);
    						if (tableColumn != null) {
    							columnType.put(ci, md.getColumnType(ci));
    							++ci;
    						}
    					}
    					Session._log.info("batch insert: " + insert);
    					if (delete != null) {
        					Session._log.info("batch delete: " + delete);
    					}
                	}

					int insI = 1;
					int delI = 1;
					int ci = 1;
					for (Column column: universalPrimaryKey.getColumns()) {
						Column tableColumn = match.get(column);
						if (tableColumn != null) {
							Object v = resultSet.getObject(ci++);

							setObject(ins, insI++, v);
							if (del != null) {
								setObject(del, delI++, v);
								if (tableColumn.isNullable) {
									setObject(del, delI++, v);
								}
							}
						}
					}

					ins.addBatch();
					if (del != null) {
						del.addBatch();
					}
	                ++batchSize;
	                if (batchSize >= MAX_BATCH_SIZE) {
                    	executeBatch();
                        batchSize = 0;
                        if (updateStatistics != null) {
                			updateStatistics.run();
                		}
                    }
                }
                catch (SQLException e) {
                	throw new SqlException("\"" + e.getMessage() + "\" (" + columnType + ") in statement \"" + insert + "\"", finalSelect, e);
                }
			}
			private void setObject(final PreparedStatement statement, int i, Object value) throws SQLException {
				if (value == null) {
					statement.setNull(i, columnType.get(i));
				} else {
					statement.setObject(i, value);
				}
			}
			private void executeBatch() throws SQLException {
				if (del != null) {
					int[] batchRc = del.executeBatch();
					for (int brc: batchRc) {
						if (brc < 0) {
							rcValid[0] = false;
						}
		                rc[0] -= brc;
		    			totalRowcount -= brc;
					}
				}
				int brc = ins.executeBatch().length;
				rc[0] += brc;
				totalRowcount += brc;
			}
			@Override
			public void close() {
                try {
                	if (batchSize > 0) {
						executeBatch();
					}
                    batchSize = 0;
                    ins.close();
                    if (del != null) {
                    	del.close();
                    }
				} catch (SQLException e) {
					throw new RuntimeException(new SqlException("\"" + e.getMessage() + "\" in statement \"" + insert + "\"", finalSelect, e));
	            }
			}
		}, null, null, limitDefinition.limit != null? limitDefinition.limit : 0, false);

		if (updateStatistics != null) {
			updateStatistics.run();
		}

		return rcValid[0]? rc[0] : -1;
	}

	/**
	 * Resolves an association. Retrieves and adds all entities
	 * associated with an entity born yesterday in the graph
	 * and adds the dependencies.
	 *
	 * @param table the table
	 * @param association the association to resolve
	 * @param today the birthday of the new entities
	 *
	 * @return row-count or -1, if association is ignored
	 */
	@Override
	public long resolveAssociation(Table table, Association association, int today) throws SQLException {
		String jc = association.getJoinCondition();
		if (jc != null) {
			String destAlias, sourceAlias;
			if (association.reversed) {
				destAlias = "A";
				sourceAlias = "B";
			} else {
				destAlias = "B";
				sourceAlias = "A";
			}
			return addEntities(association.destination, destAlias, "E.r_entitygraph=" + graphID + " and E.birthday = " + (today - 1) + " and E.type=" + typeName(table) + " and " + pkEqualsEntityID(table, sourceAlias, "E"), table, sourceAlias, association.source, jc, true, today, association.reversed);
		}
		return -1;
	}

	/**
	 * Resolves an association. Retrieves and adds all entities
	 * associated with an entity into a given entity-graph.
	 * Restrictions are ignored.
	 *
	 * @param table the table
	 * @param association the association to resolve
	 *
	 * @return row-count
	 */
	@Override
	public long resolveAssociation(final Table table, Association association, EntityGraph otherGraph, EntityGraph universum, boolean forDelete) throws SQLException {
		String jc = association.getUnrestrictedJoinCondition();
		if (jc != null) {
			String destAlias, sourceAlias;
			if (association.reversed) {
				destAlias = "A";
				sourceAlias = "B";
			} else {
				destAlias = "B";
				sourceAlias = "A";
			}
			Table table1 = association.destination;
			String condition = "E.r_entitygraph=" + graphID + " and E.birthday >= 0 and E.type=" + typeName(association.destination) + " and " + pkEqualsEntityID(association.destination, destAlias, "E");
			Table source = association.source;
			String select;
			LimitTransactionSizeInfo limitTransactionSize = session.dbms.getLimitTransactionSize();
			select =
					"Select " + (table != null? "distinct " : "") + limitTransactionSize.afterSelectFragment(executionContext) + otherGraph.graphID + " as graph_id, " + pkList(source, sourceAlias) + ", " + 1 + " as birthday, " + typeName(source) + " as type" +
							" From " + quoting.requote(table1.getName()) + " " + destAlias +
							" left join " + dmlTableReference(ENTITY, session) + " Duplicate on Duplicate.r_entitygraph=" + otherGraph.graphID + " and Duplicate.type=" + typeName(table1) + " and " +
							pkEqualsEntityID(table1, destAlias, "Duplicate") +
							(table != null? ", " + quoting.requote(table.getName()) + " " + sourceAlias + " ": "") +
							" left join " + dmlTableReference(ENTITY, session) + " DUnivers on DUnivers.r_entitygraph=" + universum.graphID + " and DUnivers.type=" + typeName(source) + " and " +
							pkEqualsEntityID(source, sourceAlias, "DUnivers") +
							", " + dmlTableReference(ENTITY, session) + " E" +
							" Where (" + condition + ") and Duplicate.type is null and DUnivers.type " +
							(forDelete? "is null" : "is not null") +
							(table != null? " and (" + jc + ") " : " ") + limitTransactionSize.additionalWhereConditionFragment(executionContext) +
							limitTransactionSize.statementSuffixFragment(executionContext);

			long incrementSize = limitTransactionSize.getSize(executionContext);
			String insert = "Insert into " + dmlTableReference(ENTITY, session) + " (r_entitygraph, " + upkColumnList(source, null) + ", birthday, type) " + select;
			if (DBMS.SYBASE.equals(session.dbms)) session.execute("set forceplan on ");
			long rc = 0;
			for (;;) {
				long incRc = session.executeUpdate(insert);
				rc += incRc;
				totalRowcount += incRc;
				if (updateStatistics != null) {
					updateStatistics.run();
				}
				if (incRc != incrementSize || incrementSize == 0) {
					break;
				}
			}
			if (DBMS.SYBASE.equals(session.dbms)) session.execute("set forceplan off ");
			return rc;
		}
		return -1;
	}

	/**
	 * Adds entities to the graph.
	 *
	 * @param table the table
	 * @param condition the condition in SQL that the entities must fulfill with 'E' as alias for the entity-table
	 * @param joinedTable optional table to join with
	 * @param source optional, the source-table
	 * @param joinCondition optional condition to join with <code>joinedTable</code>
	 * @param joinWithEntity whether to join with entity-table too
	 * @param today the birthday of the new entities
	 *
	 * @return row-count
	 */
	private long addEntities(Table table, String alias, String condition, Table joinedTable, String joinedTableAlias, Table source, String joinCondition, boolean joinWithEntity, int today, boolean isInverseAssociation) throws SQLException {
		if (joinCondition != null) {
			joinCondition = SqlUtil.resolvePseudoColumns(joinCondition, isInverseAssociation? null : "E", isInverseAssociation? "E" : null, today, birthdayOfSubject, inDeleteMode);
		}
		String select;
		LimitTransactionSizeInfo limitTransactionSize = session.dbms.getLimitTransactionSize();
		if (joinedTable == null && !joinWithEntity && !limitTransactionSize.isApplicable(executionContext)) {
			select =
					"Select " + graphID + " " + limitTransactionSize.afterSelectFragment(executionContext) + "as graph_id, " + pkList(table, alias) + ", " + today + " as birthday, " + typeName(table) + " as type" +
					" From " + quoting.requote(table.getName()) + " " + alias +
					(condition != null && !SqlUtil.SQL_TRUE.equals(condition) ? " Where (" + condition + ") " : " ") + 
					limitTransactionSize.additionalWhereConditionFragment(executionContext) +
					limitTransactionSize.statementSuffixFragment(executionContext);
		} else {
			if (session.dbms.isAvoidLeftJoin()) {
				// bug fix for [https://sourceforge.net/p/jailer/bugs/12/ ] "Outer Join for selecting dependant entries and Oracle 10"
				// mixing left joins and theta-style joins causes problems on oracle DBMS

				// TODO is this still necessary?
				select =
					"Select " + (joinedTable != null? "distinct " : "") + limitTransactionSize.afterSelectFragment(executionContext) + graphID + " as graph_id, " + pkList(table, alias) + ", " + today + " as birthday, " + typeName(table) + " as type" +
					" From " + quoting.requote(table.getName()) + " " + alias
						+
					(joinedTable != null? ", " + quoting.requote(joinedTable.getName()) + " " + joinedTableAlias + " ": "") +
					(joinWithEntity? ", " + dmlTableReference(ENTITY, session) + " E" : "") +
					" Where (" + condition + ") " +
						// CW	"and Duplicate.type is null" +
					(joinedTable != null? " and (" + joinCondition + ")" : "") +

						" AND NOT EXISTS (select * from " + dmlTableReference(ENTITY, session)
						+ " DuplicateExists where r_entitygraph=" + graphID + " " + "AND DuplicateExists.type="
						+ typeName(table)
						+ " and " + pkEqualsEntityID(table, alias, "DuplicateExists") + ") " + limitTransactionSize.additionalWhereConditionFragment(executionContext) +
						limitTransactionSize.statementSuffixFragment(executionContext);

			} else {
				select =
					"Select " + (joinedTable != null? "distinct " : "") + limitTransactionSize.afterSelectFragment(executionContext) + graphID + " as graph_id, " + pkList(table, alias) + ", " + today + " as birthday, " + typeName(table) + " as type" +
					" From " + quoting.requote(table.getName()) + " " + alias +
					" left join " + dmlTableReference(ENTITY, session) + " Duplicate on Duplicate.r_entitygraph=" + graphID + " and Duplicate.type=" + typeName(table) + " and " +
					pkEqualsEntityID(table, alias, "Duplicate") +
					(joinedTable != null? ", " + quoting.requote(joinedTable.getName()) + " " + joinedTableAlias + " ": "") +
					(joinWithEntity? ", " + dmlTableReference(ENTITY, session) + " E" : "") +
					" Where (" + condition + ") and Duplicate.type is null" +
					(joinedTable != null? " and (" + joinCondition + ") " : " ") + limitTransactionSize.additionalWhereConditionFragment(executionContext) +
					limitTransactionSize.statementSuffixFragment(executionContext);
			}
		}

		long incrementSize = limitTransactionSize.getSize(executionContext);
		String insert = "Insert into " + dmlTableReference(ENTITY, session) + " (r_entitygraph, " + upkColumnList(table, null) + ", birthday, type) " + select;
		if (DBMS.SYBASE.equals(session.dbms)) session.execute("set forceplan on ");
		long rc = 0;
		for (;;) {
			long incRc = session.executeUpdate(insert);
			rc += incRc;
			totalRowcount += incRc;
			if (updateStatistics != null) {
				updateStatistics.run();
			}
			if (incRc != incrementSize || incrementSize == 0) {
				break;
			}
		}
		if (DBMS.SYBASE.equals(session.dbms)) session.execute("set forceplan off ");
		return rc;
	}

	/**
	 * Adds dependencies.
	 *
	 * @param from source of dependency
	 * @param fromAlias alias for from-table
	 * @param to destination of dependency
	 * @param toAlias alias for to-table
	 * @param condition condition of dependency
	 * @param aggregationId id of aggregation association (for XML export), 0 if not applicable
	 * @param dependencyId id of dependency
	 */
	@Override
	public void addDependencies(Table from, String fromAlias, Table to, String toAlias, String condition, int aggregationId, int dependencyId, boolean isAssociationReversed) throws SQLException {
		condition = SqlUtil.resolvePseudoColumns(condition, isAssociationReversed? "E1" : "E2", isAssociationReversed? "E2" : "E1", 0, birthdayOfSubject, inDeleteMode);
		String insert = "Insert into " + dmlTableReference(DEPENDENCY, session) + "(r_entitygraph, assoc, depend_id, from_type, to_type, " + upkColumnList(from, "FROM_") + ", " + upkColumnList(to, "TO_") + ") " +
			"Select " + graphID + ", " + aggregationId  + ", " + dependencyId + ", " + typeName(from) + ", " + typeName(to) + ", " + pkList(from, fromAlias, "FROM") + ", " + pkList(to, toAlias, "TO") +
			" From " + dmlTableReference(ENTITY, session) + " E1, " + dmlTableReference(ENTITY, session) + " E2, " + quoting.requote(from.getName()) + " " + fromAlias + " ," + quoting.requote(to.getName()) + " " + toAlias + " " +
			" Where E1.r_entitygraph=" + graphID + " and E2.r_entitygraph=" + graphID + "" +
			" and (" + condition + ")" +
			" and E1.type=" + typeName(from) + " and E2.type=" + typeName(to) + "" +
			" and " + pkEqualsEntityID(from, fromAlias, "E1") +
			" and " + pkEqualsEntityID(to, toAlias, "E2");
		totalRowcount += session.executeUpdate(insert);
	}

	/**
	 * Gets distinct association-ids of all edged.
	 */
	@Override
	public Set<Integer> getDistinctDependencyIDs() throws SQLException {
		String select = "Select distinct depend_id from " + dmlTableReference(DEPENDENCY, session) + " Where r_entitygraph=" + graphID;
		final Set<Integer> ids = new HashSet<Integer>();
		session.executeQuery(select, new Session.ResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				ids.add(resultSet.getInt(1));
			}
			@Override
			public void close() {
			}
		});
		return ids;
	}

	/**
	 * Marks all entities of a given table which don't dependent on other entities,
	 * s.t. they can be read and deleted.
	 */
	@Override
	public void markIndependentEntities(Table table) throws SQLException {
		StringBuffer fromEqualsPK = new StringBuffer();
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		for (Column column: universalPrimaryKey.getColumns()) {
			if (fromEqualsPK.length() > 0) {
				fromEqualsPK.append(" and ");
			}
			if (match.get(column) != null) {
				if (match.get(column).isNullable) {
					fromEqualsPK.append("(D.FROM_" + column.name + "=" + dmlTableReference(ENTITY, session) + "." + column.name + " or (");
					fromEqualsPK.append("D.FROM_" + column.name + " is null and " + dmlTableReference(ENTITY, session) + "." + column.name + " is null))");
				} else {
					fromEqualsPK.append("D.FROM_" + column.name + "=" + dmlTableReference(ENTITY, session) + "." + column.name);
				}
			} else {
				fromEqualsPK.append("D.FROM_" + column.name + " is null and " + dmlTableReference(ENTITY, session) + "." + column.name + " is null");
			}
		}
		session.executeUpdate(
				"Update " + dmlTableReference(ENTITY, session) + " set birthday=0 " +
				"Where r_entitygraph=" + graphID + " and birthday>0 and " +
					   "type=" + typeName(table) + " and " +
					   "not exists (Select * from " + dmlTableReference(DEPENDENCY, session) + " D " +
						   "Where D.r_entitygraph=" + graphID + " and D.assoc=0 and D.from_type=" + dmlTableReference(ENTITY, session) + ".type and " +
								 fromEqualsPK + ")");
	}

	/**
	 * Marks all rows which are not target of a dependency.
	 */
	@Override
	public void markRoots(Table table) throws SQLException {
		StringBuffer toEqualsPK = new StringBuffer();
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		for (Column column: universalPrimaryKey.getColumns()) {
			if (toEqualsPK.length() > 0) {
				toEqualsPK.append(" and ");
			}
			if (match.get(column) != null) {
				if (match.get(column).isNullable) {
					toEqualsPK.append("(D.TO_" + column.name + "=" + dmlTableReference(ENTITY, session) + "." + column.name + " or (");
					toEqualsPK.append("D.TO_" + column.name + " is null and " + dmlTableReference(ENTITY, session) + "." + column.name + " is null))");
				} else {
					toEqualsPK.append("D.TO_" + column.name + "=" + dmlTableReference(ENTITY, session) + "." + column.name);
				}
			} else {
				toEqualsPK.append("D.TO_" + column.name + " is null and " + dmlTableReference(ENTITY, session) + "." + column.name + " is null");
			}
		}
		session.executeUpdate(
				"Update " + dmlTableReference(ENTITY, session) + " set birthday=0 " +
				"Where r_entitygraph=" + graphID + " and birthday>0 and type=" + typeName(table) + " and " +
					   "not exists (Select * from " + dmlTableReference(DEPENDENCY, session) + " D " +
						   "Where D.r_entitygraph=" +graphID + " and D.to_type=" + dmlTableReference(ENTITY, session) + ".type and " +
								 toEqualsPK + ")");
	}

	/**
	 * Reads all entities of a given table which are marked as independent or as roots.
	 *
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	@Override
	public void readMarkedEntities(Table table, boolean orderByPK) throws SQLException {
		Session.ResultSetReader reader = getTransformerFactory().create(table);
		readMarkedEntities(table, reader, filteredSelectionClause(table, false), orderByPK);
	}

	/**
	 * Reads all entities of a given table which are marked as independent or as roots.
	 *
	 * @param reader for reading the result-set
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	private void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, boolean orderByPK) throws SQLException {
		String orderBy = "";
		if (orderByPK) {
			orderBy = " order by " + rowIdSupport.getPrimaryKey(table).columnList("T.", quoting);
		}
		long rc = session.executeQuery(
				"Select " + selectionSchema + " From " + dmlTableReference(ENTITY, session) + " E join " + quoting.requote(table.getName()) + " T on " +
				pkEqualsEntityID(table, "T", "E") +
				" Where E.birthday=0 and E.r_entitygraph=" + graphID + " and E.type=" + typeName(table) + "" +
				orderBy,
				reader, withExplicitCommit());
		executionContext.getProgressListenerRegistry().fireExported(table, rc);
		addExportedCount(rc);
	}

	/**
	 * Reads all entities of a given table which are marked as independent or as roots.
	 *
	 * @param reader for reading the result-set
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	@Override
	public void readMarkedEntities(Table table, Session.ResultSetReader reader, String selectionSchema, String originalPKAliasPrefix, boolean orderByPK) throws SQLException {
		if (originalPKAliasPrefix == null) {
			readMarkedEntities(table, reader, selectionSchema, orderByPK);
			return;
		}
		String orderBy = "";
		StringBuffer sb = new StringBuffer();
		StringBuffer selectOPK = new StringBuffer();
		List<Column> pkColumns = rowIdSupport.getPrimaryKey(table).getColumns();
		for (int i = 0; i < pkColumns.size(); ++i) {
			if (i > 0) {
				sb.append(", ");
				selectOPK.append(", ");
			}
			sb.append(originalPKAliasPrefix + i);
			selectOPK.append("T." + quoting.requote(pkColumns.get(i).name) + " as " + originalPKAliasPrefix + i);
		}
		orderBy = "order by " + sb;
		String sqlQuery = "Select " + selectionSchema + " From (" +
						"Select " + selectOPK + ", " + filteredSelectionClause(table, false) + " From " + dmlTableReference(ENTITY, session) + " E join " + quoting.requote(table.getName()) + " T on " +
						pkEqualsEntityID(table, "T", "E") +
						" Where E.birthday=0 and E.r_entitygraph=" + graphID + " and E.type=" + typeName(table) + "" +
						") T ";
		long rc = session.executeQuery(
				sqlQuery + (orderByPK? orderBy : ""),
				reader,
				(!orderByPK? sqlQuery : null), null, 0, withExplicitCommit());
		executionContext.getProgressListenerRegistry().fireExported(table, rc);
		addExportedCount(rc);
	}

	/**
	 * Reads all entities of a given table.
	 *
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	@Override
	public void readEntities(Table table, boolean orderByPK) throws SQLException {
		Session.ResultSetReader reader = getTransformerFactory().create(table);
		long rc = readEntities(table, orderByPK, reader);
		executionContext.getProgressListenerRegistry().fireExported(table, rc);
		addExportedCount(rc);
	}

	/**
	 * Reads some columns of all entities of a given table without using filters.
	 *
	 * @param table the table
	 * @param columns the columns
	 * @param reader to read
	 */
	@Override
	public long readUnfilteredEntityColumns(final Table table, final List<Column> columns, final Session.ResultSetReader reader) throws SQLException {
		StringBuilder sb = new StringBuilder();
		boolean first = true;

		for (Column c: columns) {
			if (!first) {
				sb.append(", ");
			}
			sb.append("T." + quoting.requote(c.name));
			sb.append(" as " + quoting.requote(c.name));
			first = false;
		}
		final String columnList = sb.toString();

		String sqlQuery = "Select " + columnList + " From " + dmlTableReference(ENTITY, session) + " E join " + quoting.requote(table.getName()) + " T on " +
				pkEqualsEntityID(table, "T", "E") +
				" Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type=" + typeName(table) + "";
		return session.executeQuery(sqlQuery, reader, withExplicitCommit());
	}

	/**
	 * Reads all entities of a given table.
	 *
	 * @param table the table
	 * @param orderByPK if <code>true</code>, result will be ordered by primary keys
	 */
	protected long readEntities(Table table, boolean orderByPK, Session.ResultSetReader reader) throws SQLException {
		String sqlQuery = "Select " + filteredSelectionClause(table, false) + " From " + dmlTableReference(ENTITY, session) + " E join " + quoting.requote(table.getName()) + " T on " +
			pkEqualsEntityID(table, "T", "E") +
			" Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type=" + typeName(table) + "";
		long rc;
		if (orderByPK) {
			String sqlQueryWithOrderBy = sqlQuery +
				" order by " + rowIdSupport.getPrimaryKey(table).columnList("T.", quoting);
			rc = session.executeQuery(sqlQueryWithOrderBy, reader, sqlQuery, null, 0, withExplicitCommit());
		} else {
			rc = session.executeQuery(sqlQuery, reader, withExplicitCommit());
		}
		return rc;
	}

	private boolean withExplicitCommit() {
		return DBMS.POSTGRESQL.equals(session.dbms);
	}

	/**
	 * Updates columns of a table.
	 *
	 * @param table the table
	 * @param columns the columns;
	 * @param inSourceSchema if <code>true</code>, use source-schema-mapping, else use schema-mapping
	 * @param reason to be written as comment
	 */
	@Override
	public void updateEntities(Table table, Set<Column> columns, OutputStreamWriter scriptFileWriter, DBMS targetConfiguration, boolean inSourceSchema, String reason) throws SQLException {
		Session.ResultSetReader reader = new UpdateTransformer(table, columns, scriptFileWriter, executionContext.getNumberOfEntities(), getTargetSession(), targetConfiguration, importFilterManager, inSourceSchema, reason, executionContext);
		readEntities(table, false, reader);
	}

	/**
	 * Gets select clause for reading rows of given type
	 * with respect of the column filters.
	 *
	 * @param table the table to read rows from
	 * @return select clause
	 */
	protected String filteredSelectionClause(Table table, boolean appylImportFilter) {
		return filteredSelectionClause(table, null, quoting, appylImportFilter);
	}

	/**
	 * Gets select clause for reading rows of given type
	 * with respect of the column filters.
	 *
	 * @param table the table to read rows from
	 * @param columnPrefix optional prefix for aliases
	 * @param quoting for unquoting of column names if columnPrefix is given
	 * @return select clause
	 */
	protected String filteredSelectionClause(Table table, String columnPrefix, Quoting quoting, boolean appylImportFilter) {
		StringBuilder sb = new StringBuilder();
		boolean first = true;

		for (Column c: getSelectionClause(table)) {
			if (!first) {
				sb.append(", ");
			}
			String filterExpression = null;
			if (c.getFilter() != null) {
				if (c.getFilter().isApplyAtExport()) {
					filterExpression = c.getFilterExpression();
				} else if (appylImportFilter && importFilterManager != null) {
					filterExpression = importFilterManager.transform(c, "T." + quoting.requote(c.name));
				}
			}
			if (filterExpression != null) {
				if (filterExpression.trim().toLowerCase(Locale.ENGLISH).startsWith("select")) {
					sb.append("(" + filterExpression + ")");
				} else {
					sb.append(filterExpression);
				}
			} else {
				sb.append("T." + quoting.requote(c.name));
			}
			sb.append(" as " + prefixColumnName(columnPrefix, quoting, c));
			first = false;
		}

		return sb.toString();
	}

	/**
	 * Adds a prefix to a column name. Respects quoting.
	 */
	protected String prefixColumnName(String prefix, Quoting quoting, Column column) {
		if (prefix == null) return quoting.requote(column.name);
		String name = quoting.unquote(column.name);
		return quoting.quote(prefix + name);
	}

	/**
	 * Deletes all entities which are marked as independent.
	 */
	@Override
	public void deleteIndependentEntities(Table table) throws SQLException {
		StringBuffer fromEqualsPK = new StringBuffer();
		StringBuffer toEqualsPK = new StringBuffer();
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		for (Column column: universalPrimaryKey.getColumns()) {
			if (fromEqualsPK.length() > 0) {
				fromEqualsPK.append(" and ");
			}
			if (match.get(column) != null) {
				if (match.get(column).isNullable) {
					fromEqualsPK.append("(" + dmlTableReference(DEPENDENCY, session) + ".FROM_" + column.name + "=" + column.name + " or (");
					fromEqualsPK.append(dmlTableReference(DEPENDENCY, session) + ".FROM_" + column.name + " is null and " + column.name + " is null))");
				} else {
					fromEqualsPK.append(dmlTableReference(DEPENDENCY, session) + ".FROM_" + column.name + "=" + column.name);
				}
			} else {
				fromEqualsPK.append(dmlTableReference(DEPENDENCY, session) + ".FROM_" + column.name + " is null and " + column.name + " is null");
			}
			if (toEqualsPK.length() > 0) {
				toEqualsPK.append(" and ");
			}
			if (match.get(column) != null) {
				if (match.get(column).isNullable) {
					toEqualsPK.append("(" + dmlTableReference(DEPENDENCY, session) + ".TO_" + column.name + "=" + column.name + " or (");
					toEqualsPK.append(dmlTableReference(DEPENDENCY, session) + ".TO_" + column.name + " is null and " + column.name + " is null))");
				} else {
					toEqualsPK.append(dmlTableReference(DEPENDENCY, session) + ".TO_" + column.name + "=" + column.name);
				}
			} else {
				toEqualsPK.append(dmlTableReference(DEPENDENCY, session) + ".TO_" + column.name + " is null and " + column.name + " is null");
			}
		}
		deleteRows(session,
				dmlTableReference(DEPENDENCY, session),
				dmlTableReference(DEPENDENCY, session) + ".r_entitygraph=" + graphID + " and assoc=0 and from_type=" + typeName(table) + " and " +
					  "exists (Select * from " + dmlTableReference(ENTITY, session) + " E Where " +
						  "E.r_entitygraph=" + graphID + " and " +
						  fromEqualsPK + " and " + dmlTableReference(DEPENDENCY, session) + ".from_type=E.type and " +
						  "E.birthday=0)");
		deleteRows(session,
				dmlTableReference(DEPENDENCY, session),
				dmlTableReference(DEPENDENCY, session) + ".r_entitygraph=" + graphID + " and assoc=0 and to_type=" + typeName(table) + " and " +
					  "exists (Select * from " + dmlTableReference(ENTITY, session) + " E Where " +
						  "E.r_entitygraph=" + graphID + " and " +
						  toEqualsPK + " and " + dmlTableReference(DEPENDENCY, session) + ".to_type=E.type and " +
						  "E.birthday=0)");
		deleteRows(session,
				dmlTableReference(ENTITY, session),
				"r_entitygraph=" + graphID + " and type=" + typeName(table) + " and " +
					   "birthday=0");
	}

	/**
	 * Deletes all entities from a given table.
	 */
	@Override
	public long deleteEntities(Table table) throws SQLException {
		return deleteRows(session,
				dmlTableReference(ENTITY, session),
				"r_entitygraph=" + graphID + " and " +
					   "type=" + typeName(table));
	}

	/**
	 * Counts the entities of a given table in this graph.
	 *
	 * @param table the table
	 * @return the number of entities from table in this graph
	 */
	@Override
	public long countEntities(Table table) throws SQLException {
		final long[] count = new long[1];
		session.executeQuery(
				"Select count(*) from " + dmlTableReference(ENTITY, session) + " E " +
				"Where E.birthday>=0 and E.r_entitygraph=" + graphID + " and E.type=" + typeName(table) + "",
				new Session.AbstractResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						count[0] = resultSet.getLong(1);
					}
				});
		return count[0];
	}

	/**
	 * Removes all entities from this graph which are associated with an entity
	 * outside the graph.
	 *
	 * @param deletedEntitiesAreMarked if true, consider entity as deleted if its birthday is negative
	 * @param allTables set of tables from which there are entities in E
	 * @param association the association
	 * @return number of removed entities
	 */
	@Override
	public long removeAssociatedDestinations(Association association, boolean deletedEntitiesAreMarked, Set<Table> allTables) throws SQLException {
		String jc = association.getJoinCondition();
		if (jc != null) {
			String destAlias, sourceAlias;
			if (association.reversed) {
				destAlias = "A";
				sourceAlias = "B";
			} else {
				destAlias = "B";
				sourceAlias = "A";
			}
			int setId = getNextSetId();
			jc = SqlUtil.resolvePseudoColumns(jc, association.reversed? "EB" : "EA", association.reversed? "EA" : "EB", 0, birthdayOfSubject, "orig_birthday", inDeleteMode);
			boolean checkDest = allTables.contains(association.source);
			if (!checkDest) {
				jc = jc.replace("EA.orig_birthday", "(null)");
			}

			String remove = "Insert into " + dmlTableReference(ENTITY_SET_ELEMENT, session) + "(set_id, type, " + universalPrimaryKey.columnList(null) + ") " +
				"Select distinct " + setId + ", EB.type, " + universalPrimaryKey.columnList("EB.") + " from " + dmlTableReference(ENTITY, session) + " EB " +
				"join " + quoting.requote(association.destination.getName()) + " " + destAlias + " on "+ pkEqualsEntityID(association.destination, destAlias, "EB") + " " +
				"join " + quoting.requote(association.source.getName()) + " " + sourceAlias + " " +
				"on (" + jc + ") " +
				(checkDest?
						(deletedEntitiesAreMarked? "join " : "left join ") + dmlTableReference(ENTITY, session) + " EA on EA.r_entitygraph=" + graphID + " and EA.type=" + typeName(association.source) + " and " + pkEqualsEntityID(association.source, sourceAlias, "EA") + " "
						:
						"") +
				"Where EB.r_entitygraph=" + graphID + " and EB.type=" + typeName(association.destination);
				if (checkDest) {
					remove += " and " + (deletedEntitiesAreMarked? "EA.birthday=-1 and EB.birthday>=0" : "EA.type is null");
				}
			long rc = session.executeUpdate(remove);
			if (rc > 0) {
				Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(association.destination));
				StringBuffer sEqualsE = new StringBuffer();
				StringBuffer sEqualsEWoAlias = new StringBuffer();
				for (Column column: universalPrimaryKey.getColumns()) {
					if (sEqualsE.length() > 0) {
						sEqualsE.append(" and ");
					}
					if (sEqualsEWoAlias.length() > 0) {
						sEqualsEWoAlias.append(" and ");
					}
					if (match.get(column) != null) {
						if (match.get(column).isNullable) {
							sEqualsE.append("(S." + column.name + "=E." + column.name + " or (");
							sEqualsE.append("S." + column.name + " is null and E." + column.name + " is null))");

							sEqualsEWoAlias.append("(S." + column.name + "=" + dmlTableReference(ENTITY, session) + "." + column.name + " or (");
							sEqualsEWoAlias.append("S." + column.name + " is null and " + dmlTableReference(ENTITY, session) + "." + column.name + " is null))");
						} else {
							sEqualsE.append("S." + column.name + "=E." + column.name);
							sEqualsEWoAlias.append("S." + column.name + "=" + dmlTableReference(ENTITY, session) + "." + column.name);
						}
					} else {
						sEqualsE.append("S." + column.name + " is null and E." + column.name + " is null");
						sEqualsEWoAlias.append("S." + column.name + " is null and " + dmlTableReference(ENTITY, session) + "." + column.name + " is null");
					}
				}
				remove = "Update " + dmlTableReference(ENTITY, session) + " E set E.birthday=-1 Where E.r_entitygraph=" + graphID + " and E.type=" + typeName(association.destination) + " " +
						  "and exists (Select * from " + dmlTableReference(ENTITY_SET_ELEMENT, session) + " S where S.set_id=" + setId + " and E.type=S.type and " + sEqualsE + ") " +
						  "and E.birthday<>-1";
				String removeWOAlias = "Update " + dmlTableReference(ENTITY, session) + " set birthday=-1 Where " + dmlTableReference(ENTITY, session) + ".r_entitygraph=" + graphID + " and " + dmlTableReference(ENTITY, session) + ".type=" + typeName(association.destination) + " " +
						"and exists (Select * from " + dmlTableReference(ENTITY_SET_ELEMENT, session) + " S where S.set_id=" + setId + " and " + dmlTableReference(ENTITY, session) + ".type=S.type and " + sEqualsEWoAlias + ") " +
						"and " + dmlTableReference(ENTITY, session) + ".birthday<>-1";
				boolean tryWithAliasFirst = !DBMS.POSTGRESQL.equals(session.dbms);
				boolean silent = session.getSilent();
				try {
					session.setSilent(true);
					rc = session.executeUpdate(tryWithAliasFirst? remove : removeWOAlias);
					totalRowcount += rc;
				} catch (SQLException e) {
					Session._log.debug("failed, retry with/without alias (" + e.getMessage() + ")");
					rc = session.executeUpdate(tryWithAliasFirst? removeWOAlias : remove);
					totalRowcount += rc;
				} finally {
					session.setSilent(silent);
				}
				deleteRows(session, dmlTableReference(ENTITY_SET_ELEMENT, session), "set_id=" + setId + "");
			}
			return rc;
		}
		return 0;
	}

	/**
	 * Reads all entities which depends on given entity.
	 *
	 * @param table the table from which to read entities
	 * @param association the dependency
	 * @param resultSet current row is given entity
	 * @param reader reads the entities
	 * @param selectionSchema the selection schema
	 */
	@Override
	public void readDependentEntities(Table table, Association association, ResultSet resultSet, ResultSetMetaData resultSetMetaData, ResultSetReader reader, Map<String, Integer> typeCache, String selectionSchema, String originalPKAliasPrefix) throws SQLException {
		String select;
		CellContentConverter cellContentConverter = new CellContentConverter(resultSetMetaData, session, session.dbms);
		if (originalPKAliasPrefix != null) {
			StringBuffer selectOPK = new StringBuffer();
			List<Column> pkColumns = rowIdSupport.getPrimaryKey(table).getColumns();
			for (int i = 0; i < pkColumns.size(); ++i) {
				if (i > 0) {
					selectOPK.append(", ");
				}
				selectOPK.append("T." + quoting.requote(pkColumns.get(i).name) + " as " + originalPKAliasPrefix + i);
			}
			select =
				"Select " + selectionSchema + " from (" +
				"Select " + selectOPK + ", " + filteredSelectionClause(table, false) + " from " + quoting.requote(table.getName()) + " T join " + dmlTableReference(DEPENDENCY, session) + " D on " +
				 pkEqualsEntityID(table, "T", "D", "TO_") + " and D.to_type=" + typeName(table) + "" +
				 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", cellContentConverter) +
				 " and D.from_type=" + typeName(association.source) + " and assoc=" + association.getId() +
				 " and D.r_entitygraph=" + graphID + ") T";
		} else {
			select = "Select " + selectionSchema + " from " + quoting.requote(table.getName()) + " T join " + dmlTableReference(DEPENDENCY, session) + " D on " +
				 pkEqualsEntityID(table, "T", "D", "TO_") + " and D.to_type=" + typeName(table) + "" +
				 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", cellContentConverter) +
				 " and D.from_type=" + typeName(association.source) + " and assoc=" + association.getId() +
				 " and D.r_entitygraph=" + graphID;
		}
		long rc = session.executeQuery(select, reader, withExplicitCommit());
		executionContext.getProgressListenerRegistry().fireExported(table, rc);
		addExportedCount(rc);
	}

	/**
	 * Marks all entities which depends on given entity as traversed.
	 *
	 * @param association the dependency
	 * @param resultSet current row is given entity
	 */
	@Override
	public void markDependentEntitiesAsTraversed(Association association, ResultSet resultSet, ResultSetMetaData resultSetMetaData, Map<String, Integer> typeCache) throws SQLException {
		String update;
		CellContentConverter cellContentConverter = new CellContentConverter(resultSetMetaData, session, session.dbms);
		if (DBMS.SYBASE.equals(session.dbms)) {
			update = "Update " + dmlTableReference(DEPENDENCY, session) + " set traversed=1" +
			 " Where " + pkEqualsEntityID(association.source, resultSet, dmlTableReference(DEPENDENCY, session), "FROM_", cellContentConverter) +
			 " and " + dmlTableReference(DEPENDENCY, session) + ".from_type=" + typeName(association.source) + " and assoc=" + association.getId() +
			 " and " + dmlTableReference(DEPENDENCY, session) + ".r_entitygraph=" + graphID;
		} else {
			update = "Update " + dmlTableReference(DEPENDENCY, session) + " D set traversed=1" +
			 " Where " + pkEqualsEntityID(association.source, resultSet, "D", "FROM_", cellContentConverter) +
			 " and D.from_type=" + typeName(association.source) + " and assoc=" + association.getId() +
			 " and D.r_entitygraph=" + graphID;
		}
		session.executeUpdate(update);
	}

	/**
	 * Reads all non-traversed dependencies.
	 *
	 * @param table the source of dependencies to look for
	 * @param reader reads the entities
	 */
	@Override
	public void readNonTraversedDependencies(Table table, ResultSetReader reader) throws SQLException {
		String select = "Select * from " + dmlTableReference(DEPENDENCY, session) + " D " +
			 " Where (traversed is null or traversed <> 1)" +
			 " and D.from_type=" + typeName(table) + "" +
			 " and D.r_entitygraph=" + graphID;
		session.executeQuery(select, reader, withExplicitCommit());
	}

	/**
	 * Removes all reflexive dependencies of given table.
	 *
	 * @param table the table
	 */
	@Override
	public void removeReflexiveDependencies(Table table) throws SQLException {
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		StringBuffer sb = new StringBuffer();
		for (Column column: universalPrimaryKey.getColumns()) {
			Column tableColumn = match.get(column);
			if (tableColumn != null) {
				if (sb.length() > 0) {
					sb.append(" and ");
				}
				if (tableColumn.isNullable) {
					sb.append("(FROM_" + column.name + " = TO_" + column.name);
					sb.append(" or (FROM_" + column.name + " is null and TO_" + column.name + " is null))");
				} else {
					sb.append("FROM_" + column.name + " = TO_" + column.name);
				}
			}
		}
		deleteRows(session, dmlTableReference(DEPENDENCY, session), sb +
			" and from_type=" + typeName(table) + "" +
			" and to_type=" + typeName(table) + "" +
			" and r_entitygraph=" + graphID);
	}

	/**
	 * Gets a SQL comparison expression for comparing rows with given entity.
	 *
	 * @param table the table
	 * @param resultSet
	 * @return a SQL comparison expression for comparing rows of <code>table</code> with current row of resultSet
	 */
	private String pkEqualsEntityID(Table table, ResultSet resultSet, String alias, String columnPrefix, CellContentConverter cellContentConverter) throws SQLException {
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		StringBuffer sb = new StringBuffer();
		for (Column column: universalPrimaryKey.getColumns()) {
			if (sb.length() > 0) {
				sb.append(" and ");
			}
			sb.append(alias + "." + columnPrefix + column.name);
			Column tableColumn = match.get(column);
			if (tableColumn != null) {
				int i = 0;
				for (Column c: rowIdSupport.getPrimaryKey(table).getColumns()) {
					if (c.name.equals(tableColumn.name)) {
						break;
					}
					++i;
				}
				Object object = cellContentConverter.getObject(resultSet, "PK" + i);
				if (object == null) {
					sb.append(" is null");
				} else {
					sb.append("=" + cellContentConverter.toSql(object));
				}
			} else {
				sb.append(" is null");
			}
		}
		return sb.toString();
	}

	/**
	 * Gets a SQL comparition expression for comparing rows with entities.
	 *
	 * @param table the table
	 * @return a SQL comparition expression for comparing rows of <code>table</code> with entities
	 */
	protected String pkEqualsEntityID(Table table, String tableAlias, String entityAlias) {
		return pkEqualsEntityID(table, tableAlias, entityAlias, "");
	}

	/**
	 * Gets a SQL comparison expression for comparing rows with entities.
	 *
	 * @param table the table
	 * @return a SQL comparison expression for comparing rows of <code>table</code> with entities
	 */
	private String pkEqualsEntityID(Table table, String tableAlias, String entityAlias, String columnPrefix) {
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		StringBuffer sb = new StringBuffer();
		for (Column column: universalPrimaryKey.getColumns()) {
			if (sb.length() > 0) {
				sb.append(" and ");
			}
			Column tableColumn = match.get(column);
			if (tableColumn != null) {
				if (tableColumn.isNullable) {
					sb.append("(");
				}
				sb.append(entityAlias + "." + columnPrefix + column.name);
				sb.append("=" + tableAlias + "." + quoting.requote(tableColumn.name));
				if (tableColumn.isNullable) {
					sb.append(" or (");
					sb.append(entityAlias + "." + columnPrefix + column.name + " is null and ");
					sb.append(tableAlias + "." + quoting.requote(tableColumn.name) + " is null)");
				}
				if (tableColumn.isNullable) {
					sb.append(")");
				}
			} else {
				sb.append(entityAlias + "." + columnPrefix + column.name);
				sb.append(" is null");
			}
		}
		return sb.toString();
	}

	/**
	 * Gets PK-column list for a table.
	 *
	 * @param table the table
	 * @param tableAlias the alias for table
	 * @return PK-column list for table
	 */
	private String pkList(Table table, String tableAlias) {
		return pkList(table, tableAlias, null);
	}

	/**
	 * Gets PK-column list for a table. (for Select clause)
	 *
	 * @param table the table
	 * @param tableAlias the alias for table
	 * @param columnAliasPrefix optional prefix for column names
	 */
	private String pkList(Table table, String tableAlias, String columnAliasPrefix) {
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		StringBuffer sb = new StringBuffer();
		for (Column column: universalPrimaryKey.getColumns()) {
			Column tableColumn = match.get(column);
			if (tableColumn != null) {
				if (sb.length() > 0) {
					sb.append(", ");
				}
				sb.append(tableAlias + "." + quoting.requote(tableColumn.name));
				sb.append(" as " + (columnAliasPrefix == null? "" : columnAliasPrefix) + column.name);
		   }
		}
		return sb.toString();
	}

	/**
	 * Gets PK-column list for a table. (for Insert clause)
	 *
	 * @param table the table
	 * @param columnAliasPrefix optional prefix for column names
	 */
	private String upkColumnList(Table table, String columnAliasPrefix) {
		Map<Column, Column> match = universalPrimaryKey.match(rowIdSupport.getPrimaryKey(table));
		StringBuffer sb = new StringBuffer();
		for (Column column: universalPrimaryKey.getColumns()) {
			Column tableColumn = match.get(column);
			if (tableColumn != null) {
				if (sb.length() > 0) {
					sb.append(", ");
				}
				if (columnAliasPrefix != null) {
					sb.append(columnAliasPrefix);
				}
				sb.append(column.name);
			}
		}
		return sb.toString();
	}

	/**
	 * Total row-count.
	 */
	private long totalRowcount = 0;

	/**
	 * Gets total row-count.
	 *
	 * @return total row-count
	 */
	@Override
	public long getTotalRowcount() {
		return totalRowcount;
	}

	/**
	 * Gets the universal primary key.
	 *
	 * @return the universal primary key
	 */
	@Override
	public PrimaryKey getUniversalPrimaryKey() {
		return universalPrimaryKey;
	}

	/**
	 * For creation of unique set-ids.
	 */
	private int nextSetId = 1;

	/**
	 * Creates a unique set id.
	 *
	 * @return a unique set id
	 */
	private synchronized int getNextSetId() {
		return graphID + (nextSetId++);
	}

	/**
	 * Shuts down statement-executor.
	 */
	@Override
	public void shutDown() throws SQLException {
		session.shutDown();
	}

	@Override
	public Session getSession() {
		return session;
	}

	@Override
	public DataModel getDatamodel() {
		return dataModel;
	}

	@Override
	public void close() throws SQLException {
		// nothing to do
	}

	@Override
	public Session getTargetSession() {
		return session;
	}

}

// TODO 1
// TODO insert into jailer_entity(pk0..n): insert null in all pk columns which are not relevant for current table
