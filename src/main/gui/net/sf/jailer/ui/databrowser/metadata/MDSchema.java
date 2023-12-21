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
package net.sf.jailer.ui.databrowser.metadata;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.ImageIcon;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.Quoting;

/**
 * Information about a database schema.
 * 
 * @author Ralf Wisser
 */
public class MDSchema extends MDObject {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MDObject.class);

	public final boolean isDefaultSchema;
	private List<MDTable> tables;
	private static final BlockingQueue<Runnable> loadTableColumnsQueue = new LinkedBlockingQueue<Runnable>();
	private static final BlockingQueue<Runnable> loadTablesQueue = new LinkedBlockingQueue<Runnable>();
	private static final BlockingQueue<Runnable> loadMetaDataQueues[] = new LinkedBlockingQueue[3];
	private boolean valid = true;
	private AtomicBoolean loaded = new AtomicBoolean(false);
	private AtomicBoolean constraintsLoaded = new AtomicBoolean(false);

	/**
	 * Constructor.
	 * 
	 * @param name
	 *            schema name
	 * @param isDefaultSchema
	 *            <code>true</code> iff it's the users default schema
	 * @param metaDataSource
	 *            the source
	 */
	public MDSchema(String name, boolean isDefaultSchema, MetaDataSource metaDataSource) {
		super(name, metaDataSource);
		this.isDefaultSchema = isDefaultSchema;
	}

	static {
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						loadTableColumnsQueue.take().run();
					} catch (Throwable t) {
						// ignore
					}
				}
			}
		}, "Metadata-LoadColumns");
		thread.setDaemon(true);
		thread.start();

		thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						loadTablesQueue.take().run();
					} catch (Throwable t) {
						logger.info("error", t);
					}
				}
			}
		}, "Metadata-LoadTables");
		thread.setDaemon(true);
		thread.start();

		for (int i = 0; i < loadMetaDataQueues.length; ++i) {
			final LinkedBlockingQueue<Runnable> loadMetaDataQueue = new LinkedBlockingQueue<Runnable>();
			loadMetaDataQueues[i] = loadMetaDataQueue;
			thread = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						try {
							loadMetaDataQueue.take().run();
						} catch (Throwable t) {
							logger.info("error", t);
						}
					}
				}
			}, "Metadata-LoadOther-" + (i + 1));
			thread.setDaemon(true);
			thread.start();
		}
	}

	/**
	 * Gets tables of schema
	 * 
	 * @return tables of schema
	 */
	public List<MDTable> getTables() {
		return getTables(true, null, null);
	}

	private Object getTablesLock = new String("getTablesLock");
	
	/**
	 * Gets tables of schema
	 * @param afterLoadAction 
	 * 
	 * @return tables of schema
	 */
	public List<MDTable> getTables(boolean loadTableColumns, Runnable afterLoadAction, final Runnable afterLoadERCAction) {
		synchronized (getTablesLock) {
			if (tables == null) {
				try {
					tables = new ArrayList<MDTable>();
					MetaDataSource metaDataSource = getMetaDataSource();
					Set<String> partitions = JDBCMetaDataBasedModelElementFinder.findPartitions(metaDataSource.getSession(), Quoting.staticUnquote(getName()));
					synchronized (metaDataSource.getSession().getMetaData()) {
						ResultSet rs = metaDataSource.readTables(getName());
						Map<String, Runnable> loadJobs = new TreeMap<String, Runnable>();
						while (rs.next()) {
//	            			1.TABLE_CAT String => table catalog (may be null) 
//	            			2.TABLE_SCHEM String => table schema (may be null) 
//	            			3.TABLE_NAME String => table name 
	            			if (!Quoting.equalsWROSearchPattern(getName(), rs.getString(1), rs.getString(2))) {
	            				continue;
	            			}
	            			final String name = rs.getString(3);
	            			if (partitions.contains(name)) {
	            				continue;
	            			}
							String tableName = metaDataSource.getQuoting().quote(name);
							final MDTable table = new MDTable(tableName, this, rs.getString(4) != null && rs.getString(4).toUpperCase().contains("VIEW"),
									"SYNONYM".equalsIgnoreCase(rs.getString(4))
								 || "ALIAS".equalsIgnoreCase(rs.getString(4)));
							table.setTableType(rs.getString(4));
							table.setComment(rs.getString(5));
							tables.add(table);
							if (loadTableColumns) {
								loadJobs.put(tableName, new Runnable() {
									@Override
									public void run() {
										if (valid) {
											try {
												table.getColumns();
											} catch (SQLException e) {
												// logger.info("error", e);
											}
										}
									}
								});
							}
						}
						rs.close();
						
						for (Runnable loadJob : loadJobs.values()) {
							loadTableColumnsQueue.add(loadJob);
						}
						if (afterLoadAction != null) {
							loadTableColumnsQueue.add(afterLoadAction);
						}
					}
					Collections.sort(tables, new Comparator<MDTable>() {
						@Override
						public int compare(MDTable o1, MDTable o2) {
							return o1.getUnquotedName().compareToIgnoreCase(o2.getUnquotedName());
						}
					});
				} catch (SQLException e) {
					if (!getMetaDataSource().getSession().isDown()) {
						logger.info("error", e);
					}
				} finally {
					loaded.set(true);
				}
			}
			if (tables != null && afterLoadERCAction != null) {
				loadMetaDataQueues[1].add(new Runnable() {
					@Override
					public void run() {
						loadEstimatedRowCounts(afterLoadERCAction);
					}
				});
			}
			return tables;
		}
	}

	private Map<String, Long> estimatedRowCounts = Collections.synchronizedMap(new HashMap<String, Long>());
	private AtomicBoolean estimatedRowCountsLoaded = new AtomicBoolean(false);
	private Object estimatedRowCountsLock = new String("estimatedRowCounts");

	private void loadEstimatedRowCounts(final Runnable afterLoadAction) {
		synchronized (estimatedRowCountsLock) {
			if (!estimatedRowCountsLoaded.get()) {
				readEstimatedRowCounts();
				estimatedRowCountsLoaded.set(true);
			}
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					MetaDataSource metaDataSource = getMetaDataSource();
					for (MDTable table: tables) {
						table.setEstimatedRowCount(
								estimatedRowCounts.get(
										metaDataSource.getQuoting().unquote(
												table.getName())), false);
					}
					if (afterLoadAction != null) {
						afterLoadAction.run();
					}
				}
			});
		}
	}

	public boolean addEST(MDTable table, long delta, Boolean isLowerBound) {
		if (estimatedRowCountsLoaded.get()) {
			String key = getMetaDataSource().getQuoting().unquote(table.getName());
			Long rc = estimatedRowCounts.get(key);
			if (rc != null) {
				return setEST(table, rc + delta, isLowerBound);
			}
		}
		return false;
	}
	
	public boolean setEST(MDTable table, long rc, Boolean isLowerBound) {
		if (estimatedRowCountsLoaded.get()) {
			String key = getMetaDataSource().getQuoting().unquote(table.getName());
			if (rc < 0) {
				rc = 0;
			}
			Long old = estimatedRowCounts.get(key);
			if (old == null || old.longValue() != rc) {
				estimatedRowCounts.put(key, rc);
				table.setEstimatedRowCount(rc, isLowerBound);
				return true;
			}
		}
		return false;
	}
	
	private void readEstimatedRowCounts() {
		String query = getMetaDataSource().getSession().dbms.getEstimatedRowCountQuery();
		if (query != null) {
			try {
				getMetaDataSource().getSession().executeQuery(String.format(Locale.ENGLISH, query, getUnquotedName()), new AbstractResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						String tableName = resultSet.getString(1);
						long rowCount = resultSet.getLong(2);
						if (!resultSet.wasNull() && rowCount >= 0) {
							estimatedRowCounts.put(tableName, rowCount);
						}
					}
				});
			} catch (SQLException e) {
				// ignore
			}
		}
	}

	/**
	 * Have the tables of the schema been loaded?
	 */
	public boolean isLoaded() {
		return loaded.get();
	}

	/**
	 * Asynchronously loads the tables.
	 */
	public void loadTables(final boolean loadTableColumns, final Runnable afterLoadAction, final Runnable afterAvailableAction, final Runnable afterLoadESTAction) {
		loadTablesQueue.add(new Runnable() {
			@Override
			public void run() {
				if (!getMetaDataSource().getSession().isDown()) {
					getTables(loadTableColumns, afterLoadAction, afterLoadESTAction);
					if (afterAvailableAction != null) {
						afterAvailableAction.run();
					}
				}
			}
		});
	}

	private final Map<String, MDTable> tablePerUnquotedNameUC = new HashMap<String, MDTable>();

	/**
	 * Find table by name.
	 * 
	 * @param tableName
	 *            table name
	 * @return table by name
	 */
	public MDTable find(String tableName) {
		synchronized (tablePerUnquotedNameUC) {
			if (tablePerUnquotedNameUC.isEmpty()) {
				for (MDTable table : getTables()) {
					tablePerUnquotedNameUC.put(Quoting.normalizeIdentifier(table.getName()), table);
				}
			}
			return tablePerUnquotedNameUC.get(Quoting.normalizeIdentifier(tableName));
		}
	}

	private Object validLock = new Object();
	
	public void setValid(boolean valid) {
		synchronized (validLock) {
			this.valid = valid;
		}
	}

	/**
	 * Asynchronously load some meta data.
	 * 
	 * @param metaDataLoad
	 *            loads the meta data
	 * @param queueID
	 *            queue-ID. 0 for cheap loads, 1 for heavy loads
	 */
	public static void loadMetaData(Runnable metaDataLoad, int queueID) {
		loadMetaDataQueues[queueID].add(metaDataLoad);
	}

	private MemorizedResultSet constraints;
	
	public boolean isConstraintsLoaded() {
		return constraintsLoaded.get();
	}
	
	private Object getConstraintsLock = new String("getConstraintsLock");

	/**
	 * Loads constraint list.
	 * 
	 * @param table table or <code>null</code> for loading constraints of all tables
	 * @param dataModel the data model
	 * @return constraint list
	 */
	public MemorizedResultSet getConstraints(MDTable table, DataModel dataModel) throws SQLException {
		synchronized (getConstraintsLock) {
			if (constraints == null) {
				Statement cStmt = null;
				try {
					Connection connection = getMetaDataSource().getSession().getConnection();
					cStmt = connection.createStatement();
					String schema = getName();
					if (schema != null) {
						schema = Quoting.staticUnquote(schema);
					}
					String[] queries = getMetaDataSource().getSession().dbms.getConstraintsQuery().split("\\s*fallback contraint query\\s*");
					for (String query: queries) {
						ResultSet rs = null;
						try {
							rs = cStmt.executeQuery(String.format(Locale.ENGLISH, query, schema));
							MemorizedResultSet result = new MemorizedResultSet(rs, null, getMetaDataSource().getSession(), schema);
							rs.close();
							rs = null;
							List<Object[]> rows = new ArrayList<Object[]>();
							Object[] predRow = null;
							Map<String, String> constrType = new HashMap<String, String>();
							boolean hasPK = false;
							for (Object[] row: result.getRowList()) {
								String constr = String.valueOf(row[0]);
								String tab = String.valueOf(row[1]);
								String col = row[2] == null? null : String.valueOf(row[2]);
								String type = String.valueOf(row[3]);
								String detail = row[4] == null? "" : String.valueOf(row[4]).trim();
								if (type.equals("PK")) {
									hasPK = true;
								}
								if (type.equals("Check") && detail.matches("\"?" + col + "\"? IS NOT NULL")) {
									continue;
								}
								String pCType = constrType.get(constr);
								if (pCType != null && !pCType.equals(type)) {
									continue;
								}
								constrType.put(constr, type);
								if (predRow != null && constr.equals(String.valueOf(predRow[0])) && tab.equals(String.valueOf(predRow[1])) && (col != null && String.valueOf(predRow[2]) != null) && type.equals(String.valueOf(predRow[3]))) {
									rows.get(rows.size() - 1)[3] += ", " + col;
								} else {
									rows.add(new Object[] { getConstraintTypeIcon(type), constr, tab, col, detail });
								}
								predRow = row;
							}
							result.close();
							if (!hasPK && dataModel != null) {
								Map<String, String> tabs = new TreeMap<>();
								dataModel.getTables().forEach(t -> tabs.put(Quoting.staticUnquote(t.getName()).toUpperCase(), t.primaryKey.columnList("")));
								tabs.forEach((t, c) -> rows.add(new Object[] { getConstraintTypeIcon("PK"), "Primary Key", t, c, "" }));
							}
							constraints = new MemorizedResultSet(rows, 5, new String[] { "Type", "Constraint", "Table", "Columns", "Details" }, new int[] { Types.OTHER, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR } );
							constraintsLoaded.set(true);
							break;
						} catch (Exception e) {
							if (rs != null) {
								try {
									rs.close();
								} catch (Exception e2) {
									// ignore
								}
							}
							if (queries.length > 0 && query.equals(queries[queries.length - 1])) {
								throw e;
							}
						}
					}
				} finally {
					if (cStmt != null) {
						try {
							cStmt.close();
						} catch (SQLException e) {
						}
					}
				}
			}
			if (constraints != null) {
				constraints.reset();
				if (table != null) {
					String uCName = Quoting.normalizeIdentifier(table.getName());
					List<Object[]> rows = new ArrayList<Object[]>();
					for (Object[] row: constraints.getRowList()) {
						if (uCName.equals(String.valueOf(row[2]).toUpperCase(Locale.ENGLISH))) {
							rows.add(row);
						}
					}
					return new MemorizedResultSet(rows, 5, new String[] { "Type", "Constraint", "Table", "Columns", "Details" }, new int[] { Types.OTHER, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR } );
				}
			}
			return constraints;
		}
	}

	private static Map<String, ImageIcon> constraintTypeIcons = Collections.synchronizedMap(new HashMap<String, ImageIcon>());

	public static UIUtil.IconWithText getConstraintTypeIcon(final String type) {
		ImageIcon icon = null;
		if (type != null) {
			String iconURL = "constraint_" + (type.replaceAll(" +", "").toLowerCase(Locale.ENGLISH)) + ".png";
			icon = constraintTypeIcons.get(iconURL);
			if (icon == null) {
				try {
		            icon = UIUtil.readImage("/" + iconURL);
		        } catch (Exception e) {
		        }
			}
			constraintTypeIcons.put(iconURL, icon);
		}
		return new UIUtil.IconWithText(type, icon);
	}

}
