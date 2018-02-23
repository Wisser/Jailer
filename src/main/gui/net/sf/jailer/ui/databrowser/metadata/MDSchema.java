/*
 * Copyright 2007 - 2017 the original author or authors.
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
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

import org.apache.log4j.Logger;

import net.sf.jailer.modelbuilder.MetaDataCache;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;
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
	private static final Logger logger = Logger.getLogger(MDObject.class);

	public final boolean isDefaultSchema;
	private List<MDTable> tables;
	private static final BlockingQueue<Runnable> loadTableColumnsQueue = new LinkedBlockingQueue<Runnable>();
	private static final BlockingQueue<Runnable> loadTablesQueue = new LinkedBlockingQueue<Runnable>();
	private static final BlockingQueue<Runnable> loadMetaDataQueues[] = new LinkedBlockingQueue[2];
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
		});
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
		});
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
			});
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
		return getTables(true);
	}

	private Object getTablesLock = new String("getTablesLock");
	
	/**
	 * Gets tables of schema
	 * 
	 * @return tables of schema
	 */
	public List<MDTable> getTables(boolean loadTableColumns) {
		synchronized (getTablesLock) {
			if (tables == null) {
				try {
					tables = new ArrayList<MDTable>();
					MetaDataSource metaDataSource = getMetaDataSource();
					synchronized (metaDataSource.getSession().getMetaData()) {
						ResultSet rs = metaDataSource.readTables(getName());
						Map<String, Runnable> loadJobs = new TreeMap<String, Runnable>();
						while (rs.next()) {
							String tableName = metaDataSource.getQuoting().quote(rs.getString(3));
							final MDTable table = new MDTable(tableName, this, "VIEW".equalsIgnoreCase(rs.getString(4)),
									"SYNONYM".equalsIgnoreCase(rs.getString(4))
											|| "ALIAS".equalsIgnoreCase(rs.getString(4)));
							tables.add(table);
							if (loadTableColumns) {
								loadJobs.put(tableName, new Runnable() {
									@Override
									public void run() {
										if (valid) {
											try {
												table.getColumns();
											} catch (SQLException e) {
												logger.info("error", e);
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
					}
					Collections.sort(tables, new Comparator<MDTable>() {
						@Override
						public int compare(MDTable o1, MDTable o2) {
							return o1.getName().compareTo(o2.getName());
						}
					});
				} catch (SQLException e) {
					logger.info("error", e);
				} finally {
					loaded.set(true);
				}
			}
			return tables;
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
	public void loadTables() {
		loadTablesQueue.add(new Runnable() {
			@Override
			public void run() {
				getTables(false);
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
					tablePerUnquotedNameUC.put(Quoting.staticUnquote(table.getName().toUpperCase(Locale.ENGLISH)), table);
				}
			}
			return tablePerUnquotedNameUC.get(Quoting.staticUnquote(tableName.toUpperCase(Locale.ENGLISH)));
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

	private CachedResultSet constraints;
	
	public boolean isConstraintsLoaded() {
		return constraintsLoaded.get();
	}
	
	private Object getConstraintsLock = new String("getConstraintsLock");

	/**
	 * Loads constraint list.
	 * 
	 * @param table table or <code>null</code> for loading constraints of all tables
	 * @return constraint list
	 */
	public CachedResultSet getConstraints(MDTable table) throws SQLException {
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
					String query = getMetaDataSource().getSession().dbms.getConstraintsQuery();
					ResultSet rs = cStmt.executeQuery(String.format(query, schema));
					CachedResultSet result = new MetaDataCache.CachedResultSet(rs, null, getMetaDataSource().getSession(), schema);
					rs.close();
					List<Object[]> rows = new ArrayList<Object[]>();
					Object[] predRow = null;
					for (Object[] row: result.getRowList()) {
						String constr = String.valueOf(row[0]);
						String tab = String.valueOf(row[1]);
						String col = row[2] == null? null : String.valueOf(row[2]);
						String type = String.valueOf(row[3]);
						String detail = row[4] == null? "" : String.valueOf(row[4]).trim();
						if (type.equals("Check") && detail.matches("\"?" + col + "\"? IS NOT NULL")) {
							continue;
						}
						if (predRow != null && constr.equals(predRow[0]) && tab.equals(predRow[1]) && (col != null && predRow[2] != null) && type.equals(predRow[3])) {
							rows.get(rows.size() - 1)[3] += ", " + col;
						} else {
							rows.add(new Object[] { getConstraintTypeIcon(type), constr, tab, col, detail });
						}
						predRow = row;
					}
					result.close();
					constraints = new MetaDataCache.CachedResultSet(rows, 5, new String[] { "Type", "Constraint", "Table", "Columns", "Details" }, new int[] { Types.OTHER, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR } );
					constraintsLoaded.set(true);
				} finally {
					if (cStmt != null) {
						try {
							cStmt.close();
						} catch (SQLException e) {
						}
					}
				}
			}
			constraints.reset();
			if (table != null) {
				String uCName = Quoting.staticUnquote(table.getName()).toUpperCase(Locale.ENGLISH);
				List<Object[]> rows = new ArrayList<Object[]>();
				for (Object[] row: constraints.getRowList()) {
					if (uCName.equals(String.valueOf(row[2]).toUpperCase(Locale.ENGLISH))) {
						rows.add(row);
					}
				}
				return new MetaDataCache.CachedResultSet(rows, 5, new String[] { "Type", "Constraint", "Table", "Columns", "Details" }, new int[] { Types.OTHER, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR, Types.VARCHAR } );
			}
			return constraints;
		}
	}

	private static Map<String, ImageIcon> constraintTypeIcons = Collections.synchronizedMap(new HashMap<String, ImageIcon>());
	
	public static JLabel getConstraintTypeIcon(final String type) {
		ImageIcon icon = null;
		if (type != null) {
			String iconURL = "constraint_" + (type.replaceAll(" +", "").toLowerCase()) + ".png";
			icon = constraintTypeIcons.get(iconURL);
			if (icon == null) {
				try {
		            icon = new ImageIcon(MDSchema.class.getResource("/net/sf/jailer/ui/resource/" + iconURL));
		        } catch (Exception e) {
		        }
			}
			constraintTypeIcons.put(iconURL, icon);
		}
		JLabel label = new JLabel(type) {
			@Override
			public String toString() {
				return type;
			}
		};
		label.setIcon(icon);
		return label;
	}

}
