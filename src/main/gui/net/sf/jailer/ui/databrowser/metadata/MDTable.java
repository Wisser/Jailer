/*
 * Copyright 2007 - 2026 Ralf Wisser.
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

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JComponent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Information about a database table.
 *
 * @author Ralf Wisser
 */
public class MDTable extends MDObject {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MDObject.class);

	private final MDSchema schema;
    private List<String> primaryKey;
    private List<String> columns;
    private List<String> columnComments = new ArrayList<String>();
    private String comment;
    private List<Column> columnTypes;
    private String pkConstraintName;
    private AtomicBoolean loading = new AtomicBoolean(false);
    private AtomicBoolean loaded = new AtomicBoolean(false);
    private final boolean isView;
    private final boolean isSynonym;
    private String tableType;

	private Long estimatedRowCount;

    // DDL of the table or <code>null</code>, if no DDL is available
    private volatile String ddl;

    /**
     * Constructor.
     *
     * @param name table name
     * @param schema the tables schema
     */
    public MDTable(String name, MDSchema schema, boolean isView, boolean isSynonym) {
        super(name, schema != null? schema.getMetaDataSource() : null);
        this.isView = isView;
        this.isSynonym = isSynonym;
        this.schema = schema;
    }

    /**
     * Gets the schema the tables schema
     *
     * @return the schema the tables schema
     */
    public MDSchema getSchema() {
        return schema;
    }

    /**
     * Gets columns of table
     *
     * @return columns of table
     */
    public List<String> getColumns() throws SQLException {
        return getColumns(true);
    }

    /**
     * Sets columns of table. If columns are not explicitly set, they will be lazy-loaded.
     *
     * @param columns columns of table
     */
    public void setColumns(List<String> columns) {
        this.columns = columns;
        loaded.set(true);
        loading.set(false);
    }

    /**
     * Gets columns of table
     *
     * @return columns of table
     */
    public List<String> getColumns(boolean cached) throws SQLException {
        readColumns(cached, null);
        return columns;
    }

    /**
     * Gets columns of table. Waits until a given timeout and sets the wait cursor.
     *
     * @return columns of table
     */
    public List<String> getColumns(long timeOut, JComponent waitCursorSubject) throws SQLException {
        if (isLoaded()) {
            return getColumns();
        }
        UIUtil.setWaitCursor(waitCursorSubject);
        try {
        	if (!loading.get()) {
                loading.set(true);
	            queue.add(new Runnable() {
	                @Override
	                public void run() {
	                    try {
	                        getColumns(false);
	                    } catch (SQLException e) {
	                    	logger.info("error", e);
	                    }
	                    loading.set(false);
	                }
	            });
	            while (loading.get() && System.currentTimeMillis() < timeOut) {
	                try {
	                    Thread.sleep(100);
	                } catch (InterruptedException e) {
	                }
	            }
        	}
        } finally {
        	UIUtil.resetWaitCursor(waitCursorSubject);
        }
        if (loading.get()) {
            return new ArrayList<String>();
        }
        return getColumns();
    }

    /**
     * Gets primary key columns of table
     *
     * @return primary key columns of table
     */
    public List<String> getPrimaryKeyColumns() throws SQLException {
        return getPrimaryKeyColumns(true);
    }

    /**
     * Gets primary key columns of table
     *
     * @return primary key columns of table
     */
    public List<String> getPrimaryKeyColumns(boolean cached) throws SQLException {
        readColumns(cached, null);
        return primaryKey;
    }

    private boolean retryReading = false;
    
    private synchronized void readColumns(boolean cached, Connection fallbackConnection) throws SQLException {
        if (columns == null || retryReading) {
        	retryReading = false;
            columns = new ArrayList<String>();
            columnTypes = new ArrayList<Column>();
            primaryKey = new ArrayList<String>();
            Session session = getSchema().getMetaDataSource().getSession();
			try {
                MetaDataSource metaDataSource = getMetaDataSource();
                synchronized (session.getMetaData()) {
                	ResultSet resultSet;
                	if (fallbackConnection != null) {
                		DatabaseMetaData metaData = fallbackConnection.getMetaData();
                		if (session.dbms.isUsesCatalog()) {
                			resultSet = metaData.getColumns(Quoting.staticUnquote(getSchema().getName()), null, Quoting.staticUnquote(getName()), "%");
            			} else {
            				resultSet = metaData.getColumns(null, Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()), "%");
            			}
                	} else {
                		resultSet = JDBCMetaDataBasedModelElementFinder.getColumns(session, Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()), "%",
                    		cached, false, isSynonym? "SYNONYM" : null);
                	}
                	while (resultSet.next()) {
//            			1.TABLE_CAT String => table catalog (may be null) 
//            			2.TABLE_SCHEM String => table schema (may be null) 
//            			3.TABLE_NAME String => table name 
            			if (!Quoting.equalsWROSearchPattern(Quoting.staticUnquote(getSchema().getName()), resultSet.getString(1), resultSet.getString(2))) {
            				continue;
            			}
            			if (!Quoting.equalsWROSearchPattern(Quoting.staticUnquote(getName()), resultSet.getString(3))) {
            				continue;
            			}
            			String colName = metaDataSource.getQuoting().quote(resultSet.getString(4));
                        columns.add(colName);
                        String comment = null;
                        try {
                        	comment = resultSet.getString(12);
                        	if ("".equals(comment)) {
                        		comment = null;
                        	}
                        } catch (Exception e) {
                        	// ignore
                        }
                        columnComments.add(comment);
                        int type = resultSet.getInt(5);
                        int length = 0;
                        int precision = -1;
                        String sqlType;
                        sqlType = resultSet.getString(6);
                        if (sqlType == null || sqlType.length() == 0) {
                        	sqlType = SqlUtil.SQL_TYPE.get(type);
                        }
                        if (type == Types.NUMERIC || type == Types.DECIMAL || JDBCMetaDataBasedModelElementFinder.TYPES_WITH_LENGTH.contains(sqlType.toUpperCase(Locale.ENGLISH)) || type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR || type == Types.BINARY || type == Types.VARBINARY) {
                            length = resultSet.getInt(7);
                        }
                        if (DBMS.MSSQL.equals(metaDataSource.getSession().dbms) && sqlType != null && sqlType.equalsIgnoreCase("timestamp")) {
                            length = 0;
                        }
                        if (sqlType != null && sqlType.equalsIgnoreCase("uniqueidentifier")) {
                            length = 0;
                        }
                        if (type == Types.NUMERIC || type == Types.DECIMAL || type == Types.VARCHAR || type == Types.CHAR) {
                            precision = resultSet.getInt(9);
                            if (resultSet.wasNull() || precision == 0) {
                                precision = -1;
                            }
                        }
                        if (type == Types.DISTINCT) {
                            length = 0;
                            precision = -1;
                        }
                        Column column = new Column(colName, JDBCMetaDataBasedModelElementFinder.filterType(sqlType, length, resultSet.getString(6), type, metaDataSource.getSession().dbms, resultSet.getInt(7)), JDBCMetaDataBasedModelElementFinder.filterLength(length, precision, resultSet.getString(6), type, metaDataSource.getSession().dbms, resultSet.getInt(7)), JDBCMetaDataBasedModelElementFinder.filterPrecision(length, precision, resultSet.getString(6), type, metaDataSource.getSession().dbms, resultSet.getInt(7)));
                        columnTypes.add(column);
                        column.isNullable = resultSet.getInt(11) == DatabaseMetaData.columnNullable;
                    }
                    resultSet.close();

                    if (fallbackConnection != null) {
                    	DatabaseMetaData metaData = fallbackConnection.getMetaData();
                		if (session.dbms.isUsesCatalog()) {
                    		resultSet = metaData.getPrimaryKeys(Quoting.staticUnquote(getSchema().getName()), null, Quoting.staticUnquote(getName()));
                		} else {
                			resultSet = metaData.getPrimaryKeys(null, Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()));
                		}
                    } else {
                    	resultSet = JDBCMetaDataBasedModelElementFinder.getPrimaryKeys(session, Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()), false);
                    }
                    Map<Integer, String> pk = new TreeMap<Integer, String>();
                    int nextKeySeq = 0;
                    while (resultSet.next()) {
                        int keySeq = resultSet.getInt(5);
                        pkConstraintName = resultSet.getString(6);
                        if (DBMS.SQLITE.equals(session.dbms) || pk.containsKey(keySeq)) {
                            // SQlite driver doesn't return the keySeq
                            keySeq = nextKeySeq++;
                            while (pk.containsKey(keySeq)) {
                            	++keySeq;
                            }
                        }
                        pk.put(keySeq, metaDataSource.getQuoting().quote(resultSet.getString(4)));
                    }
                    if (pk.isEmpty()) {
                    	Table table = metaDataSource.toTable(this);
                    	if (table != null) {
                    		if (table.primaryKey != null) {
                    			for (Column c: table.primaryKey.getColumns()) {
                    				for (String mc: columns) {
                    					if (Quoting.equalsIgnoreQuotingAndCase(c.name, mc)) {
                    						primaryKey.add(mc);
                    						break;
                    					}
                    				}
                    			}
                    		}
                    	}
                    } else {
                    	primaryKey.addAll(pk.values());
                    }
                    resultSet.close();
                }
                loaded.set(true);
            } catch (Exception e) {
            	retryReading = true;
            	if (!cached && fallbackConnection == null) {
	            	if (DBMS.MySQL.equals(session.dbms)) {
	            		Connection con = null;
	            		try {
	            			con = session.createNewConnection();
	            			readColumns(cached, con);
	            			return;
	            		} catch (Exception e2) {
	            			if (!warned2.get() && !session.isDown()) {
	                    		LogUtil.warn(new RuntimeException("NOkWithFBCon!: " + e.getMessage(), e2));
	                    		warned2.set(true);
	                    	}
						} finally {
							try {
								if (con != null) {
									con.close();
								}
							} catch (Throwable t) {
								// ignore
							}
						}
	            	}
            	}
            	if (!warned.get() && !session.isDown()) {
            		LogUtil.warn(e);
            		warned.set(true);
            	}
            	throw e;
            }
        }
    }

    private static final AtomicBoolean warned = new AtomicBoolean();
    private static final AtomicBoolean warned2 = new AtomicBoolean();

    /**
     * Compares data model table with this table.
     *
     * @param table the data model table
     * @return <code>true</code> iff table is uptodate
     */
    public boolean isUptodate(Table table) {
    	Set<String> unquotedUCColumnNames = new HashSet<String>();
        for (Column column: table.getColumns()) {
            unquotedUCColumnNames.add(Quoting.normalizeIdentifier(column.name));
        }
        try {
            if (getColumns().size() != table.getColumns().size()) {
                return false;
            }
            for (String column: getColumns()) {
                if (!unquotedUCColumnNames.contains(Quoting.normalizeIdentifier(column))) {
                    return false;
                }
            }
        } catch (SQLException e) {
            return true;
        }
        return true;
    }

	public boolean isSystem() {
		if (getTableType() != null) {
			if (getTableType().toUpperCase().contains("SYSTEM")) {
				return true;
			}
			if (isView && "INFORMATION_SCHEMA".equals(getSchema().getName())) {
				return true;
			}
    	}
   		return false;
	}

    public boolean isView() {
        return isView;
    }

    public boolean isSynonym() {
        return isSynonym;
    }

    public boolean isLoaded() {
        return loaded.get();
    }

    private static final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();

    static {
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                for (;;) {
                    try {
                        queue.take().run();
                    } catch (Throwable t) {
                    	logger.info("error", t);
                    }
                }
            }
        }, "Metadata-Tabledetails");
        thread.setDaemon(true);
        thread.start();
    }

    private static Object DDL_LOCK = new String("DDL_LOCK");

    /**
     * Gets DDL of the table.
     *
     * @return DDL of the table or <code>null</code>, if no DDL is available
     * @throws InterruptedException
     */
    public String getDDL() {
        if (ddlLoaded.get()) {
        	return ddl;
        }
    	synchronized (DDL_LOCK) {
	        if (!ddlLoaded.get()) {
	            Session session = getSchema().getMetaDataSource().getSession();

	            String statement = session.dbms.getDdlCall();

	            if (statement != null) {
	                CallableStatement cStmt = null;
	                try {
	                    Connection connection = session.getConnection();
	                    cStmt = connection.prepareCall(statement.replace("${type}", isView? "VIEW" : "TABLE").replace("${table}", Quoting.staticUnquote(getName())).replace("${schema}", Quoting.staticUnquote(getSchema().getName())));
	                    cStmt.registerOutParameter(1, Types.VARCHAR);
	                    cStmt.execute();
	                    ddl = cStmt.getString(1).trim();
	                } catch (Exception e) {
	                	// ignore
	                } finally {
	                    if (cStmt != null) {
	                        try {
	                            cStmt.close();
	                        } catch (SQLException e) {
	                        }
	                    }
	                }
	            }
	            statement = session.dbms.getDdlQuery();
	            if (statement != null) {
	                Statement cStmt = null;
	                try {
	                    Connection connection = session.getConnection();
	                    cStmt = connection.createStatement();
	                    ResultSet rs = cStmt.executeQuery(statement.replace("${type}", isView? "VIEW" : "TABLE").replace("${table}", Quoting.staticUnquote(getName())).replace("${schema}", Quoting.staticUnquote(getSchema().getName())));
	                    if (rs.next()) {
	                        ddl = rs.getString(session.dbms.isUsesCatalog()? 2 : 1).trim();
	                    }
	                    rs.close();
	                } catch (Exception e) {
	                    // ignore
	                } finally {
	                    if (cStmt != null) {
	                        try {
	                            cStmt.close();
	                        } catch (SQLException e) {
	                        }
	                    }
	                }
	            }
	            if (ddl == null && isView) {
	            	String viewTextOrDDLQuery = session.dbms.getViewTextOrDDLQuery();
	    			if (viewTextOrDDLQuery != null) {
	    				String viewTextQuery = String.format(Locale.ENGLISH, viewTextOrDDLQuery, Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()));
	    				try {
	    					session.executeQuery(viewTextQuery, new Session.AbstractResultSetReader() {
	    						@Override
	    						public void readCurrentRow(ResultSet resultSet) throws SQLException {
	    							ddl = resultSet.getString(1);
	    						}
	    					});
	    				} catch (Exception e) {
	    					// ignore
	    				}
	    			}
	            }
	            if (ddl != null && !ddl.trim().contains("\n")) {
	            	ddl = new BasicFormatterImpl().format(ddl);
	            }
	            try {
	            	readColumns(true, null); // load primary key
	            	if (ddl == null) {
	            		ddl = createDDL();
	            	}
	            } catch (Exception e) {
	            	ddl = "-- DDL not available";
	            	if (!session.isDown()) {
	            		logger.info("error", e);
	            	}
	            }
	            if (ddl != null) {
	            	ddl = ddl.trim();
	            	if (!ddl.endsWith(";")) {
	            		ddl += ";";
	            	}
	            	for (Entry<String, StringBuilder> e: getIndexDDL().entrySet()) {
	            		if (e.getValue().length() > 0) {
	            			ddl += "\n\n" + e.getValue() + ";";
	            		}
	            	}
	            }
	        }
	        ddlLoaded.set(true);
	        return ddl;
    	}
    }

    private Map<String, StringBuilder> getIndexDDL() {
    	Map<String, StringBuilder> result = new TreeMap<String, StringBuilder>();
    	Map<String, List<Pair<Integer, String>>> columns = new TreeMap<String, List<Pair<Integer,String>>>();
    	Session session = getSchema().getMetaDataSource().getSession();
    	ResultSet rs = null;
    	try {
			rs = JDBCMetaDataBasedModelElementFinder.getIndexes(session, Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()));
			while (rs.next()) {
				String indexName = rs.getString(6);
				String schemaName = rs.getString(5);
				String ascDesc = rs.getString(10);
				boolean unique = !rs.getBoolean(4);

				if (indexName != null) {
					MDSchema schema = schemaName != null? getMetaDataSource().find(schemaName) : null;
			        if (!result.containsKey(indexName)) {
			        	result.put(indexName, new StringBuilder("CREATE " + (unique? "UNIQUE " : "") + "INDEX " + (schema == null || schema.isDefaultSchema? "" : (schema.getName() + ".")) + indexName + "\n"
			        			+ "ON " + (getSchema().isDefaultSchema? "" : (getSchema().getName() + ".")) + getName() + " ("));
			        }
					List<Pair<Integer, String>> cols = columns.get(indexName);
					if (cols == null) {
						cols = new ArrayList<Pair<Integer,String>>();
						columns.put(indexName, cols);
					}
					cols.add(new Pair<Integer, String>(rs.getInt(8), rs.getString(9) + ("D".equals(ascDesc)? " DESC" : "")));
				}
			}
			rs.close();

			for (Entry<String, StringBuilder> e: result.entrySet()) {
	        	List<Pair<Integer, String>> cols = columns.get(e.getKey());
	        	if (cols == null) {
	        		e.getValue().setLength(0);
	        	} else {
	        		Collections.sort(cols, new Comparator<Pair<Integer, String>>() {
						@Override
						public int compare(Pair<Integer, String> o1, Pair<Integer, String> o2) {
							int i1 = o1.a  == null? 0 : o1.a;
							int i2 = o2.a  == null? 0 : o2.a;
							return i1 - i2;
						}
					});
	        		if (!primaryKey.isEmpty() && primaryKey.size() == cols.size()) {
	        			boolean isPKIndex = true;
	        			int i = 0;
		        		for (Pair<Integer, String> col: cols) {
	        				if (!Quoting.equalsIgnoreQuotingAndCase(primaryKey.get(i), col.b)) {
	        					isPKIndex = false;
	        					break;
	        				}
	        				++i;
	        			}
		        		if (isPKIndex) {
			        		e.getValue().setLength(0);
		        			continue;
		        		}
	        		}
	        		boolean f = true;
	        		for (Pair<Integer, String> col: cols) {
	        			if (!f) {
	        				e.getValue().append(", ");
	        			}
	        			e.getValue().append(getSchema().getMetaDataSource().getQuoting().quote(col.b));
	        			f = false;
	        		}
	        		e.getValue().append(")");
	        	}
	        }
		} catch (Throwable e) {
			if (!session.isDown()) {
				e.printStackTrace();
			}
			if (rs != null) {
				try {
					rs.close();
				} catch (SQLException e1) {
					// ignore
				}
			}
		}

		return result;
	}

	/**
     * Creates DDL for this table.
     *
     * @return DDL for this table
     */
    private String createDDL() {
        StringBuilder sb = new StringBuilder();
        if (comment != null) {
        	sb.append("/*\n" + (comment.replace("*/", "* /")) + "\n*/\n");
        }
        sb.append("CREATE TABLE " + (getSchema().isDefaultSchema? "" : (getSchema().getName() + ".")) + getName() + " (\n");
    	String nullableContraint = getMetaDataSource().getSession().dbms.getNullableContraint();
    	boolean prepComma = false;
    	int i = 0;
        for (Column column: columnTypes)  {
        	if (prepComma) {
        		sb.append(",\n");
        	}
        	prepComma = true;
        	String constraint = "";
        	if (nullableContraint != null) {
        		constraint = column.isNullable? " " + nullableContraint :  " NOT NULL";
        	} else {
        		constraint = column.isNullable? "" : " NOT NULL";
        	}
			sb.append("    " + column + constraint);
			String cc = null;
			if (columnComments.size() > i) {
				cc = columnComments.get(i);
			}
			if (cc != null) {
	        	sb.append(" /* " + (cc.replace("*/", "* /").replaceAll("\n|\r", " ")) + " */");
	        }
			++i;
        }
        if (!primaryKey.isEmpty()) {
        	if (prepComma) {
        		sb.append(",\n");
        	}
        	sb.append("    ");
        	if (pkConstraintName == null) {
            	sb.append("-- ");
        	}
        	sb.append("CONSTRAINT ");
        	if (pkConstraintName == null) {
            	sb.append(getName());
        		if (getName() != null && getName().toLowerCase().equals(getName())) {
                	sb.append("_pk");
        		} else {
        			sb.append("_PK");
        		}
        	} else {
            	sb.append(pkConstraintName);
        	}
        	sb.append(" PRIMARY KEY (");
        	prepComma = false;
        	for (String pk: primaryKey) {
        		if (prepComma) {
        			sb.append(", ");
        		}
        		prepComma = true;
        		sb.append(pk);
        	}
    		sb.append(")\n");
        }
        sb.append(");\n");
        return sb.toString();
    }

    public List<Column> getColumnTypes() {
		return columnTypes;
	}

    public boolean isDDLLoaded() {
        return ddlLoaded.get();
    }
    
    private boolean estRCIsLowerBound;

    public boolean isEstRCIsLowerBound() {
		return estRCIsLowerBound;
	}

	public Long getEstimatedRowCount() {
		return estimatedRowCount;
	}

	public void setEstimatedRowCount(Long erc, Boolean isLowerBound) {
    	if (isLowerBound != null) {
    		this.estRCIsLowerBound = isLowerBound;
    	}
		this.estimatedRowCount = erc;
		
//		if (!estUpdatePending.get()) {
//			estUpdatePending.set(true);
//			UIUtil.invokeLater(() -> {
//				Object desktopSplitPane = getMetaDataSource().getSession().getSessionProperty(DataBrowser.class, "jSplitPane1");
//				if (desktopSplitPane instanceof JSplitPane) {
//					int l = ((JSplitPane) desktopSplitPane).getDividerLocation();
//					((JSplitPane) desktopSplitPane).setDividerLocation(l - 1); 
//					UIUtil.invokeLater(2, () -> {
//						estUpdatePending.set(false);
//						((JSplitPane) desktopSplitPane).setDividerLocation(l);
//					});
//				} else {
//					estUpdatePending.set(false);
//				}
//			});
//		}
	}
    
//	static AtomicBoolean estUpdatePending = new AtomicBoolean(false);
	
    public void setComment(String comment) {
    	this.comment = "".equals(comment)? null : comment;
    }

    public String getTableType() {
		return tableType;
	}

	public void setTableType(String tableType) {
		this.tableType = tableType;
	}

    private AtomicBoolean ddlLoaded = new AtomicBoolean(false);

}
