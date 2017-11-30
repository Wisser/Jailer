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

import java.awt.Cursor;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JComponent;

import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.Quoting;

/**
 * Information about a database table.
 * 
 * @author Ralf Wisser
 */
public class MDTable extends MDObject {

	private final MDSchema schema;
	private List<String> primaryKey;
	private List<String> columns;
	private AtomicBoolean loading = new AtomicBoolean(false);
	private AtomicBoolean loaded = new AtomicBoolean(false);
	private final boolean isView;
	private final boolean isSynonym;

	// DDL of the table or <code>null</code>, if no DDL is available
	private String ddl;
	
	/**
	 * Constructor.
	 * 
	 * @param name table name
	 * @param schema the tables schema
	 */
	public MDTable(String name, MDSchema schema, boolean isView, boolean isSynonym) {
		super(name, schema.getMetaDataSource());
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
		readColumns();
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
		waitCursorSubject.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		try {
			loading.set(true);
			queue.add(new Runnable() {
				@Override
				public void run() {
					try {
						getColumns();
					} catch (SQLException e) {
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
		} finally {
			waitCursorSubject.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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
		readColumns();
		return primaryKey;
	}

	private synchronized void readColumns() throws SQLException {
		if (columns == null) {
			columns = new ArrayList<String>();
			primaryKey = new ArrayList<String>();
    		try {
				MetaDataSource metaDataSource = getMetaDataSource();
				synchronized (metaDataSource.getSession().getMetaData()) {
	    			ResultSet rs = JDBCMetaDataBasedModelElementFinder.getColumns(getSchema().getMetaDataSource().getSession(), getSchema().getMetaDataSource().getSession().getMetaData(), Quoting.staticUnquote(getSchema().getName()), Quoting.staticUnquote(getName()), "%", false);
					while (rs.next()) {
						columns.add(metaDataSource.getQuoting().quote(rs.getString(4)));
					}
					rs.close();
					
					rs = JDBCMetaDataBasedModelElementFinder.getPrimaryKeys(getSchema().getMetaDataSource().getSession(), getSchema().getMetaDataSource().getSession().getMetaData(), getSchema().getName(), getName(), false);
					while (rs.next()) {
						primaryKey.add(rs.getString(4));
					}
					rs.close();
	    		}
    		} finally {
    			loaded.set(true);
    		}
		}
	}

	/**
	 * Compares data model table with this table.
	 * 
	 * @param table the data model table
	 * @return <code>true</code> iff table is uptodate
	 */
	public boolean isUptodate(Table table) {
		Set<String> unquotedUCColumnNames = new HashSet<String>();
		for (Column column: table.getColumns()) {
			unquotedUCColumnNames.add(Quoting.staticUnquote(column.name).toUpperCase(Locale.ENGLISH));
		}
		try {
			if (getColumns().size() != table.getColumns().size()) {
				return false;
			}
			for (String column: getColumns()) {
				if (!unquotedUCColumnNames.contains(Quoting.staticUnquote(column).toUpperCase(Locale.ENGLISH))) {
					return false;
				}
			}
		} catch (SQLException e) {
			return true;
		}
		return true;
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
					}
				}
			}
		});
        thread.setDaemon(true);
        thread.start();
	}

	/**
	 * Gets DDL of the table.
	 * 
	 * @return DDL of the table or <code>null</code>, if no DDL is available
	 * @throws InterruptedException 
	 */
	public synchronized String getDDL() {
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
						ddl = rs.getString(session.dbms.equals(DBMS.MySQL)? 2 : 1).trim();
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
		}
		ddlLoaded.set(true);
		return ddl;
	}

	public boolean isDDLLoaded() {
		return ddlLoaded.get();
	}
	
	private AtomicBoolean ddlLoaded = new AtomicBoolean(false);
	
}
