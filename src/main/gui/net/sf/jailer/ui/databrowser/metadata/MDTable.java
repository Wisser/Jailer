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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

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
	
	/**
	 * Constructor.
	 * 
	 * @param name table name
	 * @param schema the tables schema
	 */
	public MDTable(String name, MDSchema schema) {
		super(name, schema.getMetaDataSource());
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
			if (getColumns().size() != unquotedUCColumnNames.size()) {
				return false;
			}
			for (String column: getColumns()) {
				if (!unquotedUCColumnNames.contains(column.toUpperCase(Locale.ENGLISH))) {
					return false;
				}
			}
		} catch (SQLException e) {
			return true;
		}
		return true;
	}

}
