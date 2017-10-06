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

import javax.swing.JTable;
import javax.swing.table.TableColumnModel;

import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;

/**
 * Meta Data Details.
 *
 * @author Ralf Wisser
 */
public enum MetaDataDetails {
	
	COLUMNS("Columns", 0) {
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			return JDBCMetaDataBasedModelElementFinder.getColumns(session, session.getMetaData(), mdTable.getSchema().getName(), mdTable.getName(), "%", false);
		}
		public void adjustRowsTable(JTable rowsTable) {
			TableColumnModel columnModel = rowsTable.getColumnModel();
			if (columnModel.getColumnCount() > 17) {
				columnModel.moveColumn(3, 0);
				columnModel.moveColumn(5, 1);
				columnModel.moveColumn(17, 2);
				columnModel.moveColumn(6 + 1, 3);
				columnModel.moveColumn(8 + 1, 4);
			}
		}
	},
	PRIMARYKEY("Primary Key", 0) {
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			return JDBCMetaDataBasedModelElementFinder.getPrimaryKeys(session, session.getMetaData(), mdTable.getSchema().getName(), mdTable.getName(), false);
		}
		public void adjustRowsTable(JTable rowsTable) {
			TableColumnModel columnModel = rowsTable.getColumnModel();
			if (columnModel.getColumnCount() > 4) {
				columnModel.moveColumn(3, 0);
				columnModel.moveColumn(4, 1);
			}
		}
	},
	INDEXES("Indexes", 0) {
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			return JDBCMetaDataBasedModelElementFinder.getIndexes(session, session.getMetaData(), mdTable.getSchema().getName(), mdTable.getName());
		}
	},
	EXPORTEDKEY("Exported Keys", 1) {
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			return JDBCMetaDataBasedModelElementFinder.getExportedKeys(session, session.getMetaData(), mdTable.getSchema().getName(), mdTable.getName());
		}
	},
	IMPORTEDKEY("Imported Keys", 1) {
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			return JDBCMetaDataBasedModelElementFinder.getImportedKeys(session, session.getMetaData(), mdTable.getSchema().getName(), mdTable.getName(), false);
		}
	};
	
	MetaDataDetails(String name, int queueIndex) {
		this.name = name;
		this.queueIndex = queueIndex;
	}
	
	public final String name;
	public final int queueIndex;
	
	public abstract ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException;
	public void adjustRowsTable(JTable rowsTable) {
	}

}
