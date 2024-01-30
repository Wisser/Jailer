/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
import java.util.List;

import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.modelbuilder.MemorizedResultSetTransformer;
import net.sf.jailer.util.Quoting;

/**
 * Meta Data Details.
 *
 * @author Ralf Wisser
 */
public enum MetaDataDetails {

	COLUMNS("Columns", 0) {
		@Override
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			ResultSet rs = JDBCMetaDataBasedModelElementFinder.getColumns(session, Quoting.staticUnquote(mdTable.getSchema().getName()), Quoting.staticUnquote(mdTable.getName()), "%", true, true, mdTable.isSynonym()? "SYNONYM" : null);
			MemorizedResultSet mRs = new MemorizedResultSet(rs, null, session, "");
			rs.close();
			if (mRs.getRowList().isEmpty()) {
				mRs.close();
				rs = JDBCMetaDataBasedModelElementFinder.getColumns(session, Quoting.staticUnquote(mdTable.getSchema().getName()), Quoting.staticUnquote(mdTable.getName()), "%", false, false, mdTable.isSynonym()? "SYNONYM" : null);
				mRs = new MemorizedResultSet(rs, null, session, "");
				rs.close();
			}
			
			List<Object[]> result = new ArrayList<Object[]>();
			for (Object[] row: mRs.getRowList()) {
//					1.TABLE_CAT String => table catalog (may be null) 
//					2.TABLE_SCHEM String => table schema (may be null) 
//					3.TABLE_NAME String => table name 
				if (!Quoting.equalsWROSearchPattern(Quoting.staticUnquote(mdTable.getSchema().getName()), String.valueOf(row[1 - 1]), String.valueOf(row[2 - 1]))) {
					continue;
				}
				if (!Quoting.equalsWROSearchPattern(Quoting.staticUnquote(mdTable.getName()), String.valueOf(row[3 - 1]))) {
					continue;
				}

	        	result.add(row);
	        }
			mRs.close();
			return new MemorizedResultSet(result, mRs.getMetaData());
		}
		@Override
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
	INDEXES("Indexes", 0) {
		@Override
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			ResultSet rs = JDBCMetaDataBasedModelElementFinder.getIndexes(session, Quoting.staticUnquote( mdTable.getSchema().getName()), Quoting.staticUnquote(mdTable.getName()));
			MemorizedResultSet mRs = new MemorizedResultSet(rs, null, session, "");
			mRs.removeNullRows(6);
			rs.close();
			try {
				return MemorizedResultSetTransformer.transform(mRs,
					new MemorizedResultSetTransformer.ColumnTransformation[] {
							new MemorizedResultSetTransformer.ColumnTransformation(6, null),
							new MemorizedResultSetTransformer.ColumnTransformation(9,
									new MemorizedResultSetTransformer.ListAggregation()),
							new MemorizedResultSetTransformer.ColumnTransformation(4, null),
							new MemorizedResultSetTransformer.ColumnTransformation(10, null),
							new MemorizedResultSetTransformer.ColumnTransformation(7, null)
				});
			} catch (Throwable e) {
				return mRs;
			}
		}
		@Override
		public void adjustRowsTable(JTable rowsTable) {
			TableModel dm = rowsTable.getModel();
			TableColumnModel columnModel = rowsTable.getColumnModel();
			if (columnModel.getColumnCount() > 6) {
				columnModel.moveColumn(1, columnModel.getColumnCount() - 1);
				columnModel.moveColumn(0, columnModel.getColumnCount() - 1);
				columnModel.moveColumn(6, 1);
				columnModel.moveColumn(4, 2);
				if (dm instanceof DefaultTableModel) {
					DefaultTableModel tm = (DefaultTableModel) dm;
					if (tm.getRowCount() > 0) {
						Object columnName = tm.getValueAt(0, 8);
						if (columnName == null || !(columnName instanceof String) && "null".equals(columnName.toString())) {
							// remove first row with COLUMN_NAME == null (ORACLE)
							tm.removeRow(0);
						}
					}
				}
			}
		}
	},
	PRIMARYKEY("Primary Key", 0) {
		@Override
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			ResultSet rs =  JDBCMetaDataBasedModelElementFinder.getPrimaryKeys(session, Quoting.staticUnquote(mdTable.getSchema().getName()), Quoting.staticUnquote(mdTable.getName()), false);
			MemorizedResultSet mRs = new MemorizedResultSet(rs, null, session, "");
			rs.close();
			try {
				return MemorizedResultSetTransformer.transform(mRs,
					new MemorizedResultSetTransformer.ColumnTransformation[] {
							new MemorizedResultSetTransformer.ColumnTransformation(6, null),
							new MemorizedResultSetTransformer.ColumnTransformation(4,
									new MemorizedResultSetTransformer.ListAggregation())
				});
			} catch (Throwable e) {
				return mRs;
			}
		}
		@Override
		public void adjustRowsTable(JTable rowsTable) {
			TableColumnModel columnModel = rowsTable.getColumnModel();
			if (columnModel.getColumnCount() > 4) {
				columnModel.moveColumn(3, 0);
				columnModel.moveColumn(4, 1);
			}
		}
	},
	EXPORTEDKEY("Exported Keys", 1) {
		@Override
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			ResultSet rs = JDBCMetaDataBasedModelElementFinder.getExportedKeys(session, Quoting.staticUnquote(mdTable.getSchema().getName()), Quoting.staticUnquote(mdTable.getName()));
			MemorizedResultSet mRs = new MemorizedResultSet(rs, null, session, "");
			rs.close();

			try {
				return MemorizedResultSetTransformer.transform(mRs,
					new MemorizedResultSetTransformer.ColumnTransformation[] {
							new MemorizedResultSetTransformer.ColumnTransformation(3, null),
							new MemorizedResultSetTransformer.ColumnTransformation(4,
									new MemorizedResultSetTransformer.ListAggregation()),
							new MemorizedResultSetTransformer.ColumnTransformation(7, null),
							new MemorizedResultSetTransformer.ColumnTransformation(8,
									new MemorizedResultSetTransformer.ListAggregation()),
							new MemorizedResultSetTransformer.ColumnTransformation(12, null),
							new MemorizedResultSetTransformer.ColumnTransformation(1, null),
							new MemorizedResultSetTransformer.ColumnTransformation(2, null),
							new MemorizedResultSetTransformer.ColumnTransformation(5, null),
							new MemorizedResultSetTransformer.ColumnTransformation(6, null),
							new MemorizedResultSetTransformer.ColumnTransformation(10, null),
							new MemorizedResultSetTransformer.ColumnTransformation(11, null),
							new MemorizedResultSetTransformer.ColumnTransformation(14, null)
				});
			} catch (Throwable e) {
				return mRs;
			}
		}
	},
	IMPORTEDKEY("Imported Keys", 1) {
		@Override
		public ResultSet readMetaDataDetails(Session session, MDTable mdTable) throws SQLException {
			ResultSet rs = JDBCMetaDataBasedModelElementFinder.getImportedKeys(session, Quoting.staticUnquote(mdTable.getSchema().getName()), Quoting.staticUnquote(mdTable.getName()), false);
			MemorizedResultSet mRs = new MemorizedResultSet(rs, null, session, "");
			rs.close();

			try {
				return MemorizedResultSetTransformer.transform(mRs,
					new MemorizedResultSetTransformer.ColumnTransformation[] {
							new MemorizedResultSetTransformer.ColumnTransformation(3, null),
							new MemorizedResultSetTransformer.ColumnTransformation(4,
									new MemorizedResultSetTransformer.ListAggregation()),
							new MemorizedResultSetTransformer.ColumnTransformation(7, null),
							new MemorizedResultSetTransformer.ColumnTransformation(8,
									new MemorizedResultSetTransformer.ListAggregation()),
							new MemorizedResultSetTransformer.ColumnTransformation(12, null),
							new MemorizedResultSetTransformer.ColumnTransformation(1, null),
							new MemorizedResultSetTransformer.ColumnTransformation(2, null),
							new MemorizedResultSetTransformer.ColumnTransformation(5, null),
							new MemorizedResultSetTransformer.ColumnTransformation(6, null),
							new MemorizedResultSetTransformer.ColumnTransformation(10, null),
							new MemorizedResultSetTransformer.ColumnTransformation(11, null),
							new MemorizedResultSetTransformer.ColumnTransformation(14, null)
				});
			} catch (Throwable e) {
				return mRs;
			}
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
