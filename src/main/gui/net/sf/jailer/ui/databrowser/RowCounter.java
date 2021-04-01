/*
 * Copyright 2007 - 2021 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import net.sf.jailer.database.InlineViewStyle;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.RowIdSupport;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Counts number of rows associated with a given row.
 */
public class RowCounter {

	private final Table table;
	private final Association association;
	private final Session session;
	private final List<Row> theRows;
	private final RowIdSupport rowIdSupport;
	private final int TIMEOUT = 6;

	public static class RowCount {
		public final long count;
		public final boolean isExact;

		public RowCount(long count, boolean isExact) {
			this.count = count;
			this.isExact = isExact;
		}
	}

	public RowCounter(Table table, Association association, List<Row> theRows, Session session, RowIdSupport rowIdSupport) {
		this.table = table;
		this.association = association;
		this.theRows = theRows;
		this.session = session;
		this.rowIdSupport = rowIdSupport;
	}

	/**
	 * Counts rows from {@link #table}.
	 *
	 * @param context
	 *            cancellation context
	 * @param limit
	 *            row number limit
	 */
	public RowCount countRows(String andCond, Object context, int limit, boolean selectDistinct) throws SQLException {

		List<Row> pRows = theRows;
		pRows = new ArrayList<Row>(pRows);
		Map<String, Row> rowSet = new HashMap<String, Row>();
		long maxTime = System.currentTimeMillis() + 1000 * TIMEOUT;

		if (association != null && rowIdSupport.getPrimaryKey(association.source).getColumns().isEmpty()) {
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 1, maxTime, null);
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				if (System.currentTimeMillis() >= maxTime) {
					return new RowCount(-1, true);
				}
				throw e;
			}
		} else {
			if (BrowserContentPane.useInlineViewForResolvingAssociation(session)) {
				try {
					InlineViewStyle inlineViewStyle = session.getInlineViewStyle();
					if (inlineViewStyle != null) {
						return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 258, maxTime, inlineViewStyle);
					}
				} catch (Throwable e) { // embedded DBMS may throw non-SQLException
					if (System.currentTimeMillis() >= maxTime) {
						return new RowCount(-1, true);
					}
					Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
				}
			}
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 258, maxTime, null);
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				if (System.currentTimeMillis() >= maxTime) {
					return new RowCount(-1, true);
				}
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 100, maxTime, null);
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				if (System.currentTimeMillis() >= maxTime) {
					return new RowCount(-1, true);
				}
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 40, maxTime, null);
			} catch (Throwable e) { // embedded DBMS may throw non-SQLException
				if (System.currentTimeMillis() >= maxTime) {
					return new RowCount(-1, true);
				}
				Session._log.warn("failed, try another blocking-size (" +  e.getMessage() + ")");
			}
		}

		try {
			return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 1, maxTime, null);
		} catch (Throwable e) { // embedded DBMS may throw non-SQLException
			if (System.currentTimeMillis() >= maxTime) {
				return new RowCount(-1, true);
			}
			throw e;
		}
	}

	private RowCount loadRowBlocks(String andCond, Object context, int limit, boolean selectDistinct, List<Row> pRows,
			Map<String, Row> rowSet, int NUM_PARENTS, long maxTime, InlineViewStyle inlineViewStyle) throws SQLException {
		List<List<Row>> parentBlocks = new ArrayList<List<Row>>();
		List<Row> currentBlock = new ArrayList<Row>();
		parentBlocks.add(currentBlock);
		for (Row pRow : pRows) {
			if (currentBlock.size() >= NUM_PARENTS) {
				currentBlock = new ArrayList<Row>();
				parentBlocks.add(currentBlock);
			}
			currentBlock.add(pRow);
		}

		long rc = 0;
		boolean isExact = true;

		if (!pRows.isEmpty()) for (List<Row> pRowBlockI : parentBlocks) {

			if (System.currentTimeMillis() >= maxTime) {
				return new RowCount(-1, true);
			}

			List<Row> pRowBlock = pRowBlockI;
			Map<String, List<Row>> newBlockRows = new HashMap<String, List<Row>>();
			boolean loaded = false;

			if (pRowBlock.size() == 1 && pRowBlock.get(0) == null) {
				pRowBlock = null;
			}

			long brc = 0;
			if (session.dbms.getSqlLimitSuffix() != null) {
				try {
					session.setSilent(true);
					brc += countRows(andCond, pRowBlock, newBlockRows, context, limit, false, session.dbms.getSqlLimitSuffix(), selectDistinct, maxTime, inlineViewStyle);
					loaded = true;
				} catch (Throwable e) { // embedded DBMS may throw non-SQLException
					if (System.currentTimeMillis() >= maxTime) {
						return new RowCount(-1, true);
					}
					Session._log.warn("failed, try another limit-strategy (" +  e.getMessage() + ")");
				} finally {
					session.setSilent(false);
				}
			}
			if (!loaded) {
				try {
					session.setSilent(true);
					brc += countRows(andCond, pRowBlock, newBlockRows, context, limit, true, null, selectDistinct, maxTime, inlineViewStyle);
					loaded = true;
				} catch (Throwable e) { // embedded DBMS may throw non-SQLException
					if (System.currentTimeMillis() >= maxTime) {
						return new RowCount(-1, true);
					}
					Session._log.warn("failed, try another limit-strategy (" +  e.getMessage() + ")");
				} finally {
					session.setSilent(false);
				}
				if (!loaded) {
					try {
						session.setSilent(true);
						brc += countRows(andCond, pRowBlock, newBlockRows, context, limit, false, null, selectDistinct, maxTime, inlineViewStyle);
					} finally {
						session.setSilent(false);
					}
				}
			}
			if (rc > 0 && brc > 0) {
				isExact = false;
				break;
			}
			rc += brc;
			limit -= brc;
			if (limit <= 0) {
				break;
			}
		}

		return new RowCount(rc, isExact);
	}

	/**
	 * Alias for row number column.
	 */
	private static final String ROWNUMBERALIAS = "RN";

	/**
	 * count rows from {@link #table}.
	 *
	 * @param rows
	 *            to put the rows into
	 * @param context
	 *            cancellation context
	 * @param selectDistinct
	 * @param inlineViewStyle
	 */
	public long countRows(String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, boolean selectDistinct, long maxTime, InlineViewStyle inlineViewStyle) throws SQLException {
		final Quoting quoting = Quoting.getQuoting(session);
		String sql = "Select "; // + (selectDistinct? "distinct " : "");
		if (association != null) {
			sql += "distinct ";
		}

		{
			String olapPrefix = "Select 1, row_number() over(order by -1) as " + ROWNUMBERALIAS;
			String olapSuffix = ") S";
			boolean limitSuffixInSelectClause = sqlLimitSuffix != null &&
					(sqlLimitSuffix.toLowerCase(Locale.ENGLISH).startsWith("top ") || sqlLimitSuffix.toLowerCase(Locale.ENGLISH).startsWith("first "));
			if (sqlLimitSuffix != null && limitSuffixInSelectClause) {
				sql += (sqlLimitSuffix.replace("%s", Integer.toString(limit))) + " ";
			}

			if (association != null) {
				olapPrefix += " From (Select distinct ";
				olapSuffix += ") S2 Where S2." + ROWNUMBERALIAS + " <= \" + limit";
				boolean f = true;
				for (Column pkColumn: rowIdSupport.getPrimaryKey(association.destination).getColumns()) {
					if (!f) {
						sql += ", ";
						olapPrefix += ", ";
					}
					sql += "A." + pkColumn.name;
					olapPrefix += "S." + pkColumn.name;
					f = false;
				}
			} else {
				sql += "1";
			}

			sql += " From ";
			if (association != null) {
				sql += qualifiedTableName(association.destination, quoting) + " A join ";
			}
			sql += qualifiedTableName(table, quoting) + " B";
			if (association != null) {
				if (association.reversed) {
					sql += " on " + association.getUnrestrictedJoinCondition();
				} else {
					sql += " on " + SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition());
				}
			}

			if (parentRows != null && !parentRows.isEmpty()) {
				if (parentRows.size() == 1) {
					sql += " Where (" + parentRows.get(0).rowId + ")";
				} else {
					StringBuilder sb = new StringBuilder();
					if (inlineViewStyle != null) {
						sb.append(" join ");
						List<String> columnNames = new ArrayList<String>();
						for (Column pkColumn: rowIdSupport.getPrimaryKey(table).getColumns()) {
							columnNames.add(pkColumn.name);
						}
						String[] columnNamesAsArray = columnNames.toArray(new String[columnNames.size()]);
						sb.append(inlineViewStyle.head(columnNamesAsArray));
						int rowNumber = 0;
						for (Row parentRow: parentRows) {
							if (rowNumber > 0) {
								sb.append(inlineViewStyle.separator());
							}
							sb.append(inlineViewStyle.item(parentRow.primaryKey, columnNamesAsArray, rowNumber));
							++rowNumber;
						}
						sb.append(inlineViewStyle.terminator("C", columnNamesAsArray));
						sb.append(" on (");
						boolean f2 = true;
						for (String pkColumnName: columnNames) {
							if (!f2) {
								sb.append(" and ");
							}
							sb.append("B." + pkColumnName + " = " + "C." + pkColumnName);
							f2 = false;
						}
						sb.append(")");
					} else {
						for (Row parentRow: parentRows) {
							if (sb.length() == 0) {
								sb.append(" Where ((");
							} else {
								sb.append(" or (");
							}
							sb.append(parentRow.rowId).append(")");
						}
						sb.append(")");
					}
					sql += sb.toString();
				}
			}

			olapPrefix += " From (";
			if (useOLAPLimitation) {
				sql = olapPrefix + sql + olapSuffix;
			}
			if (sqlLimitSuffix != null && !limitSuffixInSelectClause) {
				sql += " " + (sqlLimitSuffix.replace("%s", Integer.toString(limit)));
			}
		}
		final long[] rc = new long[1];
		if (sql.length() > 0) {
			sql = "Select count(*) From (" + sql + ") JLASRCNT";
			int timeout = (int) Math.max(1, (maxTime - System.currentTimeMillis()) / 1000);

			session.executeQuery(sql, new Session.ResultSetReader() {

				@Override
				public void readCurrentRow(ResultSet resultSet) throws SQLException {
					rc[0] = resultSet.getLong(1);
				}

				@Override
				public void close() {
				}
			}, null, context, 0, timeout, false);
		}
		return rc[0];
	}

	/**
	 * Gets qualified table name.
	 *
	 * @param t the table
	 * @return qualified name of t
	 */
	private String qualifiedTableName(Table t, Quoting quoting) {
		String schema = t.getSchema("");
		if (schema.length() == 0) {
			return quoting.requote(t.getUnqualifiedName());
		}
		return quoting.requote(schema) + "." + quoting.requote(t.getUnqualifiedName());
	}

}
