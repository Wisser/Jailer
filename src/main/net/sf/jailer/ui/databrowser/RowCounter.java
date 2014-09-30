package net.sf.jailer.ui.databrowser;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.jailer.Configuration;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.SqlUtil;

/**
 * Counts number of rows associated with a given row.
 */
public class RowCounter {

	private final Table table;
	private final Association association;
	private final Session session;
	private final List<Row> theRows;
	private final int TIMEOUT = 6;
	
	public RowCounter(Table table, Association association, List<Row> theRows, Session session) {
		this.table = table;
		this.association = association;
		this.theRows = theRows;
		this.session = session;
	}
	
	/**
	 * Counts rows from {@link #table}.
	 * 
	 * @param context
	 *            cancellation context
	 * @param limit
	 *            row number limit
	 */
	public long countRows(String andCond, Object context, int limit, boolean selectDistinct) throws SQLException {

		List<Row> pRows = theRows;
		pRows = new ArrayList<Row>(pRows);
		Map<String, Row> rowSet = new HashMap<String, Row>();
		long maxTime = System.currentTimeMillis() + 1000 * TIMEOUT;
		
		if (association != null && association.source.primaryKey.getColumns().isEmpty()) {
			try {
				loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 1, maxTime);
			} catch (SQLException e) {
				if (System.currentTimeMillis() >= maxTime) {
					return -1;
				}
				throw e;
			}
		} else {
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 258, maxTime);
			} catch (SQLException e) {
				if (System.currentTimeMillis() >= maxTime) {
					return -1;
				}
				Session._log.warn("failed, try another blocking-size");
			}
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 100, maxTime);
			} catch (SQLException e) {
				if (System.currentTimeMillis() >= maxTime) {
					return -1;
				}
				Session._log.warn("failed, try another blocking-size");
			}
			try {
				return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 40, maxTime);
			} catch (SQLException e) {
				if (System.currentTimeMillis() >= maxTime) {
					return -1;
				}
				Session._log.warn("failed, try another blocking-size");
			}
		}
		
		try {
			return loadRowBlocks(andCond, context, limit, selectDistinct, pRows, rowSet, 1, maxTime);
		} catch (SQLException e) {
			if (System.currentTimeMillis() >= maxTime) {
				return -1;
			}
			throw e;
		}
	}

	private long loadRowBlocks(String andCond, Object context, int limit, boolean selectDistinct, List<Row> pRows,
			Map<String, Row> rowSet, int NUM_PARENTS, long maxTime) throws SQLException {
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
		
		if (!pRows.isEmpty()) for (List<Row> pRowBlockI : parentBlocks) {

			if (System.currentTimeMillis() >= maxTime) {
				return -1;
			}

			List<Row> pRowBlock = pRowBlockI;
			Map<String, List<Row>> newBlockRows = new HashMap<String, List<Row>>();
			boolean loaded = false;
			
			if (pRowBlock.size() == 1 && pRowBlock.get(0) == null) {
				pRowBlock = null;
			}
			
			long brc = 0;
			if (Configuration.forDbms(session).getSqlLimitSuffix() != null) {
				try {
					session.setSilent(true);
					brc += countRows(andCond, pRowBlock, newBlockRows, context, limit, false, Configuration.forDbms(session).getSqlLimitSuffix(), selectDistinct, maxTime);
					loaded = true;
				} catch (SQLException e) {
					if (System.currentTimeMillis() >= maxTime) {
						return -1;
					}
					Session._log.warn("failed, try another limit-strategy");
				} finally {
					session.setSilent(false);
				}
			}
			if (!loaded) {
				try {
					session.setSilent(true);
					brc += countRows(andCond, pRowBlock, newBlockRows, context, limit, true, null, selectDistinct, maxTime);
					loaded = true;
				} catch (SQLException e) {
					if (System.currentTimeMillis() >= maxTime) {
						return -1;
					}
					Session._log.warn("failed, try another limit-strategy");
				} finally {
					session.setSilent(false);
				}
				if (!loaded) {
					brc += countRows(andCond, pRowBlock, newBlockRows, context, limit, false, null, selectDistinct, maxTime);
				}
			}
			rc += brc;
			limit -= brc;
			if (limit <= 0) {
				break;
			}
		}
		
		return rc;
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
	 */
	public long countRows(String andCond, final List<Row> parentRows, final Map<String, List<Row>> rows, Object context, int limit, boolean useOLAPLimitation,
			String sqlLimitSuffix, boolean selectDistinct, long maxTime) throws SQLException {
		String sql = "Select "; // + (selectDistinct? "distinct " : "");
		
		{
			String olapPrefix = "Select ";
			String olapSuffix = ") S Where S." + ROWNUMBERALIAS + " <= " + limit;
			boolean limitSuffixInSelectClause = sqlLimitSuffix != null &&
					(sqlLimitSuffix.toLowerCase().startsWith("top ") || sqlLimitSuffix.toLowerCase().startsWith("first "));
			if (sqlLimitSuffix != null && limitSuffixInSelectClause) {
				sql += (sqlLimitSuffix.replace("%s", Integer.toString(limit))) + " ";
			}
			boolean f = true;
			int i = 0;
			
			for (Column column : association.destination.getColumns()) {
				String name = column.name;
				sql += (!f ? ", " : "") + "A." + name + " AS A" + i;
				olapPrefix += (!f ? ", " : "") + "S.A" + i;
				++i;
				f = false;
			}
			f = true;
			f = true;
			String orderBy = "";
			for (Column pk : association.destination.primaryKey.getColumns()) {
				orderBy += (f ? "" : ", ") + "B." + pk.name;
				f = false;
			}
			if (useOLAPLimitation) {
				sql += ", row_number() over(";
				// sql += "order by " + orderBy;
				sql += "order by -1";
				sql += ") as " + ROWNUMBERALIAS + "";
			}
			sql += " From ";
			if (association != null) {
				sql += association.destination.getName() + " A join ";
			}
			sql += table.getName() + " B";
			if (association != null) {
				if (association.reversed) {
					sql += " on " + association.getUnrestrictedJoinCondition();
				} else {
					sql += " on " + SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition());
				}
			}
	
			boolean whereExists = false;
			if (parentRows != null && !parentRows.isEmpty()) {
				if (parentRows.size() == 1) {
					sql += " Where (" + parentRows.get(0).rowId + ")";
				} else {
					StringBuilder sb = new StringBuilder();
					for (Row parentRow: parentRows) {
						if (sb.length() == 0) {
							sb.append(" Where ((");
						} else {
							sb.append(" or (");
						}
						sb.append(parentRow.rowId).append(")");
					}
					sb.append(")");
					sql += sb.toString();
				}
				whereExists = true;
			}
//			if (andCond.trim().length() > 0) {
//				sql += (whereExists ? " and" : " Where") + " (" + ConditionEditor.toMultiLine(andCond) + ")";
//			}
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
			}, null, context, 0, timeout);
		}
		return rc[0];
	}

}
