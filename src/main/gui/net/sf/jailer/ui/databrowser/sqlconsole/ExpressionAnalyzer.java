/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.sql.SQLException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.util.Pair;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.Parenthesis;
import net.sf.jsqlparser.expression.operators.conditional.AndExpression;
import net.sf.jsqlparser.expression.operators.relational.EqualsTo;
import net.sf.jsqlparser.schema.Column;

/**
 * Analyzes a SQL expression to find column equivalences.
 * 
 * @author Ralf Wisser
 */
class ExpressionAnalyzer extends ExpressionVisitorAdapter {

	private final LinkedHashMap<String, MDTable> fromClause;
	private boolean isOuterJoinExpression;
	private Set<Equiv> equivs = new HashSet<Equiv>();

	public ExpressionAnalyzer(LinkedHashMap<String, MDTable> fromClause) {
		this.fromClause = fromClause;
	}
	
	public static class Equiv {
		public final Pair<String, String> leftColumn;
		public final Pair<String, String> rightColumn;
		public final int numberOfOuterJoins;
		
		Equiv(Pair<String, String> leftColumn, Pair<String, String> rightColumn, int numberOfOuterJoins) {
			this.leftColumn = leftColumn;
			this.rightColumn = rightColumn;
			this.numberOfOuterJoins = numberOfOuterJoins;
		}
		
		@Override
		public int hashCode() {
			return leftColumn.hashCode() + 7 * rightColumn.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == null || !obj.getClass().equals(getClass())) {
				return false;
			}
			return ((Equiv) obj).leftColumn.equals(leftColumn) && ((Equiv) obj).rightColumn.equals(rightColumn);
		}

		@Override
		public String toString() {
			return leftColumn + " = " + rightColumn + " (" + numberOfOuterJoins + ")";
		}
		
		public Equiv join(Equiv other) {
			if (other.leftColumn.equals(rightColumn)) {
				return new Equiv(leftColumn, other.rightColumn, numberOfOuterJoins + other.numberOfOuterJoins);
			} else if (other.rightColumn.equals(leftColumn)) {
				return new Equiv(other.leftColumn, rightColumn, numberOfOuterJoins + other.numberOfOuterJoins);
			} else {
				return null;
			}
		}
	}
	
	public void setOuterJoinExpression(boolean isOuterJoinExpression) {
		this.isOuterJoinExpression = isOuterJoinExpression;
	}

	/**
	 * Gets transitive closure of column equivalences.
	 * 
	 * @return transitive closure of column equivalences
	 */
	public Map<Pair<String, String>, Collection<Pair<String, String>>> getEquivs() {
		int size = equivs.size();
		for (int i = 2 * size; i > 0; i /= 2) {
			Map<Equiv, Equiv> joined = new HashMap<Equiv, Equiv>();
			for (Equiv e: equivs) {
				joined.put(e, e);
			}
			boolean change = false;
			for (Equiv e1: equivs) {
				for (Equiv e2: equivs) {
					if (e1 != e2) {
						Equiv j = e1.join(e2);
						if (j != null && !j.leftColumn.equals(j.rightColumn)) {
							Equiv jPrev = joined.get(j);
							if (jPrev == null || jPrev.numberOfOuterJoins > j.numberOfOuterJoins) {
								joined.put(j, j);
								change = true;
							}
						}
					}
				}
			}
			if (!change) {
				break;
			}
			equivs.addAll(joined.keySet());
		}
		
		Map<Pair<String, String>, Collection<Pair<String, String>>>  result = new HashMap<Pair<String, String>, Collection<Pair<String, String>>>();
		for (Equiv e: equivs) {
			Collection<Pair<String, String>> left = result.get(e.leftColumn);
			if (left == null) {
				left = new HashSet<Pair<String, String>>();
				result.put(e.leftColumn, left);
			}
			left.add(e.rightColumn);
		}
		return result;
	}

	@Override
	public void visit(EqualsTo expr) {
		ColumnAnalyzer leftColumnAnalyzer = new ColumnAnalyzer();
		ColumnAnalyzer rightColumnAnalyzer = new ColumnAnalyzer();
		expr.getLeftExpression().accept(leftColumnAnalyzer);
		expr.getRightExpression().accept(rightColumnAnalyzer);
		
		if (leftColumnAnalyzer.columnInFrom != null && rightColumnAnalyzer.columnInFrom != null) {
			equivs.add(new Equiv(leftColumnAnalyzer.columnInFrom, rightColumnAnalyzer.columnInFrom, isOuterJoinExpression? 1 : 0));
			equivs.add(new Equiv(rightColumnAnalyzer.columnInFrom, leftColumnAnalyzer.columnInFrom, isOuterJoinExpression? 1 : 0));
		}
	}

	@Override
    public void visit(Parenthesis parenthesis) {
        parenthesis.getExpression().accept(this);
	}

	@Override
    public void visit(AndExpression and) {
        and.getLeftExpression().accept(this);
        and.getRightExpression().accept(this);
	}

	private class ColumnAnalyzer extends ExpressionVisitorAdapter {
		Pair<String, String> columnInFrom;
		
		@Override
		public void visit(Column column) {
			String columnName = column.getColumnName();
			String tableName = null;
			if (column.getTable() != null) {
				tableName = column.getTable().getName();
			}
			try {
				columnInFrom = findColumn(tableName, columnName);
			} catch (SQLException e) {
				// ignore
			}
		}
		
		private Pair<String, String> findColumn(String tableName, String columnName) throws SQLException {
			for (boolean strict: new boolean[] { false, true }) {
				for (Entry<String, MDTable> e: fromClause.entrySet()) {
					if (tableName == null || QueryTypeAnalyser.idEquals(e.getKey(), tableName, strict)) {
						if (e.getValue() != null) {
							for (String col: e.getValue().getColumns(false)) {
								if (QueryTypeAnalyser.idEquals(col, columnName, strict)) {
									return new Pair<String, String>(e.getKey(), col);
								}
							}
						}
					}
				}
			}
			return null;
		}
	}
	
}
