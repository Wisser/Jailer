/*
 * Copyright 2007 - 2022 Ralf Wisser.
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.util.JSqlParserUtil;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.AnyComparisonExpression;
import net.sf.jsqlparser.expression.ArrayConstructor;
import net.sf.jsqlparser.expression.ArrayExpression;
import net.sf.jsqlparser.expression.BinaryExpression;
import net.sf.jsqlparser.expression.CaseExpression;
import net.sf.jsqlparser.expression.CastExpression;
import net.sf.jsqlparser.expression.CollateExpression;
import net.sf.jsqlparser.expression.ConnectByRootOperator;
import net.sf.jsqlparser.expression.DateTimeLiteralExpression;
import net.sf.jsqlparser.expression.DateValue;
import net.sf.jsqlparser.expression.DoubleValue;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.ExpressionVisitor;
import net.sf.jsqlparser.expression.ExtractExpression;
import net.sf.jsqlparser.expression.Function;
import net.sf.jsqlparser.expression.HexValue;
import net.sf.jsqlparser.expression.IntervalExpression;
import net.sf.jsqlparser.expression.JdbcNamedParameter;
import net.sf.jsqlparser.expression.JdbcParameter;
import net.sf.jsqlparser.expression.JsonAggregateFunction;
import net.sf.jsqlparser.expression.JsonExpression;
import net.sf.jsqlparser.expression.JsonFunction;
import net.sf.jsqlparser.expression.KeepExpression;
import net.sf.jsqlparser.expression.LongValue;
import net.sf.jsqlparser.expression.MySQLGroupConcat;
import net.sf.jsqlparser.expression.NextValExpression;
import net.sf.jsqlparser.expression.NotExpression;
import net.sf.jsqlparser.expression.NullValue;
import net.sf.jsqlparser.expression.NumericBind;
import net.sf.jsqlparser.expression.OracleHierarchicalExpression;
import net.sf.jsqlparser.expression.OracleHint;
import net.sf.jsqlparser.expression.OracleNamedFunctionParameter;
import net.sf.jsqlparser.expression.Parenthesis;
import net.sf.jsqlparser.expression.RowConstructor;
import net.sf.jsqlparser.expression.RowGetExpression;
import net.sf.jsqlparser.expression.SignedExpression;
import net.sf.jsqlparser.expression.StringValue;
import net.sf.jsqlparser.expression.TimeKeyExpression;
import net.sf.jsqlparser.expression.TimeValue;
import net.sf.jsqlparser.expression.TimestampValue;
import net.sf.jsqlparser.expression.TimezoneExpression;
import net.sf.jsqlparser.expression.UserVariable;
import net.sf.jsqlparser.expression.ValueListExpression;
import net.sf.jsqlparser.expression.VariableAssignment;
import net.sf.jsqlparser.expression.WhenClause;
import net.sf.jsqlparser.expression.XMLSerializeExpr;
import net.sf.jsqlparser.expression.operators.arithmetic.Addition;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseAnd;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseLeftShift;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseOr;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseRightShift;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseXor;
import net.sf.jsqlparser.expression.operators.arithmetic.Concat;
import net.sf.jsqlparser.expression.operators.arithmetic.Division;
import net.sf.jsqlparser.expression.operators.arithmetic.IntegerDivision;
import net.sf.jsqlparser.expression.operators.arithmetic.Modulo;
import net.sf.jsqlparser.expression.operators.arithmetic.Multiplication;
import net.sf.jsqlparser.expression.operators.arithmetic.Subtraction;
import net.sf.jsqlparser.expression.operators.conditional.AndExpression;
import net.sf.jsqlparser.expression.operators.conditional.OrExpression;
import net.sf.jsqlparser.expression.operators.conditional.XorExpression;
import net.sf.jsqlparser.expression.operators.relational.Between;
import net.sf.jsqlparser.expression.operators.relational.EqualsTo;
import net.sf.jsqlparser.expression.operators.relational.ExistsExpression;
import net.sf.jsqlparser.expression.operators.relational.ExpressionList;
import net.sf.jsqlparser.expression.operators.relational.FullTextSearch;
import net.sf.jsqlparser.expression.operators.relational.GreaterThan;
import net.sf.jsqlparser.expression.operators.relational.GreaterThanEquals;
import net.sf.jsqlparser.expression.operators.relational.InExpression;
import net.sf.jsqlparser.expression.operators.relational.IsBooleanExpression;
import net.sf.jsqlparser.expression.operators.relational.IsNullExpression;
import net.sf.jsqlparser.expression.operators.relational.JsonOperator;
import net.sf.jsqlparser.expression.operators.relational.LikeExpression;
import net.sf.jsqlparser.expression.operators.relational.Matches;
import net.sf.jsqlparser.expression.operators.relational.MinorThan;
import net.sf.jsqlparser.expression.operators.relational.MinorThanEquals;
import net.sf.jsqlparser.expression.operators.relational.NotEqualsTo;
import net.sf.jsqlparser.expression.operators.relational.RegExpMatchOperator;
import net.sf.jsqlparser.expression.operators.relational.RegExpMySQLOperator;
import net.sf.jsqlparser.expression.operators.relational.SimilarToExpression;
import net.sf.jsqlparser.parser.ASTNodeAccess;
import net.sf.jsqlparser.parser.SimpleNode;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.StatementVisitorAdapter;
import net.sf.jsqlparser.statement.select.AllColumns;
import net.sf.jsqlparser.statement.select.AllTableColumns;
import net.sf.jsqlparser.statement.select.FromItemVisitor;
import net.sf.jsqlparser.statement.select.Join;
import net.sf.jsqlparser.statement.select.LateralSubSelect;
import net.sf.jsqlparser.statement.select.ParenthesisFromItem;
import net.sf.jsqlparser.statement.select.PlainSelect;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.select.SelectExpressionItem;
import net.sf.jsqlparser.statement.select.SelectItem;
import net.sf.jsqlparser.statement.select.SelectItemVisitor;
import net.sf.jsqlparser.statement.select.SelectVisitor;
import net.sf.jsqlparser.statement.select.SelectVisitorAdapter;
import net.sf.jsqlparser.statement.select.SetOperationList;
import net.sf.jsqlparser.statement.select.SubJoin;
import net.sf.jsqlparser.statement.select.SubSelect;
import net.sf.jsqlparser.statement.select.TableFunction;
import net.sf.jsqlparser.statement.select.ValuesList;
import net.sf.jsqlparser.statement.select.WithItem;
import net.sf.jsqlparser.statement.values.ValuesStatement;

/**
 * Parses a SQL query and tries to find out the type.
 *
 * @author Ralf Wisser
 */
public class QueryTypeAnalyser {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);

	@SuppressWarnings("serial")
	private static class QueryTooComplexException extends RuntimeException {
	}

	/**
	 * Parses a SQL query and tries to find out the type.
	 *
	 * @param sqlSelect the query
	 * @return the type or <code>null</code>
	 */
	public static List<Table> getType(String sqlSelect, boolean checkPKs, Map<Integer, String> columnExpression, final MetaDataSource metaDataSource) {
		net.sf.jsqlparser.statement.Statement st;
		String simplifiedSQL;
		try {
			try {
				simplifiedSQL = SqlUtil.removeNonMeaningfulFragments(sqlSelect);
				st = JSqlParserUtil.parse(simplifiedSQL, 2);
			} catch (Exception e) {
				return null;
			}
			Map<Pair<String, String>, Collection<Pair<String, String>>> equivs = new HashMap<Pair<String,String>, Collection<Pair<String,String>>>();
			final LinkedHashMap<String, MDTable> fromClause = analyseFromClause(st, equivs, metaDataSource);
			final List<Pair<String, String>> selectClause = new ArrayList<Pair<String, String>>();
			st.accept(new StatementVisitorAdapter() {
				@Override
				public void visit(Select select) {
					select.getSelectBody().accept(new SelectVisitorAdapter() {

						@Override
						public void visit(WithItem withItem) {
							throw new QueryTooComplexException();
						}

						@Override
						public void visit(SetOperationList setOpList) {
							throw new QueryTooComplexException();
						}

						@Override
						public void visit(PlainSelect plainSelect) {
							for (SelectItem si: plainSelect.getSelectItems()) {
								final boolean stop[] = new boolean[] { false };

								si.accept(new SelectItemVisitor() {
									@Override
									public void visit(SelectExpressionItem selectExpressionItem) {
										final boolean noSubexpression[] = new boolean[] { true };
										final Column column[] = new Column[1];

										selectExpressionItem.getExpression().accept(createExpressionVisitor(noSubexpression, column, fromClause));

										if (!noSubexpression[0] && checkPKs) {
											column[0] = null;
										}
										if (column[0] != null) {
											String alias = null;
											if (column[0].getTable() != null) {
												if (column[0].getTable().getAlias() != null) {
													alias = column[0].getTable().getAlias().getName();
												} else {
													alias = column[0].getTable().getName();
												}
											}
											if (noSubexpression[0] || !checkPKs) {
												Pair<String, String> col;
												try {
													col = findColumn(alias, column[0].getColumnName(), fromClause);
												} catch (SQLException e) {
													logger.info("error", e);
													throw new QueryTooComplexException();
												}
												selectClause.add(col);
											} else {
												if (columnExpression != null) {
													columnExpression.put(selectClause.size(), asSQL(selectExpressionItem));
												}
												selectClause.add(null);
											}
										} else {
											if (columnExpression != null) {
												columnExpression.put(selectClause.size(), asSQL(selectExpressionItem));
											}
											selectClause.add(null);
										}
									}

									private String asSQL(SelectExpressionItem selectExpressionItem) {
										String fragment = selectExpressionItem.toString();
										try {
											Pair<Integer, Integer> pos = null;
												
											Pattern pattern = Pattern.compile(SqlUtil.createSQLFragmentSearchPattern(fragment, false), Pattern.CASE_INSENSITIVE);
											Matcher matcher = pattern.matcher(simplifiedSQL);
												
											if (matcher.find()) {
												pos = new Pair<Integer, Integer>(matcher.start(), matcher.end());
												if (matcher.find()) { // not unique
													pos = null;
												}
											}
											if (pos != null) {
												fragment = sqlSelect.substring(pos.a, pos.b);
											} else {
												Expression o = selectExpressionItem.getExpression();
												if (o != null && !(o instanceof LikeExpression) && (selectExpressionItem instanceof ASTNodeAccess && ((ASTNodeAccess) selectExpressionItem).getASTNode() != null)) {
													SimpleNode node = ((ASTNodeAccess) selectExpressionItem).getASTNode();
													fragment = sqlSelect.substring(node.jjtGetFirstToken().absoluteBegin - 1, node.jjtGetLastToken().absoluteEnd - 1);
												}
											}
										} catch (Exception e) {
											LogUtil.warn(e);
										}
										return fragment.replaceAll("\\s+", " ").replaceFirst("^(.{128}).+$", "$1...");
									}

									@Override
									public void visit(AllTableColumns allTableColumns) {
										String tableName = allTableColumns.getTable().getName();
										String tableAlias = null;
										if (tableName != null) {
											tableAlias = findTable(tableName, fromClause);
										}
										if (tableAlias == null) {
											throw new QueryTooComplexException();
										}
										MDTable mdTable = fromClause.get(tableAlias);
										try {
											for (String col: mdTable.getColumns(false)) {
												selectClause.add(new Pair<String, String>(tableAlias, col));
											}
										} catch (SQLException e) {
											logger.info("error", e);
											throw new QueryTooComplexException();
										}
									}

									@Override
									public void visit(AllColumns allColumns) {
										for (Entry<String, MDTable> e: fromClause.entrySet()) {
											MDTable mdTable = e.getValue();
											if (mdTable == null) {
												stop[0] = true;
												break;
											}
											try {
												for (String col: mdTable.getColumns(false)) {
													selectClause.add(new Pair<String, String>(e.getKey(), col));
												}
											} catch (SQLException e2) {
												LogUtil.warn(e2);
												throw new QueryTooComplexException();
											}
										}
									}
								});
								if (stop[0]) {
									break;
								}
							}
						}

						@Override
						public void visit(ValuesStatement aThis) {

						}
					});
				}
			});

			List<Table> result = new ArrayList<Table>();
			for (String tableAlias: fromClause.keySet()) {
				MDTable theTable = fromClause.get(tableAlias);
				if (theTable != null) {
					List<String> columnNames = new ArrayList<String>();
					for (Pair<String, String> e: selectClause) {
						if (e != null && e.a.equals(tableAlias)) {
							columnNames.add(e.b);
						} else {
							columnNames.add(null);
						}
					}
					Table origTable = metaDataSource.toTable(theTable);
					Table table = createTable(theTable, origTable, checkPKs, tableAlias, columnNames, selectClause, equivs, metaDataSource);
					if (table != null) {
						result.add(table);
					}
				}
			}
			return result;
		} catch (QueryTooComplexException e) {
			// ignore
		} catch (Throwable e) {
			LogUtil.warn(e);
		}
		return null;
	}

	private static LinkedHashMap<String, MDTable> analyseFromClause(Statement st, final Map<Pair<String, String>, Collection<Pair<String, String>>> equivs, final MetaDataSource metaDataSource) {
		final LinkedHashMap<String, MDTable> result = new LinkedHashMap<String, MDTable>();

		st.accept(new StatementVisitorAdapter() {
			private int unknownTableCounter = 0;
			@Override
			public void visit(Select select) {
				select.getSelectBody().accept(new SelectVisitor() {

					@Override
					public void visit(WithItem withItem) {
					}

					@Override
					public void visit(SetOperationList setOpList) {
					}

					@Override
					public void visit(PlainSelect plainSelect) {
						final ExpressionAnalyzer expressionAnalyzer = new ExpressionAnalyzer(result);
						if (plainSelect.getFromItem() != null) {
							final FromItemVisitor fromItemVisitor = new FromItemVisitor() {
								private void unknownTable() {
									result.put("-unknown-" + (unknownTableCounter++), null);
								}
								@Override
								public void visit(TableFunction tableFunction) {
									unknownTable();
								}

								@Override
								public void visit(ValuesList valuesList) {
									unknownTable();
								}

								@Override
								public void visit(LateralSubSelect lateralSubSelect) {
									unknownTable();
								}

								@Override
								public void visit(SubJoin subjoin) {
									subjoin.getLeft().accept(this);
									if (subjoin.getJoinList() != null) {
										for (Join join: subjoin.getJoinList()) {
											join.getRightItem().accept(this);
										}
										for (Join join: subjoin.getJoinList()) {
											expressionAnalyzer.setOuterJoinExpression(join.isOuter() || join.isLeft() || join.isRight());
											if (join.getOnExpressions() != null) {
												join.getOnExpressions().forEach(e -> e.accept(expressionAnalyzer));
											}
										}
									}
								}

								@Override
								public void visit(SubSelect subSelect) {
									unknownTable();
								}

								@Override
								public void visit(net.sf.jsqlparser.schema.Table tableName) {
									String schema = tableName.getSchemaName();
									String name = tableName.getName();
									if (tableName.getPivot() != null) {
										unknownTable();
									} else {
										MDSchema mdSchema = null;
										if (metaDataSource.isInitialized()) {
											if (schema == null) {
												mdSchema = metaDataSource.getDefaultSchema();
											} else {
												mdSchema = metaDataSource.find(schema);
											}
										}
										if (mdSchema != null) {
											MDTable mdTable;
											if (!mdSchema.isLoaded()) {
												mdSchema.loadTables(true, null, null, null);
												mdTable = null;
									    	} else {
									    		mdTable = mdSchema.find(name);
									    	}
											if (mdTable != null) {
												result.put(tableName.getAlias() != null? tableName.getAlias().getName() : mdTable.getName(), mdTable);
											} else {
												unknownTable();
											}
										}
									}
								}
								@Override
								public void visit(ParenthesisFromItem parenthesisFromItem) {
									if (parenthesisFromItem.getFromItem() != null) {
										parenthesisFromItem.getFromItem().accept(this);
									}
								}
							};
							plainSelect.getFromItem().accept(fromItemVisitor);
							if (plainSelect.getJoins() != null) {
								for (Join join: plainSelect.getJoins()) {
									join.getRightItem().accept(fromItemVisitor);
								}
								for (Join join: plainSelect.getJoins()) {
									expressionAnalyzer.setOuterJoinExpression(join.isOuter() || join.isLeft() || join.isRight());
									if (join.getOnExpressions() != null) {
										join.getOnExpressions().forEach(e -> e.accept(expressionAnalyzer));
									}
								}
							}
						}
						if (plainSelect.getWhere() != null) {
							expressionAnalyzer.setOuterJoinExpression(false);
							plainSelect.getWhere().accept(expressionAnalyzer);
						}
						equivs.putAll(expressionAnalyzer.getEquivs());
					}

					@Override
					public void visit(ValuesStatement aThis) {

					}
				});
			}
		});

		return result;
	}

	private static Table createTable(MDTable theTable, Table origTable, boolean checkPKs, String tableAlias, List<String> columnNames, List<Pair<String, String>> selectClause, Map<Pair<String, String>, Collection<Pair<String, String>>> equivs, MetaDataSource metaDataSource) throws SQLException {
		if (checkPKs) {
			for (String pk: theTable.getPrimaryKeyColumns()) {
				if (!columnNames.contains(pk)) {
					boolean ok = false;
					Collection<Pair<String, String>> eq = equivs.get(new Pair<String, String>(tableAlias, pk));
					if (eq != null) {
						int i = 0;
						for (Pair<String, String> e: selectClause) {
							if (columnNames.get(i) == null) {
								if (eq.contains(e)) {
									columnNames.set(i, e.b);
									ok = true;
									break;
								}
							}
							++i;
						}
					}
					if (!ok) {
						return null;
					}
				}
			}
		}
		String schemaName = theTable.getSchema().isDefaultSchema? null : theTable.getSchema().getName();
		String tableName = theTable.getName();
		List<net.sf.jailer.datamodel.Column> pkColumns = new ArrayList<net.sf.jailer.datamodel.Column>();
		for (String pk: theTable.getPrimaryKeyColumns()) {
			pkColumns.add(new net.sf.jailer.datamodel.Column(pk, "", 0, -1));
		}
		PrimaryKey primaryKey = new PrimaryKeyFactory(null).createPrimaryKey(pkColumns, null);
		Table toTable = theTable.getMetaDataSource().toTable(theTable);
		Table table;
		if (toTable != null) {
			table = new Table(toTable.getName(), primaryKey, false, false);
			table.associations.addAll(toTable.associations);
		} else {
			table = new Table((schemaName == null? "" : schemaName + ".") + tableName, primaryKey, false, false);
		}
		List<net.sf.jailer.datamodel.Column> columns = new ArrayList<net.sf.jailer.datamodel.Column>();
		for (String pk: columnNames) {
			net.sf.jailer.datamodel.Column col = new net.sf.jailer.datamodel.Column(pk, "", 0, -1);
			columns.add(col);
			if (origTable != null) {
				origTable.getColumns()
					.stream()
					.filter(column -> Quoting.equalsIgnoreQuotingAndCase(col.name, column.name))
					.findFirst().ifPresent(column -> {
						col.isVirtual = column.isVirtual;
						col.isNullable = column.isNullable;
						col.isIdentityColumn = column.isIdentityColumn;
					});
			}
		}
		table.setColumns(columns);
		table.setIsArtifical(true);
		return table;
	}

	private static Pair<String, String> findColumn(String alias, String columnName, LinkedHashMap<String, MDTable> fromClause) throws SQLException {
		for (boolean strict: new boolean[] { false, true }) {
			for (Entry<String, MDTable> e: fromClause.entrySet()) {
				if (alias == null || idEquals(e.getKey(), alias, strict)) {
					if (e.getValue() != null) {
						for (String column: e.getValue().getColumns(false)) {
							if (idEquals(column, columnName, strict)) {
								return new Pair<String, String>(e.getKey(), column);
							}
						}
					}
				}
			}
		}
		return null;
	}

	private static String findTable(String tableName, LinkedHashMap<String, MDTable> fromClause) {
		for (boolean strict: new boolean[] { false, true }) {
			for (Entry<String, MDTable> e: fromClause.entrySet()) {
				if (idEquals(e.getKey(), tableName, strict)) {
					return e.getKey();
				}
			}
		}
		return null;
	}

	static boolean idEquals(String a, String b, boolean strict) {
		if (strict) {
			return a.equals(b);
		}
		return Quoting.normalizeIdentifier(a).equals(Quoting.normalizeIdentifier(b));
	}

	private static ExpressionVisitor createExpressionVisitor(final boolean[] noSubexpression, final Column[] column, LinkedHashMap<String, MDTable> fromClause) {
		return new ExpressionVisitor() {
			Set<String> cols = new HashSet<String>();
			
			@Override
			public void visit(Column tableColumn) {
				String alias = null;
				if (tableColumn.getTable() != null) {
					if (tableColumn.getTable().getAlias() != null) {
						alias = tableColumn.getTable().getAlias().getName();
					} else {
						alias = tableColumn.getTable().getName();
					}
				}
				Pair<String, String> col;
				try {
					col = findColumn(alias, tableColumn.getColumnName(), fromClause);
				} catch (SQLException e) {
					logger.info("error", e);
					throw new QueryTooComplexException();
				}
				if (col != null) {
					cols.add(col.a);
					column[0] = cols.size() == 1? tableColumn : null;
				}
			}

			private void visitSubExpression(Expression expression) {
				if (expression != null) {
					expression.accept(this);
				}
			}
			private void visitSubExpression(ExpressionList expressionList) {
				if (expressionList != null && expressionList.getExpressions() != null) {
					expressionList.getExpressions().forEach(e -> e.accept(this));
				}
			}

			private void visitBinaryExpression(BinaryExpression binaryExpression) {
				visitSubExpression(binaryExpression.getLeftExpression());
				visitSubExpression(binaryExpression.getRightExpression());
			}

			private void rejectColumn() {
				cols.add("?");
				column[0] = null;
			}
			
			@Override
			public void visit(NotExpression aThis) {
				noSubexpression[0] = false;
				visitSubExpression(aThis.getExpression());
			}

			@Override
			public void visit(DateTimeLiteralExpression literal) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(TimeKeyExpression timeKeyExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(OracleHint hint) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(RowConstructor rowConstructor) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(MySQLGroupConcat groupConcat) {
				noSubexpression[0] = false;
				visitSubExpression(groupConcat.getExpressionList());
			}

			@Override
			public void visit(KeepExpression aexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(NumericBind bind) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(UserVariable var) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(RegExpMySQLOperator regExpMySQLOperator) {
				noSubexpression[0] = false;
				visitBinaryExpression(regExpMySQLOperator);
			}

			@Override
			public void visit(JsonOperator jsonExpr) {
				noSubexpression[0] = false;
				visitBinaryExpression(jsonExpr);
			}

			@Override
			public void visit(JsonExpression jsonExpr) {
				noSubexpression[0] = false;
				visitSubExpression(jsonExpr.getExpression());
			}

			@Override
			public void visit(RegExpMatchOperator rexpr) {
				noSubexpression[0] = false;
				visitSubExpression(rexpr.getLeftExpression());
				visitSubExpression(rexpr.getRightExpression());
			}

			@Override
			public void visit(OracleHierarchicalExpression oexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(IntervalExpression iexpr) {
				noSubexpression[0] = false;
				visitSubExpression(iexpr.getExpression());
			}

			@Override
			public void visit(ExtractExpression eexpr) {
				noSubexpression[0] = false;
				visitSubExpression(eexpr.getExpression());
			}

			@Override
			public void visit(AnalyticExpression aexpr) {
				noSubexpression[0] = false;
				visitSubExpression(aexpr.getPartitionExpressionList());
				visitSubExpression(aexpr.getExpression());
			}

			@Override
			public void visit(Modulo modulo) {
				noSubexpression[0] = false;
				visitBinaryExpression(modulo);
			}

			@Override
			public void visit(CastExpression cast) {
				noSubexpression[0] = false;
				visitSubExpression(cast.getLeftExpression());
			}

			@Override
			public void visit(BitwiseXor bitwiseXor) {
				noSubexpression[0] = false;
				visitBinaryExpression(bitwiseXor);
			}

			@Override
			public void visit(BitwiseOr bitwiseOr) {
				noSubexpression[0] = false;
				visitBinaryExpression(bitwiseOr);
			}

			@Override
			public void visit(BitwiseAnd bitwiseAnd) {
				noSubexpression[0] = false;
				visitBinaryExpression(bitwiseAnd);
			}

			@Override
			public void visit(Matches matches) {
				noSubexpression[0] = false;
				visitBinaryExpression(matches);
			}

			@Override
			public void visit(Concat concat) {
				noSubexpression[0] = false;
				visitBinaryExpression(concat);
			}

			@Override
			public void visit(AnyComparisonExpression anyComparisonExpression) {
				noSubexpression[0] = false;
				rejectColumn();
			}

			@Override
			public void visit(ExistsExpression existsExpression) {
				noSubexpression[0] = false;
				visitSubExpression(existsExpression.getRightExpression());
			}

			@Override
			public void visit(WhenClause whenClause) {
				noSubexpression[0] = false;
				visitSubExpression(whenClause.getWhenExpression());
				visitSubExpression(whenClause.getThenExpression());
			}

			@Override
			public void visit(CaseExpression caseExpression) {
				noSubexpression[0] = false;
				visitSubExpression(caseExpression.getSwitchExpression());
				visitSubExpression(caseExpression.getElseExpression());
				if (caseExpression.getWhenClauses() != null) {
					caseExpression.getWhenClauses().forEach(wc -> wc.accept(this));
				}
			}

			@Override
			public void visit(SubSelect subSelect) {
				noSubexpression[0] = false;
				rejectColumn();
			}

			@Override
			public void visit(NotEqualsTo notEqualsTo) {
				noSubexpression[0] = false;
				visitBinaryExpression(notEqualsTo);
			}

			@Override
			public void visit(MinorThanEquals minorThanEquals) {
				noSubexpression[0] = false;
				visitBinaryExpression(minorThanEquals);
			}

			@Override
			public void visit(MinorThan minorThan) {
				noSubexpression[0] = false;
				visitBinaryExpression(minorThan);
			}

			@Override
			public void visit(LikeExpression likeExpression) {
				noSubexpression[0] = false;
				visitBinaryExpression(likeExpression);
			}

			@Override
			public void visit(IsNullExpression isNullExpression) {
				noSubexpression[0] = false;
				visitSubExpression(isNullExpression.getLeftExpression());
			}

			@Override
			public void visit(InExpression inExpression) {
				noSubexpression[0] = false;
				visitSubExpression(inExpression.getLeftExpression());
				visitSubExpression(inExpression.getRightExpression());
			}

			@Override
			public void visit(GreaterThanEquals greaterThanEquals) {
				noSubexpression[0] = false;
				visitBinaryExpression(greaterThanEquals);
			}

			@Override
			public void visit(GreaterThan greaterThan) {
				noSubexpression[0] = false;
				visitBinaryExpression(greaterThan);
			}

			@Override
			public void visit(EqualsTo equalsTo) {
				noSubexpression[0] = false;
				visitBinaryExpression(equalsTo);
			}

			@Override
			public void visit(Between between) {
				noSubexpression[0] = false;
				visitSubExpression(between.getLeftExpression());
				visitSubExpression(between.getBetweenExpressionStart());
				visitSubExpression(between.getBetweenExpressionEnd());
			}

			@Override
			public void visit(OrExpression orExpression) {
				noSubexpression[0] = false;
				visitBinaryExpression(orExpression);
			}

			@Override
			public void visit(AndExpression andExpression) {
				noSubexpression[0] = false;
				visitBinaryExpression(andExpression);
			}

			@Override
			public void visit(Subtraction subtraction) {
				noSubexpression[0] = false;
				visitBinaryExpression(subtraction);
			}

			@Override
			public void visit(Multiplication multiplication) {
				noSubexpression[0] = false;
				visitBinaryExpression(multiplication);
			}

			@Override
			public void visit(Division division) {
				noSubexpression[0] = false;
				visitBinaryExpression(division);
			}

			@Override
			public void visit(Addition addition) {
				noSubexpression[0] = false;
				visitBinaryExpression(addition);
			}

			@Override
			public void visit(StringValue stringValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Parenthesis parenthesis) {
				noSubexpression[0] = false;
				visitSubExpression(parenthesis.getExpression());
			}

			@Override
			public void visit(TimestampValue timestampValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(TimeValue timeValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(DateValue dateValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(HexValue hexValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(LongValue longValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(DoubleValue doubleValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(JdbcNamedParameter jdbcNamedParameter) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(JdbcParameter jdbcParameter) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(SignedExpression signedExpression) {
				noSubexpression[0] = false;
				visitSubExpression(signedExpression.getExpression());
			}

			@Override
			public void visit(Function function) {
				noSubexpression[0] = false;
				visitSubExpression(function.getAttribute());
				if (function.getParameters() != null && function.getParameters().getExpressions() != null) {
					function.getParameters().getExpressions().forEach(e -> e.accept(this));
				}
				if (function.getNamedParameters() != null && function.getNamedParameters().getExpressions() != null) {
					function.getNamedParameters().getExpressions().forEach(e -> e.accept(this));
				}
			}

			@Override
			public void visit(NullValue nullValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(BitwiseRightShift aThis) {
				noSubexpression[0] = false;
				visitBinaryExpression(aThis);
			}

			@Override
			public void visit(BitwiseLeftShift aThis) {
				noSubexpression[0] = false;
				visitBinaryExpression(aThis);
			}

			@Override
			public void visit(ValueListExpression valueList) {
				noSubexpression[0] = false;
				visitSubExpression(valueList);
			}

			@Override
			public void visit(IntegerDivision division) {
				noSubexpression[0] = false;
				visitBinaryExpression(division);
			}

			@Override
			public void visit(FullTextSearch fullTextSearch) {
				noSubexpression[0] = false;
				visitSubExpression(fullTextSearch.getAgainstValue());
				if (fullTextSearch.getMatchColumns() != null) {
					fullTextSearch.getMatchColumns().forEach(e -> visit(e));
				}
			}

			@Override
			public void visit(IsBooleanExpression isBooleanExpression) {
				noSubexpression[0] = false;
				visitSubExpression(isBooleanExpression.getLeftExpression());
			}

			@Override
			public void visit(NextValExpression aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(CollateExpression aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(SimilarToExpression aThis) {
				noSubexpression[0] = false;
				visitBinaryExpression(aThis);
			}

			@Override
			public void visit(ArrayExpression aThis) {
				noSubexpression[0] = false;
				visitSubExpression(aThis.getIndexExpression());
				visitSubExpression(aThis.getObjExpression());
				visitSubExpression(aThis.getStartIndexExpression());
				visitSubExpression(aThis.getStopIndexExpression());
			}
			public void visit(XorExpression arg0) {
				noSubexpression[0] = false;
				visitBinaryExpression(arg0);
			}
			public void visit(RowGetExpression arg0) {
				noSubexpression[0] = false;
				visitSubExpression(arg0.getExpression());
			}
			public void visit(ArrayConstructor arg0) {
				noSubexpression[0] = false;
				if (arg0.getExpressions() != null) {
					arg0.getExpressions().forEach(e -> e.accept(this));
				}
			}
			public void visit(VariableAssignment arg0) {
				noSubexpression[0] = false;
			}
			public void visit(XMLSerializeExpr arg0) {
				noSubexpression[0] = false;
				visitSubExpression(arg0.getExpression());
			}
			public void visit(TimezoneExpression arg0) {
				noSubexpression[0] = false;
			}
			public void visit(JsonAggregateFunction arg0) {
				noSubexpression[0] = false;
				visitSubExpression(arg0.getExpression());
				visitSubExpression(arg0.getFilterExpression());
			}
			public void visit(JsonFunction arg0) {
				noSubexpression[0] = false;
				if (arg0.getExpressions() != null) {
					arg0.getExpressions().forEach(e -> visitSubExpression(e.getExpression()));
				}
			}
			public void visit(ConnectByRootOperator arg0) {
				noSubexpression[0] = false;
				if (arg0.getColumn() != null) {
					visit(arg0.getColumn());
				}
			}
			public void visit(OracleNamedFunctionParameter arg0) {
				noSubexpression[0] = false;
				visitSubExpression(arg0.getExpression());
			}
		};
	}

}
