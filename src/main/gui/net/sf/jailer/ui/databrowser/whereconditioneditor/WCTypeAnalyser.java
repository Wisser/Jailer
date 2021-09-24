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
package net.sf.jailer.ui.databrowser.whereconditioneditor;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.log4j.Logger;

import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.util.JSqlParserUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.AnyComparisonExpression;
import net.sf.jsqlparser.expression.ArrayConstructor;
import net.sf.jsqlparser.expression.ArrayExpression;
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
import net.sf.jsqlparser.statement.select.OrderByElement;
import net.sf.jsqlparser.statement.select.ParenthesisFromItem;
import net.sf.jsqlparser.statement.select.PlainSelect;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.select.SelectExpressionItem;
import net.sf.jsqlparser.statement.select.SelectItem;
import net.sf.jsqlparser.statement.select.SelectItemVisitor;
import net.sf.jsqlparser.statement.select.SelectVisitor;
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
public class WCTypeAnalyser {

	/**
	 * The logger.
	 */
	private static final Logger logger = Logger.getLogger(MetaDataDetailsPanel.class);

	public static class Result {
		public Table table;
		public String cte;
		public boolean hasCondition;
		public boolean isHaving; // having/where
		public int conditionStart;
		public int conditionEnd;
		public String originalQuery;

		@Override
		public String toString() {
			return "Result [table=" + table + ", Columns=" + table.getColumns() + ", hasCondition=" + hasCondition + ", isHaving=" + isHaving
					+ ", conditionStart=" + conditionStart + ", conditionEnd=" + conditionEnd + ", Cond=" + table.getName().substring(conditionStart, conditionEnd) + ", Length=" + table.getName().length()
					+ ", Cte=" + cte + "]";
		}
	}
	
	/**
	 * Parses a SQL query and tries to find out the type.
	 *
	 * @param sqlSelect the query
	 * @return the type or <code>null</code>
	 */
	public static Result getType(String sqlSelect, final MetaDataSource metaDataSource) {
		Result result = new Result();
		result.originalQuery = sqlSelect;
		net.sf.jsqlparser.statement.Statement st;
		try {
			StringBuilder woComments = new StringBuilder(SqlUtil.removeComments(sqlSelect));
			String woCommentsAndLiterals = SqlUtil.removeCommentsAndLiterals(sqlSelect);
			String topLevelSql = createTopLevelSQL(sqlSelect);
			StringBuilder cte = new StringBuilder();
			
			st = JSqlParserUtil.parse(woComments.toString(), 2);

			int[] unknownTableCounter = new int[1];
			final LinkedHashMap<String, MDTable> fromClause = analyseFromClause(st, unknownTableCounter, metaDataSource);
			final List<net.sf.jailer.datamodel.Column> selectClause = new ArrayList<net.sf.jailer.datamodel.Column>();
			
			st.accept(new StatementVisitorAdapter() {
				
				private void clearInStatement(Object o) {
					if (o != null) {
						Pair<Integer, Integer> pos = null;
						
						if ((o instanceof ASTNodeAccess && ((ASTNodeAccess) o).getASTNode() != null)) {
							SimpleNode node = ((ASTNodeAccess) o).getASTNode();
							pos = new Pair<Integer, Integer>(node.jjtGetFirstToken().absoluteBegin - 1, node.jjtGetLastToken().absoluteEnd - 1);
						} else {
							String sql = o.toString();
							pos = findFragment(sql, woComments.toString());
							if (pos == null) {
								pos = findFragment(sql, woCommentsAndLiterals);
							}
							if (pos == null) {
								pos = findFragment(sql, topLevelSql);
							}
						}
						if (pos != null) {
							for (int i = pos.a; i < pos.b; ++i) {
								woComments.setCharAt(i, ' ');
							}
						}
					}
				}

				@Override
				public void visit(Select select) {
					select.getSelectBody().accept(new SelectVisitor() {

						@Override
						public void visit(WithItem withItem) {
						}

						@Override
						public void visit(SetOperationList setOpList) {
							throw new QueryTooComplexException();
						}

						@Override
						public void visit(PlainSelect plainSelect) {
							clearInStatement(plainSelect.getLimit());
							clearInStatement(plainSelect.getDistinct());
							clearInStatement(plainSelect.getTop());
							clearInStatement(plainSelect.getFetch());
							clearInStatement(plainSelect.getFirst());
							clearInStatement(plainSelect.getForUpdateTable());
							clearInStatement(plainSelect.getForXmlPath());
							clearInStatement(plainSelect.getIntoTables());
							clearInStatement(plainSelect.getOffset());
							clearInStatement(plainSelect.getKsqlWindow());
							clearInStatement(plainSelect.getMySqlHintStraightJoin());
							clearInStatement(plainSelect.getMySqlSqlCalcFoundRows());
							clearInStatement(plainSelect.getMySqlSqlNoCache());
							clearInStatement(plainSelect.getOptimizeFor());
							clearInStatement(plainSelect.getOracleHierarchical());
							clearInStatement(plainSelect.getOracleHint());
							clearInStatement(plainSelect.getSkip());
							clearInStatement(plainSelect.getWait());
							List<OrderByElement> orderByElements = plainSelect.getOrderByElements();
							if (orderByElements != null) {
								clearInStatement("order by " + orderByElements.stream().map(e -> e.toString()).collect(Collectors.joining(", ")));
							}
							
							Expression cond = null;
							if (plainSelect.getGroupBy() != null) {
								result.isHaving = true;
								cond = plainSelect.getHaving();
							} else {
								result.isHaving = false;
								cond = plainSelect.getWhere();
							}
							if (cond != null) {
								result.hasCondition = true;
								Pair<Integer, Integer> pos = null;
								if ((cond instanceof ASTNodeAccess && ((ASTNodeAccess) cond).getASTNode() != null)) {
									SimpleNode node = ((ASTNodeAccess) cond).getASTNode();
									pos = new Pair<Integer, Integer>(node.jjtGetFirstToken().absoluteBegin - 1, node.jjtGetLastToken().absoluteEnd - 1);
								} else {
									String sql = cond.toString();
									pos = findFragment(sql, woComments.toString());
									if (pos == null) {
										pos = findFragment(sql, woCommentsAndLiterals);
									}
									if (pos == null) {
										pos = findFragment(sql, topLevelSql);
									}
									if (pos == null) {
										pos = findFragment((result.isHaving? "having " : "where ") + sql, topLevelSql);
										if (pos != null) {
											Pattern p = Pattern.compile("^(" + (result.isHaving? "having" : "where") + "\\s+)", Pattern.CASE_INSENSITIVE);
											Matcher matcher = p.matcher(topLevelSql.substring(pos.a, pos.b));
											if (matcher.find()) {
												pos = new Pair<Integer, Integer>(pos.a + matcher.end(), pos.b);
											} else {
												pos = null;
											}
										}
									}
								}
								if (pos != null) {
									result.conditionStart = pos.a;
									result.conditionEnd = pos.b;
								} else {
									throw new QueryTooComplexException();
								}
							} else {
								result.hasCondition = false;
							}
							
							int selectEnd = plainSelect.getSelectItems().get(plainSelect.getSelectItems().size() - 1).getASTNode().jjtGetLastToken().absoluteEnd - 1;
							int plainSelectStart = plainSelect.getASTNode().jjtGetFirstToken().absoluteBegin - 1;
							
							cte.append(woComments.toString().substring(0, plainSelectStart));
							for (int i = 0; i < selectEnd; ++i) {
								woComments.setCharAt(i, ' ');
							}
							woComments.setCharAt(selectEnd - 1, '\f');
							Pair<Integer, Integer> pos = findFragment("\f from", woComments.toString());
							if (pos != null) {
								for (int i = pos.a; i < pos.b; ++i) {
									woComments.setCharAt(i, ' ');
								}
							} else {
								throw new QueryTooComplexException();
							}
							
							for (SelectItem si: plainSelect.getSelectItems()) {
								final boolean stop[] = new boolean[] { false };

								si.accept(new SelectItemVisitor() {
									@Override
									public void visit(SelectExpressionItem selectExpressionItem) {
										try {
											int a = selectExpressionItem.getASTNode().jjtGetFirstToken().absoluteBegin - 1;
											int b = selectExpressionItem.getASTNode().jjtGetLastToken().absoluteEnd - 1;
											String sql = sqlSelect.substring(a, b);
											if (selectExpressionItem.getAlias() != null) {
												sql = sql.replaceAll("\\s*" + (selectExpressionItem.getAlias().isUseAs()? "as\\s+" : "") + Pattern.quote(selectExpressionItem.getAlias().getName()) + "\\s*$", "");
											}
											boolean[] noSubexpression = new boolean[] { true };
											final Column column[] = new Column[1];
	
											selectExpressionItem.getExpression().accept(createExpressionVisitor(noSubexpression, column));
	
											boolean isNullable = true;
											if (column[0] != null) {
												String alias = null;
												if (column[0].getTable() != null) {
													if (column[0].getTable().getAlias() != null) {
														alias = column[0].getTable().getAlias().getName();
													} else {
														alias = column[0].getTable().getName();
													}
												}
												if (noSubexpression[0]) {
													net.sf.jailer.datamodel.Column col;
													try {
														col = findColumn(alias, column[0].getColumnName(), fromClause, metaDataSource);
														if (col != null) {
															isNullable = col.isNullable;
														}
													} catch (SQLException e) {
														// ignore
													}
												}
											}
											net.sf.jailer.datamodel.Column col = new net.sf.jailer.datamodel.Column(sql, null, -1, -1);
											col.isNullable = isNullable;
											selectClause.add(col);
										} catch (Exception e) {
											selectClause.add(null);
										}
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
											for (String c: mdTable.getColumns(false)) {
												net.sf.jailer.datamodel.Column col = findColumn(tableAlias, c, fromClause, metaDataSource);
												boolean isNullable = col == null || col.isNullable;
												col = new net.sf.jailer.datamodel.Column((unknownTableCounter[0] != 0 || fromClause.size() != 1? tableAlias + "." : "") + c, null, -1, -1);
												col.isNullable = isNullable;
												selectClause.add(col);
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
												for (String c: mdTable.getColumns(false)) {
													net.sf.jailer.datamodel.Column col = findColumn(e.getKey(), c, fromClause, metaDataSource);
													boolean isNullable = col == null || col.isNullable;
													col = new net.sf.jailer.datamodel.Column((unknownTableCounter[0] != 0 || fromClause.size() != 1? e.getKey() + "." : "") + c, null, -1, -1);
													col.isNullable = isNullable;
													selectClause.add(col);
												}
											} catch (SQLException e2) {
												logger.info("error", e2);
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
							throw new QueryTooComplexException();
						}
					});
				}
			});

			PrimaryKey pk = new PrimaryKey(new ArrayList<net.sf.jailer.datamodel.Column>(), false);
			Table table = new Table(woComments.toString(), pk, false, false);
			table.setColumns(selectClause);
			result.table = table;
			result.cte = cte.toString();
			
			System.out.println(result); // TODO
			
			return result;
		} catch (Exception e) {
			e.printStackTrace(); // TODO
			// logger.info("error", e);
		}
		return null;
	}

	protected static Pair<Integer, Integer> findFragment(String fragment, String sql) {
		Pair<Integer, Integer> pos = null;
		
		Pattern pattern = Pattern.compile(SqlUtil.createSQLFragmentSearchPattern(fragment), Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(sql);
		
		if (matcher.find()) {
			pos = new Pair<Integer, Integer>(matcher.start(), matcher.end());
			if (matcher.find()) { // not unique
				pos = null;
			}
		}

		return pos;
	}

	private static String createTopLevelSQL(String sqlSelect) {
		StringBuilder result = new StringBuilder(SqlUtil.removeComments(sqlSelect));
		
		int level = 0;
		for (int i = 0; i < result.length(); ++i) {
			char c = result.charAt(i);
			int pl = level;
			if (c == '(') {
				++level;
			} else if (c == ')') {
				--level;
			}
			if (level > 1 || level == 1 && level == pl) {
				result.setCharAt(i, ' ');
			}
		}
		
		return result.toString();
	}

	private static LinkedHashMap<String, MDTable> analyseFromClause(Statement st, int[] unknownTableCounter, final MetaDataSource metaDataSource) {
		final LinkedHashMap<String, MDTable> result = new LinkedHashMap<String, MDTable>();

		unknownTableCounter[0] = 0;
		st.accept(new StatementVisitorAdapter() {
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
						if (plainSelect.getFromItem() != null) {
							final FromItemVisitor fromItemVisitor = new FromItemVisitor() {
								private void unknownTable() {
									result.put("-unknown-" + (unknownTableCounter[0]++), null);
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
							}
						}
					}

					@Override
					public void visit(ValuesStatement aThis) {

					}
				});
			}
		});

		return result;
	}

	private static net.sf.jailer.datamodel.Column findColumn(String alias, String columnName, LinkedHashMap<String, MDTable> fromClause, MetaDataSource metaDataSource) throws SQLException {
		for (boolean strict: new boolean[] { false, true }) {
			for (Entry<String, MDTable> e: fromClause.entrySet()) {
				if (alias == null || idEquals(e.getKey(), alias, strict)) {
					if (e.getValue() != null) {
						Table table = metaDataSource.toTable(e.getValue());
						if (table != null) {
							for (net.sf.jailer.datamodel.Column column: table.getColumns()) {
								if (column.name != null && idEquals(column.name, columnName, strict)) {
									return column;
								}
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

	private static boolean idEquals(String a, String b, boolean strict) {
		if (strict) {
			return a.equals(b);
		}
		return Quoting.normalizeIdentifier(a).equals(Quoting.normalizeIdentifier(b));
	}

	private static ExpressionVisitor createExpressionVisitor(final boolean[] noSubexpression, final Column[] column) {
		return new ExpressionVisitor() {

			@Override
			public void visit(Column tableColumn) {
				column[0] = tableColumn;
			}

			@Override
			public void visit(NotExpression aThis) {
				noSubexpression[0] = false;
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
			}

			@Override
			public void visit(JsonOperator jsonExpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(JsonExpression jsonExpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(RegExpMatchOperator rexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(OracleHierarchicalExpression oexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(IntervalExpression iexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(ExtractExpression eexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(AnalyticExpression aexpr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Modulo modulo) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(CastExpression cast) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(BitwiseXor bitwiseXor) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(BitwiseOr bitwiseOr) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(BitwiseAnd bitwiseAnd) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Matches matches) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Concat concat) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(AnyComparisonExpression anyComparisonExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(ExistsExpression existsExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(WhenClause whenClause) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(CaseExpression caseExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(SubSelect subSelect) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(NotEqualsTo notEqualsTo) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(MinorThanEquals minorThanEquals) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(MinorThan minorThan) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(LikeExpression likeExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(IsNullExpression isNullExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(InExpression inExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(GreaterThanEquals greaterThanEquals) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(GreaterThan greaterThan) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(EqualsTo equalsTo) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Between between) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(OrExpression orExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(AndExpression andExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Subtraction subtraction) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Multiplication multiplication) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Division division) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Addition addition) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(StringValue stringValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(Parenthesis parenthesis) {
				noSubexpression[0] = false;
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
			}

			@Override
			public void visit(Function function) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(NullValue nullValue) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(BitwiseRightShift aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(BitwiseLeftShift aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(ValueListExpression valueList) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(IntegerDivision division) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(FullTextSearch fullTextSearch) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(IsBooleanExpression isBooleanExpression) {
				noSubexpression[0] = false;
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
			}

			@Override
			public void visit(ArrayExpression aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(XorExpression orExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(RowGetExpression rowGetExpression) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(ArrayConstructor aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(VariableAssignment aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(XMLSerializeExpr aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(TimezoneExpression aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(JsonAggregateFunction aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(JsonFunction aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(ConnectByRootOperator aThis) {
				noSubexpression[0] = false;
			}

			@Override
			public void visit(OracleNamedFunctionParameter aThis) {
				noSubexpression[0] = false;
			}
		};
	}
	
	private static class QueryTooComplexException extends RuntimeException {
	}

}
