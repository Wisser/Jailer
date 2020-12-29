/*
 * Copyright 2007 - 2020 Ralf Wisser.
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.log4j.Logger;

import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.util.JSqlParserUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;
import net.sf.jsqlparser.expression.AllComparisonExpression;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.AnyComparisonExpression;
import net.sf.jsqlparser.expression.ArrayExpression;
import net.sf.jsqlparser.expression.CaseExpression;
import net.sf.jsqlparser.expression.CastExpression;
import net.sf.jsqlparser.expression.CollateExpression;
import net.sf.jsqlparser.expression.DateTimeLiteralExpression;
import net.sf.jsqlparser.expression.DateValue;
import net.sf.jsqlparser.expression.DoubleValue;
import net.sf.jsqlparser.expression.ExpressionVisitor;
import net.sf.jsqlparser.expression.ExtractExpression;
import net.sf.jsqlparser.expression.Function;
import net.sf.jsqlparser.expression.HexValue;
import net.sf.jsqlparser.expression.IntervalExpression;
import net.sf.jsqlparser.expression.JdbcNamedParameter;
import net.sf.jsqlparser.expression.JdbcParameter;
import net.sf.jsqlparser.expression.JsonExpression;
import net.sf.jsqlparser.expression.KeepExpression;
import net.sf.jsqlparser.expression.LongValue;
import net.sf.jsqlparser.expression.MySQLGroupConcat;
import net.sf.jsqlparser.expression.NextValExpression;
import net.sf.jsqlparser.expression.NotExpression;
import net.sf.jsqlparser.expression.NullValue;
import net.sf.jsqlparser.expression.NumericBind;
import net.sf.jsqlparser.expression.OracleHierarchicalExpression;
import net.sf.jsqlparser.expression.OracleHint;
import net.sf.jsqlparser.expression.Parenthesis;
import net.sf.jsqlparser.expression.RowConstructor;
import net.sf.jsqlparser.expression.SignedExpression;
import net.sf.jsqlparser.expression.StringValue;
import net.sf.jsqlparser.expression.TimeKeyExpression;
import net.sf.jsqlparser.expression.TimeValue;
import net.sf.jsqlparser.expression.TimestampValue;
import net.sf.jsqlparser.expression.UserVariable;
import net.sf.jsqlparser.expression.ValueListExpression;
import net.sf.jsqlparser.expression.WhenClause;
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
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.statement.Block;
import net.sf.jsqlparser.statement.Commit;
import net.sf.jsqlparser.statement.CreateFunctionalStatement;
import net.sf.jsqlparser.statement.DeclareStatement;
import net.sf.jsqlparser.statement.DescribeStatement;
import net.sf.jsqlparser.statement.ExplainStatement;
import net.sf.jsqlparser.statement.SetStatement;
import net.sf.jsqlparser.statement.ShowColumnsStatement;
import net.sf.jsqlparser.statement.ShowStatement;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.StatementVisitor;
import net.sf.jsqlparser.statement.Statements;
import net.sf.jsqlparser.statement.UseStatement;
import net.sf.jsqlparser.statement.alter.Alter;
import net.sf.jsqlparser.statement.alter.sequence.AlterSequence;
import net.sf.jsqlparser.statement.comment.Comment;
import net.sf.jsqlparser.statement.create.index.CreateIndex;
import net.sf.jsqlparser.statement.create.schema.CreateSchema;
import net.sf.jsqlparser.statement.create.sequence.CreateSequence;
import net.sf.jsqlparser.statement.create.table.CreateTable;
import net.sf.jsqlparser.statement.create.view.AlterView;
import net.sf.jsqlparser.statement.create.view.CreateView;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.drop.Drop;
import net.sf.jsqlparser.statement.execute.Execute;
import net.sf.jsqlparser.statement.grant.Grant;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.merge.Merge;
import net.sf.jsqlparser.statement.replace.Replace;
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
import net.sf.jsqlparser.statement.select.SetOperationList;
import net.sf.jsqlparser.statement.select.SubJoin;
import net.sf.jsqlparser.statement.select.SubSelect;
import net.sf.jsqlparser.statement.select.TableFunction;
import net.sf.jsqlparser.statement.select.ValuesList;
import net.sf.jsqlparser.statement.select.WithItem;
import net.sf.jsqlparser.statement.truncate.Truncate;
import net.sf.jsqlparser.statement.update.Update;
import net.sf.jsqlparser.statement.upsert.Upsert;
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
	private static final Logger logger = Logger.getLogger(MetaDataDetailsPanel.class);

	@SuppressWarnings("serial")
	private static class QueryTooComplexException extends RuntimeException {
	}

	/**
	 * Parses a SQL query and tries to find out the type.
	 *
	 * @param sqlSelect the query
	 * @return the type or <code>null</code>
	 */
	public static List<Table> getType(String sqlSelect, final MetaDataSource metaDataSource) {
		net.sf.jsqlparser.statement.Statement st;
		try {
			st = JSqlParserUtil.parse(SqlUtil.removeNonMeaningfulFragments(sqlSelect), 2);
			Map<Pair<String, String>, Collection<Pair<String, String>>> equivs = new HashMap<Pair<String,String>, Collection<Pair<String,String>>>();
			final LinkedHashMap<String, MDTable> fromClause = analyseFromClause(st, equivs, metaDataSource);
			final List<Pair<String, String>> selectClause = new ArrayList<Pair<String, String>>();
			st.accept(new DefaultStatementVisitor() {
				@Override
				public void visit(Select select) {
					select.getSelectBody().accept(new SelectVisitor() {

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

										selectExpressionItem.getExpression().accept(createExpressionVisitor(noSubexpression, column));

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
												Pair<String, String> col;
												try {
													col = findColumn(alias, column[0].getColumnName(), fromClause);
												} catch (SQLException e) {
													logger.info("error", e);
													throw new QueryTooComplexException();
												}
												selectClause.add(col);
											} else {
												selectClause.add(new Pair<String, String>(null, alias));
											}
										} else {
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
					Table table = createTable(theTable, origTable, tableAlias, columnNames, selectClause, equivs, metaDataSource);
					if (table != null) {
						result.add(table);
					}
				}
			}
			return result;
		} catch (Exception e) {
			// logger.info("error", e);
		}
		return null;
	}

	private static LinkedHashMap<String, MDTable> analyseFromClause(Statement st, final Map<Pair<String, String>, Collection<Pair<String, String>>> equivs, final MetaDataSource metaDataSource) {
		final LinkedHashMap<String, MDTable> result = new LinkedHashMap<String, MDTable>();

		st.accept(new DefaultStatementVisitor() {
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
											if (join.getOnExpression() != null) {
												join.getOnExpression().accept(expressionAnalyzer);
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
									if (join.getOnExpression() != null) {
										join.getOnExpression().accept(expressionAnalyzer);
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

	private static Table createTable(MDTable theTable, Table origTable, String tableAlias, List<String> columnNames, List<Pair<String, String>> selectClause, Map<Pair<String, String>, Collection<Pair<String, String>>> equivs, MetaDataSource metaDataSource) throws SQLException {
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
			public void visit(AllComparisonExpression allComparisonExpression) {
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

			}

			@Override
			public void visit(FullTextSearch fullTextSearch) {

			}

			@Override
			public void visit(IsBooleanExpression isBooleanExpression) {

			}

			@Override
			public void visit(NextValExpression aThis) {

			}

			@Override
			public void visit(CollateExpression aThis) {

			}

			@Override
			public void visit(SimilarToExpression aThis) {

			}

			@Override
			public void visit(ArrayExpression aThis) {

			}
		};
	}

	private static class DefaultStatementVisitor implements StatementVisitor {

		@Override
		public void visit(Commit commit) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Delete delete) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Update update) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Insert insert) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Replace replace) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Drop drop) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Truncate truncate) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(CreateIndex createIndex) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(CreateTable createTable) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(CreateView createView) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(AlterView alterView) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Alter alter) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Statements stmts) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Execute execute) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(SetStatement set) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Merge merge) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Select select) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Upsert upsert) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(UseStatement use) {
			throw new QueryTooComplexException();
		}

		@Override
		public void visit(Block arg0) {
		}

		@Override
		public void visit(Comment comment) {

		}

		@Override
		public void visit(CreateSchema aThis) {

		}

		@Override
		public void visit(ShowColumnsStatement set) {

		}

		@Override
		public void visit(ValuesStatement values) {

		}

		@Override
		public void visit(DescribeStatement describe) {

		}

		@Override
		public void visit(ExplainStatement aThis) {

		}

		@Override
		public void visit(ShowStatement aThis) {

		}

		@Override
		public void visit(DeclareStatement aThis) {

		}

		@Override
		public void visit(Grant grant) {

		}

		@Override
		public void visit(CreateSequence createSequence) {

		}

		@Override
		public void visit(AlterSequence alterSequence) {

		}

		@Override
		public void visit(CreateFunctionalStatement createFunctionalStatement) {

		}

	}

}
