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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.util.Quoting;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.expression.AllComparisonExpression;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.AnyComparisonExpression;
import net.sf.jsqlparser.expression.CaseExpression;
import net.sf.jsqlparser.expression.CastExpression;
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
import net.sf.jsqlparser.expression.WhenClause;
import net.sf.jsqlparser.expression.WithinGroupExpression;
import net.sf.jsqlparser.expression.operators.arithmetic.Addition;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseAnd;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseOr;
import net.sf.jsqlparser.expression.operators.arithmetic.BitwiseXor;
import net.sf.jsqlparser.expression.operators.arithmetic.Concat;
import net.sf.jsqlparser.expression.operators.arithmetic.Division;
import net.sf.jsqlparser.expression.operators.arithmetic.Modulo;
import net.sf.jsqlparser.expression.operators.arithmetic.Multiplication;
import net.sf.jsqlparser.expression.operators.arithmetic.Subtraction;
import net.sf.jsqlparser.expression.operators.conditional.AndExpression;
import net.sf.jsqlparser.expression.operators.conditional.OrExpression;
import net.sf.jsqlparser.expression.operators.relational.Between;
import net.sf.jsqlparser.expression.operators.relational.EqualsTo;
import net.sf.jsqlparser.expression.operators.relational.ExistsExpression;
import net.sf.jsqlparser.expression.operators.relational.GreaterThan;
import net.sf.jsqlparser.expression.operators.relational.GreaterThanEquals;
import net.sf.jsqlparser.expression.operators.relational.InExpression;
import net.sf.jsqlparser.expression.operators.relational.IsNullExpression;
import net.sf.jsqlparser.expression.operators.relational.JsonOperator;
import net.sf.jsqlparser.expression.operators.relational.LikeExpression;
import net.sf.jsqlparser.expression.operators.relational.Matches;
import net.sf.jsqlparser.expression.operators.relational.MinorThan;
import net.sf.jsqlparser.expression.operators.relational.MinorThanEquals;
import net.sf.jsqlparser.expression.operators.relational.NotEqualsTo;
import net.sf.jsqlparser.expression.operators.relational.RegExpMatchOperator;
import net.sf.jsqlparser.expression.operators.relational.RegExpMySQLOperator;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.statement.Commit;
import net.sf.jsqlparser.statement.SetStatement;
import net.sf.jsqlparser.statement.StatementVisitor;
import net.sf.jsqlparser.statement.Statements;
import net.sf.jsqlparser.statement.alter.Alter;
import net.sf.jsqlparser.statement.create.index.CreateIndex;
import net.sf.jsqlparser.statement.create.table.CreateTable;
import net.sf.jsqlparser.statement.create.view.AlterView;
import net.sf.jsqlparser.statement.create.view.CreateView;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.drop.Drop;
import net.sf.jsqlparser.statement.execute.Execute;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.merge.Merge;
import net.sf.jsqlparser.statement.replace.Replace;
import net.sf.jsqlparser.statement.select.AllColumns;
import net.sf.jsqlparser.statement.select.AllTableColumns;
import net.sf.jsqlparser.statement.select.FromItemVisitor;
import net.sf.jsqlparser.statement.select.LateralSubSelect;
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

/**
 * Parses a SQL query and tries to find out the type.
 * 
 * @author Ralf Wisser
 */
public class QueryTypeAnalyser {

	@SuppressWarnings("serial")
	private static class QueryTooComplexException extends RuntimeException {
	};
	
	/**
	 * Parses a SQL query and tries to find out the type.
	 * 
	 * @param sqlSelect the query
	 * @return the type or <code>null</code>
	 */
	public static Table getType(String sqlSelect, final MetaDataSource metaDataSource) {
		net.sf.jsqlparser.statement.Statement st;
		final Table[] result = new Table[1];
		try {
			st = CCJSqlParserUtil.parse(sqlSelect);
			st.accept(new StatementVisitor() {
				@Override
				public void visit(Upsert upsert) {
					throw new QueryTooComplexException();					
				}
				
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
							final List<MDTable> tables = new ArrayList<MDTable>();
							if (plainSelect.getFromItem() != null) {
								if (plainSelect.getJoins() != null) {
									throw new QueryTooComplexException();
								}
								plainSelect.getFromItem().accept(new FromItemVisitor() {
									@Override
									public void visit(TableFunction tableFunction) {
										throw new QueryTooComplexException();
									}
									
									@Override
									public void visit(ValuesList valuesList) {
										throw new QueryTooComplexException();
									}
									
									@Override
									public void visit(LateralSubSelect lateralSubSelect) {
										throw new QueryTooComplexException();
									}
									
									@Override
									public void visit(SubJoin subjoin) {
										throw new QueryTooComplexException();
									}
									
									@Override
									public void visit(SubSelect subSelect) {
										throw new QueryTooComplexException();
									}
									
									@Override
									public void visit(net.sf.jsqlparser.schema.Table tableName) {
										String schema = tableName.getSchemaName();
										String name = tableName.getName();
																				
										MDSchema mdSchema;
										if (schema == null) {
											mdSchema = metaDataSource.getDefaultSchema();
										} else {
											mdSchema = metaDataSource.find(schema);
										}
										if (mdSchema != null) {
											MDTable mdTable = mdSchema.find(name);
											if (mdTable != null) {
												if (!tables.isEmpty()) {
													throw new QueryTooComplexException();
												}
												tables.add(mdTable);
											} else {
												throw new QueryTooComplexException();
											}
										}
									}
								});
							}
							
							if (tables.size() != 1) {
								throw new QueryTooComplexException();
							}
							
							final MDTable theTable = tables.get(0);
							final List<String> columnNames = new ArrayList<String>();
							
							for (SelectItem si: plainSelect.getSelectItems()) {
								si.accept(new SelectItemVisitor() {
									
									@Override
									public void visit(SelectExpressionItem selectExpressionItem) {
										final boolean noSubexpression[] = new boolean[] { true };
										final String columnName[] = new String[1];
										
										selectExpressionItem.getExpression().accept(new ExpressionVisitor() {

											@Override
											public void visit(Column tableColumn) {
												columnName[0] = tableColumn.getColumnName();
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
											public void visit(WithinGroupExpression wgexpr) {
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
										});
									
										if (noSubexpression[0] && columnName[0] != null) {
											columnNames.add(columnName[0]);
										} else {
											columnNames.add(null);
										}
									}
									
									@Override
									public void visit(AllTableColumns allTableColumns) {
										try {
											columnNames.addAll(theTable.getColumns());
										} catch (SQLException e) {
											throw new QueryTooComplexException();
										}
									}
									
									@Override
									public void visit(AllColumns allColumns) {
										try {
											columnNames.addAll(theTable.getColumns());
										} catch (SQLException e) {
											throw new QueryTooComplexException();
										}
									}
								});
							}
							
							Map<String, String> ucColumnNames = new HashMap<String, String>();
							try {
								for (String col: theTable.getColumns()) {
									ucColumnNames.put(Quoting.staticUnquote(col.toUpperCase(Locale.ENGLISH)), col);
								}
								for (int i = 0; i < columnNames.size(); ++i) {
									String colName = columnNames.get(i);
									if (colName != null) {
										columnNames.set(i, ucColumnNames.get(Quoting.staticUnquote(colName.toUpperCase(Locale.ENGLISH))));
									}
								}
								result[0] = createTable(theTable, columnNames, metaDataSource);
							} catch (SQLException e) {
								throw new QueryTooComplexException();
							}
						}
					});
				}
				
				@Override
				public void visit(Merge merge) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(SetStatement set) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Execute execute) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Statements stmts) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Alter alter) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(AlterView alterView) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(CreateView createView) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(CreateTable createTable) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(CreateIndex createIndex) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Truncate truncate) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Drop drop) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Replace replace) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Insert insert) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Update update) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Delete delete) {
					throw new QueryTooComplexException();
				}
				
				@Override
				public void visit(Commit commit) {
					throw new QueryTooComplexException();
				}
			});
		} catch (JSQLParserException e) {
		} catch (QueryTooComplexException e) {
		}
		return result[0];
	}

	private static Table createTable(MDTable theTable, List<String> columnNames, MetaDataSource metaDataSource) throws SQLException {
		if (theTable.getPrimaryKeyColumns().isEmpty()) {
			return null;
		}
		for (String pk: theTable.getPrimaryKeyColumns()) {
			if (!columnNames.contains(pk)) {
				return null;
			}
		}
		String schemaName = theTable.getSchema().isDefaultSchema? null : theTable.getSchema().getName();
		String tableName = theTable.getName();
		List<net.sf.jailer.datamodel.Column> pkColumns = new ArrayList<net.sf.jailer.datamodel.Column>();
		for (String pk: theTable.getPrimaryKeyColumns()) {
			pkColumns.add(new net.sf.jailer.datamodel.Column(pk, "", 0, -1));
		}
		PrimaryKey primaryKey = new PrimaryKeyFactory().createPrimaryKey(pkColumns);
		Table table = new Table((schemaName == null? "" : schemaName + ".") + tableName, primaryKey, false, false);
		List<net.sf.jailer.datamodel.Column> columns = new ArrayList<net.sf.jailer.datamodel.Column>();
		for (String pk: columnNames) {
			columns.add(new net.sf.jailer.datamodel.Column(pk, "", 0, -1));
		}
		table.setColumns(columns);
		return table;
	}

}
