/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
package net.sf.jailer.ui.associationproposer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.UUID;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.JSqlParserUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.CaseExpression;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.NotExpression;
import net.sf.jsqlparser.expression.Parenthesis;
import net.sf.jsqlparser.expression.WhenClause;
import net.sf.jsqlparser.expression.operators.relational.EqualsTo;
import net.sf.jsqlparser.expression.operators.relational.ExistsExpression;
import net.sf.jsqlparser.parser.ParseException;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.schema.Table;
import net.sf.jsqlparser.statement.Block;
import net.sf.jsqlparser.statement.Commit;
import net.sf.jsqlparser.statement.CreateFunctionalStatement;
import net.sf.jsqlparser.statement.DeclareStatement;
import net.sf.jsqlparser.statement.DescribeStatement;
import net.sf.jsqlparser.statement.ExplainStatement;
import net.sf.jsqlparser.statement.IfElseStatement;
import net.sf.jsqlparser.statement.PurgeStatement;
import net.sf.jsqlparser.statement.ResetStatement;
import net.sf.jsqlparser.statement.RollbackStatement;
import net.sf.jsqlparser.statement.SavepointStatement;
import net.sf.jsqlparser.statement.SetStatement;
import net.sf.jsqlparser.statement.ShowColumnsStatement;
import net.sf.jsqlparser.statement.ShowStatement;
import net.sf.jsqlparser.statement.StatementVisitor;
import net.sf.jsqlparser.statement.Statements;
import net.sf.jsqlparser.statement.UnsupportedStatement;
import net.sf.jsqlparser.statement.UseStatement;
import net.sf.jsqlparser.statement.alter.Alter;
import net.sf.jsqlparser.statement.alter.AlterSession;
import net.sf.jsqlparser.statement.alter.AlterSystemStatement;
import net.sf.jsqlparser.statement.alter.RenameTableStatement;
import net.sf.jsqlparser.statement.alter.sequence.AlterSequence;
import net.sf.jsqlparser.statement.analyze.Analyze;
import net.sf.jsqlparser.statement.comment.Comment;
import net.sf.jsqlparser.statement.create.index.CreateIndex;
import net.sf.jsqlparser.statement.create.schema.CreateSchema;
import net.sf.jsqlparser.statement.create.sequence.CreateSequence;
import net.sf.jsqlparser.statement.create.synonym.CreateSynonym;
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
import net.sf.jsqlparser.statement.select.SelectBody;
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
import net.sf.jsqlparser.statement.show.ShowTablesStatement;
import net.sf.jsqlparser.statement.truncate.Truncate;
import net.sf.jsqlparser.statement.update.Update;
import net.sf.jsqlparser.statement.upsert.Upsert;
import net.sf.jsqlparser.statement.values.ValuesStatement;

/**
 * Analyzes SQL statements and proposes association definitions. <br>
 * This allows to reverse-engineer the data model based on existing SQL queries. <br><br>
 *
 * It uses this recursive procedure:
 * <ul>
 * <li>Open new scope.</li>
 * <li>Retrieve information about used tables and aliases.</li>
 * <li>Analyze sub-queries</li>
 * <li>Collect equations between columns.</li>
 * <li>Assemble association-proposals based on transitive closure of the equations.</li>
 * <li>Close the scope.</li>
 * </ul>
 *
 * @author Ralf Wisser
 */
public class AssociationProposer {

	private final DataModel dataModel;
	private final Map<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Table> columnToTable = new IdentityHashMap<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Table>();
	private final Set<Equation> equations = new HashSet<Equation>();
	private final Set<Association> fromDataModel = new HashSet<Association>();

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model
	 */
	public AssociationProposer(DataModel dataModel) {
		this.dataModel = dataModel;
		for (Entry<String, Association> e: dataModel.namedAssociations.entrySet()) {
			Association association = e.getValue();
			fromDataModel.add(association);
			addAssociation(e.getKey(), new Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>(association.source, association.destination), association, false);
			association = association.reversalAssociation;
			fromDataModel.add(association);
			addAssociation(e.getKey(), new Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>(association.source, association.destination), association, false);
		}
	}

	/**
	 * Analyzes a given statement.
	 *
	 * @param sqlStatement the statement
	 * @param startLineNumber the line number at which the statement begins in the script
	 * @return an error message if statement is invalid, else <code>null</code>
	 */
	public synchronized String analyze(String sqlStatement, int startLineNumber, int timeoutSec) {
		sqlStatement = SqlUtil.removeNonMeaningfulFragments(sqlStatement);
		net.sf.jsqlparser.statement.Statement st;
		try {
			st = JSqlParserUtil.parse(SqlUtil.removeNonMeaningfulFragments(sqlStatement), timeoutSec);
		} catch (/* JSQLParserException */ Throwable e) {
			String prefix = "Line " + startLineNumber + ": ";
			if (e.getCause() instanceof ParseException) {
				ParseException pe = (ParseException) e.getCause();
				return prefix + pe.getMessage().replaceAll("\\s+", " ");
			}
			Throwable t = e;
			while (t.getMessage() == null && t.getCause() != null) {
				t = t.getCause();
			}
			return prefix + t.getMessage();
		}
		st.accept(new APStatementVisitor());
		closeEquations();
		proposeJoinConditions();
		return null;
	}

	/**
	 * Scope of a "PlainSelect".
	 */
	private class Scope {
		Map<String, String> aliasToTable = new HashMap<String, String>();
		List<Expression> expressions = new ArrayList<Expression>();
		// List<Cond>

		@Override
		public String toString() {
			return aliasToTable.toString() + "\n" + expressions;
		}
	}

	private class APStatementVisitor extends ExpressionVisitorAdapter implements StatementVisitor, SelectItemVisitor, SelectVisitor {

		private Stack<Scope> scopes = new Stack<Scope>();

		@Override
		public void visit(Commit commit) {
		}

		@Override
		public void visit(Delete delete) {
		}

		@Override
		public void visit(Update update) {
			if (update.getSelect() != null) {
				update.getSelect().accept(this);
			}
		}

		@Override
		public void visit(Insert insert) {
			if (insert.getSelect() != null) {
				insert.getSelect().accept(this);
			}
		}

		@Override
		public void visit(Replace replace) {
		}

		@Override
		public void visit(Drop drop) {
		}

		@Override
		public void visit(Truncate truncate) {
		}

		@Override
		public void visit(CreateIndex createIndex) {
		}

		@Override
		public void visit(CreateTable createTable) {
		}

		@Override
		public void visit(CreateView createView) {
		}

		@Override
		public void visit(AlterView alterView) {
		}

		@Override
		public void visit(Alter alter) {
		}

		@Override
		public void visit(Statements stmts) {
		}

		@Override
		public void visit(Execute execute) {
		}

		@Override
		public void visit(SetStatement set) {
		}

		@Override
		public void visit(Merge merge) {
			if (merge.getUsingSelect() != null) {
				if (merge.getUsingSelect().getWithItemsList() != null) {
					for (WithItem item: merge.getUsingSelect().getWithItemsList()) {
						item.accept(this);
					}
				}
				if (merge.getUsingSelect().getSelectBody() != null) {
					merge.getUsingSelect().getSelectBody().accept(this);
				}
			}
		}

		@Override
		public void visit(Select select) {
			if (select.getWithItemsList() != null) {
				for (WithItem item: select.getWithItemsList()) {
					item.accept(this);
				}
			}
			if (select.getSelectBody() != null) {
				select.getSelectBody().accept(this);
			}
		}

		@Override
		public void visit(Upsert upsert) {
		}

		@Override
		public void visit(AllColumns allColumns) {
		}

		@Override
		public void visit(AllTableColumns allTableColumns) {
		}

		@Override
		public void visit(SelectExpressionItem selectExpressionItem) {
			selectExpressionItem.getExpression().accept(this);
		}

		/**
		 * Analyzes a "PlainSelect".
		 * <ul>
		 * <li>Opens new scope.</li>
		 * <li>Retrieves information about used tables and aliases.</li>
		 * <li>Analyzes sub-queries</li>
		 * <li>Collects equations between columns.</li>
		 * <li>Assembles associations based on transitive closure of the equations.</li>
		 * <li>Closes the scope.</li>
		 * </ul>
		 */
		@Override
		public void visit(PlainSelect plainSelect) {
			scopes.push(new Scope());

			FromItemVisitor fromItemVisitor = new FromItemVisitor() {
				@Override
				public void visit(TableFunction tableFunction) {
				}
				@Override
				public void visit(ValuesList valuesList) {
				}
				@Override
				public void visit(LateralSubSelect lateralSubSelect) {
					if (lateralSubSelect.getSubSelect() != null) {
						if (lateralSubSelect.getSubSelect().getSelectBody() != null) {
							lateralSubSelect.getSubSelect().getSelectBody().accept(APStatementVisitor.this);
						}
					}
				}
				@Override
				public void visit(SubJoin subjoin) {
					if (subjoin.getLeft() != null) {
						subjoin.getLeft().accept(this);
					}
					if (subjoin.getJoinList() != null) {
						for (Join join: subjoin.getJoinList()) {
							if (join != null) {
								if (join.getRightItem() != null) {
									join.getRightItem().accept(this);
								}
								if (join.getOnExpression() != null) {
									scopes.peek().expressions.add(join.getOnExpression());
								}
							}
						}
					}
				}

				@Override
				public void visit(SubSelect subSelect) {
					if (subSelect.getSelectBody() != null) {
						subSelect.getSelectBody().accept(APStatementVisitor.this);
					}
				}

				@Override
				public void visit(Table tableName) {
					String alias;
					if (tableName.getAlias() != null) {
						alias = tableName.getAlias().getName();
					} else {
						alias = tableName.getName();
					}
					String fn = "";
					if (tableName.getSchemaName() != null) {
						fn = Quoting.normalizeIdentifier(tableName.getSchemaName()) + ".";
					}
					fn += Quoting.normalizeIdentifier(tableName.getName());
					scopes.peek().aliasToTable.put(Quoting.normalizeIdentifier(alias), fn);
				}

				@Override
				public void visit(ParenthesisFromItem aThis) {
					if (aThis.getFromItem() != null) {
						aThis.getFromItem().accept(this);
					}
				}
			};
			if (plainSelect.getFromItem() != null) {
				plainSelect.getFromItem().accept(fromItemVisitor);
			}
			if (plainSelect.getJoins() != null) {
				for (Join join: plainSelect.getJoins()) {
					if (join.getOnExpressions() != null) {
						join.getOnExpressions().forEach(e -> scopes.peek().expressions.add(e));
					}
					if (join.getRightItem() != null) {
						join.getRightItem().accept(fromItemVisitor);
					}
				}
			}
			if (plainSelect.getWhere() != null) {
				scopes.peek().expressions.add(plainSelect.getWhere());
			}

			for (Expression expr: scopes.peek().expressions) {
				expr.accept(this);
			}

			if (plainSelect.getSelectItems() != null) {
				for (SelectItem item: plainSelect.getSelectItems()) {
					item.accept(this);
				}
			}

			analyseTopScope();
			scopes.pop();
		}

		@Override
		public void visit(SetOperationList setOpList) {
			for (SelectBody select: setOpList.getSelects()) {
				select.accept(this);
			}
		}

		@Override
		public void visit(WithItem withItem) {
			if (withItem.getWithItemList() != null) {
				for (SelectItem item: withItem.getWithItemList()) {
					item.accept(new SelectItemVisitor() {
						@Override
						public void visit(SelectExpressionItem selectExpressionItem) {
							selectExpressionItem.accept(APStatementVisitor.this);
						}
						@Override
						public void visit(AllTableColumns allTableColumns) {
						}
						@Override
						public void visit(AllColumns allColumns) {
						}
					});
				}
			}
			if (withItem.getSubSelect() != null) {
				if (withItem.getSubSelect().getSelectBody() != null) {
					withItem.getSubSelect().getSelectBody().accept(this);
				}
			}
		}

		@Override
		public void visit(SubSelect subSelect) {
			if (subSelect.getSelectBody() != null) {
				subSelect.getSelectBody().accept(this);
			}
		}

		@Override
	    public void visit(AnalyticExpression expr) {
		}

		@Override
		public void visit(UseStatement use) {
		}

		private void analyseTopScope() {
			for (Expression expr: scopes.peek().expressions) {
				expr.accept(new ExpressionVisitorAdapter() {

					@Override
					public void visit(NotExpression aThis) {
					}

					@Override
					public void visit(ExistsExpression existsExpression) {
					}

					@Override
					public void visit(WhenClause whenClause) {
					}

					@Override
					public void visit(CaseExpression caseExpression) {
					}

					@Override
					public void visit(SubSelect subSelect) {
					}

					@Override
					public void visit(Column tableColumn) {
					}

					@Override
					public void visit(EqualsTo equalsTo) {
						Expression leftExpression = equalsTo.getLeftExpression();
						Expression rightExpression = equalsTo.getRightExpression();
						while (leftExpression instanceof Parenthesis) {
							leftExpression = ((Parenthesis) leftExpression).getExpression();
						}
						while (rightExpression instanceof Parenthesis) {
							rightExpression = ((Parenthesis) rightExpression).getExpression();
						}
						if (leftExpression instanceof Column && rightExpression instanceof Column) {
							Column left = (Column) leftExpression;
							Column right = (Column)	rightExpression;

							net.sf.jailer.datamodel.Column leftColumn = getColumn(left);
							if (leftColumn != null) {
								net.sf.jailer.datamodel.Column rightColumn = getColumn(right);
								if (rightColumn != null) {
									boolean sameTable = false;
									Table leftTable = ((Column) leftExpression).getTable();
									String leftAlias = leftTable != null? leftTable.getName() : null;
									Table rightTable = ((Column) rightExpression).getTable();
									String rightAlias = rightTable != null? rightTable.getName() : null;
									if (leftAlias != null && rightAlias != null) {
										if (Quoting.equalsIgnoreQuotingAndCase(leftAlias, rightAlias)) {
											sameTable = true;
										}
									} else {
										if (columnToTable.get(leftColumn) == columnToTable.get(rightColumn)) {
											sameTable = true;
										}
									}
									if (!sameTable) {
										if (leftAlias == null) {
											leftAlias = columnToTable.get(leftColumn).getUnqualifiedName();
										}
										if (rightAlias == null) {
											rightAlias = columnToTable.get(rightColumn).getUnqualifiedName();
										}
										leftAlias = Quoting.normalizeIdentifier(leftAlias);
										rightAlias = Quoting.normalizeIdentifier(rightAlias);
										Equation e1 = new Equation(leftAlias, leftColumn, rightAlias, rightColumn, false);
										Equation e2 = new Equation(rightAlias, rightColumn, leftAlias, leftColumn, false);
										e1.reversal = e2;
										e2.reversal = e1;
										equations.add(e1);
										equations.add(e2);
									}
								}
							}
						}
					}
				});
			}
		}

		private net.sf.jailer.datamodel.Column getColumn(Column column) {
			String alias = null;
			if (column.getTable() != null) {
				alias = column.getTable().getName();
			}
			Set<String> tableCandidat = new HashSet<String>();
			for (boolean withSchema: new boolean[] { true, false }) {
				for (int i = scopes.size() - 1; i >= 0; --i) {
					tableCandidat.clear();
					if (alias != null) {
						tableCandidat.add(scopes.get(i).aliasToTable.get(Quoting.normalizeIdentifier(alias)));
					} else {
						tableCandidat.addAll(scopes.get(i).aliasToTable.values());
					}
					for (String tn: tableCandidat) {
						if (tn != null) {
							int iDot = tn.indexOf('.');
							String tnSchema;
							String tnName;
							if (iDot >= 0) {
								tnSchema = tn.substring(0, iDot);
								tnName = tn.substring(iDot + 1);
							} else {
								tnSchema = "";
								tnName = tn;
							}
							for (net.sf.jailer.datamodel.Table table: dataModel.getTables()) {
								String schema = Quoting.normalizeIdentifier(table.getSchema(""));
								String uName = Quoting.normalizeIdentifier(table.getUnqualifiedName());
								if (uName.equals(tnName)) {
									if (!withSchema || schema.equals(tnSchema)) {
										String uqColName = Quoting.staticUnquote(column.getColumnName());
										for (net.sf.jailer.datamodel.Column dColumn: table.getColumns()) {
											if (Quoting.staticUnquote(dColumn.name).equalsIgnoreCase(uqColName)) {
												columnToTable.put(dColumn, table);
												return dColumn;
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return null;
		}

		@Override
		public void visit(Block arg0) {
			// ignore
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

		@Override
		public void visit(SavepointStatement savepointStatement) {
		}

		@Override
		public void visit(RollbackStatement rollbackStatement) {
		}

		@Override
		public void visit(ResetStatement reset) {
		}

		@Override
		public void visit(ShowTablesStatement showTables) {
		}

		@Override
		public void visit(CreateSynonym createSynonym) {
		}

		@Override
		public void visit(AlterSession alterSession) {
		}

		@Override
		public void visit(IfElseStatement aThis) {
		}

		@Override
		public void visit(RenameTableStatement renameTableStatement) {
		}

		@Override
		public void visit(PurgeStatement purgeStatement) {
		}

		@Override
		public void visit(AlterSystemStatement alterSystemStatement) {
		}

		@Override
		public void visit(Analyze arg0) {
		}

		@Override
		public void visit(UnsupportedStatement arg0) {
		}
	}

	/**
	 * Equation between two columns.
	 */
	private static class Equation {
		public final String aliasA;
		public final net.sf.jailer.datamodel.Column a;
		public final String aliasB;
		public final net.sf.jailer.datamodel.Column b;
		public final boolean isTransitive;
		public Equation reversal;

		public Equation(String aliasA, net.sf.jailer.datamodel.Column a, String aliasB, net.sf.jailer.datamodel.Column b, boolean isTransive) {
			this.aliasA = aliasA;
			this.aliasB = aliasB;
			this.a = a;
			this.b = b;
			this.isTransitive = isTransive;
		}

		public String toString() {
			return a + " = " + b;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((a == null) ? 0 : System.identityHashCode(a));
			result = prime * result + ((b == null) ? 0 : System.identityHashCode(b));
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Equation other = (Equation) obj;
			if (a == null) {
				if (other.a != null)
					return false;
			} else if (a != other.a)
				return false;
			if (b == null) {
				if (other.b != null)
					return false;
			} else if (b != other.b)
				return false;
			return true;
		}

		public Equation join(Equation other) {
			if (b == other.a && aliasB.equals(other.aliasA)) {
				if (!(a == other.b && aliasA.equals(other.aliasB))) {
					return new Equation(aliasA, a, other.aliasB, other.b, true);
				}
			}
			return null;
		}
	}

	/**
	 * Creates transitive closure of the equations.
	 */
	private void closeEquations() {
		Set<Equation> joined = new HashSet<Equation>();
		do {
			for (Equation e1: equations) {
				for (Equation e2: equations) {
					Equation join = e1.join(e2);
					if (join != null && !equations.contains(join)) {
						joined.add(join);
					}
				}
			}
			equations.addAll(joined);
			joined.clear();
		} while (!joined.isEmpty());
	}

	/**
	 * Assemble association-proposals based on transitive closure of the equations.
	 */
	private void proposeJoinConditions() {
		Set<Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>> pairs = new LinkedHashSet<Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>>();
		List<Equation> sortedEquations = new ArrayList<Equation>(equations);
		Collections.sort(sortedEquations, new Comparator<Equation>() {
			@Override
			public int compare(Equation o1, Equation o2) {
				net.sf.jailer.datamodel.Table tab1 = columnToTable.get(o1.a);
				net.sf.jailer.datamodel.Table tab2 = columnToTable.get(o2.a);
				if (tab1 != tab2) {
					return tab1.getName().compareTo(tab2.getName());
				}
				return o1.a.name.compareTo(o2.a.name);
			}
		});
		for (Equation e: sortedEquations) {
			net.sf.jailer.datamodel.Table tabA = columnToTable.get(e.a);
			net.sf.jailer.datamodel.Table tabB = columnToTable.get(e.b);
			if (!pairs.contains(new Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>(tabB, tabA))) {
				pairs.add(new Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>(tabA, tabB));
			}
		}
		for (Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table> pair: pairs) {
			List<Equation> theEquations = new ArrayList<Equation>();
			Set<Pair<String, String>> aliasesPairs = new HashSet<Pair<String, String>>();
			for (Equation e: sortedEquations) {
				if (columnToTable.get(e.a) == pair.a) {
					if (columnToTable.get(e.b) == pair.b) {
						theEquations.add(e);
						aliasesPairs.add(new Pair<String, String>(e.aliasA, e.aliasB));
					}
				}
			}

			for (Pair<String, String> aliasesPair: aliasesPairs) {
				StringBuilder condition = new StringBuilder();
				StringBuilder conditionNonTransitive = new StringBuilder();
				Set<Equation> seen = new HashSet<Equation>();
				for (boolean transitive: new boolean[] { false, true }) { // non-transitive first
					for (Equation e: theEquations) {
						if (e.isTransitive == transitive) {
							if (e.aliasA.equals(aliasesPair.a) && e.aliasB.equals(aliasesPair.b)) {
								if (e.reversal == null || !seen.contains(e.reversal)) {
									seen.add(e);
									if (!e.isTransitive) {
										if (conditionNonTransitive.length() > 0) {
											conditionNonTransitive.append(" and \n");
										}
										conditionNonTransitive.append("A." + e.a.name + "=B." + e.b.name);
									}
									if (condition.length() > 0) {
										condition.append(" and \n");
									}
									condition.append("A." + e.a.name + "=B." + e.b.name);
								}
							}
						}
					}
				}
				if (conditionNonTransitive.length() > 0) {
					String name = ("AP" + (UUID.randomUUID().toString()));
					Association association = new Association(pair.a, pair.b, false, false, conditionNonTransitive.toString(), dataModel, false, null, "Association Proposer");
					Association revAssociation = new Association(pair.b, pair.a, false, false, conditionNonTransitive.toString(), dataModel, true, null, "Association Proposer");
					association.setName(name);
					association.reversalAssociation = revAssociation;
					revAssociation.reversalAssociation = association;
					addAssociation(name, pair, association, true);
					if (!conditionNonTransitive.toString().equals(condition.toString())) {
						name = ("AP" + (UUID.randomUUID().toString()));
						association = new Association(pair.a, pair.b, false, false, condition.toString(), dataModel, false, null, "Association Proposer");
						revAssociation = new Association(pair.b, pair.a, false, false, condition.toString(), dataModel, true, null, "Association Proposer");
						association.setName(name);
						association.reversalAssociation = revAssociation;
						revAssociation.reversalAssociation = association;
						addAssociation(name, pair, association, true);
					}
				}
			}
		}
	}

	private final List<Association> newAssociations = new ArrayList<Association>();
	private final List<Association> newKnownAssociations = new ArrayList<Association>();
	private final List<Association> knownAssociations = new ArrayList<Association>();
	private final Map<Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>, List<Association>> assocPerSourceDest
		= new HashMap<Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>, List<Association>>();

	/**
	 * Adds an association to the proposals list.
	 *
	 * @param name name of association
	 * @param pair (source, destination) table pair
	 * @param association the association
	 * @param check check if association is already known?
	 * @return <code>true</code> if association was added
	 */
	public boolean addAssociation(String name, Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table> pair,
			Association association, boolean check) {
		List<Association> assocList = assocPerSourceDest.get(pair);
		if (assocList == null) {
			assocList = new ArrayList<Association>();
			assocPerSourceDest.put(pair, assocList);
		}
		if (check) {
			Set<Pair<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column>> mapping = new HashSet<Pair<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column>>();
			Set<Pair<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column>> revMapping = new HashSet<Pair<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column>>();
			association.createSourceToDestinationKeyMapping(mapping);
			association.createSourceToDestinationKeyMapping(revMapping);
			for (Association other: assocList) {
				Set<Pair<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column>> otherMapping = new HashSet<Pair<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column>>();
				other.createSourceToDestinationKeyMapping(otherMapping);
				if (mapping.equals(otherMapping) || revMapping.equals(otherMapping)) {
					if (fromDataModel.contains(other) && !knownAssociations.contains(other)) {
						newKnownAssociations.add(association);
						knownAssociations.add(other);
					}
					return false;
				}
			}
		}
		assocList.add(association);

		if (check) {
			newAssociations.add(association);
		}

		return true;
	}

	/**
	 * Picks up new associations.
	 *
	 * @return new associations
	 */
	public synchronized List<Association> pickUpNewAssociations() {
		ArrayList<Association> result = new ArrayList<Association>(newAssociations);
		newAssociations.clear();
		return result;
	}

	/**
	 * Picks up already known associations (Associations that are in data model)
	 *
	 * @return already known associations
	 */
	public synchronized List<Association> pickUpKnownAssociations() {
		ArrayList<Association> result = new ArrayList<Association>(newKnownAssociations);
		newKnownAssociations.clear();
		return result;
	}

}
