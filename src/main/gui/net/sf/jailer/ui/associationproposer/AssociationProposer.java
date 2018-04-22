/*
 * Copyright 2007 - 2018 the original author or authors.
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
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.expression.AllComparisonExpression;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.CaseExpression;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.NotExpression;
import net.sf.jsqlparser.expression.Parenthesis;
import net.sf.jsqlparser.expression.WhenClause;
import net.sf.jsqlparser.expression.operators.relational.EqualsTo;
import net.sf.jsqlparser.expression.operators.relational.ExistsExpression;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.parser.ParseException;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.schema.Table;
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
import net.sf.jsqlparser.statement.select.Join;
import net.sf.jsqlparser.statement.select.LateralSubSelect;
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
import net.sf.jsqlparser.statement.truncate.Truncate;
import net.sf.jsqlparser.statement.update.Update;
import net.sf.jsqlparser.statement.upsert.Upsert;

/**
 * Analyzes SQL statements and proposes association definitions.
 *
 * @author Ralf Wisser
 */
public class AssociationProposer {

	private String[] FUNCTIONS_TO_IGNORE = new String[] { "XMLSERIALIZE", "XMLFOREST" };

	private final DataModel dataModel;
	private final Map<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Table> columnToTable = new IdentityHashMap<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Table>();
	private final Set<Equation> equations = new HashSet<Equation>();
	private final Set<Association> fromDataModel = new HashSet<Association>();
	
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

	public synchronized String analyze(String sqlStatement, int startLineNumber) {
		sqlStatement = removeCommentsAndLiterals(sqlStatement);
		for (String function : FUNCTIONS_TO_IGNORE) {
			sqlStatement = removeFunction(function, sqlStatement);
		}
		net.sf.jsqlparser.statement.Statement st;
		try {
			st = CCJSqlParserUtil.parse(sqlStatement);
		} catch (JSQLParserException e) {
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
		sqlStatement = st.toString();
		return null;
	}

	private String removeFunction(String function, String sqlStatement) {
		Pattern pattern = Pattern.compile("(\\b\\w+\\b)|\\(\\)|.", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(sqlStatement);
		int level = 0;
		Integer funcLevel = null;
		boolean result = matcher.find();
		StringBuffer sb = new StringBuffer();
		if (result) {
			do {
				String token = matcher.group(0);
				if ("(".equals(token)) {
					++level;
				} else if (")".equals(token)) {
					--level;
				}

				int l = token.length();
				matcher.appendReplacement(sb, "");
				if (function.equalsIgnoreCase(token)) {
					if (funcLevel == null) {
						funcLevel = level;
						--l;
						sb.append("1");
					}
				}
				if (funcLevel != null) {
					while (l > 0) {
						--l;
						sb.append(' ');
					}
				} else {
					sb.append(token);
				}
				if (funcLevel != null && ")".equals(token)) {
					if (funcLevel == level) {
						funcLevel = null;
					}
				}
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		return sb.toString();
	}

	/**
	 * Removes comments and literals from SQL statement.
	 * 
	 * @param statement
	 *            the statement
	 * 
	 * @return statement the statement without comments and literals
	 */
	private String removeCommentsAndLiterals(String statement) {
		Pattern pattern = Pattern.compile("('([^']*'))|(/\\*.*?\\*/)|(\\-\\-.*?(?=\n|$))", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(statement);
		boolean result = matcher.find();
		StringBuffer sb = new StringBuffer();
		if (result) {
			do {
				int l = matcher.group(0).length();
				matcher.appendReplacement(sb, "");
				if (matcher.group(1) != null) {
					l -= 2;
					sb.append("'");
				}
				while (l > 0) {
					--l;
					sb.append(' ');
				}
				if (matcher.group(1) != null) {
					sb.append("'");
				}
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		return sb.toString();
	}

	/**
	 * Select scope.
	 */
	private class Scope {
		Map<String, String> aliasToTable = new HashMap<String, String>();
		public List<Expression> expressions = new ArrayList<Expression>();
		// List<Cond>

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

		@Override
		public void visit(PlainSelect plainSelect) {
			scopes.push(new Scope());
			if (plainSelect.getSelectItems() != null) {
				for (SelectItem item: plainSelect.getSelectItems()) {
					item.accept(this);
				}
			}
			
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
					if (subjoin.getJoin() != null) {
						if (subjoin.getJoin().getRightItem() != null) {
							subjoin.getJoin().getRightItem().accept(this);
						}
						if (subjoin.getJoin().getOnExpression() != null) {
							scopes.peek().expressions.add(subjoin.getJoin().getOnExpression());
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
						fn = Quoting.staticUnquote(tableName.getSchemaName().toUpperCase(Locale.ENGLISH)) + ".";
					}
					fn += Quoting.staticUnquote(tableName.getName().toUpperCase(Locale.ENGLISH));
					scopes.peek().aliasToTable.put(Quoting.staticUnquote(alias.toUpperCase(Locale.ENGLISH)), fn);
				}
			};
			if (plainSelect.getFromItem() != null) {
				plainSelect.getFromItem().accept(fromItemVisitor);
			}
			if (plainSelect.getJoins() != null) {
				for (Join join: plainSelect.getJoins()) {
					if (join.getOnExpression() != null) {
						scopes.peek().expressions.add(join.getOnExpression());
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
			withItem.getSelectBody().accept(this);
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

		private void analyseTopScope() {
			for (Expression expr: scopes.peek().expressions) {
				expr.accept(new ExpressionVisitorAdapter() {
					
					@Override
					public void visit(NotExpression aThis) {
					}
					
					@Override
					public void visit(AllComparisonExpression allComparisonExpression) {
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
								// TODO: nicht, wenn aliase gleich  bei reflex nixht beide
								// TODO: equalsIgnoreQuoting
								// TODO: new line nach ; bei QBuilder statements
								// TODO: tracking icons bei statement border anzeige alternierend hell/dunkel grün
								/* TODO: join cond umdrehen bei autom. gen. stmts in cons. codecomp + query builder, "join A -> A.* = B.*)
								  ColMapping verwenden
								*/
								// TODO: Desktop -> QueryBuilder scheint left joins auszulassen, wenn tab rechts leer ist. Sollte nicht so sein.
								// TODO: Desktop, "DML -> insert new row" scheint nicht zu funktionieren
								net.sf.jailer.datamodel.Column rightColumn = getColumn(right);
								if (rightColumn != null) {
									equations.add(new Equation(leftColumn, rightColumn, false));
									equations.add(new Equation(rightColumn, leftColumn, false));
								}								
							}
						}
					}
				});
			}
		}

		private net.sf.jailer.datamodel.Column getColumn(Column column) {
			String alias = column.getTable().getName();
			if (alias != null) {
				for (boolean withSchema: new boolean[] { true, false }) {
					for (int i = scopes.size() - 1; i >= 0; --i) {
						String tn = scopes.get(i).aliasToTable.get(Quoting.staticUnquote(alias.toUpperCase(Locale.ENGLISH)));
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
								String schema = Quoting.staticUnquote(table.getSchema("").toUpperCase(Locale.ENGLISH));
								String uName = Quoting.staticUnquote(table.getUnqualifiedName().toUpperCase(Locale.ENGLISH));
								if (uName.equals(tnName)) {
									if (!withSchema || schema.equals(tnSchema)) {
										String uqColName = Quoting.staticUnquote(column.getColumnName());
										for (net.sf.jailer.datamodel.Column dColumn: table.getColumns()) {
											if (Quoting.staticUnquote(dColumn.name).equalsIgnoreCase(uqColName)) {
												columnToTable.put(dColumn, table);
												return dColumn;
											}
										}
										return null;
									}
								}
							}
						}
					}
				}
			}
			return null;
		}
	}

	private static class Equation {
		public final net.sf.jailer.datamodel.Column a;
		public final net.sf.jailer.datamodel.Column b;
		public final boolean isTransient;
		
		public Equation(net.sf.jailer.datamodel.Column a, net.sf.jailer.datamodel.Column b, boolean isTransient) {
			this.a = a;
			this.b = b;
			this.isTransient = isTransient;
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
			if (b == other.a && a != other.b) {
				return new Equation(a, other.b, true);
			}
			return null;
		}
	}

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
			boolean hasNonTransientEquation = false;
			StringBuilder sb = new StringBuilder();
			for (Equation e: sortedEquations) {
				if (columnToTable.get(e.a) == pair.a) {
					if (columnToTable.get(e.b) == pair.b) {
						if (!e.isTransient) {
							hasNonTransientEquation = true;
						}
						if (sb.length() > 0) {
							sb.append(" and \n");
						}
						sb.append("A." + e.a.name + " = B." + e.b.name);
					}
				}
			}
			if (hasNonTransientEquation) {
				String name = ("AP" + (UUID.randomUUID().toString()));
				Association association = new Association(pair.a, pair.b, false, false, sb.toString(), dataModel, false, null, "Association Proposer");
				Association revAssociation = new Association(pair.b, pair.a, false, false, sb.toString(), dataModel, true, null, "Association Proposer");
				association.setName(name);
				association.reversalAssociation = revAssociation;
				revAssociation.reversalAssociation = association;
				addAssociation(name, pair, association, true);
			}
		}
	}

	private final List<Association> newAssociations = new ArrayList<Association>();
	private final List<Association> newKnownAssociations = new ArrayList<Association>();
	private final List<Association> knownAssociations = new ArrayList<Association>();
	private final Map<Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>, List<Association>> assocPerSourceDest
		= new HashMap<Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table>, List<Association>>();
	
	private void addAssociation(String name, Pair<net.sf.jailer.datamodel.Table, net.sf.jailer.datamodel.Table> pair,
			Association association, boolean check) {
		List<Association> assocList = assocPerSourceDest.get(pair);
		if (assocList == null) {
			assocList = new ArrayList<Association>();
			assocPerSourceDest.put(pair, assocList);
		}
		if (check) {
			Map<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column> mapping = association.createSourceToDestinationKeyMapping();
			Map<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column> revMapping = association.reversalAssociation.createSourceToDestinationKeyMapping();
			if (!mapping.isEmpty() && !revMapping.isEmpty()) {
				for (Association other: assocList) {
					Map<net.sf.jailer.datamodel.Column, net.sf.jailer.datamodel.Column> otherMapping = other.createSourceToDestinationKeyMapping();
					if (mapping.equals(otherMapping) || revMapping.equals(otherMapping)) {
						if (fromDataModel.contains(other) && !knownAssociations.contains(other)) {
							newKnownAssociations.add(association);
							knownAssociations.add(other);
						}
						return;
					}
				}
			}
		}
		assocList.add(association);
		
		if (check) {
			newAssociations.add(association);
		}
	}

	public synchronized List<Association> pickUpNewAssociations() {
		ArrayList<Association> result = new ArrayList<Association>(newAssociations);
		newAssociations.clear();
		return result;
	}

	public synchronized List<Association> pickUpKnownAssociations() {
		ArrayList<Association> result = new ArrayList<Association>(newKnownAssociations);
		newKnownAssociations.clear();
		return result;
	}

}
