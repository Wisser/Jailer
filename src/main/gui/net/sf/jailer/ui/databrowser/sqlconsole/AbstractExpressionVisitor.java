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
package net.sf.jailer.ui.databrowser.sqlconsole;

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
import net.sf.jsqlparser.statement.select.SubSelect;

/**
 * Abstract {@link ExpressionVisitor}.
 * 
 * @author Ralf Wisser
 */
class AbstractExpressionVisitor implements ExpressionVisitor {

	@Override
	public void visit(NullValue arg0) {

	}

	@Override
	public void visit(Function arg0) {

	}

	@Override
	public void visit(SignedExpression arg0) {

	}

	@Override
	public void visit(JdbcParameter arg0) {

	}

	@Override
	public void visit(JdbcNamedParameter arg0) {

	}

	@Override
	public void visit(DoubleValue arg0) {

	}

	@Override
	public void visit(LongValue arg0) {

	}

	@Override
	public void visit(HexValue arg0) {

	}

	@Override
	public void visit(DateValue arg0) {

	}

	@Override
	public void visit(TimeValue arg0) {

	}

	@Override
	public void visit(TimestampValue arg0) {

	}

	@Override
	public void visit(Parenthesis arg0) {

	}

	@Override
	public void visit(StringValue arg0) {

	}

	@Override
	public void visit(Addition arg0) {

	}

	@Override
	public void visit(Division arg0) {

	}

	@Override
	public void visit(Multiplication arg0) {

	}

	@Override
	public void visit(Subtraction arg0) {

	}

	@Override
	public void visit(AndExpression arg0) {

	}

	@Override
	public void visit(OrExpression arg0) {

	}

	@Override
	public void visit(Between arg0) {

	}

	@Override
	public void visit(EqualsTo arg0) {

	}

	@Override
	public void visit(GreaterThan arg0) {

	}

	@Override
	public void visit(GreaterThanEquals arg0) {

	}

	@Override
	public void visit(InExpression arg0) {

	}

	@Override
	public void visit(IsNullExpression arg0) {

	}

	@Override
	public void visit(LikeExpression arg0) {

	}

	@Override
	public void visit(MinorThan arg0) {

	}

	@Override
	public void visit(MinorThanEquals arg0) {

	}

	@Override
	public void visit(NotEqualsTo arg0) {

	}

	@Override
	public void visit(Column arg0) {

	}

	@Override
	public void visit(SubSelect arg0) {

	}

	@Override
	public void visit(CaseExpression arg0) {

	}

	@Override
	public void visit(WhenClause arg0) {

	}

	@Override
	public void visit(ExistsExpression arg0) {

	}

	@Override
	public void visit(AllComparisonExpression arg0) {

	}

	@Override
	public void visit(AnyComparisonExpression arg0) {

	}

	@Override
	public void visit(Concat arg0) {

	}

	@Override
	public void visit(Matches arg0) {

	}

	@Override
	public void visit(BitwiseAnd arg0) {

	}

	@Override
	public void visit(BitwiseOr arg0) {

	}

	@Override
	public void visit(BitwiseXor arg0) {

	}

	@Override
	public void visit(CastExpression arg0) {

	}

	@Override
	public void visit(Modulo arg0) {

	}

	@Override
	public void visit(AnalyticExpression arg0) {

	}

	@Override
	public void visit(ExtractExpression arg0) {

	}

	@Override
	public void visit(IntervalExpression arg0) {

	}

	@Override
	public void visit(OracleHierarchicalExpression arg0) {

	}

	@Override
	public void visit(RegExpMatchOperator arg0) {

	}

	@Override
	public void visit(JsonExpression arg0) {

	}

	@Override
	public void visit(JsonOperator arg0) {

	}

	@Override
	public void visit(RegExpMySQLOperator arg0) {

	}

	@Override
	public void visit(UserVariable arg0) {

	}

	@Override
	public void visit(NumericBind arg0) {

	}

	@Override
	public void visit(KeepExpression arg0) {

	}

	@Override
	public void visit(MySQLGroupConcat arg0) {

	}

	@Override
	public void visit(RowConstructor arg0) {

	}

	@Override
	public void visit(OracleHint arg0) {

	}

	@Override
	public void visit(TimeKeyExpression arg0) {

	}

	@Override
	public void visit(DateTimeLiteralExpression arg0) {

	}

	@Override
	public void visit(NotExpression arg0) {

	}

	@Override
	public void visit(BitwiseRightShift arg0) {
		
	}

	@Override
	public void visit(BitwiseLeftShift arg0) {
		
	}

	@Override
	public void visit(ValueListExpression arg0) {
		
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
}
