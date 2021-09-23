import net.sf.jsqlparser.parser.ASTNodeAccess;
import net.sf.jsqlparser.parser.CCJSqlParser;
import net.sf.jsqlparser.parser.ParseException;
import net.sf.jsqlparser.parser.StringProvider;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.StatementVisitorAdapter;
import net.sf.jsqlparser.statement.select.PlainSelect;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.select.SelectExpressionItem;
import net.sf.jsqlparser.statement.select.SelectItem;
import net.sf.jsqlparser.statement.select.SelectItemVisitorAdapter;
import net.sf.jsqlparser.statement.select.SelectVisitorAdapter;

/**
 * @see https://github.com/JSQLParser/JSqlParser/issues/1339
 */
public class Issue1339 {

	public static void main(String[] args) throws ParseException {
		parse("Select x, 1 + 2 From T");
		parse("Select * From Values(1)");
		parse("Select * From T Where x is null");
		parse("Select * From T Where x + y");
		parse("Select * From T Where x + y > 100 or x = null");
	}

	private static void parse(String sql) throws ParseException {
		CCJSqlParser parser = new CCJSqlParser(new StringProvider(sql));
		Statement statement = parser.Statement();
		statement.accept(new StatementVisitorAdapter() {
			@Override
			public void visit(Select select) {
				select.getSelectBody().accept(new SelectVisitorAdapter() {
					@Override
					public void visit(PlainSelect plainSelect) {
						for (SelectItem si: plainSelect.getSelectItems()) {
							si.accept(new SelectItemVisitorAdapter() {
								@Override
								public void visit(SelectExpressionItem selectExpressionItem) {
									check(selectExpressionItem.getExpression());
								}
							});
						}
						check(plainSelect.getFromItem());
						check(plainSelect.getWhere());
					}
				});
			}
			private void check(Object o) {
				if (o != null) {
					if (o instanceof ASTNodeAccess) {
						if (((ASTNodeAccess) o).getASTNode() == null) {
							System.out.println("'" + o + "': is ASTNodeAccess but ASTNode is null");
						}
					} else {
						System.out.println("'" + o + "': is no ASTNodeAccess. It's a " + o.getClass().getName());
					}
				}
			}
		});
	}

}
