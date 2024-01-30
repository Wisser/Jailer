/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser.metadata;

import java.awt.FlowLayout;
import java.lang.reflect.Method;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.ui.UIUtil;

/**
 * Information about the database.
 * 
 * @author Ralf Wisser
 */
public class MDDatabase extends MDGeneric {

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MDDatabase.class);

	private final DataModel dataModel;

	/**
	 * Constructor.
	 * 
	 * @param name the object name
	 */
	public MDDatabase(String name, MetaDataSource metaDataSource, DataModel dataModel, ExecutionContext executionContext) {
		super(name, metaDataSource);
		this.dataModel = dataModel;
	}

	/**
	 * Gets the render of the database object.
	 * 
	 * @return render of the database object
	 */
	@SuppressWarnings("deprecation")
	@Override
	public JComponent createRender(Session session, ExecutionContext executionContext) throws Exception {
		String[] names = new String[] {
	        "getURL",
	        "getUserName",
	        "isReadOnly",
	        "getDatabaseMajorVersion",
	        "getDatabaseMinorVersion",
	        "getDatabaseProductName",
	        "getDatabaseProductVersion",
	        "getDriverName",
	        "getDriverVersion",
	        "getDriverMajorVersion",
	        "getDriverMinorVersion",
	        "getIdentifierQuoteString",
	        "getCatalogSeparator",
	        "getCatalogTerm",
	        "getSchemaTerm",
	        "getProcedureTerm",
	        "getJDBCMajorVersion",
	        "getJDBCMinorVersion",
	        "getResultSetHoldability",
	        "supportsNamedParameters",
	        "supportsMultipleOpenResults",
	        "supportsGetGeneratedKeys",
	        "getSQLStateType",
	        "locatorsUpdateCopy",
	        "supportsStatementPooling",
	        "generatedKeyAlwaysReturned",
	        "getRowIdLifetime",
	        "supportsCatalogsInTableDefinitions",
	        "supportsSchemasInTableDefinitions",
	        "supportsMixedCaseQuotedIdentifiers",
	        "storesUpperCaseQuotedIdentifiers",
	        "storesLowerCaseQuotedIdentifiers",
	        "storesMixedCaseQuotedIdentifiers",
	        "supportsAlterTableWithAddColumn",
	        "supportsAlterTableWithDropColumn",
	        "supportsTableCorrelationNames",
	        "supportsDifferentTableCorrelationNames",
	        "supportsANSI92IntermediateSQL",
	        "supportsIntegrityEnhancementFacility",
	        "supportsSchemasInProcedureCalls",
	        "supportsSchemasInIndexDefinitions",
	        "supportsSchemasInPrivilegeDefinitions",
	        "supportsCatalogsInIndexDefinitions",
	        "supportsCatalogsInPrivilegeDefinitions",
	        "supportsSubqueriesInComparisons",
	        "supportsSubqueriesInQuantifieds",
	        "supportsOpenCursorsAcrossCommit",
	        "supportsOpenCursorsAcrossRollback",
	        "supportsOpenStatementsAcrossCommit",
	        "supportsOpenStatementsAcrossRollback",
	        "getDefaultTransactionIsolation",
	        "supportsDataManipulationTransactionsOnly",
	        "dataDefinitionCausesTransactionCommit",
	        "dataDefinitionIgnoredInTransactions",
	        "autoCommitFailureClosesAllResultSets",
	        "supportsStoredFunctionsUsingCallSyntax",
	        "supportsCatalogsInProcedureCalls",
	        "storesMixedCaseIdentifiers",
	        "storesUpperCaseIdentifiers",
	        "supportsSavepoints",
	        "supportsMultipleResultSets",
	        "allProceduresAreCallable",
	        "allTablesAreSelectable",
	        "nullsAreSortedHigh",
	        "nullsAreSortedLow",
	        "nullsAreSortedAtStart",
	        "nullsAreSortedAtEnd",
	        "usesLocalFiles",
	        "usesLocalFilePerTable",
	        "supportsMixedCaseIdentifiers",
	        "storesLowerCaseIdentifiers",
	        "getSearchStringEscape",
	        "getExtraNameCharacters",
	        "supportsColumnAliasing",
	        "nullPlusNonNullIsNull",
	        "supportsConvert",
	        "supportsExpressionsInOrderBy",
	        "supportsOrderByUnrelated",
	        "supportsGroupBy",
	        "supportsGroupByUnrelated",
	        "supportsGroupByBeyondSelect",
	        "supportsLikeEscapeClause",
	        "supportsMultipleTransactions",
	        "supportsNonNullableColumns",
	        "supportsMinimumSQLGrammar",
	        "supportsCoreSQLGrammar",
	        "supportsExtendedSQLGrammar",
	        "supportsANSI92EntryLevelSQL",
	        "supportsANSI92FullSQL",
	        "supportsOuterJoins",
	        "supportsFullOuterJoins",
	        "supportsLimitedOuterJoins",
	        "isCatalogAtStart",
	        "supportsPositionedDelete",
	        "supportsPositionedUpdate",
	        "supportsSelectForUpdate",
	        "supportsSubqueriesInExists",
	        "supportsSubqueriesInIns",
	        "supportsCorrelatedSubqueries",
	        "supportsUnion",
	        "supportsUnionAll",
	        "getMaxBinaryLiteralLength",
	        "getMaxCharLiteralLength",
	        "getMaxColumnNameLength",
	        "getMaxColumnsInGroupBy",
	        "getMaxColumnsInIndex",
	        "getMaxColumnsInOrderBy",
	        "getMaxColumnsInSelect",
	        "getMaxColumnsInTable",
	        "getMaxConnections",
	        "getMaxCursorNameLength",
	        "getMaxIndexLength",
	        "getMaxSchemaNameLength",
	        "getMaxProcedureNameLength",
	        "getMaxCatalogNameLength",
	        "getMaxRowSize",
	        "doesMaxRowSizeIncludeBlobs",
	        "getMaxStatementLength",
	        "getMaxStatements",
	        "getMaxTableNameLength",
	        "getMaxTablesInSelect",
	        "getMaxUserNameLength",
	        "supportsTransactions",
	        "supportsBatchUpdates",
	        "supportsSchemasInDataManipulation",
	        "supportsCatalogsInDataManipulation",
	        "supportsStoredProcedures"
	    };

		JPanel panel = new JPanel();
		panel.setLayout(new javax.swing.BoxLayout(panel, javax.swing.BoxLayout.LINE_AXIS));
		
		JLabel loading = new JLabel("loading...");
		panel.add(loading);
		
        Thread thread = new Thread(() -> {
			try {
				List<Object[]> rowList = new ArrayList<Object[]>();
				Session.setThreadSharesConnection();
	    		DatabaseMetaData md = getMetaDataSource().getSession().getMetaData();
	            for (String name : names) {
	            	try {
	        	        Method m = md.getClass().getMethod(name);
						String displayName = name.startsWith("get") ? name.substring(3) : name;
						displayName = displayName.substring(0, 1).toUpperCase() + displayName.substring(1);
						rowList.add(new Object[] { displayName, m.invoke(md) });
	            	} catch (Throwable t) {
	    				// ignore
	    			}
	            }
	            try {
	        	    ResultSet rs = md.getTableTypes();
	        	    List<String> types = new ArrayList<>();
	        	    while (rs.next()) {
	        	    	types.add(rs.getString(1));
	        	    }
	        	    rs.close();
	        	    rowList.add(new Object[] { "TableTypes", types.stream().collect(Collectors.joining(", "))});
	            } catch (Throwable t) {
    				// ignore
    			}
	            UIUtil.invokeLater(() -> {
	            	MemorizedResultSet rs = new MemorizedResultSet(rowList, 2, new String[] { "Property", "Value" }, new int[] { Types.VARCHAR, Types.VARCHAR });
		            try {
		            	loading.setVisible(false);
						panel.add(new ResultSetRenderer(rs, getName(), dataModel, getMetaDataSource().getSession(), executionContext));
					} catch (SQLException e) {
						// ignore
					}
	            });
			} catch (Throwable t) {
				loading.setText("Error: " + t.getMessage());
				logger.info("error", t);
			}
		});
        thread.setDaemon(true);
        thread.start();

        return panel;
	}

}
