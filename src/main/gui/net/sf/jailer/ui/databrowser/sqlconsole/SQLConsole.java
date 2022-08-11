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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.WeakHashMap;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.LineBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableModel;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Segment;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;
import org.fife.ui.rtextarea.SmartHighlightPainter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.JComboBox2;
import net.sf.jailer.ui.QueryBuilderDialog;
import net.sf.jailer.ui.QueryBuilderDialog.Relationship;
import net.sf.jailer.ui.SessionForUI;
import net.sf.jailer.ui.StringSearchPanel.StringSearchDialog;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.associationproposer.AssociationProposerView;
import net.sf.jailer.ui.databrowser.BrowserContentCellEditor;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.LoadJob;
import net.sf.jailer.ui.databrowser.BrowserContentPane.RowsClosure;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.databrowser.Desktop;
import net.sf.jailer.ui.databrowser.Desktop.FindClosureContext;
import net.sf.jailer.ui.databrowser.Desktop.RowBrowser;
import net.sf.jailer.ui.databrowser.Desktop.RunnableWithPriority;
import net.sf.jailer.ui.databrowser.FullTextSearchPanel;
import net.sf.jailer.ui.databrowser.Reference;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.SQLValue;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel.OutlineInfo;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.metadata.ResultSetRenderer;
import net.sf.jailer.ui.databrowser.whereconditioneditor.WCTypeAnalyser;
import net.sf.jailer.ui.databrowser.whereconditioneditor.WCTypeAnalyser.Result;
import net.sf.jailer.ui.databrowser.whereconditioneditor.WhereConditionEditorPanel;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.FixedColumnTable;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * SQL Console.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class SQLConsole extends javax.swing.JPanel {
	
	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);

    private static final int MAX_TAB_COUNT = 8;
    private static final int MAX_HISTORY_SIZE = 100;

    private Session session;
    MetaDataSource metaDataSource;
    private RSyntaxTextAreaWithSQLSyntaxStyle editorPane;
	private final MetaDataBasedSQLCompletionProvider provider;
    private final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
    private final Reference<DataModel> datamodel;
    private final ExecutionContext executionContext;
    private final List<String> history = new ArrayList<String>();
    private final AtomicBoolean running = new AtomicBoolean(false);
    private final AtomicBoolean updatingStatus = new AtomicBoolean(false);
    private final ImageIcon scaledCancelIcon;
    private final ImageIcon scaledExplainIcon;
    private final SQLPlusSupport sqlPlusSupport = new SQLPlusSupport();
	private File file;
	private JMenuItem menuItemToggle;
	private JMenuItem menuItemToSingleLine;
	private JMenuItem menuItemSubstituteVariables;
	private JMenuItem menuItemAnalyse;
	private int initialTabbedPaneSelection = 0;
	private List<? extends SortKey> initialSortKeys = null;
	private Map<String, FullTextSearchPanel> fullTextSearchPanel = new WeakHashMap<String, FullTextSearchPanel>();
	private Point initialRowsTablesPos;
	private Point initialTextTablesPos;
	private Point initialColumnsTablesPos;
	private Set<Integer> initialFoundColumn = new HashSet<Integer>();
	private String initialSortKeysSql = null;
	private int initialTabbedPaneSelectionLoc = -1;

	private final String IGNORED_STATEMENTS = "(\\s*/\\s*)";

	/**
	 * Stops the consumer thread.
	 */
    private final Runnable STOP = new Runnable() {
		@Override
		public void run() {
			// nothing to do
		}
	};

	private Map<MDSchema, MDSchema> triedToLoad = new WeakHashMap<MDSchema, MDSchema>();

	/**
     * Creates new form SQLConsole
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public SQLConsole(Session session, MetaDataSource metaDataSource, Reference<DataModel> datamodel, ExecutionContext executionContext) throws SQLException {
        this.session = session;
        this.metaDataSource = metaDataSource;
        this.datamodel = datamodel;
        this.executionContext = executionContext;
        initComponents();
        initMenuItems();
        historyComboBox.setMaximumRowCount(25);
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
        jPanel5.add(historyComboBox, gridBagConstraints);

        updateResultUI();
        
        this.editorPane = new RSyntaxTextAreaWithSQLSyntaxStyle(true, true) {
        	{
        		setBracketMatchingEnabled(true);
        	}
        	
        	@Override
			protected boolean canExplain() {
				return SQLConsole.this.canExplain();
			}
            @Override
            protected void runBlock() {
            	if (this.hasFocus()) {
            		jTabbedPane1.grabFocus();
            		this.grabFocus();
            	}
                executeSelectedStatements(false, null, true);
            }
            @Override
            protected void explainBlock() {
                executeSelectedStatements(true, null, true);
            }
            @Override
            protected void runAll() {
                executeAllStatements();
            }
            @Override
            public void updateMenuItemState() {
                updateMenuItemState(!running.get(), !running.get());
            }
            @Override
            protected void selectTable(MDTable mdTable) {
                SQLConsole.this.selectTable(mdTable);
            }
            @Override
        	protected void updateMenuItems(boolean isTextSelected) {
            	SQLConsole.this.updateMenuItems(isTextSelected);
        	}
            @Override
            protected MDTable getSelectedTable() {
                Document doc = getDocument();

                int dot = getCaretPosition();
                int start = getLineStartOffsetOfCurrentLine();
                int lineEndOffsetOfCurrentLine = getLineEndOffsetOfCurrentLine();
                int len = lineEndOffsetOfCurrentLine - start;
                Segment seg = new Segment();
                try {
                    doc.getText(start, len, seg);
                } catch (BadLocationException ble) {
                    logger.info("error", ble);
                    return null;
                }

                start = dot - getLineStartOffsetOfCurrentLine();
                String segment = seg.toString();
                char ch = start < segment.length()? segment.charAt(start) : ' ';
                while (start<segment.length() && (Character.isLetterOrDigit(ch) || ch == '"' || ch == '`'|| ch == '_')) {
                    start++;
                    ch = start < segment.length()? segment.charAt(start) : ' ';
                }

                String line = seg.toString().substring(0, start);

                String reIdentifier = "(?:[\"][^\"]+[\"])|(?:[`][^`]+[`])|(?:['][^']+['])|(?:[\\w]+)";

                Pattern pattern = Pattern.compile("(?:(" + reIdentifier + ")\\s*\\.\\s*)?(" + reIdentifier + ")$");
                Matcher matcher = pattern.matcher(line);
                if (matcher.find()) {
                    MDSchema schema;
                    if (matcher.group(1) != null) {
                        schema = SQLConsole.this.metaDataSource.find(matcher.group(1));
                        if (schema == null) {
                            return null;
                        }
                    } else {
                        schema = SQLConsole.this.metaDataSource.getDefaultSchema();
                        if (schema == null) {
                            return null;
                        }
                    }
                    String tableName = matcher.group(2);
                    if (tableName != null) {
	                    if (schema.isLoaded()) {
	                    	return schema.find(tableName);
	                    } else {
	                    	if (!triedToLoad.containsKey(schema)) {
	            				triedToLoad.put(schema, schema);
	            				schema.loadTables(true, null, null, null);
	            				for (int i = 0; i < 10; ++i) {
	            					if (schema.isLoaded()) {
	            						return schema.find(tableName);
		                    		}
		                    		try {
		                    			Thread.sleep(100);
		                    		} catch (InterruptedException e) {
		                    			// ignore
		                    		}
		             			}
	                    	}
	                    }
                    }
                }
                return null;
            }

        	@Override
        	protected void appendPopupMenu(JPopupMenu menu) {
        		menu.add(new JSeparator());
        		menu.add(menuItemToSingleLine);
        		menu.add(menuItemToggle);
        		menu.add(menuItemSubstituteVariables);
        		menu.add(new JSeparator());
        		menu.add(menuItemAnalyse);
        	}

        };

        clearButton.setText(null);
        clearButton.setToolTipText("Clear Console");
        clearButton.setIcon(UIUtil.scaleIcon(clearButton, clearIcon));
        clearButton.setEnabled(editorPane.getDocument().getLength() > 0);
        editorPane.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				clearButton.setEnabled(editorPane.getDocument().getLength() > 0);
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				clearButton.setEnabled(editorPane.getDocument().getLength() > 0);
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				clearButton.setEnabled(editorPane.getDocument().getLength() > 0);
			}
		});

        historyComboBox.setRenderer(new DefaultListCellRenderer() {
        	ListCellRenderer renderer = historyComboBox.getRenderer();
    		@Override
			public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                 if (index == 0) {
                	 return new JPanel(null);
                 }
                 Object shortValue = value;
                 if (value instanceof String) {
                     shortValue = shortSQL((String) value, 120);
                 }
                 Component c = renderer.getListCellRendererComponent(list, shortValue, index, isSelected, cellHasFocus);
                 if (c instanceof JLabel) {
                     if (value instanceof String && value.toString().length() > 0) {
                         ((JLabel) c).setToolTipText(UIUtil.toHTML(value.toString(), 100));
                     } else {
                         ((JLabel) c).setToolTipText(null);
                     }
                 }
                 return c;
             }
        });
        restoreHistory();

        provider = new MetaDataBasedSQLCompletionProvider(session, metaDataSource) {
            @Override
			protected String prepareStatementForAliasAnalysis(String statement) {
            	return sqlPlusSupport.replaceVariables(statement, null);
            }
        };
        new SQLAutoCompletion(provider, editorPane);

        RTextScrollPane jScrollPane = new RTextScrollPane();
        jScrollPane.setViewportView(editorPane);
        if (jScrollPane.getGutter() != null) {
        	jScrollPane.getGutter().setIconRowHeaderInheritsGutterBackground(true);
        }
        editorPane.setGutter(jScrollPane.getGutter());
        consoleContainerPanel.add(jScrollPane);
        jScrollPane.setLineNumbersEnabled(true);
        jScrollPane.setIconRowHeaderEnabled(true);

        runSQLButton.setAction(editorPane.runBlock);
        runnAllButton.setAction(editorPane.runAll);
        explainButton.setAction(editorPane.explain);

        runSQLButton.setText(null);
        runnAllButton.setText(null);
        explainButton.setText(null);
        cancelButton.setText(null);
        explainButton.setToolTipText("Show Query Execution Plan ");
        
        runSQLButton.setIcon(UIUtil.scaleIcon(this, runIcon));
        runnAllButton.setIcon(UIUtil.scaleIcon(this, runAllIcon));
        runSQLButton.setToolTipText("Run SQL Statement - Ctrl-Enter");
        runnAllButton.setToolTipText("Run SQL Script - Alt-Enter");

        scaledCancelIcon = UIUtil.scaleIcon(this, cancelIcon);
        cancelButton.setIcon(scaledCancelIcon);

        scaledExplainIcon = UIUtil.scaleIcon(this, explainIcon);
        explainButton.setIcon(scaledExplainIcon);

        limitComboBox.setModel(new DefaultComboBoxModel(DataBrowser.ROW_LIMITS));
        limitComboBox.setSelectedItem(1000);

        resetStatus();
        statusLabel.setVisible(true);

        editorPane.addCaretListener(new CaretListener() {
            @Override
            public void caretUpdate(CaretEvent e) {
                updateOutline(false);
            }
        });

        initialContentSize = editorPane.getDocument().getLength();
        initialContentHash = editorPane.getText().hashCode();
        editorPane.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				checkDirty();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				checkDirty();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				checkDirty();
			}
		});

        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
            	Runnable action = null;
            	do {
                    try {
                    	action = queue.take();
                        action.run();
                    } catch (Throwable t) {
                        logger.info("error", t);
                    }
                } while (action != STOP);
            }
        }, "SQLConsole-" + (threadNum++));
        UIUtil.startDemon(thread);
    }

	private void updateResultUI() {
		if (UIUtil.plaf == PLAF.FLAT) {
	        if (jTabbedPane1.getTabCount() > 0) {
	        	jPanel3.setBorder(null);
	        } else {
	        	jPanel3.setBorder(new LineBorder(UIUtil.FLAT_BORDER_COLOR));
	        }
        }
	}

	protected void initMenuItems() {
		JMenuItem item = new JMenuItem("Toggle Line Continuation");
		item.setEnabled(false);
		menuItemToggle = item;
		item.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				toggleLineContinuation();
			}
		});
		item.setToolTipText(
				"<html>Adds (or remove) line-continuation-character ('\\') <br>" +
				" to each line terminated by ';' <br>"
				+ "(allowing you to execute PL/SQL code)");

		item = new JMenuItem("To Single Line");
		item.setEnabled(false);
		menuItemToSingleLine = item;
		item.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				toSingleLine();
			}
		});

		item = new JMenuItem("Substitute Variables");
		item.setEnabled(false);
		menuItemSubstituteVariables = item;
		item.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				substituteVariables();
			}
		});
		item.setToolTipText(
				"<html>Substitutes variables <i>(&amp;VAR[.])</i> with corresponding values. <br><br>\n" +
				"<b>Statements:</b>\n" +
				"<table>\n" +
				"<tr><td><b>&nbsp;DEFINE VAR=\"VALUE\"</b></td><td>&nbsp;&nbsp;&nbsp;</td><td>Assigns a value to variable VAR</td></tr>\n" +
				"<tr><td><b>&nbsp;DEFINE</b></td><td></td><td>Lists all variables</td></tr>\n" +
				"<tr><td><b>&nbsp;&amp;VAR</b>&nbsp;&nbsp;or&nbsp;&nbsp;<b>&nbsp;&amp;VAR.</b></td><td></td><td>Variable substitution (inside other statements)</td></tr>\n" +
				"</table>");
		item = new JMenuItem("Analyze SQL");
		item.setEnabled(false);
		menuItemAnalyse = item;
		item.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				analyzeSQL();
			}
		});
		item.setToolTipText("Analyzes selected SQL and proposes association definitions.");
	}

	private void updateMenuItems(boolean isTextSelected) {
    	if (menuItemToggle != null) {
    		menuItemToggle.setEnabled(isTextSelected);
    	}
    	if (menuItemToSingleLine != null) {
    		menuItemToSingleLine.setEnabled(isTextSelected);
    	}
    	if (menuItemSubstituteVariables != null) {
    		menuItemSubstituteVariables.setEnabled(isTextSelected);
    	}
    	if (menuItemAnalyse != null) {
    		menuItemAnalyse.setEnabled(isTextSelected);
    	}
	}

	private static int threadNum = 1;

    private boolean canExplain() {
		return metaDataSource.getSession().dbms.getExplainQuery() != null && !metaDataSource.getSession().dbms.getExplainQuery().isEmpty();
	}

    private AtomicBoolean pending = new AtomicBoolean(false);
    private AtomicBoolean stopped = new AtomicBoolean(false);
    private String prevSql = null;
    private int prevCaretPos;

	protected Boolean doSync;

    /**
     * Update of outline of statement under caret and history after switching from another console to this.
     */
    public void update() {
    	updateOutline(false);
    	restoreHistory();
    }

    /**
     * Update of outline of statement under caret.
     */
    private void updateOutline(boolean checkPrevSql) {
        if (!pending.get()) {
            Pair<Integer, Integer> loc = editorPane.getCurrentStatementLocation(true, true, null, false);
            String sql = editorPane.getText(loc.a, loc.b, true);
            if (sql.trim().isEmpty()) {
                loc = editorPane.getCurrentStatementLocation(true, true, null, true);
                sql = editorPane.getText(loc.a, loc.b, true);
            }
            if (sql.trim().isEmpty()) {
            	hightlight(0, 0);
            } else {
            	try {
					hightlight(editorPane.getLineStartOffset(loc.a), editorPane.getLineEndOffset(loc.b));
				} catch (BadLocationException e) {
					hightlight(0, 0);
				}
            }
            if (checkPrevSql && sql.equals(prevSql) && editorPane.getCaretPosition() == prevCaretPos) {
                return;
            }
            prevSql = sql;
            prevCaretPos = editorPane.getCaretPosition();

            try {
                updateOutline(sql, editorPane.getLineStartOffset(loc.a));
            } catch (Exception e1) {
                logger.info("error", e1);
                return;
            }

            if (sql.length() > 20000) {
                stopped.set(false);
                pending.set(true);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            Thread.sleep(2000);
                        } catch (InterruptedException e) {
                        }
                        pending.set(false);
                        if (!stopped.get()) {
                            UIUtil.invokeLater(new Runnable() {
                                @Override
                                public void run() {
                                    updateOutline(true);
                                }
                            });
                        }
                    }
                }).start();
            } else {
                stopped.set(true);
            }
        }
    }

    protected String shortSQL(String sql, int maxLength) {
        sql = sql.trim().replaceAll("\\s+", " ");
        if (sql.length() > maxLength) {
            sql = sql.replaceFirst("^(?is)(\\bselect\\b).........*?(\\bfrom\\b)(.*)$", "$1 ... $2$3");
        }
        if (sql.length() > maxLength) {
            sql = sql.substring(0, maxLength) + "...";
        }
        return sql;
    }

    private void resetStatus() {
        statusLabel.setVisible(false);
        cancelButton.setEnabled(false);
    }

    /**
     * Executes a block of SQL statements (each statement separated by a ';' at the end of the line).
     *
     * @param sqlBlock the sql block
     * @param location location of the block in the console
     * @param emptyLineSeparatesStatements
     * @param locFragmentOffset location of statement fragment, if any
     * @param explain
     * @param tabContentPanel the panel to show result (option)
     */
    protected void executeSQLBlock(final String sqlBlock, final Pair<Integer, Integer> location, final boolean emptyLineSeparatesStatements, final Pair<Integer, Integer> locFragmentOffset, final boolean explain, final TabContentPanel tabContentPanel) {
        if (!running.get()) {
            int lineStartOffset = -1;
            try {
                if (location != null) {
                    lineStartOffset = editorPane.getLineStartOffset(location.a);
                }
            } catch (BadLocationException e) {
                lineStartOffset = -1;
            }
            disableLastErrorTab();
            final Pair<Integer, Integer> caretDotMark = new Pair<Integer, Integer>(editorPane.getCaret().getDot(), editorPane.getCaret().getMark());
            final int finalLineStartOffset = lineStartOffset;
            queue.add(new Runnable() {
                @Override
                public void run() {
                    running.set(true);
                    updatingStatus.set(false);
                    UIUtil.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            editorPane.updateMenuItemState();
                        }
                    });
                    Status status = new Status();
                    status.location = location;
                    status.linesExecuted = 0;
                    status.linesExecuting = 0;
                    status.setRunning(true);
                    int lineStartOffset = finalLineStartOffset;
                    try {
                        Pattern pattern;
                        if (emptyLineSeparatesStatements) {
                            pattern = Pattern.compile("(?:(;\\s*(\\n\\r?|$))|(\\n\\r?([ \\t\\r]*\\n\\r?)+))", Pattern.DOTALL);
                        } else {
                            pattern = Pattern.compile("(?:(;\\s*(\\n\\r?|$)))", Pattern.DOTALL);
                        }

                        Matcher matcher = pattern.matcher(sqlBlock);
                        boolean result = matcher.find();
                        StringBuffer sb = new StringBuffer();
                        if (result || locFragmentOffset != null) {
                            do {
                                String sql;
                                String pureSql;
                                if (locFragmentOffset != null) {
                                    sql = sqlBlock;
                                    pureSql = sqlBlock;
                                } else {
                                    sb.setLength(0);
                                    matcher.appendReplacement(sb, "");
                                    pureSql = sb.toString();
                                    sb.append(matcher.group());
                                    sql = sb.toString();
                                }
                                status.linesExecuting += countLines(pureSql);
                                if (sql.trim().length() > 0) {
                                    executeSQL(pureSql, status, lineStartOffset, explain, tabContentPanel, caretDotMark);
                                    if (status.failed) {
                                        if (locFragmentOffset != null) {
                                            if (status.errorPositionIsKnown) {
                                                try {
                                                    status.errorPosition += locFragmentOffset.a - editorPane.getLineStartOffset(editorPane.getLineOfOffset(locFragmentOffset.a));
                                                } catch (BadLocationException e) {
                                                    logger.info("error", e);
                                                }
                                            }
                                        }
                                        break;
                                    }
                                }
                                if (lineStartOffset >= 0) {
                                    lineStartOffset += sql.length();
                                }
                                status.linesExecuted += countLines(sql) - 1;
                                if (locFragmentOffset != null) {
                                    pattern = Pattern.compile("(\\n\\s*)$", Pattern.DOTALL);
                                    matcher = pattern.matcher(sql);
                                    if (!matcher.find()) {
                                        status.linesExecuted++;
                                    }
                                    break;
                                }
                                String terminator = matcher.group(1);
                                if (terminator != null && !terminator.contains("\n")) { // ';' without nl
                                    status.linesExecuted++;
                                }
                                status.linesExecuting = status.linesExecuted;
                                result = matcher.find();
                            } while (result);
                        }
                        if (!status.failed && locFragmentOffset == null) {
                            sb.setLength(0);
                            matcher.appendTail(sb);
                            String sbToString = sb.toString();
                            String sql = sbToString;
                            if (sql.trim().length() > 0) {
                                status.linesExecuting += countLines(sql);
                                executeSQL(sql, status, lineStartOffset, explain, tabContentPanel, caretDotMark);
                                if (!status.failed) {
                                    status.linesExecuted = status.linesExecuting;
                                }
                            }
                        }
                        if (status.numStatements <= 7) {
                            storeHistory();
                        } else {
                            restoreHistory();
                        }
                    } finally {
                        status.setRunning(false);
                        running.set(false);
                        status.updateView(true);
                        UIUtil.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                editorPane.updateMenuItemState(true, false);
                            }
                        });
                    }
                }

                private int countLines(String line) {
                    int lines = 1;
                    for (int i = 0; i < line.length(); ++i) {
                        if (line.charAt(i) == '\n') {
                            ++lines;
                        }
                    }
                    return lines;
                }
            });
        }
    }

    /**
     * Executes a single SQL statment.
     *
     * @param sql the statement
     * @param status the status to update
     * @param statementStartOffset
     * @param explain
     * @param origTabContentPanel the panel to show result (option)
     */
    private void executeSQL(final String sql, final Status status, int statementStartOffset, final boolean explain, final TabContentPanel origTabContentPanel, final Pair<Integer, Integer> caretDotMark) {
        Statement statement = null;
        ResultSet resultSet = null;
        final Status localStatus = new Status();
        String sqlStatement = null;
        String stmtId = null;
        TreeMap<Integer, Integer> positionOffsets = new TreeMap<Integer, Integer>();
        Connection resetAutoCommitConnection = null;
        Connection connection = null;
        try {
	        connection = session.getConnection();
	        if (!explain && session.dbms != null && session.dbms.equals(DBMS.POSTGRESQL)) {
	            if (connection.getAutoCommit()) {
	            	connection.setAutoCommit(false);
	            	resetAutoCommitConnection = connection;
	            }
	        }
	        status.numStatements++;
            localStatus.numStatements++;
            UISettings.s3++;
            status.updateView(false);
            statement = connection.createStatement();
			if (session.dbms != null) {
				if (session.dbms.getFetchSize() != null) {
					statement.setFetchSize(session.dbms.getFetchSize());
				}
			}
			CancellationHandler.reset(SQLConsole.this);
            CancellationHandler.begin(statement, SQLConsole.this);
            long startTime = System.currentTimeMillis();
            sqlStatement =
            		sql
            		.replaceFirst("(?is)(;\\s*)+$", "")
            		.replaceAll("((?:(?:;(?: |\\t|\\r)*?(?:--[^\\n]*)?))) ?\\\\([ \\t\\r]*\\n)", "$1$2")
            		.replaceAll("((?:\\n(?: |\\t|\\r)*?)) ?\\\\([ \\t\\r]*)(?=\\n)", "$1");
			sqlStatement = sqlPlusSupport.replaceVariables(sqlStatement, positionOffsets);
			status.statement = sqlStatement;
	        boolean loadButtonIsVisible = true;
            boolean hasResultSet;
            boolean hasUpdateCount = true;
            ResultSet sqlPlusResultSet = null;
            if (explain && session.dbms != null) {
            	if (session.dbms.getExplainCreateExplainTable() != null) {
            		Statement createStatement = connection.createStatement();
            		try {
            			createStatement.execute(session.dbms.getExplainCreateExplainTable());
            		} catch (Exception e) {
        				// ignore
        			} finally {
            			try {
            				createStatement.close();
            			} catch (Exception e) {
            				// ignore
            			}
            		}
            	}
            	synchronized (this) {
            		stmtId = "Jailer" + (nextPlanID++ % 8);
				}
            	if (session.dbms.getExplainPrepare() != null && !session.dbms.getExplainPrepare().isEmpty()) {
                	statement.execute(String.format(Locale.ENGLISH, session.dbms.getExplainPrepare(), sqlStatement, stmtId));
                	statement.close();
            	}
                statement = connection.createStatement();
				hasResultSet = statement.execute(String.format(Locale.ENGLISH, session.dbms.getExplainQuery(), sqlStatement, stmtId));
            } else {
            	sqlPlusResultSet = sqlPlusSupport.executeSQLPLusQuery(sqlStatement);
            	if (sqlPlusResultSet != null) {
            		hasResultSet = true;
            		loadButtonIsVisible = false;
            	} else if (sqlPlusSupport.executeSQLPLusStatement(sqlStatement)) {
            		hasUpdateCount = false;
            		hasResultSet = false;
            	} else if (sqlStatement.matches(IGNORED_STATEMENTS)) {
            		hasUpdateCount = false;
            		hasResultSet = false;
            	} else {
            		hasResultSet = executeStatementWithLimit(statement, sqlStatement, session);
            	}
            }
            if (hasResultSet) {
                resultSet = sqlPlusResultSet != null? sqlPlusResultSet : statement.getResultSet();
                ResultSetMetaData metaData = resultSet.getMetaData();
                ResultSet theMetaDataResultSet = null;
                String resultSetType = null;
                try {
                    resultSetType = TabContentPanel.toType(metaData, session, executionContext);
                	theMetaDataResultSet = TabContentPanel.toMetaDataResultSet(metaData, session, executionContext);
				} catch (Throwable e1) {
					logger.info("error", e1);
				}
                final ResultSet metaDataResultSet = theMetaDataResultSet;
                final String finalResultSetType = resultSetType;
				final Integer limit = (Integer) limitComboBox.getSelectedItem();
				Map<Integer, String> sqlColumnExpression = new HashMap<Integer, String>();
				
				List<Table> nfResultTypes = explain || sqlPlusResultSet != null? null : QueryTypeAnalyser.getType(sqlStatement, true, sqlColumnExpression, metaDataSource);
				List<Table> nfResultTypesWOCheck = explain || sqlPlusResultSet != null? null : QueryTypeAnalyser.getType(sqlStatement, false, sqlColumnExpression, metaDataSource);
				Result wcbt = WCTypeAnalyser.getType(sqlStatement, metaDataSource, Quoting.getQuoting(session));

				int columnCount = Math.max(0, metaData.getColumnCount());
				if (wcbt != null && wcbt.table != null && wcbt.table.getColumns().size() != columnCount) {
					wcbt = null;
				}
				Result wcBaseTable = wcbt;
				Table resultType = null;
                if (nfResultTypesWOCheck != null && !nfResultTypesWOCheck.isEmpty()) {
                	for (Table table: nfResultTypesWOCheck) {
                        while (table.getColumns().size() < columnCount) {
                            table.getColumns().add(new net.sf.jailer.datamodel.Column(null, "", 0, -1));
                        }
                    }
                }

                if (nfResultTypes != null && !nfResultTypes.isEmpty()) {
                	List<Column> columnsT0 = nfResultTypes.get(0).getColumns();
                	boolean multiTabResult = false;
                    for (int ti = 1; ti < nfResultTypes.size() && !multiTabResult; ++ti) {
                		List<Column> columns = nfResultTypes.get(ti).getColumns();
                		for (int i = 0; i < columns.size(); ++i) {
                    		Column column = columns.get(i);
                    		if (column.name != null && columnsT0.get(i).name == null) {
                    			multiTabResult = true;
                    			break;
                    		}
                    	}
                	}

                    if (nfResultTypes.size() == 1 || !multiTabResult) {
                        resultType = nfResultTypes.get(0);
                    }
					for (Table table: nfResultTypes) {
                        while (table.getColumns().size() < columnCount) {
                            table.getColumns().add(new net.sf.jailer.datamodel.Column(null, "", 0, -1));
                        }
                    }
                }
                if (resultType != null) {
                	if (resultType.getColumns().size() > columnCount) {
                		// stale meta data
                		resultType = null;
                		nfResultTypes = null;
                	}
                }
                Map<Integer, Table> tabPerIndex = new HashMap<Integer, Table>();
                if (nfResultTypesWOCheck != null && nfResultTypesWOCheck.size() > 1 && datamodel.get() != null) {
                	for (Table table: nfResultTypesWOCheck) {
                		if (table != null) {
	                		List<Column> columns = table.getColumns();
							if (columns != null) {
								for (int i = 0; i < columns.size(); ++i) {
									if (columns.get(i) != null && columns.get(i).name != null) {
										tabPerIndex.put(i, table);
									}
								}
	                		}
                		}
                	}
                }
                final String columnLabels[] = new String[columnCount];
                final String columnLabelsFull[] = new String[columnCount];
                final Color columnHeaderColors[] = new Color[columnCount];
                int a = 10;
                final Color hBG[] = new Color[] {
                   		new Color(0, 0, 255, a + 0),
                   		new Color(255, 60, 0, a + 8),
                		new Color(255, 255, 0, a * 4),
                		new Color(0, 255, 0, a),
                };
                Map<String, Integer> labelCount = new HashMap<String, Integer>();
                Map<String, Integer> labelCounter = new HashMap<String, Integer>();
	            for (int step = 0; step < 3; ++step) {
	                Map<Table, Integer> tableOrd = new HashMap<Table, Integer>();
	                int nextOrd = 0;
	                for (int i = 0; i < columnCount; ++i) {
	                	Table table = tabPerIndex.get(i);
                		String columnLabel = metaData.getColumnLabel(i + 1).replaceAll("\\s+", " ").replaceFirst("^(.{100})...+$", "$1...");
						if (columnLabel == null || columnLabel.matches("(?is:\\s*|\\?column\\?|(^\\(*select\\b.*))")) {
							if (sqlColumnExpression.containsKey(i)) {
								columnLabel = sqlColumnExpression.get(i);
							}
						}
						if (step > 0) {
							if (columnLabel != null) {
								columnLabel = columnLabel.replaceAll("\\s+", " ").replaceFirst("^(.{38})...+$", "$1...");
							}
						}
						String countKey = (table == null? "" : (table.getName() + ".")) + columnLabel;
						Integer cnt = labelCount.get(countKey);
						if (step == 1) {
							if (cnt == null) {
								cnt = 1;
							} else {
								++cnt;
							}
							labelCount.put(countKey, cnt);
						} else if (step == 2) {
							if (cnt != null && cnt > 1) {
								Integer cnter = labelCounter.get(countKey);
								if (cnter == null) {
									cnter = 1;
								} else {
									++cnter;
								}
								labelCounter.put(countKey, cnter);
								columnLabel = "#" + cnter + ": " + columnLabel;
							}
						}
						Column column = JDBCMetaDataBasedModelElementFinder.toColumn(metaData, i + 1, session);
						if (column.length >= 100000 && column.length != Integer.MAX_VALUE || column.precision > 1000) {
							column = new Column(column.name, column.type, 0, -1);
						}
	            		String type = column.toSQL("").substring(column.name.length()).trim();
	            		if (tabPerIndex.isEmpty()) {
	                		columnLabels[i] = "<html><nobr><b>" + columnLabel + "</b><br><font color=\"#808080\">" + type + "</font></html>";
	                	} else {
	                		String bgColor = "0066ff";
	                		if (table != null) {
	                			Integer ord = tableOrd.get(table);
	                			if (ord == null) {
	                				ord = nextOrd++;
	                				tableOrd.put(table, ord);
	                			}
	                			columnHeaderColors[i] = hBG[ord % hBG.length];
	                		}
							String titel = table == null? "" : datamodel.get().getDisplayName(table);
	                		if (titel.length() == 0) {
	                			titel = "&nbsp;";
	                		} else {
	                			titel = UIUtil.toHTMLFragment(titel, 64).replaceFirst("<br>$", "");
	                		}
	                		if (columnLabel == null || columnLabel.length() == 0) {
	                			columnLabel = "&nbsp;";
	                		} else {
	                			columnLabel = UIUtil.toHTMLFragment(columnLabel, 128).replaceFirst("<br>$", "");
	                		}
	                		columnLabels[i] = "<html><nobr><font color=\"#" + bgColor + "\">" + titel + "</font><br><b>" + columnLabel + "</b><br><font color=\"#808080\">" + type + "</font></html>";
	                	}
	                }
	                if (step == 0) {
	                	for (int i = 0; i < columnLabelsFull.length; ++i) {
	                		columnLabelsFull[i] = columnLabels[i];
	                	}
	                }
	            }
                final List<Table> resultTypes = nfResultTypes;
                final MemorizedResultSet metaDataDetails = new MemorizedResultSet(resultSet, limit, session, SQLConsole.this) { // lgtm [java/database-resource-leak]
            		@Override
                	protected Object convertCellContent(ContentSupplier supplier) throws SQLException {
            			Object object = supplier.get();
						boolean isBlob = object instanceof Blob;
						Object lobValue = BrowserContentPane.toLobRender(object);
            			if (lobValue != null) {
							object = supplier.get();
						}
						String smallLob = CellContentConverter.getSmallLob(object, session.dbms, BrowserContentPane.MAXBLOBLENGTH, BrowserContentPane.MAXCLOBLENGTH);
						if (smallLob != null && lobValue != null) {
							final String val = lobValue.toString();
							lobValue = new SQLValue() {
								@Override
								public String getSQLExpression() {
									return smallLob;
								}
								@Override
								public String toString() {
									return isBlob? smallLob : val;
								}
							};
						}
						if (lobValue != null) {
                			return lobValue;
                		}
                		return object;
            		}

            		@Override
            		protected void prepareHook(ResultSetMetaData rmd) throws SQLException {
            			sqlPlusSupport.prepareColumnSubstitution(rmd);
            		}

            		@Override
            		protected void readRowHook(ResultSet resultSet) throws SQLException {
                        try {
                        	sqlPlusSupport.substituteColumns(resultSet);
                        } catch (SQLException e) {
                        	// ignore
                        }
            		}
                };
                resultSet.close();
                long now = System.currentTimeMillis();
                status.hasSelected = true;
                status.timeInMS += (now - startTime);
                localStatus.timeInMS += (now - startTime);
                status.numRowsRead += metaDataDetails.getSize();
                localStatus.numRowsRead += metaDataDetails.getSize();
                localStatus.hasSelected = true;
                if (metaDataDetails.getSize() > limit) {
                    status.limitExceeded = true;
                    localStatus.limitExceeded = true;
                }
                status.updateView(false);
            	status.statement = sqlStatement;
                final String finalSqlStatement = sqlStatement;
                final Table finalResultType = resultType;
                final boolean finalLoadButtonIsVisible = loadButtonIsVisible;

                UIUtil.invokeLater(new Runnable() {
                	TabContentPanel tabContentPanel;
                	@Override
                    public void run() {
                        final ResultContentPane rb = new ResultContentPane(datamodel.get(), wcBaseTable, finalResultType, "", session, null,
                                null, (JFrame) SwingUtilities.getWindowAncestor(SQLConsole.this), new RowsClosure(), false, sql, statementStartOffset, false, executionContext) {
                        	@Override
                        	public void afterReload() {
                        		UIUtil.invokeLater(() -> {
                        			if (initialTextTablesPos != null && tabContentPanel != null) {
                            			if (tabContentPanel.textViewScrollPane.getViewport() != null) {
                            				tabContentPanel.textViewScrollPane.getViewport().setViewPosition(initialTextTablesPos);
                            			}
                            			initialTextTablesPos = null;
                            		}
                        			if (initialRowsTablesPos != null) {
                            			if (rowsTableScrollPane.getViewport() != null) {
                            				rowsTableScrollPane.getViewport().setViewPosition(initialRowsTablesPos);
                            			}
                            			initialRowsTablesPos = null;
                            		}
                            		if (initialColumnsTablesPos != null && tabContentPanel != null) {
                            			if (tabContentPanel.columnsScrollPane.getViewport() != null) {
                            				tabContentPanel.columnsScrollPane.getViewport().setViewPosition(initialColumnsTablesPos);
                            			}
                            			initialColumnsTablesPos = null;
                            		}
                        		});
                        		super.afterReload();
                        	}
                        	@Override
							protected void updateStatementLabel(String sql) {
								tabContentPanel.statementLabel.setToolTipText(UIUtil.toHTML(sql, 100));
		                        String stmt = sql;
		                        stmt = stmt.replaceAll("\\s+", " ");
		                        if (stmt.length() > 300) {
		                            stmt = stmt.substring(0, 300) + "...";
		                        }
		                        tabContentPanel.statementLabel.setText(stmt);
							}
                        };
                        rb.initSecondaryCondition();
                        if (resultTypes != null && resultTypes.size() > 1) {
                            rb.setResultSetType(resultTypes);
                        }
                        rb.setAlternativeColumnLabels(columnLabels);
                        rb.setAlternativeColumnLabelsFull(columnLabelsFull);
                        rb.setColumnHeaderColors(columnHeaderColors);
                        rb.setTableFilterEnabled(wcBaseTable == null && metaDataDetails.getSize() > 1 && metaDataDetails.getSize() <= limit);
                        rb.setStatementForReloading(finalSqlStatement);
                        metaDataDetails.reset();
                        LoadJob loadJob = rb.newLoadJob(metaDataDetails, limit);
                        loadJob.run();
                        JComponent rTabContainer = rb.getRowsTableContainer();
                        metaDataDetails.reset();
						JComponent metaDataRenderer = null;
		                try {
							metaDataRenderer = new ResultSetRenderer(metaDataResultSet, null, datamodel.get(), session, executionContext);
						} catch (Throwable e1) {
							logger.info("error", e1);
						}
						tabContentPanel =
                        		new TabContentPanel(rb.rowsCount,
                        				metaDataRenderer,
                        				finalResultSetType,
                        				explain,
                        				origTabContentPanel == null? null : origTabContentPanel.shimPanel,
                        				caretDotMark,
                        				rb.rowColumnTypes);
                        tabContentPanel.contentPanel.add(rTabContainer);

                        rb.setCurrentRowsTable(new Reference<JTable>() {
                        	public JTable get() {
                        		return tabContentPanel.tabbedPane.getSelectedComponent() == tabContentPanel.columnsPanel? columnsTable : rb.rowsTable;
                        	}
                        });
                        rb.setCurrentRowsSorted(new Reference<Boolean>() {
                        	public Boolean get() {
                        		return rb.sortColumnsCheckBox.isSelected();
                        	}
                        });

                        rb.sortColumnsCheckBox.setVisible(true);
                        rb.sortColumnsPanel.setVisible(false);
                        rb.findColumnsPanel.setVisible(false);
                        rb.statusStrutLabel.setVisible(false);
                        rb.sortColumnsCheckBox.setText(rb.sortColumnsCheckBox.getText().trim());
                        JToggleButton findButton = new JToggleButton((String) null);
                        findButton.setToolTipText("Enter additional search criteria to further refine the result.");
                        findButton.setEnabled(rb.wcBaseTable != null);
                        if (!SessionForUI.isWCEditorSupported(session)) {
                        	findButton.setToolTipText("<html>" + findButton.getToolTipText() + "<br>"
                					+ "<b>Condition editor not supported for DBMS \"" + (session.dbms == null? null : session.dbms.getId()) + "\"</b></html>");
                        	findButton.setEnabled(false);
                        }
                        findButton.setIcon(UIUtil.scaleIcon(findButton, searchIcon));
                        findButton.setFocusable(false);
                        findButton.addActionListener(e -> {
                        	if (findButton.isSelected() && wcBaseTable != null) {
                        		Set<Integer> pks = rb.getPkColumnsConsole();
                        		if (pks != null) {
                        			List<Column> pkList = new ArrayList<>();
									pks.forEach(i -> {
                        				if (i < wcBaseTable.table.getColumns().size()) {
                        					pkList.add(wcBaseTable.table.getColumns().get(i));
                        				}
                        			});
									PrimaryKey primaryKey = new PrimaryKey(pkList, false);
									Table oTable = wcBaseTable.table;
                        			wcBaseTable.table = new Table(oTable.getName(), primaryKey, false, false);
                        			wcBaseTable.table.setColumns(oTable.getColumns());
                        		}
                        		Point p = new Point(-32, findButton.getHeight());
                        		SwingUtilities.convertPointToScreen(p, findButton);
                            	rb.openConditionEditor(p, -1, () -> {
                            		findButton.setSelected(false);
                            	});
                        	}
                        });
						tabContentPanel.controlsContainer.add(findButton);
						JButton syncButton = new JButton((String) null);
                        syncButton.setEnabled(rb.wcBaseTable != null);
                        if (doSync == null) {
        					doSync = file == null;
                        }
                        tabContentPanel.doSync = rb.doSync = doSync;
                        
                        syncButton.setToolTipText("Synchronize the additional search criteria with the query in the SQL console.");
                        
                        syncButton.setIcon(tabContentPanel.doSync? UIUtil.scaleIcon(syncButton, syncIcon) : UIUtil.scaleIcon(syncButton, nosyncIcon));
                		syncButton.setFocusable(false);
                        syncButton.addActionListener(e -> {
                        	if (wcBaseTable != null) {
                        		tabContentPanel.doSync = doSync = !tabContentPanel.doSync;
                        		rb.setSync(tabContentPanel.doSync);
                        		syncButton.setIcon(tabContentPanel.doSync? UIUtil.scaleIcon(syncButton, syncIcon) : UIUtil.scaleIcon(syncButton, nosyncIcon));
                        	}
                        });
                        tabContentPanel.controlsContainer.add(syncButton);
                        tabContentPanel.controlsContainer.add(new javax.swing.JToolBar.Separator());
                        tabContentPanel.controlsContainer.add(rb.sortColumnsCheckBox);
                        JToggleButton findColumnsButton = new JToggleButton(rb.findColumnsLabel.getText());
                        findColumnsButton.setFocusable(false);
                        findColumnsButton.setIcon(UIUtil.scaleIcon(findColumnsButton, searchCIcon));
                        findColumnsButton.addActionListener(e -> {
                        	if (findColumnsButton.isSelected()) {
								Point point = new Point(0, findColumnsButton.getHeight());
								SwingUtilities.convertPointToScreen(point, findColumnsButton);
								rb.findColumns((int) point.getX(), (int) point.getY(), rb.currentRowsTableReference == null? rb.rowsTable : rb.currentRowsTableReference.get(), rb.currentRowsSortedReference == null? rb.sortColumnsCheckBox.isSelected() : rb.currentRowsSortedReference.get(), () -> { findColumnsButton.setSelected(false); });
							}
                        });
                        tabContentPanel.controlsContainer.add(findColumnsButton);
                        tabContentPanel.controlsContainer.add(new javax.swing.JToolBar.Separator());
                        rb.sortColumnsCheckBox.addActionListener(new java.awt.event.ActionListener() {
                            @Override
							public void actionPerformed(java.awt.event.ActionEvent evt) {
                            	UIUtil.invokeLater(new Runnable() {
									@Override
									public void run() {
		                            	updateColumnsAndTextView(rb, tabContentPanel);
									}
                            	});
                            }
                        });
                        final JButton loadButton = new JButton(rb.loadButton.getText(), rb.loadButton.getIcon());
                        loadButton.setText("Reload");
                        loadButton.setFocusable(false);
                        tabContentPanel.loadButton = loadButton;
                        loadButton.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								tabContentPanel.loadingPanel.setVisible(true);
								tabContentPanel.repaint();
								loadButton.setEnabled(false);
								initialSortKeysSql = rb.getStatementForReloading();
								try {
									fullTextSearchPanel.put(rb.getStatementForReloading(), rb.fullTextSearchPanel);
									initialSortKeys = rb.rowsTable.getRowSorter().getSortKeys();
									initialColumnsTablesPos = null;
									if (tabContentPanel.columnsScrollPane.getViewport() != null) {
										initialColumnsTablesPos = tabContentPanel.columnsScrollPane.getViewport().getViewPosition();
									}
									initialRowsTablesPos = null;
									initialFoundColumn.clear();
									initialFoundColumn.addAll(rb.foundColumn);
									if (rb.rowsTableScrollPane.getViewport() != null) {
										initialRowsTablesPos = rb.rowsTableScrollPane.getViewport().getViewPosition();
									}
									initialTextTablesPos = null;
									if (tabContentPanel.textViewScrollPane.getViewport() != null) {
										initialTextTablesPos = tabContentPanel.textViewScrollPane.getViewport().getViewPosition();
									}
								} catch (Exception e2) {
									initialSortKeys = null;
									fullTextSearchPanel.remove(rb.getStatementForReloading());
								}
								reload(tabContentPanel, rb.getStatementForReloading());
							}
						});
                        tabContentPanel.cancelLoadButton.addActionListener(new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								CancellationHandler.cancel(SQLConsole.this);
								tabContentPanel.loadingPanel.setVisible(false);
								tabContentPanel.repaint();
								loadButton.setEnabled(true);
							}
						});
						tabContentPanel.controlsContainer.add(loadButton);
                        loadButton.setVisible(finalLoadButtonIsVisible);
                        loadButton.setIcon(UIUtil.scaleIcon(SQLConsole.this, runIcon));
                        rb.setOnReloadAction(new Runnable() {
							@Override
							public void run() {
								updateColumnsAndTextView(rb, tabContentPanel);
							}
						});
                        String sqlE = sql.trim();
                        if (explain) {
                        	sqlE = "Explain Plan for " + sqlE;
                        	tabContentPanel.controlsContainer.removeAll();
                        	JPanel sepPanel = new JPanel(null);
                        	sepPanel.setMinimumSize(new Dimension(1, 24));
                        	sepPanel.setPreferredSize(sepPanel.getPreferredSize());
							tabContentPanel.controlsContainer.add(sepPanel);
                        }
                        String stmt = sqlE;

                        tabContentPanel.statementLabel.setToolTipText(UIUtil.toHTML(sqlE, 100));
                        stmt = stmt.replaceAll("\\s+", " ");
                        if (stmt.length() > 300) {
                            stmt = stmt.substring(0, 300) + "...";
                        }
                        tabContentPanel.statementLabel.setText(stmt);
                        rTabContainer = tabContentPanel.shimPanel;
                        final int MAXLENGTH = 30;
                        String title = shortSQL(sqlE, MAXLENGTH);
                        final int loc = status != null && status.location != null? status.location.a : -1;
                        if (initialTabbedPaneSelection >= 0 && initialTabbedPaneSelectionLoc == loc) {
                        	if (initialTabbedPaneSelection < tabContentPanel.tabbedPane.getTabCount()) {
                        		tabContentPanel.tabbedPane.setSelectedIndex(initialTabbedPaneSelection);
                        	}
                        }
                		try {
                			rb.foundColumn.clear();
                			rb.foundColumn.addAll(initialFoundColumn);
							if (initialSortKeys != null && initialSortKeysSql != null && initialSortKeysSql.equals(sql)) {
                    			rb.getRowsTable().getRowSorter().setSortKeys(initialSortKeys);
                    		}
							FullTextSearchPanel ftsp = fullTextSearchPanel.get(rb.getStatementForReloading());
							if (ftsp != null) {
								UIUtil.invokeLater(12, () -> rb.fullTextSearchPanel.updateFromPredecessor(ftsp));
							}
                		} catch (Exception e) {
                			// ignore
                		}
                		tabContentPanel.tabbedPane.addChangeListener(new ChangeListener() {
                			Container fullTextSearchPanelParent;
                			@Override
							public void stateChanged(ChangeEvent e) {
								initialTabbedPaneSelection = tabContentPanel.tabbedPane.getSelectedIndex();
								initialTabbedPaneSelectionLoc = loc;
								updateColumnsAndTextView(rb, tabContentPanel);
								if (tabContentPanel.tabbedPane.getSelectedComponent() == tabContentPanel.columnsPanel) {
									columnsTable.scrollToCurrentRow();
									if (fullTextSearchPanelParent == null) {
										fullTextSearchPanelParent = rb.fullTextSearchPanel.getParent();
										AbstractAction fa = new AbstractAction() {
											@Override
											public void actionPerformed(ActionEvent e) {
												rb.fullTextSearchPanel.open();
											}
										};
										InputMap im = columnsTable.getInputMap();
										im.put(BrowserContentPane.KS_FIND, fa);
										ActionMap am = columnsTable.getActionMap();
										am.put(fa, fa);
									}
									tabContentPanel.fullTSearchPanel.add(rb.fullTextSearchPanel);
								} else if (fullTextSearchPanelParent != null) {
									fullTextSearchPanelParent.add(rb.fullTextSearchPanel);
								}
							}
						});
                        removeLastErrorTab();

                        if (origTabContentPanel == null) {
	                        jTabbedPane1.add(rTabContainer);
	                        updateResultUI();
	                        JPanel tp;
							jTabbedPane1.setTabComponentAt(jTabbedPane1.indexOfComponent(rTabContainer), tp = getTitlePanel(jTabbedPane1, rTabContainer, tabContentPanel, title, rb));
							rowBrowserPerRTabContainer.put(rTabContainer, rb);
							rowBrowserPerRTabContainer.put(tp, rb);
	                        
	                        if (jTabbedPane1.getTabCount() > MAX_TAB_COUNT) {
	                        	Component tab0 = jTabbedPane1.getTabComponentAt(0);
	                			if (tab0 instanceof TitelPanel) {
	                				if (((TitelPanel) tab0).tabContentPanel != null) {
	                					((TitelPanel) tab0).tabContentPanel.destroy();
	                				}
	                				if (((TitelPanel) tab0).rb != null) {
	                					((TitelPanel) tab0).rb.destroy();
	                				}
	                        	}
	                            jTabbedPane1.remove(0);
	                            updateResultUI();
	                        }
	                        jTabbedPane1.setSelectedIndex(jTabbedPane1.getTabCount() - 1);
                        }
                        rb.resetRowsTableContainer();
                        jTabbedPane1.repaint();
                    }

                    private ColumnsTable columnsTable;

					public void updateColumnsAndTextView(final BrowserContentPane rb,
							final TabContentPanel tabContentPanel) {
						String tableSortAndFilterState = "";
						RowSorter<? extends TableModel> sorter = rb.rowsTable.getRowSorter();
						if (sorter.getModelRowCount() > sorter.getViewRowCount()) {
							tableSortAndFilterState = "Filtered";
						}
						List<? extends SortKey> skeys = sorter.getSortKeys();
						if (!skeys.isEmpty()) {
							for (int i = 0; i < rb.rowsTable.getColumnCount(); ++i) {
								if (rb.rowsTable.getColumnModel().getColumn(i).getModelIndex() == skeys.get(0).getColumn()) {
									String columnName = rb.rowsTable.getColumnName(i);
									if (columnName != null) {
										String[] ntPair = columnName.replaceAll("<br>", "\t").replaceAll("<[^>]*>", "").split("\t");
										if (ntPair.length == 2) {
											columnName = UIUtil.fromHTMLFragment(ntPair[0]);
										}
										if (ntPair.length == 3) {
											columnName = UIUtil.fromHTMLFragment(ntPair[1]);
										}
										if (columnName.trim().isEmpty()) {
											columnName = "#" + (i + 1);
										}
									}
									tableSortAndFilterState = tableSortAndFilterState + (tableSortAndFilterState.isEmpty()? "Sorted" : " and sorted")
											+ " by \"" + columnName + "\"";
									break;
								}
							}
						}
						if (tabContentPanel.tabbedPane.getSelectedComponent() == tabContentPanel.columnsPanel || tabContentPanel.tabbedPane.getSelectedComponent() == tabContentPanel.contentPanel) {
							Point vp = null;
							if (columnsTable != null) {
								if (tabContentPanel.columnsScrollPane.getViewport() != null) {
									vp = tabContentPanel.columnsScrollPane.getViewport().getViewPosition();
								}
							}
							columnsTable = new ColumnsTable(rb, false);
							rb.fullTextSearchPanel.setTransposedTable(columnsTable);
							rb.fullTextSearchPanel.openPermanently();
							tabContentPanel.columnsScrollPane.setViewportView(columnsTable);
							FixedColumnTable fixedColumnTable = new FixedColumnTable(1, tabContentPanel.columnsScrollPane);
							columnsTable.initFixedTable(fixedColumnTable.fixed);
							if (vp != null) {
								if (tabContentPanel.columnsScrollPane.getViewport() != null) {
									tabContentPanel.columnsScrollPane.getViewport().setViewPosition(vp);
								}
							}
							tabContentPanel.columnsSortedStateLabel.setText("  " + tableSortAndFilterState);
							tabContentPanel.columnsSortedStateLabel.setVisible(!tableSortAndFilterState.isEmpty());
						}
						if (tabContentPanel.tabbedPane.getSelectedComponent() == tabContentPanel.textTabPanel) {
							tabContentPanel.updateTextView(rb.rowsTable);
							tabContentPanel.textSortedStateLabel.setText("  " + tableSortAndFilterState);
							tabContentPanel.textSortedStateLabel.setVisible(!tableSortAndFilterState.isEmpty());
						}
					}
                });
            } else {
            	Session.lastUpdateTS = System.currentTimeMillis();
                status.timeInMS += (System.currentTimeMillis() - startTime);
                int updateCount = !hasUpdateCount? 0 : statement.getUpdateCount();
                if (updateCount >= 0) {
                    status.numRowsUpdated += updateCount;
                }
                status.updateView(false);
                status.hasUpdated = true;
                if (updateCount != 0) {
                    setDataHasChanged(true);
                }
                if (isDDLStatement(sql)) {
                    status.withDDL = true;
                }
                UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
		            	removeLastErrorTab();
		            	if (origTabContentPanel != null) {
			            	origTabContentPanel.loadingPanel.setVisible(false);
				            origTabContentPanel.repaint();
				            if (origTabContentPanel.loadButton != null) {
				            	origTabContentPanel.loadButton.setEnabled(true);
				            }
			            }
					}
				});
            }
            CancellationHandler.end(statement, SQLConsole.this);
            statement.close();
            if (!explain) {
            	appendHistory(sql);
            }
        } catch (Throwable error) {
        	if (connection != null) {
        		session.markConnectionAsPotentiallyInvalid(connection);
        	}
        	try {
                CancellationHandler.checkForCancellation(SQLConsole.this);
            } catch (CancellationException e) {
                error = e;
            }
            if (statement != null) {
                try {
                    statement.close();
                } catch (SQLException e) {
                }
            }
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException e) {
                }
            }
            if (!isCommentOnly(sqlStatement)) {
                if (error instanceof SQLException && sqlStatement != null && statementStartOffset >= 0) {
                    int pos = retrieveErrorPos(sqlStatement, error.getMessage());
                    if (pos >= 0) {
                        Entry<Integer, Integer> floor = positionOffsets.floorEntry(pos);
                        int positionOffset;
                        if (floor == null) {
                        	positionOffset = 0;
                        } else {
                        	positionOffset = floor.getValue();
                        }
						status.errorPosition = statementStartOffset + pos + positionOffset;
						status.origErrorPosition = pos;
                        status.errorPositionIsKnown = true;
                    } else {
                        status.errorPosition = statementStartOffset;
                        status.errorPositionIsKnown = false;
                    }
                }
                status.failed = true;
                status.error = error;
            }
            if (error instanceof CancellationException) {
                CancellationHandler.reset(SQLConsole.this);
                queue.clear();
            }
            status.updateView(false);
            final Throwable finalError = error;
            UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
		            if (origTabContentPanel != null) {
		            	origTabContentPanel.loadingPanel.setVisible(false);
			            origTabContentPanel.repaint();
			            if (origTabContentPanel.loadButton != null) {
			            	origTabContentPanel.loadButton.setEnabled(true);
			            }
		            }
		            if (!(finalError instanceof SQLException || finalError instanceof CancellationException)) {
		            	UIUtil.showException(SQLConsole.this, "Error", finalError);
		            }
				}
			});
        } finally {
            if (explain && session.dbms != null && session.dbms.getExplainCleanup() != null && !session.dbms.getExplainCleanup().isEmpty()) {
            	if (session.dbms.getExplainPrepare() != null) {
                    try {
                    	statement = session.getConnection().createStatement();
						statement.execute(String.format(Locale.ENGLISH, session.dbms.getExplainCleanup(), sqlStatement, stmtId));
	                	statement.close();
					} catch (SQLException e) {
	                    logger.info("error", e);
					}
            	}
            }
            if (resetAutoCommitConnection != null) {
            	try {
					resetAutoCommitConnection.setAutoCommit(true);
				} catch (SQLException e) {
					// ignore
				}
            }
        }
    }

	private boolean executeStatementWithLimit(Statement statement, String sqlStatement, Session session) throws SQLException {
		try {
			int limit = 1 + (Integer) limitComboBox.getSelectedItem();
			if (limit > 0) {
				statement.setMaxRows(limit + 1);
			}
		} catch (Exception e) {
			// ignore
		}
		return statement.execute(sqlStatement);
	}

	public void setCaretPosition(int position) {
    	if (editorPane.getDocument().getLength() >= position) {
    		try {
    			int l = editorPane.getLineOfOffset(position);
    			editorPane.setCaretPosition(position);
    			int lineHeight = editorPane.getLineHeight();
    			editorPane.scrollRectToVisible(new Rectangle(0, Math.max(0, l - 2) * lineHeight, 1, 4 * lineHeight));
    		} catch (Exception e) {
    			// ignore
    		}
    		editorPane.setCaretPosition(position);
    		grabFocus();
    	}
    }

    private void updateOutline(String sql, int startPosition) {
    	sql = sql.replaceFirst(";\\s*$", "");
        final int MAX_CONTEXT_LENGTH = 80;
        final int MAX_TOOLTIP_LENGTH = 100;
        List<OutlineInfo> outlineInfos = new ArrayList<OutlineInfo>();
        TreeMap<Integer,Integer> offsets = new TreeMap<Integer,Integer>();
        sql = sqlPlusSupport.replaceVariables(sql, offsets);
		provider.findAliases(SQLCompletionProvider.removeCommentsAndLiterals(sql), null, outlineInfos);
        adjustLevels(outlineInfos);
        List<OutlineInfo> relocatedOutlineInfos = new ArrayList<OutlineInfo>();
        int indexOfInfoAtCaret = -1;
        int caretPos = editorPane.getCaretPosition();
        OutlineInfo predInfo = null;
        for (int i = 0; i < outlineInfos.size(); ++i) {
            OutlineInfo info = outlineInfos.get(i);
            Entry<Integer, Integer> floor = offsets.floorEntry(info.position);
            if (floor != null) {
            	info.origPosition = info.position + floor.getValue();
            }
        }
        for (int i = 0; i < outlineInfos.size(); ++i) {
            OutlineInfo info = outlineInfos.get(i);
        	if (info.isBegin || info.isEnd) {
        		continue;
        	}
            int pos = info.position;
            if (pos + startPosition <= caretPos || indexOfInfoAtCaret < 0) {
                indexOfInfoAtCaret = relocatedOutlineInfos.size();
            }
			OutlineInfo rlInfo = new OutlineInfo(info.mdTable, info.alias, info.level, pos + startPosition, info.scopeDescriptor);
            rlInfo.isCTE = info.isCTE;
            rlInfo.rowCount = info.rowCount;
            if (info.withContext) {
                int cStart = info.contextPosition;
                int cEnd = sql.length();
                int ttEnd = sql.length();
                if (i < outlineInfos.size() - 1) {
                    cEnd = Math.min(cEnd, outlineInfos.get(i + 1).position);
                    ttEnd = Math.min(ttEnd, outlineInfos.get(i + 1).position);
                }
                if (info.contextEnd > 0) {
                    cEnd = Math.min(cEnd, info.contextEnd);
                    ttEnd = Math.min(ttEnd, info.contextEnd);
                }
                if (cStart < cEnd) {
	                String context = UIUtil.removesuperfluousSpaces(sql.substring(cStart, cEnd).trim().replaceAll("\\s+", " "));
	                if (context.length() > MAX_CONTEXT_LENGTH) {
	                    context = context.substring(0, MAX_CONTEXT_LENGTH) + "...";
	                }
	                rlInfo.context = UIUtil.toHTML(context, 0);
	                rlInfo.tooltip = UIUtil.toHTML(sql.substring(info.position, ttEnd), MAX_TOOLTIP_LENGTH);
                }
            }
            rlInfo.withSeparator = info.withSeparator || predInfo != null && predInfo.level + (predInfo.mdTable != null? 1 : 0) == info.level && i > 0 && outlineInfos.get(i - 1).isBegin;
            relocatedOutlineInfos.add(rlInfo);
            predInfo = info;
        }
        indexOfInfoAtCaret -= simplifyOutline(relocatedOutlineInfos, indexOfInfoAtCaret);
        setOutlineTables(relocatedOutlineInfos, indexOfInfoAtCaret);
    }

    private int simplifyOutline(List<OutlineInfo> outlineInfos, int indexOfInfoAtCaret) {
		// "From <single table>"
    	int caretOffset = 0;
    	List<OutlineInfo> toRemove = new ArrayList<OutlineInfo>();
    	for (int i = 1; i < outlineInfos.size(); ++i) {
    		OutlineInfo info = outlineInfos.get(i);
    		OutlineInfo pred = outlineInfos.get(i - 1);
    		if (info.mdTable != null && "From".equalsIgnoreCase(pred.scopeDescriptor)) {
    			if (info.level == pred.level) {
    				OutlineInfo succ = null;
    				if (i + 1 < outlineInfos.size()) {
    					succ = outlineInfos.get(i + 1);
    				}
    				if (succ == null || (succ.level != info.level || succ.mdTable == null)) {
    					info.scopeDescriptor = pred.scopeDescriptor;
    					info.position = pred.position;
    					info.origPosition = pred.origPosition;
    					toRemove.add(pred);
    					if (i - 1 < indexOfInfoAtCaret) {
    						++caretOffset;
    					}
    				}
    			}
    		}
    	}
    	outlineInfos.removeAll(toRemove);
    	return caretOffset;
	}

	private void adjustLevels(List<OutlineInfo> outlineInfos) {
		for (Iterator<OutlineInfo> i = outlineInfos.iterator(); i.hasNext(); ) {
			if ("from duaL".equals(i.next().scopeDescriptor)) {
	    		i.remove();
	    	}
		}
    	if (outlineInfos.size() > 0) {
    		int lastLevel = outlineInfos.get(outlineInfos.size() - 1).level;
    		for (int level = lastLevel - 1; level >= 0; --level) {
    			OutlineInfo info = new OutlineInfo(null, null, level, 0, "");
    			info.isEnd = true;
				outlineInfos.add(info);
    		}
    	}
		Map<Integer, Integer> lastBegin = new HashMap<Integer, Integer>();
		List<Pair<Integer, Integer>> pairs = new ArrayList<Pair<Integer, Integer>>();
		Map<Integer, Pair<Integer, Integer>> pairsPerEnd = new HashMap<Integer, Pair<Integer, Integer>>();
		for (int i = 0; i < outlineInfos.size(); ++i) {
            OutlineInfo info = outlineInfos.get(i);
            if (info.isBegin) {
            	lastBegin.put(info.level, i);
            } else if (info.isEnd) {
            	if (lastBegin.containsKey(info.level)) {
            		Pair<Integer, Integer> pair = new Pair<Integer, Integer>(lastBegin.get(info.level), i);
					pairs.add(pair);
					pairsPerEnd.put(i, pair);
            	}
            }
		}
		for (int i = pairs.size() - 1; i > 0; --i) {
            Pair<Integer, Integer> thePair = pairs.get(i);
			boolean ok = false;
			int findA = thePair.a + 1;
            Pair<Integer, Integer> pair = pairsPerEnd.get(thePair.b - 1);
            while (pair != null) {
    			if (findA == pair.a) {
    				ok = true;
    				break;
    			}
    			if (findA > pair.a) {
    				break;
    			}
    			Pair<Integer, Integer> predPair = pairsPerEnd.get(pair.a - 1);
            	if (predPair != null && pair.a - 1 != predPair.b) {
            		break;
            	}
        		pair = predPair;
            }

            if (ok) {
        		for (int j = thePair.a; j < thePair.b; ++j) {
        			outlineInfos.get(j).level--;
        		}
			}
		}
	}

	private boolean isDDLStatement(String sql) {
        return sql.trim().matches("^(?is)\\b(drop|create|alter|rename)\\b.*");
    }

    protected abstract void refreshMetaData();
    protected abstract void selectTable(MDTable mdTable);
    protected abstract void setOutlineTables(List<OutlineInfo> outlineTables, int indexOfInfoAtCaret);
    protected abstract JFrame getOwner();
    protected abstract void openDataModelEditor(boolean merge);

    private boolean dataHasChanged = false;

    public synchronized void setDataHasChanged(boolean b) {
        dataHasChanged = b;
    }

    public synchronized boolean getDataHasChanged() {
        return dataHasChanged;
    }

    /**
     * Resets the console.
     *
     * @param session
     *            new session
     * @param metaDataSource
     *            new meta data source
     */
    public void reset(Session session, MetaDataSource metaDataSource) throws SQLException {
        this.session = session;
        this.metaDataSource = metaDataSource;
        provider.reset(session, metaDataSource);
        editorPane.forceCaretEvent();
    }

    private class Status {
        public int origErrorPosition;
		public int errorPosition = -1;
        public boolean errorPositionIsKnown = false;
        protected int linesExecuting;
        protected int linesExecuted;
        public boolean withDDL;
        boolean failed;
        boolean running;
        boolean limitExceeded;
        int numRowsRead;
        int numRowsUpdated;
        int numStatements;
        boolean hasSelected = false;
        boolean hasUpdated = false;
        long timeInMS;
        Throwable error;
        String statement;
        Pair<Integer, Integer> location;
        Color failedColor = new Color(255, 210, 210);
        Color okColor = new Color(220, 255, 220);
        Color pendingColor = new Color(235, 235, 255);
        Color runningColor = new Color(255, 249, 200);
        Color runningStatusLabelColor = new Color(0, 100, 0);

        private synchronized void updateView(boolean force) {
            if (force || !updatingStatus.get()) {
                updatingStatus.set(true);

                UIUtil.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            synchronized (Status.this) {
                                Font font = new JLabel("X").getFont();
                                statusLabel.setFont(new Font(font.getName(), font.getStyle(), (font.getSize() * 14) / 10));
                                statusLabel.setVisible(false);
                                cancelButton.setEnabled(false);
                                if (!failed) {
                                    cancelButton.setEnabled(running);
                                    statusLabel.setVisible(true);
                                    statusLabel.setForeground(running? runningStatusLabelColor : Color.BLACK);
                                    statusLabel.setText(getText());
                                } else {
                                	statusLabel.setVisible(true);
                                    if (error instanceof CancellationException) {
                                        statusLabel.setForeground(Color.RED);
                                        statusLabel.setText("Cancelled");
                                        removeLastErrorTab();
                                    } else if (error instanceof SQLException) {
                                        String pos = "";
                                        int errorLine = -1;
                                        try {
                                            errorLine = editorPane.getLineOfOffset(errorPosition);
                                            if (errorPositionIsKnown) {
                                                int col = errorPosition - editorPane.getLineStartOffset(errorLine) + 1;
                                                pos = "Error at line " + (errorLine + 1) + ", column " + col + ": ";
                                            }
                                            setCaretPosition(errorPosition);
                                        } catch (BadLocationException e) {
                                        }
                                        if (errorLine >= 0) {
                                            editorPane.setLineTrackingIcon(errorLine, scaledCancelIcon);
                                        }
                                        showError(pos + error.getMessage(), statement, origErrorPosition);
                                    } else {
                                        StringWriter sw = new StringWriter();
                                        PrintWriter pw = new PrintWriter(sw);
                                        error.printStackTrace(pw);
                                        String sStackTrace = sw.toString(); // stack trace as a string
                                        showError(sStackTrace, statement, origErrorPosition);
                                    }
                                }
                            }
                            if (location != null) {
                                editorPane.removeAllLineHighlights();
                                editorPane.setHighlightCurrentLine(false);
                                try {
                                    for (int i = location.a; i <= location.b; ++i) {
                                        Color hl;
                                        if (i < linesExecuted + location.a) {
                                            hl = okColor;
                                        } else if (i >= linesExecuting + location.a) {
                                            hl = pendingColor;
                                        } else {
                                            if (failed) {
                                                hl = failedColor;
                                            } else if (running) {
                                                hl = runningColor;
                                            } else {
                                                hl = pendingColor;
                                            }
                                        }
                                        editorPane.addLineHighlight(i, hl);
                                    }
                                } catch (BadLocationException e) {
                                }
                            }

                            jPanel2.repaint();

                            if (withDDL && !running) {
                                withDDL = false;
                                refreshMetaData();
                            }
                        } finally {
                            updatingStatus.set(false);
                        }
                    }
                });
            }
        }
        
        Timer timer = null;
        int time;
        
        private void setRunning(boolean r) {
        	if (r && !running) {
        		if (timer != null) {
        			timer.stop();
        		}
        		time = 0;
        		timer = new Timer(50, new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent e) {
						if (running) {
	        				++time;
	        				Color startColor = runningStatusLabelColor;
	        				double x = Math.sin(2 * Math.PI * time / 50.0);
							double f = Math.pow(Math.abs(x), 6);
	        				Color c = new Color(startColor.getRed(), mid(f, startColor.getGreen(), 245), startColor.getBlue());
	        				statusLabel.setForeground(c);
						} else {
							timer.stop();
						}
					}
					private int mid(double f, int s, int d) {
						return (int) (s + f * (d - s));
					}
        		});
				timer.setInitialDelay(1000);
        		timer.setRepeats(true);
        		timer.start();
        	} else if (!r && running) {
        		if (timer != null) {
        			timer.stop();
        		}
        		timer = null;
        	}
        	running = r;
        }

		private String getText() {
            String text = "";
            if (running) {
                text = "Running... ";
                if (numStatements <= 1 && numRowsRead == 0 && numRowsUpdated == 0) {
                    return text;
                }
            }
            if (numStatements > 1) {
                text += numStatements + " Statements. ";
            }
            if (hasSelected) {
                text += (limitExceeded? numRowsRead - 1 : numRowsRead) + " rows read";
                if (limitExceeded) {
                    text += " (limit exceeded)";
                }
                text += ". ";
            }
            if (hasUpdated) {
                text += numRowsUpdated + " rows updated. ";
            }
            return text + "Elapsed time: " + (timeInMS / 1000.0) + " sec";
        }
    }

    private void showError(String errorMessage, String statement, int errorPosition) {
    	statusLabel.setVisible(true);
    	statusLabel.setForeground(Color.RED);
    	statusLabel.setText("Error");

    	removeLastErrorTab();

    	JComponent rTabContainer = new ErrorPanel(errorMessage, statement, errorPosition);
		jTabbedPane1.add(rTabContainer);
		updateResultUI();
        jTabbedPane1.setTabComponentAt(jTabbedPane1.indexOfComponent(rTabContainer), getTitlePanel(jTabbedPane1, rTabContainer, null, "Error", null));

        if (jTabbedPane1.getTabCount() > MAX_TAB_COUNT) {
        	Component tab0 = jTabbedPane1.getTabComponentAt(0);
        	if (((TitelPanel) tab0).tabContentPanel != null) {
				((TitelPanel) tab0).tabContentPanel.destroy();
			}
            jTabbedPane1.remove(0);
            updateResultUI();
        }
        jTabbedPane1.setSelectedIndex(jTabbedPane1.getTabCount() - 1);
        jTabbedPane1.repaint();
    }

    private void removeLastErrorTab() {
		if (jTabbedPane1.getTabCount() > 0) {
			if (jTabbedPane1.getTabComponentAt(jTabbedPane1.getTabCount() - 1) instanceof TitelPanel) {
				if (((TitelPanel) jTabbedPane1.getTabComponentAt(jTabbedPane1.getTabCount() - 1)).rTabContainer instanceof ErrorPanel) {
					jTabbedPane1.removeTabAt(jTabbedPane1.getTabCount() - 1);
					updateResultUI();
				}
			}
		}
	}

    private void disableLastErrorTab() {
		if (jTabbedPane1.getTabCount() > 0) {
			if (jTabbedPane1.getTabComponentAt(jTabbedPane1.getTabCount() - 1) instanceof TitelPanel) {
				if (((TitelPanel) jTabbedPane1.getTabComponentAt(jTabbedPane1.getTabCount() - 1)).rTabContainer instanceof ErrorPanel) {
					((ErrorPanel)((TitelPanel) jTabbedPane1.getTabComponentAt(jTabbedPane1.getTabCount() - 1)).rTabContainer).doDisable();

				}
			}
		}
	}

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jSplitPane2 = new javax.swing.JSplitPane();
        jPanel2 = new javax.swing.JPanel();
        consoleContainerPanel = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jToolBar2 = new javax.swing.JToolBar();
        clearButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        limitComboBox = new javax.swing.JComboBox();
        jToolBar1 = new javax.swing.JToolBar();
        runSQLButton = new javax.swing.JButton();
        runnAllButton = new javax.swing.JButton();
        explainButton = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JToolBar.Separator();
        cancelButton = new javax.swing.JButton();
        jSeparator2 = new javax.swing.JToolBar.Separator();
        jPanel6 = new javax.swing.JPanel();
        statusLabel = new javax.swing.JLabel();
        dummyLabel = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jLabel2 = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jSplitPane2.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane2.setResizeWeight(0.5);
        jSplitPane2.setContinuousLayout(true);
        jSplitPane2.setOneTouchExpandable(true);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        consoleContainerPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(consoleContainerPanel, gridBagConstraints);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jToolBar2.setFloatable(false);
        jToolBar2.setRollover(true);

        clearButton.setText("clear");
        clearButton.setFocusable(false);
        clearButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        clearButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        clearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearButtonActionPerformed(evt);
            }
        });
        jToolBar2.add(clearButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 6);
        jPanel5.add(jToolBar2, gridBagConstraints);

        jLabel1.setText("Row limit ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 13;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel5.add(jLabel1, gridBagConstraints);

        limitComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 14;
        gridBagConstraints.gridy = 2;
        jPanel5.add(limitComboBox, gridBagConstraints);

        jToolBar1.setFloatable(false);
        jToolBar1.setRollover(true);

        runSQLButton.setText("Run");
        runSQLButton.setFocusable(false);
        runSQLButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        runSQLButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(runSQLButton);

        runnAllButton.setText("Run all");
        runnAllButton.setFocusable(false);
        runnAllButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        runnAllButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(runnAllButton);

        explainButton.setText("Explain");
        explainButton.setToolTipText("Show Query Execution Plan ");
        explainButton.setFocusable(false);
        explainButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        explainButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar1.add(explainButton);
        jToolBar1.add(jSeparator1);

        cancelButton.setText("Cancel");
        cancelButton.setToolTipText("Cancel");
        cancelButton.setFocusable(false);
        cancelButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        cancelButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        jToolBar1.add(cancelButton);
        jToolBar1.add(jSeparator2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        jPanel5.add(jToolBar1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jPanel5, gridBagConstraints);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        statusLabel.setForeground(java.awt.Color.gray);
        statusLabel.setText(" ctrl-space for code completion");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        jPanel6.add(statusLabel, gridBagConstraints);

        dummyLabel.setForeground(java.awt.Color.gray);
        dummyLabel.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel6.add(dummyLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel2.add(jPanel6, gridBagConstraints);

        jSplitPane2.setLeftComponent(jPanel2);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jTabbedPane1.setTabLayoutPolicy(javax.swing.JTabbedPane.SCROLL_TAB_LAYOUT);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jTabbedPane1, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        jPanel3.add(jLabel2, gridBagConstraints);

        jSplitPane2.setRightComponent(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jSplitPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        statusLabel.setText("Canceling...");
        statusLabel.setForeground(Color.RED);
    	CancellationHandler.cancel(this);
    	cancelButton.setEnabled(false);
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearButtonActionPerformed
        editorPane.setText("");
        editorPane.grabFocus();
    }//GEN-LAST:event_clearButtonActionPerformed

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton clearButton;
    private javax.swing.JPanel consoleContainerPanel;
    private javax.swing.JLabel dummyLabel;
    private javax.swing.JButton explainButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JToolBar.Separator jSeparator1;
    private javax.swing.JToolBar.Separator jSeparator2;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToolBar jToolBar2;
    private javax.swing.JComboBox limitComboBox;
    private javax.swing.JButton runSQLButton;
    private javax.swing.JButton runnAllButton;
    private javax.swing.JLabel statusLabel;
    // End of variables declaration//GEN-END:variables
	private Object currentHighlightTag = null;
	
	private SmartHighlightPainter highlightPainter = new SmartHighlightPainter(WhereConditionEditorPanel.HIGHLIGHT_COLOR);
	private SmartHighlightPainter highlightPainterWOBorder = new SmartHighlightPainter(WhereConditionEditorPanel.HIGHLIGHT_COLOR);
	{
		highlightPainter.setPaintBorder(true);
	}
	
	private void hightlight(RSyntaxTextAreaWithSQLSyntaxStyle editor, int a, int b) {
		try {
			if (currentHighlightTag != null) {
				editor.getHighlighter().removeHighlight(currentHighlightTag);
				currentHighlightTag = null;
			}
			if (a != b) {
				boolean multiLine = editor.getDocument().getText(a, b - a).contains("\n");
				currentHighlightTag = editor.getHighlighter().addHighlight(a, b, multiLine? highlightPainterWOBorder : highlightPainter);
			}
		} catch (/*BadLocation*/ Exception e) {
			return;
		}
	}

	private abstract class WhereConditionEditorPanelConsole extends WhereConditionEditorPanel {
		protected final ResultContentPane resultContentPane;

		private WhereConditionEditorPanelConsole(Window parent, DataModel dataModel, Table table,
				BrowserContentCellEditor cellEditor, Boolean sorted, WhereConditionEditorPanel predecessor,
				RSyntaxTextAreaWithSQLSyntaxStyle editor, JComponent closeButton, boolean asPopup, int initialColumn, Session session,
				ExecutionContext executionContext, ResultContentPane resultContentPane) throws SQLException {
			super(parent, dataModel, table, cellEditor, sorted, predecessor, editor, closeButton, asPopup, initialColumn, true, session, 
					new MetaDataBasedSQLCompletionProvider(session, metaDataSource), executionContext);
			this.resultContentPane = resultContentPane;
			provider.removeAliases();
		}

		@Override
		protected void onEscape() {
			// do nothing
		}
	};
	
	private WhereConditionEditorPanelConsole popUpWhereConditionEditorPanel;
	private static class SearchBarRSyntaxTextArea extends RSyntaxTextAreaWithSQLSyntaxStyle {
		WhereConditionEditorPanel whereConditionEditorPanel;
		public SearchBarRSyntaxTextArea() {
			super(false, false);
			setEnabled(true);
			setMarkOccurrences(false);
			grabFocus();
		}
		@Override
		protected void runBlock() {
			super.runBlock();
			if (whereConditionEditorPanel != null) {
				whereConditionEditorPanel.parseCondition();
			}
		}
		@Override
		protected boolean withFindAndReplace() {
			return false;
		}
	};
	private final SearchBarRSyntaxTextArea popUpSearchBarEditor = new SearchBarRSyntaxTextArea();
	
    abstract class ResultContentPane extends BrowserContentPane {
    	protected boolean doSync;
		private final Frame parentFrame;
    	private final WCTypeAnalyser.Result wcBaseTable;
    	private String secodaryCond = null;
    	private boolean condEditorNeverOpened = true;
    	private final String origSql;
    	private final int origStartOffset;
        private Map<Column, Pair<Integer, Integer>> positivesPos = new HashMap<Column, Pair<Integer, Integer>>();
        
        public ResultContentPane(DataModel dataModel, WCTypeAnalyser.Result wcBaseTable, Table table, String condition, Session session,
                List<Row> parentRows, Association association, Frame parentFrame,
                RowsClosure rowsClosure, Boolean selectDistinct,
                String origSql, int origStartOffset,
                boolean reload, ExecutionContext executionContext) {
            super(dataModel, table, condition, session, parentRows, association, parentFrame,
            		rowsClosure, selectDistinct, reload, executionContext);
            this.parentFrame = parentFrame;
            this.wcBaseTable = wcBaseTable;
            this.origSql = origSql;
            this.origStartOffset = origStartOffset;
            noSingleRowDetailsView = true;
            rowsTableScrollPane.setWheelScrollingEnabled(true);
        }
        public void initSecondaryCondition() {
			openConditionEditor(null, -1, null);
		}
    	public Set<Integer> getPkColumnsConsole() {
    		HashSet<Integer> result = new HashSet<Integer>(pkColumns);
    		result.addAll(pkColumnsConsole);
    		return result;
    	}
		@Override
    	protected Table getWhereClauseEditorBaseTable() {
    		return wcBaseTable == null? null :  wcBaseTable.table;
    	}
        @Override
        protected int getReloadLimit() {
        	return (Integer) limitComboBox.getSelectedItem();
        }
        @Override
        protected void setReloadLimit(int limit) {
        	SQLConsole.this.setReloadLimit(limit);
        }
		@Override
		protected void setOwnReloadLimit(int limit) {
			setReloadLimit(limit);
		}
        @Override
        protected void unhide() {
        }
        @Override
        protected void showInNewWindow() {
        }
        @Override
        protected void reloadDataModel() throws Exception {
        }
        @Override
        protected void openSchemaMappingDialog() {
        }
        @Override
        protected void openSchemaAnalyzer() {
        }
        @Override
        protected void onRedraw() {
            jTabbedPane1.repaint();
        }
        @Override
        protected void onHide() {
        }
        @Override
        protected void onContentChange(List<Row> rows, boolean reloadChildren) {
        }
        @Override
        protected RowBrowser navigateTo(Association association, List<Row> pRows) {
        	return null;
        }
        @Override
        protected List<RowBrowser> getTableBrowser() {
            return null;
        }
        @Override
        protected PriorityBlockingQueue<RunnableWithPriority> getRunnableQueue() {
            return Desktop.runnableQueue;
        }
        @Override
        protected QueryBuilderDialog getQueryBuilderDialog() {
            return null;
        }
        @Override
        protected RowBrowser getParentBrowser() {
            return null;
        }
        @Override
        protected JFrame getOwner() {
            Window owner = SwingUtilities.getWindowAncestor(SQLConsole.this);
            if (owner instanceof JFrame) {
                return (JFrame) owner;
            }
            return null;
        }
        @Override
        protected double getLayoutFactor() {
            return 0;
        }
        @Override
        protected DbConnectionDialog getDbConnectionDialog() {
            return null;
        }
        @Override
        protected List<RowBrowser> getChildBrowsers() {
            return new ArrayList<RowBrowser>();
        }
        @Override
        protected void findClosure(Row row, Set<Pair<BrowserContentPane, Row>> closure, boolean forward, FindClosureContext findClosureContext) {
        }
        @Override
        protected void findClosure(Row row) {
        }
        @Override
        protected Relationship createQBRelations(boolean withParents) {
            return null;
        }
        @Override
        protected List<Relationship> createQBChildrenRelations(RowBrowser tabu, boolean all) {
            return null;
        }
        @Override
        protected void collectPositions(Map<String, Map<String, double[]>> positions) {
        }
        @Override
        protected void close() {
        }
        @Override
        protected void beforeReload() {
        }
        @Override
        protected void appendLayout() {
        }
        @Override
        protected void adjustClosure(BrowserContentPane tabu, BrowserContentPane thisOne) {
        }
        @Override
        protected void addRowToRowLink(Row pRow, Row exRow) {
        }
        @Override
        protected boolean renderRowAsPK(Row theRow) {
            return false;
        }
        @Override
        protected MetaDataSource getMetaDataSource() {
            return metaDataSource;
        }
        @Override
        protected SQLConsole getSqlConsole(boolean switchToConsole) {
            return SQLConsole.this;
        }
		@Override
		protected void deselectChildrenIfNeededWithoutReload() {
		}
		protected abstract void updateStatementLabel(String sql);
		
		private String latestSyncStatement = null;
		protected String getLatestSyncStatement() {
			return latestSyncStatement != null? latestSyncStatement : origSql;
		}
		protected String getStatementForReloading() {
			return statementForReloading != null? statementForReloading : origSql;
		}

		public void setSync(boolean sync) {
			if (sync != doSync) {
				if (doSync) {
					syncStatement(origSql);
				}
				doSync = sync;
				syncStatement(statementForReloading);
			}
		}
		
		private void syncStatement(String statement) {
			if (doSync) {
				Pair<Integer, Integer> pos = getCurrentStatementPos();
				if (pos != null) {
					editorPane.replaceRange(statement, pos.a, pos.b);
					latestSyncStatement = statement;
				}
			}
		}

		private Pair<Integer, Integer> getCurrentStatementPos() {
			String statement = getLatestSyncStatement().replaceFirst("\\s*$",  "");
			for (int border = 1; border <= 2048; border *= 4) {
				int min = Math.max(0, origStartOffset - border);
				int max = Math.min(editorPane.getDocument().getLength(), origStartOffset + statement.length() + border);
				try {
					int index = editorPane.getDocument().getText(min, max - min).indexOf(statement);
					if (index >= 0) {
						return new Pair<Integer, Integer>(min + index, min + index + statement.length());
					}
				} catch (BadLocationException e) {
					return null;
				}
			}
			return null;
		}

		private int operationStart(Matcher matcher) {
			int p = matcher.start(3);
			if (p < 0) {
				p = matcher.start(4);
				if (p < 0) {
					p = matcher.start(5);
				}
			}
			return p;
		}

		private JDialog conditionEditorDialog = null;
		
		private static final int MINIMUM_POPUP_RETENSION = 1000;
		private long openingTime;

		private void delayPopupAction(ActionListener action) {
			if (openingTime > 0) {
				long rest = openingTime + MINIMUM_POPUP_RETENSION - System.currentTimeMillis();
				if (rest > 0) {
					Timer timer = new Timer((int) rest, action);
					timer.setRepeats(false);
					timer.start();
				} else {
					action.actionPerformed(null);
				}
			}
		}

		@Override
		protected void openConditionEditor(Point location, int column, Runnable onClose) {
			if (conditionEditorDialog != null) {
				if (conditionEditorDialog.isVisible()) {
					conditionEditorDialog.setVisible(false);
					conditionEditorDialog.dispose();
				}
			}
			JDialog dialog = new JDialog(parentFrame, "");
			conditionEditorDialog = dialog;
			UIUtil.invokeLater(() -> {
				Runnable close = () -> {
					dialog.setVisible(false);
					dialog.dispose();
					if (currentHighlightTag != null) {
						editorPane.getHighlighter().removeHighlight(currentHighlightTag);
						currentHighlightTag = null;
					}
				};
				dialog.addWindowListener(new WindowAdapter() {
					@Override
					public void windowClosed(WindowEvent e) {
						if (onClose != null) {
							onClose.run();
						}
					}
				});
				if (getWhereClauseEditorBaseTable() != null) {
		    		BrowserContentCellEditor cellEditor = browserContentCellEditor;
					try {
						popUpWhereConditionEditorPanel = new WhereConditionEditorPanelConsole(dialog, datamodel.get(), getWhereClauseEditorBaseTable(), cellEditor, null,
								popUpWhereConditionEditorPanel, popUpSearchBarEditor, null, true, column, session, executionContext, ResultContentPane.this) {
							@Override
							protected void onEscape() {
								close.run();
							}
							
							@Override
							protected boolean inSQLConsole() {
								return true;
							}
							
							@Override
							protected String columnAlias(Column column) {
								String value = columnLabel(column);
								if (value != null && value.toString().startsWith("<html>")) {
									String[] ntPair = value.toString().replaceAll("<br>", "\t").replaceAll("<[^>]*>", "").split("\t");
									if (ntPair.length == 2) {
										return ntPair[0];
									}
									if (ntPair.length == 3) {
										return ntPair[1];
									}
								}
								return super.columnAlias(column);
							}
							
							@Override
							protected String columnLabel(Column column) {
								String[] acl = getAlternativeColumnLabels();
								if (acl != null) {
									for (int i = 0; i < wcBaseTable.table.getColumns().size() && i < acl.length; ++i) {
										if (acl[i] != null && column.name.equals(wcBaseTable.table.getColumns().get(i).name)) {
											return acl[i];
										}
									}
								}
								return super.columnLabel(column);
							}

							@Override
							protected String columnToolTip(Column column) {
								String[] acl = getAlternativeColumnLabelsFull();
								if (acl != null) {
									for (int i = 0; i < wcBaseTable.table.getColumns().size() && i < acl.length; ++i) {
										if (acl[i] != null && column.name.equals(wcBaseTable.table.getColumns().get(i).name)) {
											return acl[i];
										}
									}
								}
								return super.columnLabel(column);
							}

							@Override
							protected void hightlight(Column column) {
								if (column == null) {
									SQLConsole.this.hightlight(editorPane, 0, 0);
								} else {
									Pair<Integer, Integer> pos = getCurrentStatementPos();
									if (pos != null) {
										String statement = getLatestSyncStatement();
										Pair<Integer, Integer> positivePos = positivesPos.get(column);
										int start = -1;
										int end = 0;
										if (positivePos != null && positivePos.a != 0) {
											int cd = positivePos.a;
											Matcher matcher = createComparisionMatcher(true, column, statement);
											while (matcher != null && matcher.find()) {
												String g = matcher.group();
												String pref = g.replaceFirst("(?is)^((?:\\s*\\bAnd\\b)?\\s*).*$", "$1");
												start = matcher.start();
												if (!g.equals(pref)) {
													start += pref.length();
												}
												end = matcher.end();
												if (--cd <= 0) {
													break;
												}
											}
										} else if (wcBaseTable != null) {
											int off = 0;
											if (wcBaseTable.hasCondition) {
												off = wcBaseTable.conditionEnd;
											} else {
												off = wcBaseTable.table.getName().length();
											}
											Matcher matcher = createComparisionMatcher(true, column, statement.substring(off));
											if (matcher != null && matcher.find()) {
												String g = matcher.group();
												String pref = g.replaceFirst("(?is)^((?:\\s*\\bAnd\\b)?\\s*).*$", "$1");
												start = matcher.start() + off;
												if (!g.equals(pref)) {
													start += pref.length();
												}
												end = matcher.end() + off;
											}
										}
										if (start >= 0) {
											int dot = editorPane.getCaret().getDot();
											int mark = editorPane.getCaret().getMark();
											try {
												editorPane.setCaretPosition(pos.a + start);
												editorPane.select(Math.min(dot, mark), Math.max(dot, mark));
											} catch (Exception e) {
												// ignore
											}
											SQLConsole.this.hightlight(editorPane, pos.a + start, pos.a + end);
										} else {
											SQLConsole.this.hightlight(editorPane, 0, 0);
										}
									} else {
										SQLConsole.this.hightlight(editorPane, 0, 0);
									}
								}
							}

							@Override
							protected void consume(String condition, Set<Integer> involvedColumns) {
								ResultContentPane.this.filteredColumns = involvedColumns;
								condition = condition.trim();
								if (resultContentPane != null && !secodaryCond.trim().matches(SqlUtil.createSQLFragmentSearchPattern(condition.trim(), true))) {
									secodaryCond = condition.trim();
									UISettings.s12 += 100000;

									String tableWithCondition = createTableWithCondition(wcBaseTable.originalQuery, secodaryCond);
									
									setStatementForReloading(tableWithCondition);
									updateStatementLabel(tableWithCondition);
									UIUtil.suspectQuery = tableWithCondition;
									
									syncStatement(statementForReloading);
									
									reloadRows();
								} else {
									repaint();
								}
							}
							
							@Override
							protected String createTableWithCondition(String condition, String tabName, String extJoin) {
								return createTableWithCondition(wcBaseTable.table.getName(), condition);
							}
							
							private boolean findNth(Matcher matcher, int pos) {
								for (int i = 1; i < pos; ++i) {
									if (!matcher.find()) {
										return false;
									}
								}
								return matcher.find();
							}
							
							private String createTableWithCondition(String table, String condition) {
								if (wcBaseTable.hasCondition) {
									String mainCondition = table.substring(wcBaseTable.conditionStart, wcBaseTable.conditionEnd);
									List<Column> pCols = new ArrayList<>(positivesPos.keySet());
									pCols.sort((a, b) -> positivesPos.get(b).b - positivesPos.get(a).b);
									for (Column c: pCols) {
										if (positivesPos.get(c).a == 0) {
											continue;
										}
										Matcher matcher = createComparisionMatcher(true, c, condition);
										if (matcher != null) {
											Pair<Integer, Integer> pp = WCTypeAnalyser.getPositivePosition(Pattern.compile(createComparisionRE(true, c, condition), Pattern.CASE_INSENSITIVE | Pattern.DOTALL), condition);
											if (pp != null) {
												for (int i = 1; i < pp.a; ++i) {
													if (!matcher.find()) {
														matcher = null;
														break;
													}
												}
											}
										}
										if (matcher != null && matcher.find()) {
											int opStart = operationStart(matcher);
											Matcher mainMatcher = createComparisionMatcher(true, c, mainCondition);
											if (mainMatcher != null && findNth(mainMatcher, positivesPos.get(c).a)) {
												int mainOpStart = operationStart(mainMatcher);
												if (!condition.substring(opStart, matcher.end()).trim().matches(SqlUtil.createSQLFragmentSearchPattern(mainCondition.substring(mainOpStart, mainMatcher.end()), true))) {
													mainCondition = mainCondition.substring(0, mainOpStart)
														+ (condition.length() > opStart && Character.isWhitespace(condition.charAt(opStart - 1))? "" : " ")
														+ condition.substring(opStart, matcher.end())
														+ mainCondition.substring(mainMatcher.end());
												}
											}
											condition = removeErasedFragment("\f", condition.substring(0, matcher.start()) + "\f" + condition.substring(matcher.end()));
										} else {
											Matcher mainMatcher = createComparisionMatcher(true, c, mainCondition);
											if (mainMatcher != null && findNth(mainMatcher, positivesPos.get(c).a)) {
												int start = mainMatcher.start();
												if (mainMatcher.group().trim().matches("(?is)^\\bAnd\\b.*")) {
													start += 3;
												}
												mainCondition = removeErasedFragment("\f", 
														mainCondition.substring(0, start)
														+ "\f"
														+ mainCondition.substring(mainMatcher.end()));
											}
										}
									}
									StringBuilder result = new StringBuilder();
									result.append(table.substring(0, wcBaseTable.conditionStart));
									if (!mainCondition.trim().isEmpty()) {
										if (result.length() > 0 && !Character.isWhitespace(result.charAt(result.length() - 1))) {
											result.append(" ");
										}
										if (!WCTypeAnalyser.isPositiveExpression(mainCondition)) {
											result.append("(" + mainCondition + ")");
										} else {
											result.append(mainCondition);
										}
									}
									if (!condition.trim().isEmpty()) {
										if (result.length() > 0 && !Character.isWhitespace(result.charAt(result.length() - 1))) {
											result.append(" ");
										}
										if (!mainCondition.trim().isEmpty()) {
											result.append("and");
										}
										if (result.length() > 0 && !Character.isWhitespace(result.charAt(result.length() - 1))) {
											result.append(" ");
										}
										result.append(condition.trim().replaceAll("(?is)\\n\\s*(And )", " $1"));
									}
									if (mainCondition.trim().isEmpty() && condition.trim().isEmpty()) {
										result = new StringBuilder(result.toString().replaceFirst("(?is)\\s*\\b(where|having)\\s*$", ""));
									}
									if (result.length() > 0 && !Character.isWhitespace(result.charAt(result.length() - 1))) {
										result.append(" ");
									}
									result.append(table.substring(wcBaseTable.conditionEnd));
										
									return result.toString().trim();
								} else {
									if (condition.trim().isEmpty()) {
										return table;
									} else {
										if (table.length() > 100 || table.contains("\n")) {
											if (table.charAt(table.length() - 1) != '\n') {
												table += "\n";
											}
										} else if (table.length() > 0 && !Character.isWhitespace(table.charAt(table.length() - 1))) {
											table += " ";
										}
										return table + (wcBaseTable.isHaving? "Having " : "Where ") + condition.trim().replaceAll("(?is)\\n\\s*(And )", " $1");
									}
								}
							}
							
							@Override
							protected String getCTE() {
								return wcBaseTable.cte;
							}
							
							@Override
							protected String columnNameToRegExp(String name) {
								Set<String> names = wcBaseTable.getAlternativeNames(name);
								if (names == null) {
									return SqlUtil.createSQLFragmentSearchPattern(name, false);
								} else {
									return "(?:" + names.stream().map(aName -> "(?:" + SqlUtil.createSQLFragmentSearchPattern(aName, false) + ")").collect(Collectors.joining("|")) + ")";
								}
							}

							@Override
							protected Set<String> getAlternativeNames(String name) {
								return wcBaseTable != null? wcBaseTable.getAlternativeNames(name) : null;
							}
						};
						popUpWhereConditionEditorPanel.setTableAlias(null);
					} catch (SQLException e1) {
						UIUtil.showException(SQLConsole.this, "Error", e1);
						return;
					}
					popUpSearchBarEditor.whereConditionEditorPanel = popUpWhereConditionEditorPanel;
					
					if (location == null && secodaryCond == null) {
						StringBuilder sb = new StringBuilder();
						if (wcBaseTable.hasCondition) {
							filteredColumns = new HashSet<Integer>();
							String condition = wcBaseTable.originalQuery.substring(wcBaseTable.conditionStart, wcBaseTable.conditionEnd);
							for (int i = 0; i < wcBaseTable.table.getColumns().size(); ++i) {
								Column c = wcBaseTable.table.getColumns().get(i);
								if (c != null && c.name != null) {
									Pair<Integer, Integer> pe = WCTypeAnalyser.getPositivePosition(c.name, wcBaseTable.getAlternativeNames(c.name), condition);
									if (pe != null) {
										positivesPos.put(c, pe);
										if (pe.a != 0) {
											Matcher matcher = popUpWhereConditionEditorPanel.createComparisionMatcher(true, c, condition);
											if (matcher != null && matcher.find()) {
												String comp = matcher.group().trim().replaceFirst("(?is)^\\s*\\bAnd\\b", "");
												if (sb.length() > 0) {
													sb.append(" and ");
												}
												sb.append(comp);
												filteredColumns.add(i);
											}
										}
									}
								}
							}
						}
						secodaryCond = sb.toString();
					}
					
					if (location != null) {
						popUpWhereConditionEditorPanel.parseCondition(secodaryCond, () -> {
//							dialog.setModal(false);
							dialog.setUndecorated(true);
							openingTime = System.currentTimeMillis();
							UIUtil.invokeLater(6, () -> openingTime = System.currentTimeMillis());
							dialog.addWindowFocusListener(new WindowFocusListener() {
								@Override
								public void windowLostFocus(WindowEvent e) {
									if (!(e.getOppositeWindow() instanceof StringSearchDialog)) {
										if (System.currentTimeMillis() < openingTime + 200) {
											dialog.requestFocus();
											return;
										}
										dialog.setSize(1, 1);
										delayPopupAction(v -> close.run());
									}
								}
								@Override
								public void windowGainedFocus(WindowEvent e) {
								}
							});
							
							int x = location.x + 32;
							int y = location.y;
							
							dialog.getContentPane().add(popUpWhereConditionEditorPanel);
							
							dialog.pack();
							double mh = column >= 0? 320 : 380;
							int height = Math.max(dialog.getHeight(), (int) mh);
							dialog.setLocation(x, y);
							int minWidth = 400;
							int wid = Math.max(minWidth, dialog.getWidth());
							Window window = parentFrame;
							int maxX = window.getX() + window.getWidth() - wid - 8;;
							dialog.setSize(wid, Math.min(height, 600));
							dialog.setLocation(Math.max(0, Math.min(maxX, dialog.getX())), dialog.getY());
							int maxY = window.getY() + window.getHeight() - dialog.getHeight() - 8;
							if (maxY < dialog.getY()) {
								int deltaH = Math.min(dialog.getY() - maxY, (int) (0.30 * dialog.getHeight()));
								maxY += deltaH;
								dialog.setSize(dialog.getWidth(), dialog.getHeight() - deltaH);
								dialog.setLocation(dialog.getX(), Math.max(0, maxY));
							}
							popUpWhereConditionEditorPanel.prepareStringSearchPanelOfInitialColumn(dialog);
							UIUtil.invokeLater(4, () -> {
								popUpWhereConditionEditorPanel.openStringSearchPanelOfInitialColumn(dialog);
							});
							UIUtil.invokeLater(() -> dialog.requestFocus());
							dialog.setVisible(true);
							
							if (column >= 0) {
								if (condEditorNeverOpened) {
									condEditorNeverOpened = false;
									UIUtil.invokeLater(8, () -> {
										Timer timer = new Timer(100, e -> {
											if (!dialog.isVisible()) {
												openConditionEditor(location, column, onClose);
											}
										});
										timer.setRepeats(false);
										timer.start();
									});
								}
							}
						});
					}
				}
			});
		}
    }
    
    private class TitelPanel extends JPanel {
    	public final JComponent rTabContainer;
    	public final TabContentPanel tabContentPanel;
    	public final ResultContentPane rb;

    	public TitelPanel(final JTabbedPane tabbedPane, final JComponent rTabContainer, TabContentPanel tabContentPanel, String title, ResultContentPane rb) {
    		super(new FlowLayout(FlowLayout.LEFT, 0, 0));
    		this.rTabContainer = rTabContainer;
    		this.tabContentPanel = tabContentPanel;
    		this.rb = rb;
    		setOpaque(false);
    		JLabel titleLbl = new JLabel(title);
    		titleLbl.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
    		add(titleLbl);
    		SmallButton closeButton = new SmallButton(closeIcon) {
    			@Override
    			protected void onClick(MouseEvent e) {
    				tabbedPane.remove(rTabContainer);
    				if (tabContentPanel != null) {
    					tabContentPanel.destroy();
    	        	}
    				if (rb != null) {
    					rb.destroy();
    				}
    	            updateResultUI();
    			}
    		};
    		add(closeButton);
    	}
    }
    
    private JPanel getTitlePanel(final JTabbedPane tabbedPane, final JComponent rTabContainer, TabContentPanel tabContentPanel, String title, ResultContentPane rb) {
        return new TitelPanel(tabbedPane, rTabContainer, tabContentPanel, title, rb);
    }

    @Override
	public void grabFocus() {
        editorPane.grabFocus();
    }

    private Icon closeIcon;
    private ImageIcon clearIcon;
    {
        // load images
    	closeIcon = UIUtil.readImage("/Close-16-1.png");
    	clearIcon = UIUtil.readImage("/clear.png");
    }

    /**
     * Appends a statement and eventually executes it.
     *
     * @param sql the statement
     * @param execute execute the statement?
     */
    public void appendStatement(String sql, boolean execute) {
    	appendStatement(sql, execute, null, false);
    }

    /**
     * Appends a statement and eventually executes it.
     *
     * @param sql the statement
     * @param execute execute the statement?
     * @param tabContentPanel the panel to show result (option)
     */
    public void appendStatement(String sql, boolean execute, TabContentPanel tabContentPanel, boolean useLineContinuation) {
        if (!findAndSetCaretPosition(sql, tabContentPanel)) {
	        String pre = "";
	        int lineCount = editorPane.getLineCount();
	        if (lineCount > 0 && editorPane.getDocument().getLength() > 0) {
	            pre = "\n";
	            if (editorPane.getText(lineCount - 1, lineCount - 1, true).trim().length() > 0) {
	                pre += "\n";
	            }
	        }
	        if (useLineContinuation) {
	        	sql = addLineContinuation(sql);
	        } else {
	        	sql = sql.replaceAll("\\n\\s*\\n", "\n").trim();
	        }
	    	if (!sql.endsWith(";") && !sql.endsWith(" \\")) {
	            sql += ";";
	        }
	        editorPane.append(pre + sql.replace("\r", "") + "\n");
	        setCaretPosition(editorPane.getDocument().getLength());
    	}
        if (!running.get()) {
            resetStatus();
        }
        if (execute) {
            executeSelectedStatements(false, tabContentPanel, true);
        }
    }

    /**
     * Searches a statement and sets caret position s.t. current statement equals the given one.
     */
    private boolean findAndSetCaretPosition(String statement, TabContentPanel tabContentPanel) {
        statement = statement.replaceFirst("(;\\s*)+$", "").trim();

        if (tabContentPanel != null) {
	    	try {
	    		try {
					editorPane.setCaretPosition(0);
	    		} catch (Exception e) {
					// ignore
				}
	    		editorPane.setCaretPosition(tabContentPanel.caretDotMark.a);
		    	editorPane.moveCaretPosition(tabContentPanel.caretDotMark.b);
				String existingStatement = executeSelectedStatements(false, tabContentPanel, false);
				if (existingStatement.replaceFirst("(;\\s*)+$", "").trim().equals(statement)) {
					return true;
				}
	    	} catch (/*IllegalArgument*/ Exception e) {
	    		// ignore
	    	}
        }

        String[] statementLines = statement.split("\\s*\\n\\s*");
    	int lineCount = editorPane.getLineCount();
	    for (int lineNr = 0; lineNr < lineCount; ++lineNr) {
	    	if (lineNr + statementLines.length <= lineCount) {
	    		boolean found = true;
		    	for (int i = 0; i < statementLines.length; ++i) {
		    		String line = getLineContent(lineNr + i);
		    		line = line.trim().replaceFirst("(;\\s*)+$", "");
		    		if (!line.equals(statementLines[i])) {
		    			found = false;
		    			break;
		    		}
		    	}
		    	if (found) {
		    		try {
						editorPane.setCaretPosition(0);
		    		} catch (Exception e) {
						// ignore
					}
		    		try {
						editorPane.setCaretPosition(editorPane.getLineStartOffset(lineNr));
						String existingStatement = executeSelectedStatements(false, tabContentPanel, false).replaceFirst("(;\\s*)+$", "").trim();
						if (existingStatement.equals(statement)) {
							return true;
						}
		    		} catch (Exception e) {
						return false;
					}
		    	}
	    	}
	    }

	    return false;
    }

    private String getLineContent(int line) {
    	try {
    		Segment txt = new Segment();
    		int sOff = editorPane.getLineStartOffset(line);
            editorPane.getDocument().getText(sOff, editorPane.getLineEndOffset(line) - sOff, txt);
            return txt.toString();
        } catch (BadLocationException e) {
            return "";
        }
    }

	private String executeSelectedStatements(boolean explain, TabContentPanel tabContentPanel, boolean execute) {
        try {
        	String sql;
	        Pair<Integer, Integer> loc = null;
	        Pair<Integer, Integer> locFragmentOffset = null;
	        Pair<Pair<Integer, Integer>, Pair<Integer, Integer>> locFragment = editorPane.getCurrentStatementFragmentLocation();
	        if (locFragment != null) {
	            loc = locFragment.a;
	            locFragmentOffset = locFragment.b;
	            sql = editorPane.getDocument().getText(locFragmentOffset.a, locFragmentOffset.b - locFragmentOffset.a);
	            if (loc.a < loc.b) {
	                Pattern pattern = Pattern.compile("(\\n\\s*)$", Pattern.DOTALL);
	                Matcher matcher = pattern.matcher(sql);
	                if (matcher.find()) {
	                    loc = new Pair<Integer, Integer>(loc.a, loc.b - 1);
	                }
	            }
	        } else {
	            loc = editorPane.getCurrentStatementLocation(null);
	            sql = editorPane.getText(loc.a, loc.b, true);
	        }
	        if (loc != null && execute) {
	            executeSQLBlock(sql, loc, editorPane.getCaret().getDot() == editorPane.getCaret().getMark(), locFragmentOffset, explain, tabContentPanel);
	        }
	        return sql;
        } catch (BadLocationException e) {
            e.printStackTrace();
            return "";
        }
    }

    private void executeAllStatements() {
        if (editorPane.getLineCount() > 0) {
            int start = 0;

            try {
                while (start < editorPane.getLineCount()) {
                    Segment txt = new Segment();
                    int sOff = editorPane.getLineStartOffset(start);
                    editorPane.getDocument().getText(sOff, editorPane.getLineEndOffset(start) - sOff, txt);
                    String sLine = txt.toString().trim();
                    if (sLine.length() > 0) {
                        break;
                    }
                    ++start;
                }
            } catch (BadLocationException e) {
                // ignore
            }
            Pair<Integer, Integer> loc = new Pair<Integer, Integer>(start, editorPane.getLineCount() - 1);
            executeSQLBlock(editorPane.getText(loc.a, loc.b, true), loc, false, null, false, null);
        }
    }

    private boolean isCommentOnly(String statement) {
    	if (statement == null) {
    		return false;
    	}
        Pattern pattern = Pattern.compile("('([^']*'))|(/\\*.*?\\*/)|(\\-\\-.*?(\n|$))", Pattern.DOTALL);
        Matcher matcher = pattern.matcher(statement);
        boolean result = matcher.find();
        StringBuffer sb = new StringBuffer();
        if (result) {
            do {
                if (matcher.group(1) == null) {
                    matcher.appendReplacement(sb, "");
                } else {
                    matcher.appendReplacement(sb, "$0");
                }
                result = matcher.find();
            } while (result);
        }
        matcher.appendTail(sb);
        return sb.toString().trim().isEmpty();
    }

    private final String HISTORY_FILE = ".history";
    private final String LF = System.getProperty("line.separator", "\n");
    private final JComboBox2 historyComboBox = new JComboBox2() {
        @Override
        public java.awt.Dimension getMinimumSize() {
            Dimension s = super.getMinimumSize();
            return new Dimension(Math.min(80, s.width), s.height);
        }
    };

    private synchronized void restoreHistory() {
        try {
            File file = Environment.newFile(HISTORY_FILE);
            if (file.exists()) {
                BufferedReader in = new BufferedReader(new FileReader(file));
                history.clear();
                String line;
                while ((line = in.readLine()) != null) {
                    String[] lines = CsvFile.decodeLine(line.trim());
                    if (lines.length > 0 && !lines[0].isEmpty()) {
                        history.add(lines[0]);
                    }
                }
                in.close();
            }
        } catch (Exception e) {
            logger.info("error", e);
        }
        UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
		        historyComboBox.setModel(historyComboboxModel());
			}
		});
    }

    private synchronized void storeHistory() {
        try {
            File file = Environment.newFile(HISTORY_FILE);
            FileWriter out = new FileWriter(file);
            for (String sql: history) {
                out.write(CsvFile.encodeCell(sql) + LF);
            }
            out.close();
        } catch (Exception e) {
            logger.info("error", e);
        }
        UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				historyComboBox.setModel(historyComboboxModel());
			}
		});
    }

    private final ItemListener historyCBItemListener = new ItemListener() {
        @Override
        public void itemStateChanged(ItemEvent e) {
            if (e.getStateChange() == ItemEvent.SELECTED && e.getItem() != null && historyComboBox.getSelectedIndex() > 0) {
                appendStatement(e.getItem().toString(), false, null, true);
                historyComboBox.setSelectedIndex(0);
            }
        }
    };

    private DefaultComboBoxModel historyComboboxModel() {
        ArrayList<String> model = new ArrayList<String>(history);
        if (!model.isEmpty()) {
            model.add(0, new String(model.get(0) + " "));
        }
        historyComboBox.removeItemListener(historyCBItemListener);
        DefaultComboBoxModel cbModel = new DefaultComboBoxModel(model.toArray());
        historyComboBox.addItemListener(historyCBItemListener);
        return cbModel;
    }

    private synchronized void appendHistory(String sql) {
        sql = sql.trim();
        if (!sql.isEmpty() && sql.length() < 100000) {
            history.remove(sql);
            history.add(0, sql);
            if (history.size() > MAX_HISTORY_SIZE) {
                history.remove(history.size() - 1);
            }
        }
    }

    private int retrieveErrorPos(String sqlStatement, CharSequence errorMessage) {
        if (DBMS.ORACLE.equals(session.dbms)) {
            String statement = "declare "
                    + "l_theCursor integer default dbms_sql.open_cursor;" + "begin      " + "    begin     "
                    + "      dbms_sql.parse(  l_theCursor, ?, dbms_sql.native );"
                    + "      exception when others then ? := dbms_sql.last_error_position;" + "    end;"
                    + "    dbms_sql.close_cursor( l_theCursor );" + "end;";

            CallableStatement cStmt = null;
            Connection connection = null;
            try {
                connection = session.getConnection();
                cStmt = connection.prepareCall(statement);
                cStmt.registerOutParameter(2, Types.INTEGER);
                cStmt.setString(1, sqlStatement);

                cStmt.execute();

                return cStmt.getInt(2);
            } catch (Exception e) {
            	if (connection != null) {
            		session.markConnectionAsPotentiallyInvalid(connection);
            	}
            	logger.info("error", e);
            } finally {
                if (cStmt != null) {
                    try {
                        cStmt.close();
                    } catch (SQLException e) {
                    }
                }
            }
        } else if (DBMS.POSTGRESQL.equals(session.dbms)) {
            Pattern pattern = Pattern.compile("\\n\\s*Position: ([0-9]+)");
            Matcher matcher = pattern.matcher(errorMessage);
            if (matcher.find()) {
                return Integer.parseInt(matcher.group(1)) - 1;
            }
        }
        return -1;
    }

    private void toSingleLine() {
		String currentStatement = editorPane.getCurrentStatement(false);
		String newStatement = UIUtil.toSingleLineSQL(currentStatement);
		if (!currentStatement.equals(newStatement)) {
			editorPane.replaceCurrentStatement(newStatement, false);
		}
	}

    private void toggleLineContinuation() {
		String currentStatement = editorPane.getCurrentStatement(false);
		String newStatement;
		Pattern p = Pattern.compile("(?:(?:;( |\\t|\\r)*(?:--[^\\n]*)?)|(?:\\n( |\\t|\\r)*)) ?\\\\( |\\t|\\r)*\\n", Pattern.DOTALL);
        Matcher m = p.matcher(currentStatement);
        if (m.find()) {
			newStatement = currentStatement.replaceAll("((?:(?:;(?: |\\t|\\r)*?(?:--[^\\n]*)?))) ?\\\\([ \\t\\r]*\\n)", "$1$2");
			newStatement = newStatement.replaceAll("((?:\\n(?: |\\t|\\r)*?)) ?\\\\([ \\t\\r]*)(?=\\n)", "$1$2");
		} else {
			newStatement = addLineContinuation(currentStatement);
		}
		if (!currentStatement.equals(newStatement)) {
			editorPane.replaceCurrentStatement(newStatement, false);
		}
	}

	private String addLineContinuation(String statement) {
		String newStatement = statement;
		newStatement = newStatement.replace("\r", "");
		newStatement = newStatement.replaceAll("((?:(?:;(?: |\\t|\\r)*(?:--[^\\n]*)?)))(\\n(\\r)?)", "$1 \\\\$2");
		newStatement = newStatement.replaceAll("((?:(?:\\n(?: |\\t|\\r)*)))(?=\\n)", "$1 \\\\");
		newStatement = newStatement.replaceAll("\\\\(\\s*)$", "$1");
		return newStatement;
	}

    private void analyzeSQL() {
		String currentStatement = editorPane.getCurrentStatement(false);
		File tempFile = Configuration.getInstance().createTempFile();
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(tempFile));
			Pair<Integer, Integer> loc = editorPane.getCurrentStatementLocation(false, false, null, false);
			if (loc != null) {
				for (int i = 0; i < loc.a; ++i) {
					out.write("\n");
				}
			}
			out.write(currentStatement);
			out.close();
			AssociationProposerView associationProposer = new AssociationProposerView(getOwner(), datamodel.get(), tempFile, 6, executionContext);
			if (associationProposer.isAccepted()) {
				openDataModelEditor(true);
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
		tempFile.delete();
    }

	private void substituteVariables() {
		String currentStatement = editorPane.getCurrentStatement(true);
		String newStatement = sqlPlusSupport.replaceVariables(currentStatement, null);
		if (!currentStatement.equals(newStatement)) {
			editorPane.replaceCurrentStatement(newStatement, true);
		}
	}

	/**
	 * @return <code>true</code> iff console editor is empty
	 */
	public boolean isEmpty() {
		return editorPane.getDocument().getLength() == 0;
	}

	/**
	 * Gets the row limit.
	 */
	public Integer getRowLimit() {
		Object item = limitComboBox.getSelectedItem();
		return item instanceof Integer? (Integer) item : null;
	}

	/**
	 * Sets the row limit.
	 *
	 * @param limit the row limit
	 */
	public void setRowLimit(Integer limit) {
		limitComboBox.setSelectedItem(limit);
		Component selectedComponent = jTabbedPane1.getSelectedComponent();
		BrowserContentPane contentPane = rowBrowserPerRTabContainer.get(selectedComponent);
		if (contentPane != null) {
			contentPane.reloadRows();
		}
	}

	private int initialContentSize;
	private int initialContentHash;
	private boolean dirty;
	private Map<JComponent, BrowserContentPane> rowBrowserPerRTabContainer = new WeakHashMap<JComponent, BrowserContentPane>();
	
	/**
	 * Loads content from file.
	 *
	 * @param file the file
	 */
	public void loadFromFile(File file) throws IOException {
		this.file = file;
		if (file.exists()) {
			String path = file.getPath();
			Charset encoding = SqlUtil.retrieveEncoding(path);
			InputStream inputStream = new FileInputStream(file);
			BufferedReader in;
			if (path.toLowerCase(Locale.ENGLISH).endsWith(".gz")) {
				in = new BufferedReader(new InputStreamReader(new GZIPInputStream(inputStream), encoding));
			} else if (path.toLowerCase(Locale.ENGLISH).endsWith(".zip")){
				ZipInputStream zis = new ZipInputStream(inputStream); // lgtm [java/input-resource-leak]
				zis.getNextEntry();
				in = new BufferedReader(new InputStreamReader(zis, encoding));
			} else {
				in = new BufferedReader(new InputStreamReader(inputStream, encoding));
			}
			
			StringBuilder sb = new StringBuilder();
			int c;
	        while ((c = in.read()) != -1) {
	        	if (c != '\r') {
	        		sb.append((char) c);
	        	}
	        }
	        in.close();
	        editorPane.setText(sb.toString());
	        dirty = false;
		} else {
			editorPane.setText("");
	        dirty = true;
		}
		editorPane.discardAllEdits();
        initialContentSize = editorPane.getDocument().getLength();
        initialContentHash = editorPane.getText().hashCode();
        consoleContainerPanel.setVisible(false);
        UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
		        consoleContainerPanel.setVisible(true);
		        grabFocus();
			}
		});
        setCaretPosition(0);
        onContentStateChange(file, dirty);
	}

	/**
	 * Stores content to file if console is dirty or new file is given.
	 *
	 * @param newFile the file or <code>null</code> to store into assigned file
	 */
	public void storeToFile(File newFile) throws IOException {
		if (newFile != null || file != null && dirty) {
			if (newFile != null) {
				file = newFile;
			}
			
			Charset encoding = Charset.defaultCharset();
			Charset uTF8 = null;
			try {
				uTF8 = Charset.forName("UTF8");
			} catch (Exception e) {
				// ignore
			}
			
			if (uTF8 != null) {
				// retrieve encoding
				if (editorPane.getLineCount() > 0 && getLineContent(0).contains("encoding UTF-8")) {
					encoding = uTF8;
				}
			}
			
			OutputStream outputStream = new FileOutputStream(file);
			if (file.getPath().toLowerCase(Locale.ENGLISH).endsWith(".zip")) {
				outputStream = new ZipOutputStream(outputStream); // lgtm [java/output-resource-leak] 
				String zipFileName = file.getName();
				((ZipOutputStream)outputStream).putNextEntry(new ZipEntry(zipFileName.substring(0, zipFileName.length() - 4)));
			} else {
				if (file.getPath().toLowerCase(Locale.ENGLISH).endsWith(".gz")) {
					outputStream = new GZIPOutputStream(outputStream); // lgtm [java/output-resource-leak] 
				}
			}
			
			PrintWriter out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outputStream, encoding)));
			int lines = editorPane.getLineCount();
			for (int line = 0; line < lines; ++line) {
				out.print(getLineContent(line).replace("\r", "").replace("\n", ""));
				if (line < lines - 1) {
					out.println();
				}
			}
			out.close();
	        initialContentSize = editorPane.getDocument().getLength();
	        initialContentHash = editorPane.getText().hashCode();
	        dirty = false;
	        onContentStateChange(file, dirty);
		}
	}

	private void checkDirty() {
		boolean newState = false;
        if (initialContentSize != editorPane.getDocument().getLength()) {
        	newState = true;
        } else {
        	newState = initialContentHash != editorPane.getText().hashCode();
        }
        if (newState != dirty) {
        	dirty = newState;
            onContentStateChange(file, dirty);
        }
	}

	/**
	 * Reloads tab content
	 *
	 * @param tabContentPanel to be reloaded
	 * @param sql statement to be executed again
	 */
	private void reload(TabContentPanel tabContentPanel, String sql) {
		try {
			UIUtil.setWaitCursor(tabContentPanel);
			appendStatement(sql.replaceFirst("(;\\s*)+$", ""), true, tabContentPanel, true);
		} finally {
			UIUtil.resetWaitCursor(tabContentPanel);
		}
	}

	/**
	 * Gets the file associated with this console.
	 *
	 * @return file associated with this console or <code>null</code>, if the console is not assigned to a file
	 */
	public File getFile() {
		return file;
	}

	public boolean isDirty() {
		return dirty;
	}

	/**
	 * Closes this console.
	 */
	public void close() {
    	CancellationHandler.cancel(this);
    	queue.add(STOP);
    	editorPane.setText("");
    	editorPane.discardAllEdits();
    	editorPane.setDocument(new RSyntaxDocument(null, SyntaxConstants.SYNTAX_STYLE_NONE)); // prevent memory leak
	}

	/**
	 * Gets the editor pane.
	 *
	 * @return the editor pane
	 */
    public RSyntaxTextAreaWithSQLSyntaxStyle getEditorPane() {
		return editorPane;
	}

	private void hightlight(int a, int b) {
		hightlight(editorPane, 0, 0);
	}
	
	protected abstract void onContentStateChange(File file, boolean dirty);
    protected abstract void setReloadLimit(int limit);

	static private ImageIcon runIcon;
    static private ImageIcon runAllIcon;
    static private ImageIcon cancelIcon;
    static private ImageIcon explainIcon;
    static private ImageIcon searchIcon;
    static private ImageIcon searchCIcon;
    static private ImageIcon syncIcon;
    static private ImageIcon nosyncIcon;

    private int nextPlanID = 0;

    static {
        // load images
        runIcon = UIUtil.readImage("/run.png");
        runAllIcon = UIUtil.readImage("/runall.png");
        cancelIcon = UIUtil.readImage("/Cancel.png");
        explainIcon = UIUtil.readImage("/explain.png");
        searchIcon = UIUtil.readImage("/search.png");
        searchCIcon = UIUtil.readImage("/searchc.png");
        syncIcon = UIUtil.readImage("/sync.png");
        nosyncIcon = UIUtil.readImage("/nosync.png");
    }

    private File contentFile() {
    	File file = new File(Environment.newFile("scratch"), new File(executionContext.getQualifiedDatamodelFolder()).getName());
    	return file;
    }
    
    private boolean contentLoaded = false;
    private boolean contentModified = false;
    
	public void loadContent() {
		try {
			UIUtil.setWaitCursor(SQLConsole.this);
			if (file == null) {
				File contentFile = contentFile();
				if (contentFile.exists()) {
					ObjectInputStream in = new ObjectInputStream(new FileInputStream(contentFile)); // lgtm [java/input-resource-leak]
					String content = in.readObject().toString();
					editorPane.setText(content);
					editorPane.discardAllEdits();
					setCaretPosition(content.length());
					in.close();
				}
			}
		} catch (Throwable t) {
			// ignore
		} finally {
			UIUtil.resetWaitCursor(SQLConsole.this);
		}
		contentLoaded = true;
		editorPane.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				contentModified = true;
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				contentModified = true;
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				contentModified = true;
			}
		});
	}

	public void saveContent() {
		try {
			final int MAX_DOC_LENGTH = 1024 * 1024;
			if (contentLoaded && contentModified && file == null && editorPane.getDocument().getLength() < MAX_DOC_LENGTH) {
				File contentFile = contentFile();
				contentFile.getParentFile().mkdirs();
				ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(contentFile)); // lgtm [java/output-resource-leak]
				out.writeObject(editorPane.getText());
				out.close();
			}
		} catch (Throwable t) {
			t.printStackTrace();
			// ignore
		}
	}
	
    // TODO StringSearch component for historie (and than inc hist size a lot)
    
    // TODO automatically generated SQL statements from Desktop like:
    // "Select distinct ... from ... left join ..." with a non-comparable column in select clause (for example BLOB) fails. Make the problem go away.
    // idea: give SQLConsole an "ErrorHandler" who will be consulted if query fails and will ask user to skip "distinct" and try again.

}
