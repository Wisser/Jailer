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
package net.sf.jailer.ui.databrowser.whereconditioneditor;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ListModel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.border.LineBorder;
import javax.swing.border.SoftBevelBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import org.fife.ui.rtextarea.SmartHighlightPainter;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.InlineViewStyle;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.BrowserContentCellEditor;
import net.sf.jailer.ui.databrowser.DBConditionEditor;
import net.sf.jailer.ui.databrowser.Desktop;
import net.sf.jailer.ui.databrowser.Desktop.RunnableWithPriority;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.LRUCache;
import net.sf.jailer.ui.util.LightBorderSmallButton;
import net.sf.jailer.ui.util.MovePanel;
import net.sf.jailer.ui.util.SizeGrip;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * SQL-Where-Condition Editor.
 *
 * @author Ralf Wisser
 */
public abstract class WhereConditionEditorPanel extends javax.swing.JPanel {
	
	private static final float REDUCED_OPACITY = 0.5f;
	private static final float REDUCED_OPACITY_FADE_START = 0; // REDUCED_OPACITY + 0.15f;
	private static final float REDUCED_OPACITY_RETENTION_TIME = 0; //  1.5f;

	private final int MAX_NUM_DISTINCTEXISTINGVALUES = 100_000;
	private final int MAX_SIZE_DISTINCTEXISTINGVALUES = 500_000;
	private final int SIZE_DISTINCTEXISTINGVALUESCACHE = 40;
	private final int MAX_HISTORY_SIZE = 8;
	
	private final DataModel dataModel;
	private final Table table;
	private JToggleButton searchButton;
	private final RSyntaxTextAreaWithSQLSyntaxStyle editor;
	private Font font;
	private BrowserContentCellEditor cellEditor;
	private final ExecutionContext executionContext;
	private InlineViewStyle inlineViewStyle;
	private final Session session;
	private DocumentListener documentListener;
	private String tableAlias = "A";
	private final boolean asPopup;
	private final int initialColumn;
	public final SQLCompletionProvider provider;
	
	private class Comparison {
		Operator operator;
		String value = "";
		final Column column;
		JComponent operatorField;
		JTextField valueTextField;
		
		Comparison(Operator operator, Column column) {
			this.operator = operator;
			this.column = column;
		}
	}
	
	private List<Comparison> comparisons = new ArrayList<Comparison>();
	private Set<Integer> involvedColumns = new HashSet<Integer>();
	private Map<String, Consumer<JLabel>> columnLabelConsumers = new HashMap<String, Consumer<JLabel>>();

    /**
     * Creates new form SearchPanel
     */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public WhereConditionEditorPanel(Window parent, DataModel dataModel, Table table, BrowserContentCellEditor cellEditor, Boolean sorted,
			WhereConditionEditorPanel predecessor, RSyntaxTextAreaWithSQLSyntaxStyle editor,
			JComponent closeButton, boolean asPopup, int initialColumn, boolean locateUnderButton,
			Session session, SQLCompletionProvider theProvider, ExecutionContext executionContext) {
    	this.dataModel = dataModel;
    	this.table = table;
    	this.editor = editor;
    	this.cellEditor = cellEditor;
    	this.asPopup = asPopup;
    	this.initialColumn = initialColumn;
    	this.session = session;
    	this.executionContext = executionContext;
        initComponents(); UIUtil.initComponents(this);
        applyButton.setIcon(UIUtil.scaleIcon(applyButton, okIcon));
		
        if (asPopup) {
        	initPopupView();
        } else {
        	setBorder(UIUtil.plaf == PLAF.FLAT? null : BorderFactory.createSoftBevelBorder(SoftBevelBorder.RAISED));
    		statusPanel.setVisible(false);
        	popupTitelPanel.setVisible(false);
        }
        
		if (theProvider != null) {
			new SQLAutoCompletion(theProvider, editor);
			theProvider.removeAliases();
			if (tableAlias != null && table != null) {
				theProvider.addAlias("A", table);
			}
		}
        this.provider = theProvider;
        
        if (inSQLConsole()) {
        	removeAll();
        	GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 1;
            gridBagConstraints.gridy = 1;
            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
            gridBagConstraints.weightx = 1.0;
            gridBagConstraints.weighty = 1.0;
            add(jPanel1, gridBagConstraints);

            gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 2;
            gridBagConstraints.gridy = 1;
            gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
            gridBagConstraints.weighty = 1.0;
            tableLabel.setVisible(false);
            add(popupTitelPanel, gridBagConstraints);
        }
        
        editor.setEnabled(true);
        syntaxPanePanel.add(editor);
        
        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new Insets(0, 0, 0, 2);
        clearButton = new JButton("Clear", UIUtil.scaleIcon(this, clearIcon));
        clearButton.addActionListener(e -> {
			if (allDisabled) {
				return;
			}
			editor.setText("");
			parseCondition();
		});
        clearButton.setToolTipText("Clear all Fields");
		sortCheckBox.setVisible(false);
		jPanel6.add(clearButton, gridBagConstraints);

        font = tableLabel.getFont();
		tableLabel.setFont(font.deriveFont(font.getStyle() | Font.BOLD, (int)(font.getSize() /* * 1.2 */)));
		tableLabel.setIcon(tableIcon);
		if (closeButton != null) {
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 0;
	        gridBagConstraints.gridy = 0;
	        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
	        closeButtonContainerPanel.add(closeButton, gridBagConstraints);
		}
    	if (predecessor == null) {
    		int location;
    		if (asPopup) {
    			location = 290;
    		} else {
    			location = 348;
	    		if (parent != null) {
	    			int maxLoc = (int) (parent.getHeight() * 0.4);
	    			if (location > maxLoc + 50) {
	    				location = maxLoc;
	    			}
	    		}
    		}
    		splitPane.setDividerLocation(location);
    	}
    	
		if (table == null) {
	    	setVisible(false);
		} else {
			inlineViewStyle = session.getInlineViewStyle();
			if (inlineViewStyle == null) {
				setVisible(false);
				return;
			}
			List<String> config;
			if (initialColumn >= 0) {
				config = new ArrayList<String>();
				config.add(table.getColumns().get(initialColumn).name);
			} else {
				if (inSQLConsole()) {
					config = new ArrayList<String>();
				} else {
					config = ConditionStorage.load(table, executionContext);
				}
			}
			if (config.isEmpty()) {
				if (table.primaryKey != null && !table.primaryKey.getColumns().isEmpty()) {
					comparisons.addAll(table.primaryKey.getColumns().stream().map(c -> new Comparison(Operator.Equal, c)).collect(Collectors.toList()));
				} else {
					comparisons.addAll(table.getColumns().stream().map(c -> new Comparison(Operator.Equal, c)).collect(Collectors.toList()));
				}
			} else {
				comparisons.addAll(table.getColumns().stream().filter(c -> config.contains(c.name)).map(c -> new Comparison(Operator.Equal, c)).collect(Collectors.toList()));
			}
        	tableLabel.setText(this.dataModel.getDisplayName(table));
        	
            if (jScrollPane1.getHorizontalScrollBar() != null) {
            	jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
            }
            if (jScrollPane1.getVerticalScrollBar() != null) {
            	jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
            }
            if (jScrollPane2.getHorizontalScrollBar() != null) {
            	jScrollPane2.getHorizontalScrollBar().setUnitIncrement(16);
            }
            if (jScrollPane2.getVerticalScrollBar() != null) {
            	jScrollPane2.getVerticalScrollBar().setUnitIncrement(16);
            }
            
			if (predecessor != null) {
				if (predecessor.documentListener != null) {
					editor.getDocument().removeDocumentListener(predecessor.documentListener);
				}
				splitPane.setDividerLocation(predecessor.splitPane.getDividerLocation());
			}
			documentListener = new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateApplyButton();
				}

				@Override
				public void insertUpdate(DocumentEvent e) {
					updateApplyButton();
				}

				@Override
				public void changedUpdate(DocumentEvent e) {
					updateApplyButton();
				}

				private void updateApplyButton() {
					applyButton.setEnabled(!editor.getText().equals(latestParsedCondition));
				}
			};
			editor.getDocument().addDocumentListener(documentListener);
        	
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 20;
			final JComboBox comboBox = searchComboBox;
	        searchButton = StringSearchPanel.createSearchButton(null, comboBox, "Add Search Field", new Runnable() {
			    @Override
			    public void run() {
			        addColumn();
			    }
			}, null, null, null, false, null, true, false, columnLabelConsumers, asPopup, null);
	        searchButton.setText("Add Search Field");
	        gridBagConstraints = new GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 4;
	        gridBagConstraints.anchor = GridBagConstraints.WEST;
	        jPanel6.add(searchButton, gridBagConstraints);
	        searchComboBox.setVisible(false);
	
	        if (jScrollPane1.getHorizontalScrollBar() != null) {
	        	jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
	        }
	        if (jScrollPane1.getVerticalScrollBar() != null) {
	        	jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
	        }
	        
	        FocusListener focusListener = new FocusListener() {
				@Override
				public void focusLost(FocusEvent e) {
				}
				@Override
				public void focusGained(FocusEvent e) {
					if (onFocusGained != null) {
						onFocusGained.apply(null);
						onFocusGained = null;
					}
				}
			};
			editor.addFocusListener(focusListener);
			searchButton.addFocusListener(focusListener);
			sortCheckBox.addFocusListener(focusListener);

	        updateSearchUI();
	        sortCheckBox.setSelected(Boolean.TRUE.equals(sorted));
        }
    }
	
	private void initPopupView() {
		setBorder(BorderFactory.createSoftBevelBorder(SoftBevelBorder.RAISED));
		titlePanel.setVisible(false);
		splitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = GridBagConstraints.EAST;
        statusPanel.add(new JLabel(" "), gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = GridBagConstraints.EAST;
        statusPanel.add(new SizeGrip(), gridBagConstraints);
		
        gridBagConstraints = new java.awt.GridBagConstraints();
        if (inSQLConsole()) {
            gridBagConstraints.gridx = 3;
            gridBagConstraints.gridy = 2;
            gridBagConstraints.weighty = 1;
        } else {
	        gridBagConstraints.gridx = 2;
	        gridBagConstraints.gridy = 1;
	        gridBagConstraints.weightx = 1;
        }
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = GridBagConstraints.EAST;
        MovePanel movePanel = new MovePanel(new GridBagLayout());
        movePanel.setOpaque(false);
		popupTitelPanel.add(movePanel, gridBagConstraints);
        
		if (table.getName() != null) {
			String displayName = dataModel.getDisplayName(table);
			popupTabNameLabel.setText(displayName.replaceFirst("^(.{40})...*$", "$1..."));
			popupTabNameLabel.setToolTipText(displayName);
			popupTabNameLabel.setIcon(tableIcon);
		}
		
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.anchor = GridBagConstraints.EAST;
        popupTitelPanel.add(new SmallButton(closeIcon, closeOverIcon, true) {
			@Override
			protected void onClick(MouseEvent e) {
				Window window = SwingUtilities.getWindowAncestor(this);
				window.setVisible(false);
				window.dispose();
			}
		}, gridBagConstraints);
        
        if (inSQLConsole()) {
        	popupTitelPanel.setOpaque(false);
        	gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 3;
            gridBagConstraints.gridy = 3;
            gridBagConstraints.anchor = GridBagConstraints.NORTHEAST;
            popupTitelPanel.add(new SizeGrip(), gridBagConstraints);
            popupTabNameLabel.setVisible(false);
            jPanel7.setVisible(false);
        }
	}

	protected abstract void consume(String condition, Set<Integer> involvedColumns);
    protected abstract void onEscape();
    
    protected abstract boolean isColumnNullable(Table table, Column column);

    protected boolean isColumnNullable(Table table, Column column, MetaDataSource metaDataSource) {
    	if (column.isNullable) {
			return true;
		}
		if (metaDataSource != null && metaDataSource.isInitialized()) {
			MDTable mdTable = metaDataSource.toMDTable(table);
			if (mdTable != null) {
				String columnName = column.name;
				try {
					if (mdTable.isLoaded()) {
						List<String> mdColumns = mdTable.getColumns();
						for (int i = 0; i < mdColumns.size(); ++i) {
							if (Quoting.equalsIgnoreQuotingAndCase(mdColumns.get(i), columnName) && i < mdTable.getColumnTypes().size()) {
								return mdTable.getColumnTypes().get(i).isNullable;
							}
						}
						return false;
					}
				} catch (SQLException e) {
					return false;
				}
			}
		}
		return false;
    }

	/**
	 * Parses a condition and updates the UI accordingly.
	 * 
	 * @param condition the condition
	 */
	public void parseCondition(String condition) {
		parseCondition(condition, null);
	}
	
	private boolean hasCondition = false;
	
	/**
	 * Parses a condition and updates the UI accordingly.
	 * 
	 * @param condition the condition
	 */
	public void parseCondition(String condition, Runnable afterParsing) {
		String formated = new BasicFormatterImpl().format(condition);
		if (formated.trim().isEmpty()) {
			formated = "";
		} else {
			formated += "\n";
		}
		if (hasCondition && editor.getText().equals(condition)) {
			return;
		}
		hasCondition = true;
		editor.setText(formated);
		editor.discardAllEdits();
		editor.setCaretPosition(0);
		parseCondition(afterParsing);
	}
	
	private Runnable continueParsing = null;

	/**
	 * Parses current condition and updates the UI accordingly.
	 */
	public synchronized void parseCondition() {
		parseCondition((Runnable) null);
	}

	/**
	 * Parses current condition and updates the UI accordingly.
	 */
	public synchronized void parseCondition(Runnable afterParsing) {
		if (allDisabled) {
			return;
		}
		latestParsedCondition = editor.getText();
		consume(latestParsedCondition, involvedColumns);
		applyButton.setEnabled(false);
		AtomicBoolean disabled = new AtomicBoolean(false);
		Timer timer = new Timer(200, e -> {
			if (!disabled.getAndSet(true)) {
				disableAll();
			}
		});
		timer.setRepeats(false);
		timer.start();
		if (searchButton == null) {
			LogUtil.warn(new RuntimeException("Condition editor not initialized"));
		}
		continueParsing = () -> {
			Desktop.runnableQueue.add(new RunnableWithPriority() {
				@Override
				public void run() {
					doParseCondition();
					UIUtil.invokeLater(() -> {
						if (disabled.getAndSet(true)) {
							enableAll();
						}
						updateSearchUI();
						historize();
						if (afterParsing != null) {
							afterParsing.run();
						}
					});
				}
				@Override
				public int getPriority() {
					return 90;
				}
			});
		};
		if (cellEditor != null && cellEditor.getColumnTypes().length > 0) {
			continueParsing.run();
			continueParsing = null;
		}
	}

	protected void historize() {
		comparisons.forEach(c -> {
			if (c.column.name != null) {
				String value = c.valueTextField.getText().trim();
				if (!value.isEmpty() && !value.matches("\\s*is\\s+(not\\s+)?null\\s*")) {
					List<String> hist = getHistory(c.column);
					hist.remove(value);
					hist.add(0, value);
					if (hist.size() > MAX_HISTORY_SIZE) {
						hist.remove(hist.size() - 1);
					}
				}
			}
		});
	}

	private static final String HISTORY_KEY = "history";
	
	private List<String> getHistory(Column column) {
		@SuppressWarnings("unchecked")
		Map<Pair<String, String>, List<String>> hist = (Map<Pair<String, String>, List<String>>) session.getSessionProperty(getClass(), HISTORY_KEY);
		if (hist == null) {
			hist = new HashMap<Pair<String,String>, List<String>>();
			session.setSessionProperty(getClass(), HISTORY_KEY, hist);
		}
		Pair<String, String> key = new Pair<String, String>(table.getName(), column.name);
		List<String> result = hist.get(key);
		if (result == null) {
			result = new ArrayList<String>();
			hist.put(key, result);
		}
		return result;
	}

	public synchronized void setCellEditor(BrowserContentCellEditor cellEditor) {
		this.cellEditor = cellEditor;
		if (continueParsing != null && cellEditor != null && cellEditor.getColumnTypes().length > 0) {
			continueParsing.run();
			continueParsing = null;
		}
	}
	
	private List<JComponent> allFields = new ArrayList<JComponent>();
	private List<JComponent> disabledFields = new ArrayList<JComponent>();
	private boolean allDisabled = false;
	
	private void disableAll() {
		disabledFields.clear();
		allFields.forEach(f -> {
			if (f.isEnabled()) {
				f.setEnabled(false);
				disabledFields.add(f);
			}
		});
		allDisabled = true;
	}

	private void enableAll() {
		disabledFields.forEach(f -> {
			f.setEnabled(true);
		});
		disabledFields.clear();
		allDisabled = false;
	}

	private static final String IS_NULL = "is null";
	private static final String IS_NOT_NULL = "is not null";
	
	/**
	 * Parses current condition concurrently.
	 */
	private void doParseCondition() {
		try {
			involvedColumns.clear();
			valuePositions.clear();
			fullPositions.clear();
			Set<Comparison> seen = new HashSet<Comparison>();
			String latestParsedConditionUC = latestParsedCondition.toUpperCase(Locale.ENGLISH);
			for (int columnIndex = 0; columnIndex < table.getColumns().size(); ++columnIndex) {
				for (boolean noAlias: new boolean[] { false, true }) {
					Column column = table.getColumns().get(columnIndex);
					if (column == null || column.name == null) {
						continue;
					}
					if (noAlias && latestParsedCondition.length() > 400) {
						continue;
					}
					String uqName = Quoting.staticUnquote(column.name);
					if (uqName.matches("\\w+") && !latestParsedConditionUC.contains(uqName.toUpperCase(Locale.ENGLISH))) {
						continue;
					}
					Matcher matcher = createComparisionMatcher(noAlias, column, latestParsedCondition);
					if (matcher != null) {
						Pair<Integer, Integer> pp = WCTypeAnalyser.getPositivePosition(Pattern.compile(createComparisionRE(noAlias, column, latestParsedCondition), Pattern.CASE_INSENSITIVE | Pattern.DOTALL), latestParsedCondition);
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
						int start = matcher.start();
						String q1 = matcher.group(1);
						String q2 = matcher.group(2);
						if ("".equals(q1)) {
							q1 = null;
						}
						if ("".equals(q2)) {
							q2 = null;
						}
						if (q1 == null && q2 == null || (q1 != null && q1.equals(q2)) || "[".equals(q1) || "]".equals(q2)) {
							Operator operator;
							String sqlValue = null;
							String value = null;
							if (matcher.group(3) != null && !"".equals(matcher.group(3))) {
								operator = Operator.Equal;
								value = sqlValue = IS_NULL;
								Pair<Integer, Integer> pos = new Pair<Integer, Integer>(matcher.start(3), matcher.end(3));
								valuePositions.put(column, pos);
								fullPositions.put(column, new Pair<Integer, Integer>(start, pos.b));
							} else if (matcher.group(4) != null && !"".equals(matcher.group(4))) {
								operator = Operator.Equal;
								value = sqlValue = IS_NOT_NULL;
								Pair<Integer, Integer> pos = new Pair<Integer, Integer>(matcher.start(4), matcher.end(4));
								valuePositions.put(column, pos);
								fullPositions.put(column, new Pair<Integer, Integer>(start, pos.b));
							} else {
								String op = matcher.group(5);
								if ("!=".equals(op)) {
									operator = Operator.NotEqual;
								} else {
									operator = Stream.of(Operator.values()).filter(o -> o.sql.equals(op.toLowerCase())).findFirst().get();
								}
								sqlValue = matcher.group(6);
								int opPos = matcher.start(5);
								Pair<Integer, Integer> pos = new Pair<Integer, Integer>(opPos, matcher.end(6));
								valuePositions.put(column, pos);
								fullPositions.put(column, new Pair<Integer, Integer>(start, pos.b));
							}
							if (sqlValue != null) {
								if (value == null) {
									value = toValue(sqlValue, columnIndex);
								}
								if (value != null) {
									involvedColumns.add(columnIndex);
									String theValue = value;
									Optional<Comparison> comp = comparisons.stream().filter(c -> c.column.equals(column))
											.findAny();
									Comparison c = comp.orElseGet(() -> null);
									if (c != null) {
										c.operator = operator;
										c.value = theValue;
										seen.add(c);
									} else {
										c = new Comparison(operator, column);
										c.value = theValue;
										comparisons.add(c);
										seen.add(c);
									}
								}
							}
						}
					break;
					}
				}
			}
			comparisons.forEach(c -> {
				if (!seen.contains(c)) {
					c.value = "";
				}
			});
		} catch (Exception e) {
			LogUtil.warn(e);
			throw e;
		}
	}

	public Matcher createComparisionMatcher(boolean noAlias, Column column, String condition) {
		if (!noAlias && tableAlias == null) {
			return null;
		}
		String regex = createComparisionRE(noAlias, column, condition);
		Matcher matcher = null;
		try {
			Pattern identOperator = Pattern.compile(regex, Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
			if (SqlUtil.removeSubQueries(column.name).equals(column.name)) {
				matcher = identOperator.matcher(SqlUtil.removeSubQueries(condition));
			} else {
				matcher = identOperator.matcher(condition);
			}
		} catch (Throwable t) {
			LogUtil.warn(t);
		}
		return matcher;
	}

	public static final String VALUE_REGEX = "((?:(?:0x(?:\\d|[a-f])+)|(?:'(?:[^']|'')*')|(?:(?:[\\+\\-]\\s*)?(:?\\.|\\d)+)|(?:true|false)|(?:\\w+\\s*\\([^\\)\\(]*\\)))(?:\\s*\\:\\:\\s*(?:\\w+))?)";
	
	protected String createComparisionRE(boolean noAlias, Column column, String condition) {
		String quoteRE = "[\"\u00B4\\[\\]`]";
		String regex = "(?:(?:and\\s+)?" + "(?:"
				+ (inSQLConsole()? "" : !noAlias? "\\b" : "(?<!\\w)")
				+ (tableAlias == null || noAlias? "" : (tableAlias + "\\s*\\.")) + "\\s*))"
				+ (noAlias? "(?<!\\.\\s{0," + Math.max(condition.length(), 1000) + "})" : "")
				+ "(" + quoteRE + "?)" + columnNameToRegExp(column.name) + "(" + quoteRE
				+ "?)" + "\\s*(?:(\\bis\\s+null\\b)|(\\bis\\s+not\\s+null\\b)|(?:(" + Pattern.quote("!=") + "|"
				+ Stream.of(Operator.values()).map(o -> o.sql).sorted((a, b) -> b.length() - a.length())
						.map(sql -> Character.isAlphabetic(sql.charAt(0)) ? "\\b" + Pattern.quote(sql) + "\\b"
								: Pattern.quote(sql))
						.collect(Collectors.joining("|"))
				+ ")\\s*"  + VALUE_REGEX + "))";
		return regex;
	}

	protected String columnNameToRegExp(String name) {
		return Pattern.quote(Quoting.staticUnquote(name));
	}
    
	private String toValue(String sqlValue, int columnIndex) {
		final String CACHE = "toValueCache";
		@SuppressWarnings("unchecked")
		Map<String, Map<Integer, String>> cache = (Map<String, Map<Integer, String>>) session.getSessionProperty(getClass(), CACHE);
		if (cache == null) {
			cache = new HashMap<String, Map<Integer,String>>();
			session.setSessionProperty(getClass(), CACHE, cache);
		}
		if (cellEditor == null || cellEditor.getColumnTypes().length == 0) {
			return null;
		}
		Map<Integer, String> perType = cache.get(sqlValue);
		if (perType == null) {
			perType = new HashMap<Integer, String>();
			cache.put(sqlValue, perType);
		}
		String value = perType.get(cellEditor.getColumnTypes()[columnIndex]);
		if (value == null) {
			try {
				// TODO 1st try to cast to type read from resultSetMetadata
				StringBuilder result = new StringBuilder();
				String[] columnNames = new String[] { "v" };
				String sql = "Select vt.v from " +
						inlineViewStyle.head(columnNames) +
						inlineViewStyle.item(new String[] { sqlValue }, columnNames, 0) +
						inlineViewStyle.terminator("vt", columnNames);
				session.executeQuery(sql, new AbstractResultSetReader() {
					@Override
					public void readCurrentRow(ResultSet resultSet) throws SQLException {
						result.append(cellEditor.cellContentToText(columnIndex, getCellContentConverter(resultSet, session, session.dbms).getObject(resultSet, 1)));
					}
				});
				value = result.toString();
			} catch (Throwable t) {
				if (!(t instanceof CancellationException)) {
					if (!session.isDown()) {
						LogUtil.warn(t);
					}
				}
				value = null;
			}
			perType.put(cellEditor.getColumnTypes()[columnIndex], value);
		}
		return value;
	}

	private String latestParsedCondition = null;
	private Map<Column, Pair<Integer, Integer>> valuePositions = new HashMap<Column, Pair<Integer, Integer>>();
	private Map<Column, Pair<Integer, Integer>> fullPositions = new HashMap<Column, Pair<Integer, Integer>>();
	
    private void updateSearchUI() {
    	Optional<Comparison> focusedComparision = comparisons.stream().filter(c -> c.valueTextField != null && c.valueTextField.hasFocus()).findAny();
    	
    	allFields.clear();
    	if (searchButton != null) {
    		allFields.add(searchButton);
    	}
		allFields.add(applyButton);
    	allFields.add(sortCheckBox);
    	allFields.add(editor);
    	
    	Set<Column> searchColumns = comparisons.stream().map(c -> c.column).collect(Collectors.toSet());
    	List<String> colNames = new ArrayList<String>();
    	columnLabelConsumers.clear();
    	int i = 0;
    	for (Column column: table.getColumns()) {
    		if (!searchColumns.contains(column)) {
    			String uqName = columnAlias(column);
				if (!colNames.contains(uqName)) {
    				colNames.add(uqName);
    				if (cellEditor != null && cellEditor.getColumnTypes().length > 0 && !cellEditor.isEditable(table, i, null)) {
    					columnLabelConsumers.put(uqName, label -> { label.setEnabled(false); label.setIcon(emptyIcon); });
    				} else if (table.primaryKey != null && table.primaryKey.getColumns().stream().anyMatch(pk -> pk.equals(column))) {
    					columnLabelConsumers.put(uqName, label -> { label.setForeground(new Color(170, 0, 0)); label.setIcon(constraintPKIcon); });
    		    	} else {
    					columnLabelConsumers.put(uqName, label -> { label.setIcon(emptyIcon); });
    		    	}
    			}
    		}
    		++i;
    	}
//    	if (sortCheckBox.isSelected()) {
    		colNames.sort(String::compareToIgnoreCase);
//    	}
    	if (searchButton != null) {
        	searchButton.setEnabled(!colNames.isEmpty());
    	}
		searchComboBox.setModel(new DefaultComboBoxModel<String>(colNames.toArray(new String[0])));
		
		List<Comparison> sortedSearchComparison = new ArrayList<Comparison>(comparisons);
		if (sortCheckBox.isSelected()) {
			sortedSearchComparison.sort((a, b) -> Quoting.staticUnquote(a.column.name).compareToIgnoreCase(Quoting.staticUnquote(b.column.name)));
		}
		BiFunction<JComponent, Integer, JComponent> wrap = (c, y) -> {
			JPanel panel = new JPanel(new GridBagLayout());
			c.setForeground(Color.black);
			panel.setBackground(y % 2 == 0? UIUtil.TABLE_BACKGROUND_COLOR_1 : UIUtil.TABLE_BACKGROUND_COLOR_2);
			panel.setOpaque(true);
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = y;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        if (c instanceof JTextField) {
	        	gridBagConstraints.insets = new Insets(0, 2, 0, 2);
	        }
	        panel.add(c, gridBagConstraints);
			return panel;
		};
		int maxWidth = 16;
		for (Operator operator: Operator.values()) {
			maxWidth = Math.max(maxWidth, new JLabel(operator.render()).getPreferredSize().width + 8);
		}
		searchFieldsPanel.removeAll();
        FocusListener focusListener = new FocusListener() {
			@Override
			public void focusLost(FocusEvent e) {
			}
			@Override
			public void focusGained(FocusEvent e) {
				if (onFocusGained != null) {
					onFocusGained.apply(null);
					onFocusGained = null;
				}
			}
		};
		int y = 0;
		for (Comparison comparison: sortedSearchComparison) {
			GridBagConstraints gridBagConstraints;
			JPanel namePanel = new JPanel(new GridBagLayout());
			namePanel.setOpaque(false);
			JLabel nameLabel = new JLabel();
	        nameLabel.setFont(font.deriveFont(font.getStyle() & ~Font.BOLD, (int)(font.getSize() /* * 1.2 */)));
			nameLabel.setText(columnLabel(comparison.column));
			nameLabel.setToolTipText(columnToolTip(comparison.column));
	        
			if (!inSQLConsole()) {
				JLabel typeLabel = new JLabel();
				typeLabel.setForeground(Color.gray);
		        typeLabel.setText(comparison.column.toSQL(null).substring(comparison.column.name.length()).trim() + " ");
		        typeLabel.setToolTipText(typeLabel.getText());
		        gridBagConstraints = new java.awt.GridBagConstraints();
		        gridBagConstraints.gridx = 2;
		        gridBagConstraints.gridy = 3 * y;
		        gridBagConstraints.fill = GridBagConstraints.BOTH;
		        gridBagConstraints.anchor = GridBagConstraints.EAST;
		        namePanel.add(typeLabel, gridBagConstraints);
			}

			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.WEST;
	        if (inSQLConsole()) {
		        gridBagConstraints.insets = new Insets(0, 8, 0, 0);
	        }
	        namePanel.add(nameLabel, gridBagConstraints);
			
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y;
	        gridBagConstraints.gridwidth = 5;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(namePanel, y), gridBagConstraints);
	        boolean isPk = table.primaryKey != null && table.primaryKey.getColumns().contains(comparison.column);
			nameLabel.setForeground(isPk? new Color(170, 0, 0) : Color.black);
			
			SmallButton hideButton = new LightBorderSmallButton(UIUtil.scaleIcon(this, deleteIcon, 1.1)) {
				@Override
				protected void onClick(MouseEvent e) {
					if (allDisabled) {
						return;
					}
					accept(comparison, "", Operator.Equal);
					comparisons.remove(comparison);
					updateSearchUI();
					storeConfig();
				}
			};
			hideButton.addFocusListener(focusListener);
			allFields.add(hideButton);
	    	hideButton.setToolTipText("Remove Search Field");
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 5;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.EAST;
	        searchFieldsPanel.add(wrap.apply(hideButton, y), gridBagConstraints);
	        
	        SmallButton clearButton = new LightBorderSmallButton(UIUtil.scaleIcon(this, clearIcon, 1.1)) {
				@Override
				protected void onClick(MouseEvent e) {
					if (allDisabled) {
						return;
					}
					accept(comparison, "", Operator.Equal);
					updateSearchUI();
					storeConfig();
				}
			};
			clearButton.addFocusListener(focusListener);
			allFields.add(clearButton);
			clearButton.setToolTipText("Clear Search Field");
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 4;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.EAST;
	        searchFieldsPanel.add(wrap.apply(clearButton, y), gridBagConstraints);

	        JLabel sep = new JLabel("  ");
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(sep, y), gridBagConstraints);
	        JTextField valueTextField = new JTextField(comparison.value) {
	        	@Override
				public Dimension getMinimumSize() {
	        		String text = getText();
	        		int cp = getCaretPosition();
	        		setText("");
	        		try {
	        			return super.getMinimumSize();
	        		} finally {
	        			setText(text);
	        			setCaretPosition(cp);
	        		}
				}
	        	@Override
				public Dimension getMaximumSize() {
	        		String text = getText();
	        		int cp = getCaretPosition();
	        		setText("");
	        		try {
	        			return super.getMaximumSize();
	        		} finally {
	        			setText(text);
	        			setCaretPosition(cp);
	        		}
				}
	        	@Override
				public Dimension getPreferredSize() {
	        		String text = getText();
	        		int cp = getCaretPosition();
	        		setText("");
	        		try {
	        			return super.getPreferredSize();
	        		} finally {
	        			setText(text);
	        			setCaretPosition(cp);
	        		}
				}
	        };
	        setValueFieldText(valueTextField, comparison.value);
	        comparison.valueTextField = valueTextField;
	        allFields.add(valueTextField);
	    	
	        final int finalMaxWidth = maxWidth;
			SmallButton operatorField = new LightBorderSmallButton(null) {
				@Override
				protected void onClick(MouseEvent evt) {
					if (allDisabled) {
						return;
					}
					JPopupMenu popup = new JPopupMenu();
					for (Operator operator: Operator.values()) {
						JMenuItem item = new JMenuItem(operator.sql);
						item.addActionListener(e -> {
							setText(operator.render());
							accept(comparison, valueTextField.getText(), operator);
							comparison.operator = operator;
						});
						popup.add(item);
					}
					if (UIUtil.plaf == PLAF.FLAT) {
						setBackground(UIUtil.BG_FLATSELECTED);
						setBorder(new LineBorder(UIUtil.BG_FLATSELECTED, 2, true));
						freezed = true;
						popup.addPropertyChangeListener("visible", new PropertyChangeListener() {
							@Override
							public void propertyChange(PropertyChangeEvent evt) {
								if (Boolean.FALSE.equals(evt.getNewValue())) {
									freezed = false;
									onMouseExited();
								}
							}
						});
					}
					UIUtil.showPopup(this, 0, getHeight(), popup);
				}
				@Override
				public Dimension getPreferredSize() {
					return new Dimension(finalMaxWidth, super.getPreferredSize().height);
				}
			};
			operatorField.addFocusListener(focusListener);
			allFields.add(operatorField);
	    	comparison.operatorField = operatorField;
	        operatorField.setText(" " + comparison.operator.sql + " ");
	        operatorField.setVisible(!comparison.value.toLowerCase().matches("\\s*is\\s+(not\\s+)?null\\s*"));
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 2;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.WEST;
	        searchFieldsPanel.add(wrap.apply(operatorField, y), gridBagConstraints);
	        
	        valueTextField.addFocusListener(new FocusListener() {
				@Override
				public void focusLost(FocusEvent e) {
				}
				@Override
				public void focusGained(FocusEvent e) {
					if (!editorHadFocus) {
						editor.grabFocus();
						valueTextField.grabFocus();
						editorHadFocus = true;
					}
					boolean ok = true;
					if (onFocusGained != null) {
						ok = onFocusGained.apply(valueTextField);
						if (ok) {
							onFocusGained = null;
						}
					}
					String currentText = valueTextField.getText();
					if (ok) {
						onFocusGained = (val) -> {
							if (val == valueTextField) {
								return false;
							}
							if (allFields.contains(valueTextField) && !currentText.equals(valueTextField.getText()) && !valueTextField.hasFocus()) {
								accept(comparison, valueTextField.getText(), comparison.operator);
							}
							return true;
						};
					}
					Pair<Integer, Integer> pos = fullPositions.get(comparison.column);
					if (pos != null) {
						if (!latestParsedCondition.equals(editor.getText())) {
							editor.setText(latestParsedCondition);
						}
						
						int offset = 0;
						try {
							String nameValue = editor.getText(pos.a, pos.b - pos.a);
							String prefix = nameValue.replaceFirst("(?is)^(and\\s+).*", "$1");
							if (!prefix.equals(nameValue)) {
								offset = prefix.length();
							}
						} catch (BadLocationException e1) {
							// ignore
						}
						hightlight(editor, pos.a + offset, pos.b);
						hightlight(comparison.column);
						UIUtil.invokeLater(() -> jScrollPane1.getViewport().setViewPosition(new Point(0, jScrollPane1.getViewport().getViewPosition().y)));
					} else {
						editor.setCaretPosition(0);
					}
				}
			});
	        valueTextField.addKeyListener(new KeyListener() {
				@Override
				public void keyTyped(KeyEvent e) {
					if (e.getKeyChar() == '\n') {
						accept(comparison, valueTextField.getText(), comparison.operator);
					} else if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
						onEscape();
					}
					stopFading();
				}
				@Override
				public void keyReleased(KeyEvent e) {
				}
				@Override
				public void keyPressed(KeyEvent e) {
					stopFading();
				}
			});
			DBConditionEditor.initialObserve(valueTextField, x -> openStringSearchPanel(valueTextField, comparison), () -> openStringSearchPanel(valueTextField, comparison));
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 3;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(valueTextField, y), gridBagConstraints);
	        
	        JPanel sepPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 4, 4));
	        sepPanel.setOpaque(false);
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y + 2;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.gridwidth = 5;
	        searchFieldsPanel.add(wrap.apply(sepPanel, y), gridBagConstraints);

	        int columnIndex = 0;
	        while (columnIndex < table.getColumns().size()) {
	        	if (comparison.column == table.getColumns().get(columnIndex)) {
		        	if (cellEditor != null && cellEditor.getColumnTypes().length > 0 && !cellEditor.isEditable(table, columnIndex, null)) {
		        		valueTextField.setEnabled(false);
		        	}
	        	}
	        	++columnIndex;
			}
	        
	        ++y;
		}
        JPanel sepPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 1, 1));
        sepPanel.setOpaque(false);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3 * y;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        gridBagConstraints.gridwidth = 5;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        searchFieldsPanel.add(wrap.apply(sepPanel, 1), gridBagConstraints);
		
        clearButton.setEnabled(comparisons.stream().anyMatch(c -> c.valueTextField != null && c.valueTextField.getText().trim().length() > 0));
        
        UIUtil.initToolTips(searchFieldsPanel);
		revalidate();
		focusedComparision.ifPresent(c -> c.valueTextField.grabFocus());
    }

	protected String columnToolTip(Column column) {
		return columnLabel(column);
	}

	protected String columnLabel(Column column) {
		return " " + Quoting.staticUnquote(column.name);
	}
	
	protected String columnAlias(Column column) {
		return Quoting.staticUnquote(column.name);
	}
	
	public String getTableAlias() {
		return tableAlias;
	}

	public void setTableAlias(String tableAlias) {
		this.tableAlias = tableAlias;
	}

	private void setValueFieldText(JTextField valueTextField, String value) {
		valueTextField.setText(value);
		UIUtil.invokeLater(() -> valueTextField.setToolTipText(value == null || value.isEmpty()? null : value));
		if (value.trim().length() > 0) {
			valueTextField.setCaretPosition(0);
		}
	}

	protected void addColumn() {
    	Object toFind = searchComboBox.getSelectedItem();
    	for (Column column: table.getColumns()) {
        	if (columnAlias(column).equals(toFind)) {
        		Comparison newComparision = new Comparison(Operator.Equal, column);
				comparisons.add(newComparision);
            	updateSearchUI();
            	storeConfig();
            	UIUtil.invokeLater(() -> {
            		if (newComparision.valueTextField.isEnabled()) {
	            		jPanel3.scrollRectToVisible(new Rectangle(0, jPanel3.getHeight() - 1, 1, 1));
	            		openStringSearchPanel(newComparision.valueTextField, newComparision);
            		}
            	});
            	break;
        	}
        }
	}

	private void storeConfig() {
		if (initialColumn < 0) {
			List<String> config = new ArrayList<String>(comparisons.stream().map(c -> c.column.name).collect(Collectors.toList()));
			ConditionStorage.store(table, config, executionContext);
		}
	}
	
	private Function<Object, Boolean> onFocusGained;
	
	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        splitPane = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        searchComboBox = new javax.swing.JComboBox<>();
        sortCheckBox = new javax.swing.JCheckBox();
        titlePanel = new javax.swing.JPanel();
        tableLabel = new javax.swing.JLabel();
        closeButtonContainerPanel = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel3 = new javax.swing.JPanel();
        searchFieldsPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        statusPanel = new javax.swing.JPanel();
        popupTitelPanel = new javax.swing.JPanel();
        popupTabNameLabel = new javax.swing.JLabel();
        jPanel7 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        syntaxPanePanel = new javax.swing.JPanel();
        applyButton = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        splitPane.setBorder(null);
        splitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        splitPane.setContinuousLayout(true);
        splitPane.setOneTouchExpandable(true);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jPanel6.setLayout(new java.awt.GridBagLayout());

        searchComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel6.add(searchComboBox, gridBagConstraints);

        sortCheckBox.setText("Sort Columns  ");
        sortCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel6.add(sortCheckBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 4, 0);
        jPanel5.add(jPanel6, gridBagConstraints);

        titlePanel.setBackground(java.awt.Color.white);
        titlePanel.setLayout(new java.awt.GridBagLayout());

        tableLabel.setText("<html><i>no table selected</i></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(6, 4, 6, 0);
        titlePanel.add(tableLabel, gridBagConstraints);

        closeButtonContainerPanel.setOpaque(false);
        closeButtonContainerPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        titlePanel.add(closeButtonContainerPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel5.add(titlePanel, gridBagConstraints);

        jScrollPane2.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        searchFieldsPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(searchFieldsPanel, gridBagConstraints);

        jScrollPane2.setViewportView(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(jScrollPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel5, gridBagConstraints);

        splitPane.setTopComponent(jPanel1);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        statusPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel2.add(statusPanel, gridBagConstraints);

        popupTitelPanel.setBackground(new java.awt.Color(250, 250, 255));
        popupTitelPanel.setLayout(new java.awt.GridBagLayout());

        popupTabNameLabel.setFont(popupTabNameLabel.getFont().deriveFont(popupTabNameLabel.getFont().getStyle() | java.awt.Font.BOLD));
        popupTabNameLabel.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        popupTitelPanel.add(popupTabNameLabel, gridBagConstraints);

        jPanel7.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));
        jPanel7.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 32, 0, 0);
        popupTitelPanel.add(jPanel7, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanel2.add(popupTitelPanel, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        syntaxPanePanel.setLayout(new java.awt.BorderLayout());
        jScrollPane1.setViewportView(syntaxPanePanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(jScrollPane1, gridBagConstraints);

        applyButton.setText("Apply");
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel4.add(applyButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel2.add(jPanel4, gridBagConstraints);

        jLabel1.setText("Condition");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel2.add(jLabel1, gridBagConstraints);

        splitPane.setBottomComponent(jPanel2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(splitPane, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void sortCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortCheckBoxActionPerformed
    	updateSearchUI();
    }//GEN-LAST:event_sortCheckBoxActionPerformed

    private void applyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_applyButtonActionPerformed
        parseCondition();
    }//GEN-LAST:event_applyButtonActionPerformed

    private Timer reduceOpacityRetentionTimer = new Timer((int) (REDUCED_OPACITY_RETENTION_TIME * 1000), null);
    {
		reduceOpacityRetentionTimer.setRepeats(false);
    }
    private int fadeStep = -1;
    private boolean opacityListenerEnabled = true;
    private ActionListener fadeAction;
    private boolean opacityPending = false;
    private boolean useOpacity = false;
    private float nextOpacity;

    private void setOpacity(float opacity) {
    	if (useOpacity) {
			nextOpacity = opacity;
			if (!opacityPending) {
				opacityPending = true;
				UIUtil.invokeLater(16, () -> {
					opacityPending = false;
					setOpacityImmediatelly(nextOpacity);
			    	if (nextOpacity >= 1f) {
						fadeStep = -1;
						reduceOpacityRetentionTimer.stop();
					}
				});
			}
    	}
	}
    
    private void setOpacityImmediatelly(float opacity) {
    	if (useOpacity) {
    		UIUtil.addDW(windowAncestor(), opacity);
    	}
	}

	private Window windowAncestor() {
		return SwingUtilities.getWindowAncestor(WhereConditionEditorPanel.this);
	}

	private void startOpacityTimer() {
		if (fadeAction != null) {
			fadeAction.actionPerformed(null);
		}
		reduceOpacityRetentionTimer.restart();
	};
	
	private void stopFading() {
		if (useOpacity) {
	    	setOpacity(1f);
	    	reduceOpacityRetentionTimer.stop();
		}
	}
	
	public void prepareStringSearchPanelOfInitialColumn(Window dialog) {
		if (initialColumn >= 0) {
			useOpacity = true;
			setOpacityImmediatelly(0f);
			UIUtil.startDW();
			UIUtil.addDW(windowAncestor());
		}
	}
	
	public void openStringSearchPanelOfInitialColumn(Window dialog) {
		if (initialColumn >= 0) {
			comparisons.stream().filter(c -> table.getColumns().get(initialColumn).equals(c.column)).findAny().ifPresent(new Consumer<Comparison>() {
				@Override
				public void accept(Comparison c) {
					openStringSearchPanel(c.valueTextField, c, true);
					setOpacity(REDUCED_OPACITY);
					setOpacityImmediatelly(0f);
					fadeAction = e -> {
						if (fadeStep >= 0) {
							fadeStep += 4;
							if (fadeStep >= 100) {
								if (useOpacity && dialog.getOpacity() < 1f && !UIUtil.opacityfailed) {
									dialog.setVisible(false);
									dialog.dispose();
								}
							} else {
								setOpacity(REDUCED_OPACITY_FADE_START * (fadeStep < 66? 1f : (((100 - fadeStep) * 3f / 100.0f))));
								reduceOpacityRetentionTimer.restart();
							}
						}
					};
					reduceOpacityRetentionTimer = new Timer((int) (REDUCED_OPACITY_RETENTION_TIME * 1000 / 100 * 4), fadeAction);
					reduceOpacityRetentionTimer.setRepeats(false);
					UIUtil.invokeLater(() -> {
						setOpacity(REDUCED_OPACITY);
						reduceOpacityRetentionTimer.stop();
						addListener(dialog, new MouseAdapter() {
							@Override
							public void mouseEntered(MouseEvent e) {
								if (opacityListenerEnabled && !dialog.isFocused()) {
									setOpacity(1f);
									reduceOpacityRetentionTimer.stop();
								}
							}
							@Override
							public void mouseExited(MouseEvent e) {
								if (opacityListenerEnabled && !dialog.isFocused()) {
									setOpacity(REDUCED_OPACITY);
									reduceOpacityRetentionTimer.stop();
								}
							}
							@Override
							public void mousePressed(MouseEvent e) {
								if (fadeStep < 95) {
									setOpacity(1f);
								}
							}
							@Override
							public void mouseClicked(MouseEvent e) {
								if (fadeStep < 95) {
									setOpacity(1f);
								}
							}
						});
					});
				}
				private void addListener(Component comp, MouseListener listener) {
					comp.addMouseListener(listener);
					if (comp instanceof Container) {
						for (Component sub: ((Container) comp).getComponents()) {
							addListener(sub, listener);
						}
					}
				}
			});
		}
	}

	private void openStringSearchPanel(JTextField valueTextField, Comparison comparison) {
		openStringSearchPanel(valueTextField, comparison, false);
	}

	@SuppressWarnings("unchecked")
	private void openStringSearchPanel(JTextField valueTextField, Comparison comparison, boolean popupOnTop) {
    	if (getParent() == null) {
    		return; // too late
    	}
    	if (extJoinsUpdater != null) {
    		extJoins.clear();
    		try {
    			extJoinsUpdater.run();
    		} catch (Exception e) {
    			LogUtil.warn(e);
    		}
    	}
    	Pair<Integer, Integer> pos = fullPositions.get(comparison.column);
		if (pos != null) {
			int offset = 0;
			try {
				String nameValue = editor.getText(pos.a, pos.b - pos.a);
				String prefix = nameValue.replaceFirst("(?is)^(and\\s+).*", "$1");
				if (!prefix.equals(nameValue)) {
					offset = prefix.length();
				}
			} catch (BadLocationException e1) {
				// ignore
			}
			hightlight(editor, pos.a + offset, pos.b);
			hightlight(comparison.column);
		}

		Window owner = SwingUtilities.getWindowAncestor(valueTextField);
		if (!popupOnTop) {
			setOpacity(1f);
		}
		@SuppressWarnings("rawtypes")
		JComboBox combobox = new JComboBox();
		DefaultComboBoxModel<String> defaultComboBoxModel = new DefaultComboBoxModel<String>();
		combobox.setModel(defaultComboBoxModel);
		Map<String, Consumer<JLabel>> renderConsumer = new IdentityHashMap<String, Consumer<JLabel>>();
		ImageIcon sHistIcon = UIUtil.scaleIcon(this, histIcon);
		if (comparison.column.name != null) {
			getHistory(comparison.column).forEach(h -> {
				defaultComboBoxModel.addElement(h);
				renderConsumer.put(h, label -> {
					label.setIcon(sHistIcon);
				});
			});
		}
		Color color = new Color(0, 100, 200);
		String item;
		if (isColumnNullable(table, comparison.column)) {
			item = IS_NULL;
			ImageIcon sNullIcon = UIUtil.scaleIcon(this, nullIcon);
			defaultComboBoxModel.addElement(item);
			Consumer<JLabel> lableConsumer = label -> {
				label.setIcon(sNullIcon);
				label.setForeground(color);
			};
			renderConsumer.put(item, lableConsumer);
			item = IS_NOT_NULL;
			defaultComboBoxModel.addElement(item);
			renderConsumer.put(item, lableConsumer);
		}
		
		Window tlw = owner;
		while (tlw != null && tlw.getOwner() != null) {
			tlw = tlw.getOwner();
		}
		Window topLevelWindow = tlw;
		
		List<StringSearchPanel> theSearchPanel = new ArrayList<StringSearchPanel>();
		String origText = valueTextField.getText();
		Object cancellationContext = getNextCancellationContext();
		StringSearchPanel searchPanel = new StringSearchPanel(null, combobox, null, null, null, new Runnable() {
			@Override
			public void run() {
				if (theSearchPanel.get(0).isExplictlyClosed()) {
					setValueFieldText(valueTextField, theSearchPanel.get(0).getPlainValue());
					accept(comparison, theSearchPanel.get(0).getPlainValue(), theSearchPanel.get(0).isPlainValueFromCombobox(), comparison.operator);
					if (initialColumn >= 0 && popupOnTop) {
						if (REDUCED_OPACITY_FADE_START == 0) {
							setVisible(false);
						}
						fadeStep = 0;
						setOpacity(REDUCED_OPACITY_FADE_START);
						startOpacityTimer();
						opacityListenerEnabled = false;
					}
				}
		    	cancel(cancellationContext);
			}
		}, renderConsumer) {
			protected void onClosing() {
				setOpacity(1f);
				opacityListenerEnabled = false;
			}
			@Override
			protected void onClose(String text) {
				setValueFieldText(valueTextField, text);
		    	cancel(cancellationContext);
				setOpacity(1f);
				opacityListenerEnabled = false;
			}
			@Override
			protected void onAbort() {
				setValueFieldText(valueTextField, origText);
		    	cancel(cancellationContext);
				setOpacity(1f);
				opacityListenerEnabled = false;
			}
			@Override
			protected Integer preferredWidth() {
				return 260;
			}
			@Override
			protected Integer maxX() {
				if (topLevelWindow != null) {
					return topLevelWindow.getX() + topLevelWindow.getWidth() - preferredWidth() - 8;
				} else {
					return null;
				}
			}
			@Override
			protected Integer maxY(int height) {
				if (topLevelWindow != null) {
					return topLevelWindow.getY() + topLevelWindow.getHeight() - height - 8;
				} else {
					return null;
				}
			}
		};
		theSearchPanel.add(searchPanel);
		
		if (!latestParsedCondition.equals(editor.getText())) {
			editor.setText(latestParsedCondition);
		}
		
		String condition;
		Pair<Integer, Integer> pp = WCTypeAnalyser.getPositivePosition(comparison.column.name, getAlternativeNames(comparison.column.name), editor.getText());
		if (!(pp == null || pp.a != 0)) {
			if (tableAlias != null) {
				String name = tableAlias + "." + comparison.column.name;
				pp = WCTypeAnalyser.getPositivePosition(name, getAlternativeNames(comparison.column.name), editor.getText());
			}
		}
		boolean positiveEquals;
		if (comparison.operator == Operator.Equal && (pp == null || pp.a != 0)) {
			positiveEquals = true;
			pos = fullPositions.get(comparison.column);
			if (pos != null) {
				condition = removeErasedFragment("\f", editor.getText().substring(0, pos.a) + "\f" + editor.getText().substring(pos.b)).trim();;
			} else {
				condition = editor.getText().trim();
			}
		} else {
			positiveEquals = false;
			condition = "";
		}
		searchPanel.setCloseOwner(asPopup);
		JCheckBox fullSearchCheckbox = new JCheckBox("Show all values");
		fullSearchCheckbox.setToolTipText(
				"<html><i>selected</i>:<b> show all values from column</b><hr>"
				+ "<i>not selected</i>:<b> Show only values where a non-empty result is retrieved considering the overall condition.</b></html>");
		Point point = new Point(0, 0);
		SwingUtilities.convertPointToScreen(point, valueTextField);
		Point pointO = new Point(0, 0);
		SwingUtilities.convertPointToScreen(pointO, this);
		if (popupOnTop) {
			point.x = (point.x + pointO.x) / 2;
			point.y = pointO.y;
		}
		searchPanel.withSizeGrip();
		int estDVCount = estimateDistinctExistingValues(comparison, condition);
		Integer estimatedItemsCount = defaultComboBoxModel.getSize() + estDVCount ;
		searchPanel.setEstimatedItemsCount(estimatedItemsCount);
		searchPanel.setIndicateLeadingAndTrailingSpaces(!UIUtil.isCHARType(comparison.column));
		
		UIUtil.invokeLater(2, () -> {
			if (opacityPending) {
				if (initialColumn >= 0) {
					UIUtil.addDW(windowAncestor(), nextOpacity);
				} else {
					setOpacityImmediatelly(nextOpacity);
				}
			}
			UIUtil.startDW();
			searchPanel.find(owner, "Condition", point.x, point.y, true);
		});
		String initialSearchText = valueTextField.getText();
		searchPanel.setInitialValue(initialSearchText);
		searchPanel.setStatus("loading existing values...", null);
		JPanel bottom = new JPanel(new GridBagLayout());
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        bottom.add(fullSearchCheckbox, gridBagConstraints);
        SmallButton clearCacheButton = new LightBorderSmallButton(resetIcon) {
			@Override
			protected void onClick(MouseEvent e) {
				clearCache();
				searchPanel.abort();
				UIUtil.invokeLater(() -> openStringSearchPanel(valueTextField, comparison));
			}
		};
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1;
        bottom.add(new JPanel(null), gridBagConstraints);
		clearCacheButton.setText("Clear Cache");
        clearCacheButton.setToolTipText(
        		"<html>The values were cached by a previous request and are therefore potentially stale. <br>"
        		+ "Clearing the cache has the effect of reading up-to-date values.</html>");
        clearCacheButton.setVisible(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        bottom.add(clearCacheButton, gridBagConstraints);
		searchPanel.addBottomcomponent(bottom);
		fullSearchCheckbox.setVisible(false);
		List<String> initialModel = new ArrayList<String>();
		for (int i = 0; i < defaultComboBoxModel.getSize(); ++i) {
			initialModel.add(defaultComboBoxModel.getElementAt(i));
		}
		Pattern nullPattern = Pattern.compile("\\s*is\\s+(not\\s+)?null\\s*", Pattern.CASE_INSENSITIVE);
				
		runnableQueue.add(new Runnable() {
			public void run() {
				boolean[] fromCache = new boolean[1];
				boolean[] fromCacheFull = new boolean[1];
				int[] incomplete = new int[2];
				boolean[] withNull = new boolean[1];
				incomplete[0] = 0;
				LinkedHashMap<String, Integer> distinctExisting = null;
				List<String> distinctExistingModel = new ArrayList<String>();
				if (!condition.isEmpty() || !extJoins.isEmpty()) {
					try {
						distinctExisting = loadDistinctExistingValues(comparison, cancellationContext, incomplete, withNull, fromCache,
								condition, true);
					} catch (CancellationException e) {
						return;
					} catch (Throwable e) {
						LogUtil.warn(e);
					}
				}
				LinkedHashMap<String, Integer> finalDistinctExisting = distinctExisting;
				UIUtil.invokeLater(() -> {
					fullSearchCheckbox.setVisible(true);
					fullSearchCheckbox.setEnabled(false);
					fullSearchCheckbox.setSelected(finalDistinctExisting == null);
					if (finalDistinctExisting != null) {
						setStatus(incomplete, finalDistinctExisting);
						defaultComboBoxModel.removeAllElements();
						initialModel.forEach(s -> {
							if (finalDistinctExisting.containsKey(s) || (withNull[0] && !finalDistinctExisting.isEmpty() && nullPattern.matcher(s).matches())) {
								defaultComboBoxModel.addElement(s);
							}
						});
						finalDistinctExisting.keySet().forEach(s -> {
							if (!nullPattern.matcher(s).matches()) {
								defaultComboBoxModel.addElement(s);
								renderConsumer.put(s, label -> label.setIcon(UIUtil.scaleIcon(WhereConditionEditorPanel.this, emptyIcon)));
							}
						});
						distinctExistingModel.clear();
						for (int i = 0; i < defaultComboBoxModel.getSize(); ++i) {
							distinctExistingModel.add(defaultComboBoxModel.getElementAt(i));
						}
						searchPanel.setEstimatedItemsCount(defaultComboBoxModel.getSize());
						if (fromCache[0]) {
							clearCacheButton.setVisible(true);
						}
						searchPanel.updateList(false, true);
						UIUtil.stopDW();
					}
				});
				UIUtil.invokeLater(6, () -> {
					searchPanel.resetHeight();
				});

				int[] incompleteFull = new int[1];
				incompleteFull[0] = 0;
				boolean[] withNullFull = new boolean[1];
				withNullFull[0] = false;
				LinkedHashMap<String, Integer> distinctExistingFull = null;
				List<String> distinctExistingFullModel = new ArrayList<String>();
				try {
					distinctExistingFull = loadDistinctExistingValues(comparison, cancellationContext, incompleteFull, withNullFull, fromCacheFull, "", false);
					// dedup
//					if (distinctExisting != null) {
//						Map<String, String> fullSet = new HashMap<String, String>();
//						distinctExistingFull.forEach(s -> fullSet.put(s, s));;
//						int s = distinctExisting.size();
//						for (int i = 0; i < s; ++i) {
//							String f = fullSet.get(distinctExisting.get(i));
//							if (f != null) {
//								distinctExisting.set(i, f);
//							}
//						}
//					}
				} catch (CancellationException e) {
					return;
				} catch (Throwable e) {
					UIUtil.invokeLater(() -> {
						searchPanel.setStatus("error loading values", UIUtil.scaleIcon(searchPanel, warnIcon));
						UIUtil.stopDW();
					});
					return;
				}
				LinkedHashMap<String, Integer> finalDistinctExistingFull = distinctExistingFull;
				UIUtil.invokeLater(3, new Runnable() { public void run() {
					if (finalDistinctExistingFull != null) {
						fullSearchCheckbox.setEnabled(finalDistinctExisting != null);
						if (finalDistinctExisting != null
								&& finalDistinctExisting.equals(finalDistinctExistingFull)
								&& distinctExistingModel.size() == distinctExistingFullModel.size()) {
							fullSearchCheckbox.setSelected(true);
							fullSearchCheckbox.setEnabled(false);
						}
						finalDistinctExistingFull.keySet().forEach(s -> {
							if (!nullPattern.matcher(s).matches()) {
								defaultComboBoxModel.addElement(s);
								renderConsumer.put(s, label -> label.setIcon(UIUtil.scaleIcon(WhereConditionEditorPanel.this, emptyIcon)));
							}
						});
						distinctExistingFullModel.clear();
						if (fullSearchCheckbox.isEnabled()) {
							distinctExistingFullModel.addAll(initialModel);
						} else {
							initialModel.forEach(s -> {
								if (finalDistinctExistingFull.containsKey(s) || (withNullFull[0] && !finalDistinctExistingFull.isEmpty() && nullPattern.matcher(s).matches())) {
									distinctExistingFullModel.add(s);
								}
							});
							if (distinctExistingFullModel.isEmpty() && withNullFull[0]) {
								String text = IS_NULL;
								distinctExistingFullModel.add(text);
								ImageIcon sNullIcon = UIUtil.scaleIcon(WhereConditionEditorPanel.this, nullIcon);
								searchPanel.renderConsumer.put(text, label -> {
									label.setIcon(sNullIcon);
								});
							}
						}
						
						finalDistinctExistingFull.keySet().forEach(s -> { if (!nullPattern.matcher(s).matches()) { distinctExistingFullModel.add(s); }});
						if (fromCache[0] || fromCacheFull[0]) {
							clearCacheButton.setVisible(true);
						}
						ActionListener action = e -> {
							defaultComboBoxModel.removeAllElements();
							if (fullSearchCheckbox.isSelected()) {
								distinctExistingFullModel.forEach(s -> {
									defaultComboBoxModel.addElement(s);
								});
								setStatus(incompleteFull, finalDistinctExistingFull);
								clearCacheButton.setVisible(fromCacheFull[0]);
								searchPanel.setLightCounters(!positiveEquals || finalDistinctExisting != null && !finalDistinctExisting.equals(finalDistinctExistingFull));
							} else {
								distinctExistingModel.forEach(s -> {
									defaultComboBoxModel.addElement(s);
								});
								setStatus(incomplete, finalDistinctExisting);
								clearCacheButton.setVisible(fromCache[0]);
								searchPanel.setLightCounters(false);
							}
							searchPanel.setEstimatedItemsCount(defaultComboBoxModel.getSize());
							searchPanel.resetHeight();
							searchPanel.updateList(false, true);
						};
						fullSearchCheckbox.addActionListener(action);
						action.actionPerformed(null);
						try {
							if (comparison.column.name != null) {
								String st = searchPanel.getSearchTextField().getText();
								if (initialSearchText.equals(st)) {
									ListModel<String> m = searchPanel.getSearchList().getModel();
									if (m instanceof DefaultListModel) {
										int i;
										@SuppressWarnings("rawtypes")
										DefaultListModel model = (DefaultListModel) m;
										if (model.indexOf(st) >= 0) {
											if (getHistory(comparison.column).contains(st)) {
												i = model.indexOf(st);
											} else {
												renderConsumer.put(st, label -> {
													label.setIcon(sHistIcon);
												});
												i = 0;
												model.insertElementAt(st, i);
											}
											if (i >= 0) {
												boolean hadFocus = searchPanel.getSearchTextField().hasFocus();
												searchPanel.getSearchList().setSelectedIndex(i);
												if (hadFocus) {
													searchPanel.getSearchTextField().grabFocus();
												}
											}
										}
									}
								} else {
									searchPanel.updateList(true, true);
								}
							}
						} catch (Throwable t) {
							LogUtil.warn(t);
						}
						UIUtil.invokeLater(2, () -> UIUtil.stopDW());
					}
				}});
			}

			protected void setStatus(int[] incomplete, LinkedHashMap<String, Integer> finalDistinctExisting) {
				if (incomplete[0] > 0 || finalDistinctExisting.size() > MAX_NUM_DISTINCTEXISTINGVALUES) {
					searchPanel.setStatus("incomplete"
								+ (finalDistinctExisting.size() > MAX_NUM_DISTINCTEXISTINGVALUES? ("(>" + MAX_NUM_DISTINCTEXISTINGVALUES + " values)")
										: 
								(" (" + (incomplete[0] % MAX_NUM_DISTINCTEXISTINGVALUES) 
								+ " missing"
								+ (incomplete[0] > MAX_NUM_DISTINCTEXISTINGVALUES? ", multi-line text" : "") + ")")),
								UIUtil.scaleIcon(searchPanel, warnIcon));
				} else {
					searchPanel.setStatus(null, null);
				}
				if (!table.isDistinct()) {
					boolean containsIsNull = finalDistinctExisting.containsKey(IS_NULL);
					boolean containsIsNotNull = finalDistinctExisting.containsKey(IS_NOT_NULL);
					if (containsIsNotNull && !containsIsNull) {
						finalDistinctExisting.put(IS_NULL, Integer.MIN_VALUE);
					} else if (!containsIsNotNull && containsIsNull) {
						finalDistinctExisting.put(IS_NOT_NULL, Integer.MIN_VALUE);
					}
					searchPanel.setStringCount(finalDistinctExisting);
				}
			}
		});
    }
    
	protected Set<String> getAlternativeNames(String name) {
		return null;
	}

	private boolean editorHadFocus = false;

	private Object nextCancellationContext = new Object();
    
    public Object getNextCancellationContext() {
		Object cancellationContext = nextCancellationContext;
		nextCancellationContext = new Object();
		return cancellationContext;
	}

	private void cancel(Object cancellationContext) {
    	CancellationHandler.cancelSilently(cancellationContext);
    	nextCancellationContext = new Object();
    }
    
    private final String DISTINCTEXISTINGVALUESCACHEKEY = "DistinctExistingValuesCache";
    private final String DISTINCTEXISTINGVALUESICCACHEKEY = "DistinctExistingValuesICCache";
    private final String DISTINCTEXISTINGVALUESWNCACHEKEY = "DistinctExistingValuesWNCache";
    private final String DISTINCTEXISTINGVALUESTSKEY = "DistinctExistingValuesTS";

	protected synchronized void clearCache() {
		session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESCACHEKEY, null);
		session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESICCACHEKEY, null);
		session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESWNCACHEKEY, null);
		session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESTSKEY, null);
	}

	@SuppressWarnings("unchecked")
	private LinkedHashMap<String, Integer> loadDistinctExistingValues(Comparison comparison, Object cancellationContext, int[] incomplete, boolean[] withNull, boolean[] fromCache, String condition, boolean withExtJoins) throws SQLException {
		final int MAX_TEXT_LENGTH = 1024 * 4;
		LinkedHashMap<String, Integer> result;
		Map<Pair<String, String>, Integer> icCache;
		Map<Pair<String, String>, Boolean> wnCache;
		Map<Pair<String, String>, LinkedHashMap<String, Integer>> cache;
		String tabName = table.getName();
		Pair<String, String> key = new Pair<String, String>(tabName + "+" + condition, comparison.column.name);
		synchronized (this) {
			Long ts = (Long) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESTSKEY);
			cache = (Map<Pair<String, String>, LinkedHashMap<String, Integer>>) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESCACHEKEY);
			if (cache == null || ts == null || ts < Session.lastUpdateTS || ts < System.currentTimeMillis() - 10 * (1000 * 60 * 60) /* 10 h */) {
				cache = new LRUCache<Pair<String,String>, LinkedHashMap<String, Integer>>(SIZE_DISTINCTEXISTINGVALUESCACHE);
				session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESCACHEKEY, cache);
				session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESTSKEY, System.currentTimeMillis());
			}
			icCache = (Map<Pair<String, String>, Integer>) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESICCACHEKEY);
			if (icCache == null) {
				icCache = new LRUCache<Pair<String,String>, Integer>(SIZE_DISTINCTEXISTINGVALUESCACHE);
				session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESICCACHEKEY, icCache);
			}
			result = cache.get(key);
			Integer obj = icCache.get(key);
			if (obj instanceof Integer) {
				incomplete[0] = (Integer) obj;
			}
			wnCache = (Map<Pair<String, String>, Boolean>) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESWNCACHEKEY);
			if (wnCache == null) {
				wnCache = new LRUCache<Pair<String,String>, Boolean>(SIZE_DISTINCTEXISTINGVALUESCACHE);
				session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESWNCACHEKEY, wnCache);
			}
			if (Boolean.TRUE.equals(wnCache.get(key))) {
				withNull[0] = true;
			}
		}
		long time = -1;
		
		if (result == null) {
			long startTime = System.currentTimeMillis();
			fromCache[0] = false;
			result = new LinkedHashMap<String, Integer>();
			int columnIndex = 0;
			while (columnIndex < table.getColumns().size()) {
				if (comparison.column.equals(table.getColumns().get(columnIndex))) {
					if (tableAlias == null && condition.isEmpty() && !inSQLConsole()) {
						if (comparison.column.name.startsWith("A.")) {
							tabName = tabName.replaceFirst("^(.* A)\\s+join(.* B)\\s+on.*$", "$1");
						} else if (comparison.column.name.startsWith("B.")) {
							tabName = tabName.replaceFirst("^(.* A)\\s+join(.* B)\\s+on.*$", "$2");
						}
					}
					boolean needsSort = false;
					if (extJoins.isEmpty() || !withExtJoins) {
						loadValues(comparison, cancellationContext, incomplete, withNull, condition, MAX_TEXT_LENGTH, result, tabName,
							columnIndex, "", true);
					} else {
						try {
							if (extJoins.size() == 1) {
								loadValues(comparison, cancellationContext, incomplete, withNull, condition, MAX_TEXT_LENGTH,
										result, tabName, columnIndex, " " + extJoins.get(0), true);
							} else {
								for (String ej : extJoins) {
									loadValues(comparison, cancellationContext, incomplete, withNull, condition, MAX_TEXT_LENGTH,
											result, tabName, columnIndex, " " + ej, false);
									if (result.size() > MAX_NUM_DISTINCTEXISTINGVALUES) {
										break;
									}
								}
								needsSort = true;
							}
						} catch (SQLException e) {
							LogUtil.warn(e);
							incomplete[0] = 0;
							withNull[0] = false;
							result.clear();
							loadValues(comparison, cancellationContext, incomplete, withNull, condition, MAX_TEXT_LENGTH, result,
									tabName, columnIndex, "", true);
						}
						if (needsSort == true || cellEditor.useCaseIntensitiveOrderingInGUI(columnIndex)) {
							sortValues(result, columnIndex);
						}
					}
				}
				++columnIndex;
			}
			time = System.currentTimeMillis() - startTime;
		} else {
			fromCache[0] = true;
		}
		Long sumLength = result.keySet().stream().collect(Collectors.summingLong(String::length));
		if (sumLength == null || sumLength <= MAX_SIZE_DISTINCTEXISTINGVALUES) {
			if (time < 0 || time > 500) {
				cache.put(key, result);
				icCache.put(key, incomplete[0]);
				wnCache.put(key, withNull[0]);
			}
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private void sortValues(LinkedHashMap<String, Integer> resultMap, int columnIndex) {
		List<String> result = new ArrayList<String>(resultMap.keySet());
		if (cellEditor.useCaseIntensitiveOrderingInGUI(columnIndex)) {
			result.sort(String::compareToIgnoreCase);
		} else {
			try {
				Map<String, Comparable<Object>> objects = new HashMap<String, Comparable<Object>>();
				result.forEach(s -> objects.put(s, (Comparable<Object>) cellEditor.textToContent(columnIndex, s, null)));
				result.sort((a, b) -> objects.get(a).compareTo(objects.get(b)));
			} catch (Exception e) {
				result.sort(String::compareToIgnoreCase);
			}
		}
		Map<String, Integer> unsorted = new HashMap<String, Integer>(resultMap);
		resultMap.clear();
		result.forEach(s -> {
			resultMap.put(s, unsorted.get(s));
		});
	}

	protected boolean inSQLConsole() {
		return false;
	}

	private void loadValues(Comparison comparison, Object cancellationContext, int[] incomplete, boolean[] withNull, String condition,
			final int MAX_TEXT_LENGTH, LinkedHashMap<String, Integer> result, String tabName, int columnIndex,
			String extJoin, boolean orderBy) throws SQLException {
		String columnName = comparison.column.name;
		if (tableAlias != null) {
			columnName = tableAlias + "." + columnName;
			tabName += " " + tableAlias;
		}
		String sqlQuery;
		String tableWithCondition = createTableWithCondition(condition, tabName, extJoin);
		String cte = getCTE().trim();
		if (!cte.isEmpty()) {
			cte = cte + " ";
		}
		if (inSQLConsole()) {
			sqlQuery = cte + "Select val, count(1) from (Select " + columnName + " val From " + tableWithCondition + ") Q group by val";
		} else {
			sqlQuery = cte + "Select " + columnName + ", count(1) from " + tableWithCondition + " group by " + columnName;
		}
		AbstractResultSetReader reader = new AbstractResultSetReader() {
			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				Object obj = getCellContentConverter(resultSet, session, session.dbms).getObject(resultSet, 1);
				int rc = resultSet.getInt(2);
				if (obj == null) {
					withNull[0] = true;
					Integer count = result.get(IS_NULL);
					if (count == null) {
						result.put(IS_NULL, -rc);
					} else {
						result.put(IS_NULL, count - rc);
					}
				} else {
					Integer count = result.get(IS_NOT_NULL);
					if (count == null) {
						result.put(IS_NOT_NULL, -rc);
					} else {
						result.put(IS_NOT_NULL, count - rc);
					}
					if (cellEditor.isEditable(table, columnIndex, obj, true)) {
						String text = cellEditor.cellContentToText(columnIndex, obj);
						if (text.length() <= MAX_TEXT_LENGTH) {
							count = result.get(text);
							if (count == null) {
								result.put(text, rc);
							} else {
								result.put(text, count + rc);
							}
						} else {
							incomplete[0]++;
						}
					} else {
						if (incomplete[0] < MAX_NUM_DISTINCTEXISTINGVALUES) {
							if (String.valueOf(obj).indexOf('\n') >= 0) {
								incomplete[0] += MAX_NUM_DISTINCTEXISTINGVALUES;
							}
						}
						incomplete[0]++;
					}
				}
			}
		};
		DBMS.setTmpFetchSize(25100);
		try {
			if (orderBy) {
				Map<String, Integer> prev = new HashMap<String, Integer>(result);
				try {
					session.executeQuery(sqlQuery + " order by " + (inSQLConsole()? "val" : columnName), reader, null, cancellationContext, MAX_NUM_DISTINCTEXISTINGVALUES + 2);
				} catch (SQLException e) {
					result.clear();
					result.putAll(prev);
					// try without ordering
					session.executeQuery(sqlQuery, reader, null, cancellationContext, MAX_NUM_DISTINCTEXISTINGVALUES + 2);
					sortValues(result, columnIndex);
				}
			} else {
				session.executeQuery(sqlQuery, reader, null, cancellationContext, MAX_NUM_DISTINCTEXISTINGVALUES + 2);
			}
		} finally {
			DBMS.setTmpFetchSize(null);
		}
	}

	protected String createTableWithCondition(String condition, String tabName, String extJoin) {
		String tableWithCondition = tabName + extJoin + (condition.isEmpty()? "" : (" where " + condition));
		return tableWithCondition;
	}

	protected String getCTE() {
		return "";
	}

    private int estimateDistinctExistingValues(Comparison comparison, String condition) {
    	@SuppressWarnings("unchecked")
		Map<Pair<String, String>, LinkedHashMap<String, Integer>> cache = (Map<Pair<String, String>, LinkedHashMap<String, Integer>>) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESCACHEKEY);
		if (cache != null) {
			Pair<String, String> key = new Pair<String, String>(table.getName() + "+" + condition, comparison.column.name);
			LinkedHashMap<String, Integer> result = cache.get(key);
			if (result != null) {
				return result.size();
			}
		}
		return 12;
    }
    
	private void accept(Comparison comparison, String value, Operator operator) {
		accept(comparison, value, false, operator);
	}

	private void accept(Comparison comparison, String value, boolean dontTrim, Operator operator) {
		if (value != null) {
			if (!dontTrim) {
				value = value.trim();
			}
			if ("true".equals(value) || "false".equals(value)) {
				session.setSessionProperty(getClass(), "TFDebugInfo", "(" + comparison.column + ", " + comparison.operator + ")");
			}
			Pair<Integer, Integer> newHighlight = null;
			if (!value.equals(comparison.value.trim()) || operator != comparison.operator) {
				editor.beginAtomicEdit();
				editor.setText(latestParsedCondition);
				String erased;
				for (int i = 0;; ++i) {
					erased = "%e" + i + "%";
					if (!latestParsedCondition.contains(erased)) {
						break;
					}
				}
				comparison.operator = operator;
				++UISettings.s12;
				if (value.trim().isEmpty()) {
					// field cleared
					Pair<Integer, Integer> fullPos = fullPositions.get(comparison.column);
					if (fullPos != null) {
						editor.select(fullPos.a, fullPos.b);
						editor.replaceSelection(erased);
						comparison.value = "";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(true);
						}
					}
					valuePositions.remove(comparison.column);
					fullPositions.remove(comparison.column);
				} else {
					String oldValue = comparison.value;
					comparison.value = value;
					String sqlValue;
					String op;
					if (comparison.value.toLowerCase().matches("\\s*(is\\s+)?null\\s*")) {
						sqlValue = IS_NULL;
						op = "";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(false);
						}
					} else if (comparison.value.toLowerCase().matches("\\s*(is\\s)?+not\\s+null\\s*")) {
						sqlValue = IS_NOT_NULL;
						op = "";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(false);
						}
					} else {
						sqlValue = toSqlValue(value, comparison);
						if (sqlValue == null) {
							comparison.valueTextField.setBackground(new Color(255, 200, 200));
							comparison.value = oldValue;
							return;
						}
						op = comparison.operator.sql + " ";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(true);
						}
					}
					Pair<Integer, Integer> pos = valuePositions.get(comparison.column);
					String opSqlValue = op + sqlValue;
					if (pos != null) {
						try {
							if (pos.a > 0) {
								char lastChar = editor.getDocument().getText(pos.a - 1, 1).charAt(0);
								if (!Character.isWhitespace(lastChar)) {
									opSqlValue = " " + opSqlValue;
								}
							}
						} catch (Exception e) {
							// ignore
						}
						editor.select(pos.a, pos.b);
						editor.replaceSelection(opSqlValue);
						hightlight(editor, pos.a, pos.b);
						pos = new Pair<Integer, Integer>(pos.a, pos.a + opSqlValue.length());
						valuePositions.put(comparison.column, pos);
						Pair<Integer, Integer> fullPos = fullPositions.get(comparison.column);
						if (fullPos != null) {
							fullPositions.put(comparison.column, new Pair<Integer, Integer>(fullPos.a, pos.b));
						}
						hightlight(editor, pos.a, pos.b);
					} else {
						if (currentWhereClauseNeedsParentsToBeAppended()) {
							editor.setText("(" + (editor.getText().trim()) + ")");
						}
						
						int start = editor.getDocument().getEndPosition().getOffset() - 1;
						String prefix = "";
						try {
							if (start > 0 && !editor.getDocument().getText(start - 1, 1).matches("\\r|\\n")) {
								prefix = "\n";
							}
							if (!editor.getText().trim().isEmpty()) {
								prefix += "and ";
							}
						} catch (BadLocationException e) {
							prefix = "\n";
						}
						String name = prefix + (tableAlias != null? tableAlias + "." : "") + comparison.column.name + " ";
						editor.append(name + opSqlValue);
						pos = new Pair<Integer, Integer>(start + name.length(), start + name.length() + opSqlValue.length());
						valuePositions.put(comparison.column, pos);
						Pair<Integer, Integer> fullPos = new Pair<Integer, Integer>(start, pos.b);
						fullPositions.put(comparison.column, fullPos);
						newHighlight = fullPos;
					}
				}
				String text = editor.getText();
				text = removeErasedFragment(erased, text);
				if (!text.isEmpty()) {
					text += "\n";
				}
				editor.setText(text);
				editor.endAtomicEdit();
				if (newHighlight != null) {
					hightlight(editor, newHighlight.a, newHighlight.b);
				}
				UIUtil.suspectQuery = text;
				parseCondition();
				hightlight(value.isEmpty()? null : comparison.column);
			}
		}
	}

	protected boolean currentWhereClauseNeedsParentsToBeAppended() {
		try {
			String text = editor.getDocument().getText(0, editor.getDocument().getLength());
			if (!text.trim().isEmpty()) {
				if (!WCTypeAnalyser.isHighPrecedenceExpression(text)) {
					return true;
				}
			}
		} catch (BadLocationException e1) {
			LogUtil.warn(e1);
		}
		return false;
	}

	public static final Color HIGHLIGHT_COLOR = new Color(0, 255, 0, 50);
	private SmartHighlightPainter highlightPainterWOBorder = new SmartHighlightPainter(WhereConditionEditorPanel.HIGHLIGHT_COLOR);
	private SmartHighlightPainter highlightPainter = new SmartHighlightPainter(HIGHLIGHT_COLOR);
	{
		highlightPainter.setPaintBorder(true);
	}
	private Object currentHighlightTag = null;
	
	private void hightlight(RSyntaxTextAreaWithSQLSyntaxStyle editor, int a, int b) {
		editor.select(a, b);
		try {
			if (currentHighlightTag != null) {
				editor.getHighlighter().removeHighlight(currentHighlightTag);
			}
			if (a != b) {
				boolean multiLine = editor.getDocument().getText(a, b - a).contains("\n");
				currentHighlightTag = editor.getHighlighter().addHighlight(a, b, multiLine? highlightPainterWOBorder : highlightPainter);
			}
			editor.select(a, a);
		} catch (/*BadLocation*/ Exception e) {
			// ignore
			return;
		}
	}

	protected void hightlight(Column column) {
		// SQL Console only
	}

	protected String removeErasedFragment(String erased, String sqlCondition) {
		String whiteSpaceOrComment = "\\s*((?:(?:(?:/\\*.*?\\*/)|(?:\\-\\-.*?(?:\n|$))))\\s*)*\\s*";
		
		for (;;) {
			String cleanText = sqlCondition;
			cleanText = cleanText.replaceFirst("(?is)\\(" + whiteSpaceOrComment + erased + whiteSpaceOrComment + "\\)", "$1 " + erased + " $2");
			cleanText = cleanText.replaceFirst("(?is)\\b(and|or|not)\\b" + whiteSpaceOrComment + erased + whiteSpaceOrComment + "\\b(and|or|not)\\b", "$1$2" + erased + "$3$4");
			cleanText = cleanText.replaceFirst("(?is)\\b(and|or|not)\\b" + whiteSpaceOrComment + erased, "$2" + erased);
			
			cleanText = cleanText.replaceFirst("(?is)\\(" + whiteSpaceOrComment + erased + whiteSpaceOrComment + "\\b(and|or)\\b", "($1$2");
			
			if (cleanText.equals(sqlCondition)) {
				break;
			}
			sqlCondition = cleanText;
		}
		sqlCondition = sqlCondition
				.replace(erased, "")
				.replaceFirst("(?is)^" + whiteSpaceOrComment + "\\b(and|or)\\b\\s*", "$1")
				.replaceAll("\\n\\s*\\n", "\n")
				.trim();
		return sqlCondition;
	}

    private String toSqlValue(String value, Comparison comparison) {
    	if (cellEditor == null || cellEditor.getColumnTypes().length == 0) {
    		return null;
    	}
    	int i = 0;
    	while (i < table.getColumns().size()) {
    		if (comparison.column.equals(table.getColumns().get(i))) {
    			Object obj = cellEditor.textToContent(i, value, null);
    			if (obj == BrowserContentCellEditor.INVALID) {
    				return null;
    			}
    			return new CellContentConverter(null, session, session.dbms).toSql(obj);
    		}
    		++i;
    	}
    	return null;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton applyButton;
    private javax.swing.JPanel closeButtonContainerPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel popupTabNameLabel;
    private javax.swing.JPanel popupTitelPanel;
    private javax.swing.JComboBox<String> searchComboBox;
    private javax.swing.JPanel searchFieldsPanel;
    private javax.swing.JCheckBox sortCheckBox;
    private javax.swing.JSplitPane splitPane;
    private javax.swing.JPanel statusPanel;
    private javax.swing.JPanel syntaxPanePanel;
    private javax.swing.JLabel tableLabel;
    private javax.swing.JPanel titlePanel;
    // End of variables declaration//GEN-END:variables
    
    
	/**
	 * For concurrent loading of distinct values.
	 */
	private static final BlockingQueue<Runnable> runnableQueue = new LinkedBlockingQueue<Runnable>();
	private static final int MAX_CONCURRENT_CONNECTIONS = 1;
	static {
		// initialize listeners for #runnableQueue
		for (int i = 0; i < MAX_CONCURRENT_CONNECTIONS; ++i) {
			Thread t = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						Runnable take = null;
						try {
							take = runnableQueue.take();
							take.run();
						} catch (InterruptedException e) {
							// ignore
						} catch (CancellationException e) {
							// ignore
						} catch (Throwable t) {
							t.printStackTrace();
						}
					}
				}
			}, "DistinctValuesReader-" + (i + 1));
			t.setDaemon(true);
			t.start();
		}
	}
	
	private List<String> extJoins = new ArrayList<String>();
	private Runnable extJoinsUpdater;
	
	public void setExtJoinsUpdater(Runnable extJoinsUpdater) {
		this.extJoinsUpdater = extJoinsUpdater;
	}

	public void addExtJoin(String extJoin) {
		extJoins.add(extJoin);
	}

	public void close() {
		onFocusGained = null;
	}
	
	public void acceptValue(Column column, String valAsString) {
		Comparison comparison = null;
		for (Comparison comparisonOpt: comparisons) {
			if (comparisonOpt.column == column) {
				comparison = comparisonOpt;
				break;
			}
		}
		if (comparison == null) {
			comparison = new Comparison(Operator.Equal, column);
			comparison.operatorField = new JLabel("");
			comparison.valueTextField = new JTextField();
		}
		accept(comparison, valAsString, Operator.Equal);
	}

	private JButton clearButton;
	
	private static ImageIcon tableIcon;
	private static ImageIcon clearIcon;
	private static ImageIcon deleteIcon;
	private static ImageIcon histIcon;
	private static ImageIcon emptyIcon;
	private static ImageIcon nullIcon;
	private static ImageIcon resetIcon;
	private static ImageIcon closeIcon;
	private static ImageIcon closeOverIcon;
	private static ImageIcon constraintPKIcon;
    private static ImageIcon okIcon;
	static ImageIcon warnIcon;
	
	static {
        // load images
        tableIcon = UIUtil.readImage("/table.png");
        clearIcon = UIUtil.readImage("/clear.png");
        deleteIcon = UIUtil.readImage("/delete.png");
        histIcon = UIUtil.readImage("/history.png");
        nullIcon = UIUtil.readImage("/null.png");
        emptyIcon = UIUtil.readImage("/empty.png");
        warnIcon = UIUtil.readImage("/wanr.png");
        resetIcon = UIUtil.readImage("/reset.png");
        closeIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close.png"), 1.4);
        closeOverIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/close_over.png"), 1.4);
    	constraintPKIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/constraint_pk.png"));
    	okIcon = UIUtil.readImage("/buttonok.png");
	}
	// TODO support properties
	// TODO multi-value-select? (in clause?)

}

