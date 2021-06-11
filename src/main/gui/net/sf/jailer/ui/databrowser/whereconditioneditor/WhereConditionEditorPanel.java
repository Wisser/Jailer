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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.InlineViewStyle;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentCellEditor;
import net.sf.jailer.ui.databrowser.BrowserContentPane.RunnableWithPriority;
import net.sf.jailer.ui.databrowser.DBConditionEditor;
import net.sf.jailer.ui.databrowser.Desktop;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.util.CancellationException;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CellContentConverter;
import net.sf.jailer.util.LogUtil;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * SQL-Where-Condition Editor.
 *
 * @author Ralf Wisser
 */
public abstract class WhereConditionEditorPanel extends javax.swing.JPanel {

	// TODO remove empty lines before putting text back into sql console after user edit
	// TODO respect quoting
	// TODO offer null and not null
	
	private final int MAX_NUM_DISTINCTEXISTINGVALUES = 10000;
	// TODO caching, cache-clear after modification or timeout
	// TODO weak map
	private final int MAX_SIZE_DISTINCTEXISTINGVALUES = 10_000_000;
	private final int MAX_SIZE_DISTINCTEXISTINGVALUESCACHE = 21_000_000;
	
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
	private Map<String, Consumer<JLabel>> columnLabelConsumers = new HashMap<String, Consumer<JLabel>>();
	
    /**
     * Creates new form SearchPanel
     */
	public WhereConditionEditorPanel(Window parent, DataModel dataModel, Table table, BrowserContentCellEditor cellEditor, Boolean sorted,
			WhereConditionEditorPanel predecessor, RSyntaxTextAreaWithSQLSyntaxStyle editor,
			JComponent closeButton,
			Session session, ExecutionContext executionContext) {
    	this.dataModel = dataModel;
    	this.table = table;
    	this.editor = editor;
    	this.cellEditor = cellEditor;
    	this.session = session;
    	this.executionContext = executionContext;
        initComponents();
        
        if (dataModel != null) {
			try {
				DataModelBasedSQLCompletionProvider provider = new DataModelBasedSQLCompletionProvider(null, dataModel);
				provider.setDefaultClause(SQLCompletionProvider.Clause.WHERE);
				new SQLAutoCompletion(provider, editor);
				if (provider != null) {
					provider.removeAliases();
					if (table != null) {
						provider.addAlias("A", table);
					}
				}
			} catch (SQLException e) {
			}
        }
        
        editor.setEnabled(true);
        syntaxPanePanel.add(editor);
        
        font = tableLabel.getFont();
		tableLabel.setFont(new Font(font.getName(), font.getStyle() | Font.BOLD, (int)(font.getSize() /* * 1.2 */)));
		tableLabel.setIcon(tableIcon);
		closeButtonContainerPanel.add(closeButton);
    	if (predecessor == null) {
    		int location = 348;
    		if (parent != null) {
    			int maxLoc = (int) (parent.getHeight() * 0.4);
    			if (location > maxLoc + 50) {
    				location = maxLoc;
    			}
    		}
			splitPane.setDividerLocation(location);
    	}
    	
		if (table == null) {
	    	setVisible(false);
		} else {
			inlineViewStyle = (InlineViewStyle) session.getSessionProperty(getClass(), "inlineViewStyle");
			if (inlineViewStyle == null) {
				try {
					inlineViewStyle = InlineViewStyle.forSession(session);
					session.setSessionProperty(getClass(), "inlineViewStyle", inlineViewStyle);
				} catch (Exception e) {
					setVisible(false);
					return;
				}
			}
			List<String> config = ConditionStorage.load(table, executionContext);
			if (config.isEmpty()) {
				if (table.primaryKey != null) {
					comparisons.addAll(table.primaryKey.getColumns().stream().map(c -> new Comparison(Operator.Equal, c)).collect(Collectors.toList()));
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
            
            if (editor != null) {
        		if (predecessor != null) {
        			if (predecessor.documentListener != null) {
        				editor.getDocument().removeDocumentListener(predecessor.documentListener);
        			}
        			splitPane.setDividerLocation(predecessor.splitPane.getDividerLocation());
        			predecessor.cancel();
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
        	}
        	
	        GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 20;
			final Window owner = parent;
			final JComboBox comboBox = searchComboBox;
	        searchButton = StringSearchPanel.createSearchButton(owner, comboBox, "Add Search Field", new Runnable() {
			    @Override
			    public void run() {
			        addColumn();
			    }
			}, null, null, null, false, null, true, false, columnLabelConsumers);
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
						onFocusGained.run();
						onFocusGained = null;
					}
				}
			};
			if (editor != null) {
				editor.addFocusListener(focusListener);
			}
			searchButton.addFocusListener(focusListener);
			sortCheckBox.addFocusListener(focusListener);

	        updateSearchUI();
	        sortCheckBox.setSelected(Boolean.TRUE.equals(sorted));
        }
    }
	
	protected abstract void consume(String condition);
	
	/**
	 * Parses a condition and updates the UI accordingly.
	 * 
	 * @param condition the condition
	 */
	public void parseCondition(String condition) {
		if (latestCondition != null && latestCondition.equals(condition)) {
			return;
		}
		latestCondition = condition;
		editor.setText(new BasicFormatterImpl().format(condition) + "\n");
		editor.discardAllEdits();
		editor.setCaretPosition(0);
		parseCondition();
	}
	
	private Runnable continueParsing = null;
    
	/**
	 * Parses current condition and updates the UI accordingly.
	 */
	public synchronized void parseCondition() {
		if (allDisabled) {
			return;
		}
		latestParsedCondition = editor.getText();
		consume(latestParsedCondition);
		applyButton.setEnabled(false);
		AtomicBoolean disabled = new AtomicBoolean(false);
		Timer timer = new Timer(200, e -> {
			if (!disabled.getAndSet(true)) {
				disableAll();
			}
		});
		timer.setRepeats(false);
		timer.start();
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

	public void setCellEditor(BrowserContentCellEditor cellEditor) {
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

	/**
	 * Parses current condition concurrently.
	 */
	private void doParseCondition() {
		valuePositions.clear();
		fullPositions.clear();
		String quoteRE = "[\"\u00B4\\[\\]`]";
		Set<Comparison> seen = new HashSet<Comparison>();
		for (int columnIndex = 0; columnIndex < table.getColumns().size(); ++columnIndex) {
			Column column = table.getColumns().get(columnIndex);
			String valueRegex = "((?:(?:0x(?:\\d|[a-f])+)|(?:.?'(?:[^']|'')*')|(?:\\d|[\\.,\\-\\+])+|(?:true|false)|(?:\\w+\\([^\\)]*\\)))(?:\\s*\\:\\:\\s*(?:\\w+))?)?";
			String regex = "(?:(?:and\\s+)?(?:" + tableAlias + "\\s*\\.\\s*)?)" + "(" + quoteRE + "?)" + Pattern.quote(Quoting.staticUnquote(column.name)) + "(" + quoteRE
					+ "?)" + "\\s*(?:(\\bis\\s+null\\b)|(\\bis\\s+not\\s+null\\b)|(" + Pattern.quote("!=") + "|" + 
					Stream.of(Operator.values())
						.map(o -> o.sql)
						.sorted((a, b) -> b.length() - a.length())
						.map(sql -> Character.isAlphabetic(sql.charAt(0))? "\\b" + Pattern.quote(sql) + "\\b" : Pattern.quote(sql))
						.collect(Collectors.joining("|"))
					+ "))\\s*" + valueRegex;
			boolean found = false;
			Matcher matcher = null;
			try {
				Pattern identOperator = Pattern.compile(regex, Pattern.CASE_INSENSITIVE|Pattern.DOTALL);
				matcher = identOperator.matcher(latestParsedCondition);
				found = matcher.find();
			} catch (Throwable t) {
				// ignore
			}
			if (found) {
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
						value = sqlValue = "is null";
						Pair<Integer, Integer> pos = new Pair<Integer, Integer>(matcher.start(3), matcher.end(3));
						valuePositions.put(column, pos);
						fullPositions.put(column, new Pair<Integer, Integer>(start, pos.b));
					} else if (matcher.group(4) != null && !"".equals(matcher.group(4))) {
						operator = Operator.Equal;
						value = sqlValue = "is not null";
						Pair<Integer, Integer> pos = new Pair<Integer, Integer>(matcher.start(4), matcher.end(4));
						valuePositions.put(column, pos);
						fullPositions.put(column, new Pair<Integer, Integer>(start, pos.b));
					} else {
						String op = matcher.group(5);
						if ("!=".equals(op)) {
							operator = Operator.NotEqual;
						} else {
							operator = Stream.of(Operator.values()).filter(o -> o.sql.equals(op)).findFirst().get();
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
							String theValue = value;
							comparisons.stream().filter(c -> c.column.equals(column)).findAny().ifPresentOrElse(
									c -> {
										c.operator = operator;
										c.value = theValue;
										seen.add(c);
									}, () -> {
										Comparison c = new Comparison(operator, column);
										c.value = theValue;
										comparisons.add(c);
										seen.add(c);
									});
						}
					}
				}
			}
		}
		comparisons.forEach(c -> {
			if (!seen.contains(c)) {
				c.value = "";
				c.operator = Operator.Equal;
			}
		});
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
				LogUtil.warn(t);
				value = null;
			}
			perType.put(cellEditor.getColumnTypes()[columnIndex], value);
		}
		return value;
	}

	private String latestCondition = null;
	private String latestParsedCondition = null;
	private Map<Column, Pair<Integer, Integer>> valuePositions = new HashMap<Column, Pair<Integer, Integer>>();
	private Map<Column, Pair<Integer, Integer>> fullPositions = new HashMap<Column, Pair<Integer, Integer>>();
	
    private void updateSearchUI() {
    	Optional<Comparison> focusedComparision = comparisons.stream().filter(c -> c.valueTextField != null && c.valueTextField.hasFocus()).findAny();
    	
    	allFields.clear();
    	allFields.add(searchButton);
    	allFields.add(applyButton);
    	allFields.add(sortCheckBox);
    	allFields.add(editor);
    	
    	Set<Column> searchColumns = comparisons.stream().map(c -> c.column).collect(Collectors.toSet());
    	List<String> colNames = new ArrayList<String>();
    	columnLabelConsumers.clear();
    	int i = 0;
    	for (Column column: table.getColumns()) {
    		if (!searchColumns.contains(column)) {
    			String uqName = Quoting.staticUnquote(column.name);
				if (!colNames.contains(uqName)) {
    				colNames.add(uqName);
    				if (cellEditor != null && cellEditor.getColumnTypes().length > 0 && !cellEditor.isEditable(table, i, null)) {
    					columnLabelConsumers.put(uqName, label -> label.setEnabled(false));
    				}
    			}
    		}
    		++i;
    	}
    	if (sortCheckBox.isSelected()) {
    		colNames.sort(String::compareToIgnoreCase);
    	}
    	searchButton.setEnabled(!colNames.isEmpty());
		searchComboBox.setModel(new DefaultComboBoxModel<String>(colNames.toArray(new String[0])));
		
		List<Comparison> sortedSearchComparison = new ArrayList<Comparison>(comparisons);
		if (sortCheckBox.isSelected()) {
			sortedSearchComparison.sort((a, b) -> Quoting.staticUnquote(a.column.name).compareToIgnoreCase(Quoting.staticUnquote(b.column.name)));
		}
		BiFunction<JComponent, Integer, JComponent> wrap = (c, y) -> {
			JPanel panel = new JPanel(new GridBagLayout());
			c.setForeground(Color.black);
			panel.setBackground(y % 2 != 0? UIUtil.TABLE_BACKGROUND_COLOR_1 : UIUtil.TABLE_BACKGROUND_COLOR_2);
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
					onFocusGained.run();
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
	        nameLabel.setFont(new Font(font.getName(), font.getStyle() & ~Font.BOLD, (int)(font.getSize() /* * 1.2 */)));
			nameLabel.setText(" " + Quoting.staticUnquote(comparison.column.name));
			nameLabel.setToolTipText(nameLabel.getText());
	        
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

			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y;
	        gridBagConstraints.weightx = 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.WEST;
	        namePanel.add(nameLabel, gridBagConstraints);
			
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y;
	        gridBagConstraints.gridwidth = 4;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(namePanel, y), gridBagConstraints);
	        boolean isPk = table.primaryKey != null && table.primaryKey.getColumns().contains(comparison.column);
			nameLabel.setForeground(isPk? Color.red : Color.black);
			
	        SmallButton hideButton = new LightBorderSmallButton(UIUtil.scaleIcon(this, deleteIcon, 1.1)) {
				@Override
				protected void onClick() {
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
	        gridBagConstraints.gridx = 4;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.EAST;
	        searchFieldsPanel.add(wrap.apply(hideButton, y), gridBagConstraints);

	        JLabel sep = new JLabel("  ");
			gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 1;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        searchFieldsPanel.add(wrap.apply(sep, y), gridBagConstraints);

	        JTextField valueTextField = new JTextField(comparison.value);
	        comparison.valueTextField = valueTextField;
	        allFields.add(valueTextField);
	    	
	        final int finalMaxWidth = maxWidth;
			SmallButton operatorField = new LightBorderSmallButton(null) {
				@Override
				protected void onClick() {
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
					if (onFocusGained != null) {
						onFocusGained.run();
						onFocusGained = null;
					}
					String currentText = valueTextField.getText();
					onFocusGained = () -> {
						if (allFields.contains(valueTextField) && !currentText.equals(valueTextField.getText())) {
							accept(comparison, valueTextField.getText(), comparison.operator);
						}
					};
					Pair<Integer, Integer> pos = valuePositions.get(comparison.column);
					if (pos != null) {
						editor.select(pos.a, pos.b);
						// TODO
						editor.scrollRectToVisible(new Rectangle()); // new Rectangle(editor.getVisibleRect().getSize()));
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
					}
				}
				@Override
				public void keyReleased(KeyEvent e) {
				}
				@Override
				public void keyPressed(KeyEvent e) {
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
	        gridBagConstraints.gridwidth = 4;
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
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        searchFieldsPanel.add(wrap.apply(sepPanel, 1), gridBagConstraints);
		
		revalidate();
		focusedComparision.ifPresent(c -> c.valueTextField.grabFocus());
    }

    protected void addColumn() {
    	Object toFind = searchComboBox.getSelectedItem();
    	for (Column column: table.getColumns()) {
        	if (Quoting.staticUnquote(column.name).equals(toFind)) {
        		comparisons.add(new Comparison(Operator.Equal, column));
        	}
        }
    	updateSearchUI();
    	storeConfig();
	}

	private void storeConfig() {
		List<String> config = new ArrayList<String>(comparisons.stream().map(c -> c.column.name).collect(Collectors.toList()));
		ConditionStorage.store(table, config, executionContext);
	}
	
	private Runnable onFocusGained;
	
	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        splitPane = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        searchComboBox = new javax.swing.JComboBox<>();
        sortCheckBox = new javax.swing.JCheckBox();
        jPanel7 = new javax.swing.JPanel();
        tableLabel = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        closeButtonContainerPanel = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel3 = new javax.swing.JPanel();
        searchFieldsPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        syntaxPanePanel = new javax.swing.JPanel();
        applyButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

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

        sortCheckBox.setText("Sort Columns");
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
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel5.add(jPanel6, gridBagConstraints);

        jPanel7.setBackground(java.awt.Color.white);
        jPanel7.setLayout(new java.awt.GridBagLayout());

        tableLabel.setText("<html><i>no table selected</i></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 0);
        jPanel7.add(tableLabel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel7.add(jSeparator1, gridBagConstraints);

        closeButtonContainerPanel.setOpaque(false);
        closeButtonContainerPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel7.add(closeButtonContainerPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel5.add(jPanel7, gridBagConstraints);

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

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Condition"));
        jPanel2.setLayout(new java.awt.GridBagLayout());

        syntaxPanePanel.setLayout(new java.awt.BorderLayout());
        jScrollPane1.setViewportView(syntaxPanePanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

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
        jPanel2.add(applyButton, gridBagConstraints);

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


    private void openStringSearchPanel(JTextField valueTextField, Comparison comparison) {
    	if (getParent() == null) {
    		return; // too late
    	}
    	Window owner = SwingUtilities.getWindowAncestor(valueTextField);
		JComboBox combobox = new JComboBox();
		DefaultComboBoxModel<String> defaultComboBoxModel = new DefaultComboBoxModel<String>();
		combobox.setModel(defaultComboBoxModel);
		Map<String, Consumer<JLabel>> renderConsumer = new HashMap<String, Consumer<JLabel>>();
		String item;
		Color color;
		
		color = Color.blue;
		item = "is null";
		defaultComboBoxModel.addElement(item);
		renderConsumer.put(item, label -> label.setForeground(color));
		item = "is not null";
		defaultComboBoxModel.addElement(item);
		renderConsumer.put(item, label -> label.setForeground(color));
		
		// TODO history
		
		List<StringSearchPanel> theSearchPanel = new ArrayList<StringSearchPanel>();
		String origText = valueTextField.getText();
		StringSearchPanel searchPanel = new StringSearchPanel(null, combobox, null, null, null, new Runnable() {
			@Override
			public void run() {
				valueTextField.setText(theSearchPanel.get(0).getPlainValue());
				accept(comparison, theSearchPanel.get(0).getPlainValue(), comparison.operator);
		    	cancel();
			}
		}, renderConsumer) {
			@Override
			protected void onClose(String text) {
		    	valueTextField.setText(text);
		    	cancel();
			}
			@Override
			protected void onAbort() {
				valueTextField.setText(origText);
		    	cancel();
			}
			@Override
			protected Integer preferredWidth() {
				return 260;
			}
			@Override
			protected Integer maxX() {
				if (owner != null) {
					return owner.getX() + owner.getWidth() - preferredWidth() - 8;
				} else {
					return null;
				}
			}
			@Override
			protected Integer maxY(int height) {
				if (owner != null) {
					return owner.getY() + owner.getHeight() - height - 8;
				} else {
					return null;
				}
			}
		};
		theSearchPanel.add(searchPanel);

		Point point = new Point(0, 0);
		SwingUtilities.convertPointToScreen(point, valueTextField);
		searchPanel.withSizeGrip();
		searchPanel.find(owner, "Condition", point.x, point.y, true);
		searchPanel.setInitialValue(valueTextField.getText());
		searchPanel.setStatus("loading existing values...", null);
		Object cancellationContext = nextCancellationContext;
		
		runnableQueue.add(() -> {
			try {
				boolean[] incomplete = new boolean[1];
				incomplete[0] = false;
				List<String> distinctExisting = loadDistinctExistingValues(comparison, cancellationContext, incomplete);
				UIUtil.invokeLater(() -> {
					distinctExisting.forEach(s -> defaultComboBoxModel.addElement(s));
					searchPanel.updateList(false);
					if (incomplete[0] || distinctExisting.size() > MAX_NUM_DISTINCTEXISTINGVALUES) {
						UIUtil.invokeLater(() -> {
							searchPanel.setStatus("incomplete"
									+ (incomplete[0] ? "" : ("(>" + MAX_NUM_DISTINCTEXISTINGVALUES + " values)")),
									UIUtil.scaleIcon(searchPanel, warnIcon));
						});
						return;
					}
				});
			} catch (CancellationException e) {
				// ok;
			} catch (Throwable e) {
				UIUtil.invokeLater(() -> {
					searchPanel.setStatus("error loading values", UIUtil.scaleIcon(searchPanel, warnIcon));
				});
				return;
			}
			UIUtil.invokeLater(() -> {
				searchPanel.setStatus(null, null);
			});
		});
    }
    
    private Object nextCancellationContext = new Object();
    
    private void cancel() {
    	CancellationHandler.cancelSilently(nextCancellationContext);
    	nextCancellationContext = new Object();
    }
    
    private final String DISTINCTEXISTINGVALUESCACHEKEY = "DistinctExistingValuesCache";
    private final String DISTINCTEXISTINGVALUESICCACHEKEY = "DistinctExistingValuesICCache";

	@SuppressWarnings("unchecked")
	private List<String> loadDistinctExistingValues(Comparison comparison, Object cancellationContext, boolean incomplete[]) throws SQLException {
		final int MAX_TEXT_LENGTH = 1024 * 4;
		
		Map<Pair<String, String>, List<String>> cache = (Map<Pair<String, String>, List<String>>) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESCACHEKEY);
		if (cache == null) {
			cache = new HashMap<Pair<String,String>, List<String>>();
			session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESCACHEKEY, cache);
		}
		Set<Pair<String, String>> icCache = (Set<Pair<String, String>>) session.getSessionProperty(getClass(), DISTINCTEXISTINGVALUESICCACHEKEY);
		if (icCache == null) {
			icCache = new HashSet<Pair<String,String>>();
			session.setSessionProperty(getClass(), DISTINCTEXISTINGVALUESICCACHEKEY, icCache);
		}
		Pair<String, String> key = new Pair<String, String>(table.getName(), comparison.column.name);
		List<String> result = cache.get(key);
		if (icCache.contains(key)) {
			incomplete[0] = true;
		}
		
		if (result == null) {
			result = new ArrayList<String>();
			int columnIndex = 0;
			while (columnIndex < table.getColumns().size()) {
				if (comparison.column.equals(table.getColumns().get(columnIndex))) {
					int finalColumnIndex = columnIndex;
					List<String> finalResult = result;
					Set<Pair<String, String>> finalIcCache = icCache;
					String sqlQuery = "Select distinct " + comparison.column.name + " from " + table.getName() + " where " +  comparison.column.name + " is not null";
					AbstractResultSetReader reader = new AbstractResultSetReader() {
						@Override
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							Object obj = getCellContentConverter(resultSet, session, session.dbms).getObject(resultSet, 1);
							if (cellEditor.isEditable(table, finalColumnIndex, obj)) {
								String text = cellEditor.cellContentToText(finalColumnIndex, obj);
								if (text.length() <= MAX_TEXT_LENGTH) {
									finalResult.add(text);
								} else {
									incomplete[0] = true;
									finalIcCache.add(key);
								}
							} else {
								incomplete[0] = true;
								finalIcCache.add(key);
							}
						}
					};
					try {
						session.executeQuery(sqlQuery + " order by " + comparison.column.name, reader, null, cancellationContext, MAX_NUM_DISTINCTEXISTINGVALUES + 1);
					} catch (SQLException e) {
						// try without ordering
						session.executeQuery(sqlQuery, reader, null, cancellationContext, MAX_NUM_DISTINCTEXISTINGVALUES + 1);
						result.sort(String::compareToIgnoreCase);
					}
				}
				++columnIndex;
			}
		}
		cache.put(key, result);
		return result;
	}

	protected void accept(Comparison comparison, String value, Operator operator) {
		if (value != null) {
			if (!value.trim().equals(comparison.value.trim()) || operator != comparison.operator) {
				editor.setText(latestParsedCondition);
				comparison.operator = operator;
				if (value.trim().isEmpty()) {
					// field cleared
					Pair<Integer, Integer> fullPos = fullPositions.get(comparison.column);
					if (fullPos != null) {
						editor.select(fullPos.a, fullPos.b);
						editor.replaceSelection("");
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
						sqlValue = "is null";
						op = "";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(false);
						}
					} else if (comparison.value.toLowerCase().matches("\\s*(is\\s)?+not\\s+null\\s*")) {
						sqlValue = "is not null";
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
						pos = new Pair<Integer, Integer>(pos.a, pos.a + opSqlValue.length());
						valuePositions.put(comparison.column, pos);
						Pair<Integer, Integer> fullPos = fullPositions.get(comparison.column);
						if (fullPos != null) {
							fullPositions.put(comparison.column, new Pair<Integer, Integer>(fullPos.a, pos.b));
						}
						editor.select(pos.a, pos.b);
					} else {
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
						String name = prefix + tableAlias + "." + comparison.column.name + " ";
						editor.append(name + opSqlValue);
						pos = new Pair<Integer, Integer>(start + name.length(), start + name.length() + opSqlValue.length());
						valuePositions.put(comparison.column, pos);
						fullPositions.put(comparison.column, new Pair<Integer, Integer>(start, pos.b));
						editor.select(pos.a, pos.b);
					}
				}
				editor.setText(editor.getText().replaceFirst("^\\s*and\\s", "").replaceAll("\n\\s*\\n", "").trim() + "\n");
				parseCondition();
			}
		}
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
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JComboBox<String> searchComboBox;
    private javax.swing.JPanel searchFieldsPanel;
    private javax.swing.JCheckBox sortCheckBox;
    private javax.swing.JSplitPane splitPane;
    private javax.swing.JPanel syntaxPanePanel;
    private javax.swing.JLabel tableLabel;
    // End of variables declaration//GEN-END:variables
    
    
    private static abstract class LightBorderSmallButton extends SmallButton {
    	
		public LightBorderSmallButton(Icon icon) {
			super(icon, true);
		}
    	
		protected void onMouseExited() {
			super.onMouseExited();
			Color borderColor = new Color(0, 0, 0, 0);
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED, borderColor, borderColor));
		}

		protected void onMouseEntered() {
			super.onMouseEntered();
			setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.LOWERED, Color.LIGHT_GRAY, Color.GRAY));
		}
		
	};
    
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

	private static ImageIcon tableIcon;
	private static ImageIcon deleteIcon;
    static ImageIcon warnIcon;
	static {
        // load images
        tableIcon = UIUtil.readImage("/table.png");
        deleteIcon = UIUtil.readImage("/delete.png");
        warnIcon = UIUtil.readImage("/wanr.png");
	}
}
