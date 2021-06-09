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
import java.awt.Window;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.swing.DefaultComboBoxModel;
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
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.InlineViewStyle;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentCellEditor;
import net.sf.jailer.ui.databrowser.DBConditionEditor;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.SmallButton;
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
	
	private final DataModel dataModel;
	private final Table table;
	private JToggleButton searchButton;
	private final RSyntaxTextAreaWithSQLSyntaxStyle editor;
	private Font font;
	private final BrowserContentCellEditor cellEditor;
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
			}, null, null, null, false, null, true, false, null);
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
		editor.setText(new BasicFormatterImpl().format(condition));
		editor.discardAllEdits();
		editor.setCaretPosition(0);
		parseCondition();
	}
    
	/**
	 * Parses current condition and updates the UI accordingly.
	 */
	public void parseCondition() {
		String condition = latestParsedCondition = editor.getText();
		applyButton.setEnabled(false);
		valuePositions.clear();
		fullPositions.clear();
		String quoteRE = "[\"\u00B4\\[\\]`]";
		Set<Comparison> seen = new HashSet<Comparison>();
		for (Column column: table.getColumns()) {
			String valueRegex = "((?:(?:0x(?:\\d|[a-f])+)|(?:.?'(?:[^']|'')*')|(?:\\d|[\\.,\\-\\+])+|(?:true|false)|(?:\\w+\\([^\\)]*\\)))(?:\\s*\\:\\:\\s*(?:\\w+))?)";
			String regex = "(?:(?:and\\s+)?(?:" + tableAlias + "\\s*\\.\\s*)?)" + "(" + quoteRE + "?)" + Pattern.quote(Quoting.staticUnquote(column.name)) + "(" + quoteRE
					+ "?)" + "\\s*(?:(\\bis\\s+null\\b)|(\\bis\\s+not\\s+null\\b)|(" + Pattern.quote("!=") + "|" + 
					Stream.of(Operator.values())
						.map(o -> o.sql)
						.sorted((a, b) -> b.length() - a.length())
						.map(sql -> Character.isAlphabetic(sql.charAt(0))? "\\b" + Pattern.quote(sql) + "\\b" : Pattern.quote(sql))
						.collect(Collectors.joining("|"))
					+ "))\\s*" + valueRegex;
			Pattern identOperator = Pattern.compile(regex, Pattern.CASE_INSENSITIVE|Pattern.DOTALL);
			Matcher matcher = identOperator.matcher(condition);
			if (matcher.find()) {
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
					if (matcher.group(3) != null && !"".equals(matcher.group(3))) {
						operator = Operator.Equal;
						sqlValue = "is null";
						Pair<Integer, Integer> pos = new Pair<Integer, Integer>(matcher.start(3), matcher.end(3));
						valuePositions.put(column, pos);
						fullPositions.put(column, new Pair<Integer, Integer>(start, pos.b));
					} else if (matcher.group(4) != null && !"".equals(matcher.group(4))) {
						operator = Operator.Equal;
						sqlValue = "is not null";
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
						String value = sqlValue; // TODO
						comparisons.stream().filter(c -> c.column.equals(column)).findAny().ifPresentOrElse(
								c -> {
									c.operator = operator;
									c.value = value;
									seen.add(c);
								}, () -> {
									Comparison c = new Comparison(operator, column);
									c.value = value;
									comparisons.add(c);
									seen.add(c);
								});
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
		updateSearchUI();
		consume(latestParsedCondition);
	}
    
	private String latestCondition = null;
	private String latestParsedCondition = null;
	private Map<Column, Pair<Integer, Integer>> valuePositions = new HashMap<Column, Pair<Integer, Integer>>();
	private Map<Column, Pair<Integer, Integer>> fullPositions = new HashMap<Column, Pair<Integer, Integer>>();
	
    private void updateSearchUI() {
    	Set<Column> searchColumns = comparisons.stream().map(c -> c.column).collect(Collectors.toSet());
    	List<String> colNames = new ArrayList<String>();
    	for (Column column: table.getColumns()) {
    		if (!searchColumns.contains(column)) {
    			String uqName = Quoting.staticUnquote(column.name);
				if (!colNames.contains(uqName)) {
    				colNames.add(uqName);
    			}
    		}
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
			
	        SmallButton hideButton = new SmallButton(UIUtil.scaleIcon(this, deleteIcon, 1.1), true) {
				@Override
				protected void onClick() {
					accept(comparison, "", Operator.Equal);
					comparisons.remove(comparison);
					updateSearchUI();
					storeConfig();
				}
			};
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
	        
	        final int finalMaxWidth = maxWidth;
			SmallButton operatorField = new SmallButton(null, true) {
				@Override
				protected void onClick() {
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
					// TODO
					Pair<Integer, Integer> pos = valuePositions.get(comparison.column);
					if (pos != null) {
						editor.select(pos.a, pos.b);
					} else {
						editor.setCaretPosition(editor.getCaretPosition());
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
		combobox.setModel(new DefaultComboBoxModel<String>());
		for (Column c: table.getColumns()) {
			((DefaultComboBoxModel) combobox.getModel()).addElement(c.name); // TODO
		}
		List<StringSearchPanel> theSearchPanel = new ArrayList<StringSearchPanel>();
		StringSearchPanel searchPanel = new StringSearchPanel(null, combobox, null, null, null, new Runnable() {
			@Override
			public void run() {
				valueTextField.setText(theSearchPanel.get(0).getPlainValue());
				accept(comparison, theSearchPanel.get(0).getPlainValue(), comparison.operator);
			}
		}, null) {
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
		searchPanel.find(owner, "Condition", point.x, point.y, true);
		searchPanel.setInitialValue(valueTextField.getText());
		
		// TODO new Timer(1000, e -> { ((DefaultComboBoxModel) combobox.getModel()).addElement("" + System.currentTimeMillis()); searchPanel.updateList(); }).start();
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
					if (comparison.value.toLowerCase().matches("\\s*is\\s+null\\s*")) {
						sqlValue = "is null";
						op = "";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(false);
						}
					} else if (comparison.value.toLowerCase().matches("\\s*is\\s+not\\s+null\\s*")) {
						sqlValue = "is not null";
						op = "";
						if (comparison.operatorField != null) {
							comparison.operatorField.setVisible(false);
						}
					} else {
						sqlValue = value; // TODO
						if (sqlValue.contains("0")) {
							sqlValue = null; // TODO
						}
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
				editor.setText(editor.getText().replaceFirst("^\\s*and\\s", "").replaceAll("\n\\s*\\n", "").trim());
				parseCondition();
			}
		}
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
    
	private static ImageIcon tableIcon;
	private static ImageIcon deleteIcon;
	static {
        // load images
        tableIcon = UIUtil.readImage("/table.png");
        deleteIcon = UIUtil.readImage("/delete.png");
    }
}
