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
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

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

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.StringSearchPanel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.DBConditionEditor;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.SmallButton;
import net.sf.jailer.util.Quoting;

/**
 * SQL-Where-Condition Editor.
 *
 * @author Ralf Wisser
 */
public class WhereConditionEditorPanel extends javax.swing.JPanel {

	// TODO remove empty lines before putting text back into sql console after user edit
	// TODO respect quoting
	// TODO offer null and not null
	
	private final DataModel dataModel;
	private final Table table;
	private JToggleButton searchButton;
	private final RSyntaxTextAreaWithSQLSyntaxStyle editor;
	private Font font;
	private final ExecutionContext executionContext;

	private class Comparison {
		Operator operator;
		final Column column;
		
		Comparison(Operator operator, Column column) {
			this.operator = operator;
			this.column = column;
		}
	}
	
	private List<Comparison> comparisons = new ArrayList<Comparison>();
	
    /**
     * Creates new form SearchPanel
     * @param sorted 
     */
	public WhereConditionEditorPanel(Window parent, DataModel dataModel, Table table, Boolean sorted,
			WhereConditionEditorPanel predecessor, RSyntaxTextAreaWithSQLSyntaxStyle editor,
			JComponent closeButton,
			ExecutionContext executionContext) {
    	this.dataModel = dataModel;
    	this.table = table;
    	this.editor = editor;
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
		if (table == null) {
	    	setVisible(false);
		} else {
			List<String> config = ConditionStorage.load(table, executionContext);
			if (config.isEmpty()) {
				if (table.primaryKey != null) {
					comparisons.addAll(table.primaryKey.getColumns().stream().map(c -> new Comparison(Operator.Equal, c)).collect(Collectors.toList()));
				}
			} else {
				comparisons.addAll(table.getColumns().stream().filter(c -> config.contains(c.name)).map(c -> new Comparison(Operator.Equal, c)).collect(Collectors.toList()));
			}
        	tableLabel.setText(this.dataModel.getDisplayName(table));
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
					comparisons.remove(comparison);
					updateSearchUI();
					storeConfig();
				}
			};
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

	        final int finalMaxWidth = maxWidth;
			SmallButton operatorField = new SmallButton(null, true) {
				@Override
				protected void onClick() {
					JPopupMenu popup = new JPopupMenu();
					for (Operator operator: Operator.values()) {
						JMenuItem item = new JMenuItem(operator.sql);
						item.addActionListener(e -> {
							setText(operator.render());
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
	        operatorField.setText(" " + comparison.operator.sql + " ");
	        gridBagConstraints = new java.awt.GridBagConstraints();
	        gridBagConstraints.gridx = 2;
	        gridBagConstraints.gridy = 3 * y + 1;
	        gridBagConstraints.fill = GridBagConstraints.BOTH;
	        gridBagConstraints.anchor = GridBagConstraints.WEST;
	        searchFieldsPanel.add(wrap.apply(operatorField, y), gridBagConstraints);
	        
	        JTextField valueTextField = new JTextField();
	        DBConditionEditor.initialObserve(valueTextField, x -> openStringSearchPanel(valueTextField), () -> openStringSearchPanel(valueTextField));
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

        jPanel1 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        jPanel6 = new javax.swing.JPanel();
        searchComboBox = new javax.swing.JComboBox<>();
        sortCheckBox = new javax.swing.JCheckBox();
        searchFieldsPanel = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        tableLabel = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();
        closeButtonContainerPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        syntaxPanePanel = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

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
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 8, 0);
        jPanel5.add(jPanel6, gridBagConstraints);

        searchFieldsPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(searchFieldsPanel, gridBagConstraints);

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

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Condition"));
        jPanel2.setLayout(new java.awt.GridBagLayout());

        syntaxPanePanel.setLayout(new java.awt.BorderLayout());
        jScrollPane1.setViewportView(syntaxPanePanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(jPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void sortCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortCheckBoxActionPerformed
    	updateSearchUI();
    }//GEN-LAST:event_sortCheckBoxActionPerformed


    private void openStringSearchPanel(JTextField valueTextField) {
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
				accept(valueTextField, theSearchPanel.get(0).getPlainValue());
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

	protected void accept(JTextField valueTextField, String value) {
		if (value != null) {
			valueTextField.setText(value);
		}
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel closeButtonContainerPanel;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JComboBox<String> searchComboBox;
    private javax.swing.JPanel searchFieldsPanel;
    private javax.swing.JCheckBox sortCheckBox;
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
