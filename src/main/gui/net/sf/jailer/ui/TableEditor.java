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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JOptionPane;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.PrimaryKey;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.Quoting;

/**
 * Editor for single tables. Part of {@link DataModelEditor}.
 *
 * @author Ralf Wisser
 */
public abstract class TableEditor extends javax.swing.JDialog {

	/**
	 * All tables (as csv-lines).
	 */
	private Collection<Line> tables;

	/**
	 * All associations (as csv-lines).
	 */
	private Collection<Line> associations;
	
	/**
	 * List of tables to be excluded from deletion.
	 */
	private List<String> excludeFromDeletionList = new ArrayList<String>();
	
	/**
	 * Maps table names to display names.
	 */
	private final Map<String, String> displayNames;

	private boolean needsSave;
	
	private static class ColumnModel {
		Column column;
		boolean isPk;
	}
	
	private Table table;
	
	private void updateTable(List<ColumnModel> model) {
		List<Column> pkColumns = new ArrayList<Column>();
		hasNullablePKColumn = false;
		for (ColumnModel cm: model) {
			if (cm.isPk) {
				pkColumns.add(cm.column);
				if (cm.column.isNullable) {
					hasNullablePKColumn = true;
				}
			}
		}
		PrimaryKey primaryKey = new PrimaryKeyFactory(null).createPrimaryKey(pkColumns, null);
		table = new Table(nameField.getText().trim(), primaryKey, false, false);
		updateWarningPanel();
	}
	
	@SuppressWarnings("serial")
	private class ColumnListEditor extends ListEditor<ColumnModel> {

		public ColumnListEditor() {
			super(new String[] { "Name", "Type"}, "Column", false, false);
		}

		@Override
		protected String getDisplayName(ColumnModel element) {
			return element.column.name;
		}

		@Override
		protected ColumnModel copy(ColumnModel element) {
			ColumnModel c = new ColumnModel();
			c.column = element.column;
			c.isPk = element.isPk;
			return c;
		}

		@Override
		protected ColumnModel createNew() {
			return new ColumnModel();
		}

		@Override
		protected JComponent createDetailsView(ColumnModel element) {
			if (element.column != null) {
				columnName.setText(element.column.name);
				columnType.setText(element.column.type);
				columnLength.setText(element.column.length > 0? Integer.toString(element.column.length) : "");
				columnPrec.setText(element.column.precision >= 0? Integer.toString(element.column.precision) : "");
				columnIsIdentity.setSelected(element.column.isIdentityColumn);
				columnIsVirtual.setSelected(element.column.isVirtual);
				columnIsNullable.setSelected(element.column.isNullable);
			} else {
				columnName.setText("");
				columnType.setText("");
				columnLength.setText("");
				columnPrec.setText("");
				columnIsIdentity.setSelected(false);
				columnIsVirtual.setSelected(false);
				columnIsNullable.setSelected(false);
			}
			primaryKey1.setSelected(element.isPk);
			return columnDetailsPanel;
		}

		@Override
		protected void updateFromDetailsView(ColumnModel element,
				JComponent detailsView,
				List<ColumnModel> model,
				StringBuilder errorMessage) {
			String name = columnName.getText().trim();
			if (name.equals("")) {
				errorMessage.append("Name missing.");
				return;
			}
			if (name.equals(Quoting.staticUnquote(name)) && name.contains(" ")) {
				errorMessage.append("Unquoted name must not contain a space.");
			}
			String type = columnType.getText().trim();
			if (type.equals("")) {
				errorMessage.append("Type missing.");
				return;
			}
			if (element.column != null && !element.column.name.equals(name)) {
				for (ColumnModel m: model) {
					if (m.column.name.equals(name)) {
						errorMessage.append("Name already exists.");
						return;
					}
				}
			}
			int length = 0;
			if (columnLength.getText().trim().length() > 0) {
				try {
					length = Integer.parseInt(columnLength.getText().trim());
				} catch (NumberFormatException e) {
					errorMessage.append("Length is not valid.");
					return;
				}
			}
			int prec = -1;
			if (columnPrec.getText().trim().length() > 0) {
				try {
					prec = Integer.parseInt(columnPrec.getText().trim());
				} catch (NumberFormatException e) {
					errorMessage.append("Precision is not valid.");
					return;
				}
			}
			Column c = new Column(name, type, length, prec);
			c.isIdentityColumn = columnIsIdentity.isSelected();
			c.isVirtual = columnIsVirtual.isSelected();
			c.isNullable = columnIsNullable.isSelected();
			element.column = c;
			if (element.isPk != primaryKey1.isSelected()) {
				pkChanged = true;
				updateWarningPanel();
			}
			element.isPk = primaryKey1.isSelected();
		}

		/**
		 * Reacts on model changes.
		 */
		protected void onModelUpdate() {
			checkPKButton.setEnabled(false);
			for (ColumnModel cm: model) {
				if (cm.isPk) {
					checkPKButton.setEnabled(true);
					break;
				}
			}
			updateTable(model);
		}

		@Override
		protected Object[] toColumnList(ColumnModel element, int index) {
			String type = element.column.toSQL(null).substring(element.column.name.length());
			return new String[] { element.column.name, type };
		}

		@Override
		protected Color getForegroundColor(ColumnModel element, int column) {
			if (element.isPk && column == 0) {
				return Color.RED;
			}
			return null;
		}

		@Override
		protected Dimension detailsViewMinSize() {
			return new Dimension(300, 0);
		}

	}

	/**
	 * Creates new form TableEditor
	 * 
	 * @param tables all tables (as csv-lines)
	 * @param associations all associations (as csv-line)
	 * @param excludeFromDeletionList list of tables to be excluded from deletion
	 */
	public TableEditor(java.awt.Dialog parent, Map<String, String> displayNames, Collection<Line> tables, List<Line> associations, List<String> excludeFromDeletionList) {
		super(parent, true);
		this.tables = tables;
		this.associations = associations;
		this.excludeFromDeletionList = excludeFromDeletionList;
		this.displayNames = displayNames;
		initComponents(); UIUtil.initComponents(this);
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
		warnPanel.setVisible(false);
		UIUtil.setDialogSize(this, 600, 500);
		setLocation(parent.getLocation().x + parent.getSize().width/2 - getSize().width/2,
				parent.getLocation().y + parent.getSize().height/2 - getSize().height/2);
		
	}
	
	/**
	 * Gets list of current primary key columns.
	 * 
	 * @return list of current primary key columns
	 */
	private List<Column> getCurrentPrimaryKeys() {
		List<Column> pk = new ArrayList<Column>();
		for (int i = 2; i < currentTableLine.length; ++i) {
			if (currentTableLine.cells.get(i).length() == 0) {
				break;
			}
			pk.add(Column.parse(currentTableLine.cells.get(i)));
		}
		return pk;
	}
	
	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        columnDetailsPanel = new javax.swing.JPanel();
        primaryKey1 = new javax.swing.JCheckBox();
        columnName = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        columnType = new javax.swing.JTextField();
        columnLength = new javax.swing.JTextField();
        columnPrec = new javax.swing.JTextField();
        columnIsIdentity = new javax.swing.JCheckBox();
        columnIsVirtual = new javax.swing.JCheckBox();
        columnIsNullable = new javax.swing.JCheckBox();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        nameField = new javax.swing.JTextField();
        upsertCheckbox = new javax.swing.JCheckBox();
        jPanel1 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        jLabel3 = new javax.swing.JLabel();
        cancelButton = new javax.swing.JButton();
        checkPKButton = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        excludeFromDeletion = new javax.swing.JCheckBox();
        jPanel2 = new javax.swing.JPanel();
        slotPanel = new javax.swing.JPanel();
        jLabel4 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        displayName = new javax.swing.JTextField();
        warnPanel = new javax.swing.JPanel();
        warnPKChangedLabel = new javax.swing.JLabel();
        warnNullablePKLabel = new javax.swing.JLabel();
        warnSeparator = new javax.swing.JPanel();

        columnDetailsPanel.setLayout(new java.awt.GridBagLayout());

        primaryKey1.setText("primary key  (or part of composite PK)");
        primaryKey1.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(primaryKey1, gridBagConstraints);

        columnName.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnNameActionPerformed(evt);
            }
        });
        columnName.addInputMethodListener(new java.awt.event.InputMethodListener() {
            public void caretPositionChanged(java.awt.event.InputMethodEvent evt) {
            }
            public void inputMethodTextChanged(java.awt.event.InputMethodEvent evt) {
                columnNameInputMethodTextChanged(evt);
            }
        });
        columnName.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyReleased(java.awt.event.KeyEvent evt) {
                columnNameKeyReleased(evt);
            }
            public void keyTyped(java.awt.event.KeyEvent evt) {
                columnNameKeyTyped(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(columnName, gridBagConstraints);

        jLabel8.setText("Name  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        columnDetailsPanel.add(jLabel8, gridBagConstraints);

        jLabel9.setText("Type  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        columnDetailsPanel.add(jLabel9, gridBagConstraints);

        jLabel10.setText("Length  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        columnDetailsPanel.add(jLabel10, gridBagConstraints);

        jLabel11.setText("Precision  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        columnDetailsPanel.add(jLabel11, gridBagConstraints);

        columnType.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnTypeActionPerformed(evt);
            }
        });
        columnType.addInputMethodListener(new java.awt.event.InputMethodListener() {
            public void caretPositionChanged(java.awt.event.InputMethodEvent evt) {
            }
            public void inputMethodTextChanged(java.awt.event.InputMethodEvent evt) {
                columnTypeInputMethodTextChanged(evt);
            }
        });
        columnType.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyReleased(java.awt.event.KeyEvent evt) {
                columnTypeKeyReleased(evt);
            }
            public void keyTyped(java.awt.event.KeyEvent evt) {
                columnTypeKeyTyped(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(columnType, gridBagConstraints);

        columnLength.setColumns(4);
        columnLength.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnLengthActionPerformed(evt);
            }
        });
        columnLength.addInputMethodListener(new java.awt.event.InputMethodListener() {
            public void caretPositionChanged(java.awt.event.InputMethodEvent evt) {
            }
            public void inputMethodTextChanged(java.awt.event.InputMethodEvent evt) {
                columnLengthInputMethodTextChanged(evt);
            }
        });
        columnLength.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyReleased(java.awt.event.KeyEvent evt) {
                columnLengthKeyReleased(evt);
            }
            public void keyTyped(java.awt.event.KeyEvent evt) {
                columnLengthKeyTyped(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(columnLength, gridBagConstraints);

        columnPrec.setColumns(4);
        columnPrec.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                columnPrecActionPerformed(evt);
            }
        });
        columnPrec.addInputMethodListener(new java.awt.event.InputMethodListener() {
            public void caretPositionChanged(java.awt.event.InputMethodEvent evt) {
            }
            public void inputMethodTextChanged(java.awt.event.InputMethodEvent evt) {
                columnPrecInputMethodTextChanged(evt);
            }
        });
        columnPrec.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyReleased(java.awt.event.KeyEvent evt) {
                columnPrecKeyReleased(evt);
            }
            public void keyTyped(java.awt.event.KeyEvent evt) {
                columnPrecKeyTyped(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(columnPrec, gridBagConstraints);

        columnIsIdentity.setText(" identity column");
        columnIsIdentity.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(columnIsIdentity, gridBagConstraints);
        columnIsIdentity.getAccessibleContext().setAccessibleName("");

        columnIsVirtual.setText(" virtual");
        columnIsVirtual.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 1, 0);
        columnDetailsPanel.add(columnIsVirtual, gridBagConstraints);

        columnIsNullable.setText(" nullable");
        columnIsNullable.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 4, 0);
        columnDetailsPanel.add(columnIsNullable, gridBagConstraints);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Table");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Name ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setText(" Columns ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(jLabel2, gridBagConstraints);

        nameField.setText("jTextField1j");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(nameField, gridBagConstraints);

        upsertCheckbox.setText(" generate 'Upsert'-statements for exported rows");
        upsertCheckbox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        getContentPane().add(upsertCheckbox, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel1.add(okButton, gridBagConstraints);

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel1.add(jLabel3, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
        jPanel1.add(cancelButton, gridBagConstraints);

        checkPKButton.setText("Check Primary Key");
        checkPKButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                checkPKButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        jPanel1.add(checkPKButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        getContentPane().add(jPanel1, gridBagConstraints);

        jLabel6.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        getContentPane().add(jLabel6, gridBagConstraints);

        excludeFromDeletion.setText(" exclude from deletion");
        excludeFromDeletion.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        excludeFromDeletion.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                excludeFromDeletionActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        getContentPane().add(excludeFromDeletion, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        slotPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(slotPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 9;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        getContentPane().add(jPanel2, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        getContentPane().add(jLabel4, gridBagConstraints);

        jLabel7.setText(" Display name  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        getContentPane().add(jLabel7, gridBagConstraints);

        displayName.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        getContentPane().add(displayName, gridBagConstraints);

        warnPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Warning"));
        warnPanel.setLayout(new java.awt.GridBagLayout());

        warnPKChangedLabel.setFont(warnPKChangedLabel.getFont().deriveFont(warnPKChangedLabel.getFont().getSize()+1f));
        warnPKChangedLabel.setForeground(new java.awt.Color(205, 0, 0));
        warnPKChangedLabel.setText("<html>Primary key has been changed.<br>Keep in mind that the primary key must be unique.<br>It is recommended to check the integrity of the primary key.<br>To do that, please select the option \"check primary keys\" in the \"Data Model\" menu item or use the button below. </html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        warnPanel.add(warnPKChangedLabel, gridBagConstraints);

        warnNullablePKLabel.setFont(warnNullablePKLabel.getFont().deriveFont(warnNullablePKLabel.getFont().getSize()+1f));
        warnNullablePKLabel.setForeground(new java.awt.Color(205, 0, 0));
        warnNullablePKLabel.setText("<html>Nullable primary key columns can have a negative impact on performance.</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        warnPanel.add(warnNullablePKLabel, gridBagConstraints);

        warnSeparator.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        warnSeparator.setPreferredSize(new java.awt.Dimension(1, 2));
        warnSeparator.setLayout(null);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 8, 0);
        warnPanel.add(warnSeparator, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 38;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        getContentPane().add(warnPanel, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private String toSql(Column column) {
		return column.toSQL(null) + (column.isIdentityColumn? " identity" : "") + (column.isVirtual? " virtual" : "") + (column.isNullable? " null" : "");
	}

	private void excludeFromDeletionActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_excludeFromDeletionActionPerformed
	}//GEN-LAST:event_excludeFromDeletionActionPerformed

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		String msg = null;
		if (nameField.getText().trim().length() == 0) {
			msg = "No table name";
		} else {
			for (Line l: tables) {
				if (l != currentTableLine && l.cells.get(0).equals(nameField.getText().trim())) {
					msg = "Table already exists";
					break;
				}
			}
		}
		if (msg == null) {
			String dn = displayNames.get(currentTableLine.cells.get(0));
			if (dn == null) {
				dn = "";
			}
			if (!displayName.getText().trim().equals(dn)) {
				if (displayNames.containsValue(displayName.getText().trim())) {
					msg = "duplicate display name";
				}
			}
		}
		if (msg != null) {
			JOptionPane.showMessageDialog(this, msg, "Error", JOptionPane.ERROR_MESSAGE);
		} else {
			isOk = true;
			setVisible(false);
		}
	}//GEN-LAST:event_okButtonActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		setVisible(false);
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void columnNameActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnNameActionPerformed
		
	}//GEN-LAST:event_columnNameActionPerformed

	private void columnNameInputMethodTextChanged(java.awt.event.InputMethodEvent evt) {//GEN-FIRST:event_columnNameInputMethodTextChanged
		
	}//GEN-LAST:event_columnNameInputMethodTextChanged

	private void columnNameKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnNameKeyReleased
		
	}//GEN-LAST:event_columnNameKeyReleased

	private void columnNameKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnNameKeyTyped
		
	}//GEN-LAST:event_columnNameKeyTyped

	private void columnTypeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnTypeActionPerformed
		
	}//GEN-LAST:event_columnTypeActionPerformed

	private void columnTypeInputMethodTextChanged(java.awt.event.InputMethodEvent evt) {//GEN-FIRST:event_columnTypeInputMethodTextChanged
		
	}//GEN-LAST:event_columnTypeInputMethodTextChanged

	private void columnTypeKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnTypeKeyReleased
		
	}//GEN-LAST:event_columnTypeKeyReleased

	private void columnTypeKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnTypeKeyTyped
		
	}//GEN-LAST:event_columnTypeKeyTyped

	private void columnLengthActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnLengthActionPerformed
		
	}//GEN-LAST:event_columnLengthActionPerformed

	private void columnLengthInputMethodTextChanged(java.awt.event.InputMethodEvent evt) {//GEN-FIRST:event_columnLengthInputMethodTextChanged
		
	}//GEN-LAST:event_columnLengthInputMethodTextChanged

	private void columnLengthKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnLengthKeyReleased
		
	}//GEN-LAST:event_columnLengthKeyReleased

	private void columnLengthKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnLengthKeyTyped
		
	}//GEN-LAST:event_columnLengthKeyTyped

	private void columnPrecActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_columnPrecActionPerformed
		
	}//GEN-LAST:event_columnPrecActionPerformed

	private void columnPrecInputMethodTextChanged(java.awt.event.InputMethodEvent evt) {//GEN-FIRST:event_columnPrecInputMethodTextChanged
		
	}//GEN-LAST:event_columnPrecInputMethodTextChanged

	private void columnPrecKeyReleased(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnPrecKeyReleased
		
	}//GEN-LAST:event_columnPrecKeyReleased

	private void columnPrecKeyTyped(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_columnPrecKeyTyped
		
	}//GEN-LAST:event_columnPrecKeyTyped

    private void checkPKButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_checkPKButtonActionPerformed
    	checkPK(table);
    }//GEN-LAST:event_checkPKButtonActionPerformed

	protected abstract void checkPK(Table table);

	private boolean isOk;
	private Line currentTableLine;
	private Line currentColumnLine;
	
	/**
	 * Edits a table (as csv-line).
	 * 
	 * @param tableLine the table-line
	 * @return <code>true</code> if line was modified
	 */
	public boolean edit(Line tableLine, Map<String, Line> columnLines) {
		needsSave = false;
		currentTableLine = tableLine;
		currentColumnLine = columnLines.get(tableLine.cells.get(0));
		if (currentColumnLine == null) {
			currentColumnLine = new CsvFile.Line("", new ArrayList<String>(Arrays.asList(tableLine.cells.get(0))));
			if (currentColumnLine.length == 0) {
				// new table without name
				currentColumnLine.length = 1;
			}
		}
		
		List<String> oldTableLineCells = new ArrayList<String>(currentTableLine.cells);
		int oldTableLineLength = currentTableLine.length;
		List<String> oldColumnLineCells = new ArrayList<String>(currentColumnLine.cells);
		int oldColumnLineLength = currentColumnLine.length;
		
		String dn = "";
		if (displayNames.containsKey(tableLine.cells.get(0))) {
			dn = displayNames.get(tableLine.cells.get(0));
		}
		displayName.setText(dn);
		nameField.setText(tableLine.cells.get(0));
		upsertCheckbox.setSelected("Y".equals(tableLine.cells.get(1)));
		
		String origDisplayName = displayName.getText().trim();
		String origName = nameField.getText().trim();
		boolean origUpsert = upsertCheckbox.isSelected();
		isOk = false;

		boolean origExcludeSet = excludeFromDeletionList.contains(origName);
		excludeFromDeletion.setSelected(origExcludeSet);

		ColumnListEditor columnListEditor = new ColumnListEditor();
		slotPanel.removeAll();
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		slotPanel.add(columnListEditor, gridBagConstraints);
		List<ColumnModel> columnModel = new ArrayList<ColumnModel>();
		List<Column> currentPrimaryKeys = null;
		try {
			currentPrimaryKeys = getCurrentPrimaryKeys();
		} catch (Exception e) {
			UIUtil.showException(this, "Warning, invalid column will be removed", e, UIUtil.EXCEPTION_CONTEXT_USER_WARNING);
			needsSave = true;
		}
		for (int i = 1; i < currentColumnLine.length; ++i) {
			try {
				ColumnModel cm = new ColumnModel();
				cm.column = Column.parse(currentColumnLine.cells.get(i));
				if (currentPrimaryKeys != null) {
					for (Column pk: currentPrimaryKeys) {
						if (pk.name.equals(cm.column.name)) {
							cm.isPk = true;
							break;
						}
					}
				}
				columnModel.add(cm);
			} catch (Exception e) {
				UIUtil.showException(this, "Warning, invalid column will be removed", e, UIUtil.EXCEPTION_CONTEXT_USER_WARNING);
				needsSave = true;
			}
		}
		columnListEditor.setModel(columnModel);
		
		checkPKButton.setEnabled(false);
		for (ColumnModel cm: columnModel) {
			if (cm.isPk) {
				checkPKButton.setEnabled(true);
				break;
			}
		}
		updateTable(columnModel);

		setVisible(true);
		boolean excludeSet = excludeFromDeletion.isSelected();

		if (columnListEditor.needsSave()) {
			needsSave = true;
		}
		
		if (isOk) {
			List<String> line = new ArrayList<String>();
			List<String> tLine = new ArrayList<String>();
			line.add("");
			tLine.add("");
			tLine.add("");
			for (ColumnModel cm: columnModel) {
				line.add(toSql(cm.column));
				if (cm.isPk) {
					tLine.add(toSql(cm.column));
				}
			}
			currentColumnLine.cells.clear();
			currentColumnLine.cells.addAll(line);
			currentColumnLine.length = line.size();
			tableLine.cells.clear();
			tableLine.cells.addAll(tLine);
			tableLine.length = tLine.size();
			for (int i = 0; i < 10; ++i) {
				currentColumnLine.cells.add("");
				tableLine.cells.add("");
			}
		}
		
		if (isOk && 
				!(
						origDisplayName.equals(displayName.getText()) 
						&& origName.equals(nameField.getText()) 
						&& !needsSave
						&& origUpsert == upsertCheckbox.isSelected()
						&& origExcludeSet == excludeSet)) {
			columnLines.remove(currentColumnLine.cells.get(0));
			currentColumnLine.cells.set(0, nameField.getText().trim());
			columnLines.put(currentColumnLine.cells.get(0), currentColumnLine);

			tableLine.cells.set(0, nameField.getText().trim());
			tableLine.cells.set(1, upsertCheckbox.isSelected()? "Y" : "N");
			
			// update author
			for (int i = 0; i < tableLine.cells.size() ; ++i) {
				if ("".equals(tableLine.cells.get(i))) {
					while (tableLine.cells.size() <= i) {
						tableLine.cells.add("");
					}
					tableLine.cells.set(i+1, DataModelEditor.DATA_MODEL_EDITOR_AUTHOR);
					break;
				}
			}
			
			//rename associations source/destination
			for (Line a: associations) {
				if (a.cells.get(0).equalsIgnoreCase(origName.trim())) {
					a.cells.set(0, nameField.getText().trim());
				}
				if (a.cells.get(1).equalsIgnoreCase(origName.trim())) {
					a.cells.set(1, nameField.getText().trim());
				}
			}
			
			displayNames.remove(origName);
			dn = displayName.getText().trim();
			if (dn.length() > 0) {
				displayNames.put(nameField.getText().trim(), dn);
			}
			
			excludeFromDeletionList.remove(origName);
			
			if (excludeSet) {
				excludeFromDeletionList.add(nameField.getText().trim());
			}

			return true;
		} else {
			currentTableLine.cells.clear();
			currentTableLine.cells.addAll(oldTableLineCells);
			currentTableLine.length = oldTableLineLength;
			currentColumnLine.cells.clear();
			currentColumnLine.cells.addAll(oldColumnLineCells);
			currentColumnLine.length = oldColumnLineLength;
		}
		return false;
	}

	private boolean pkChanged = false;
	private boolean hasNullablePKColumn = false;

	private void updateWarningPanel() {
		warnPanel.setVisible(pkChanged || hasNullablePKColumn);
		warnPKChangedLabel.setVisible(pkChanged);
		warnSeparator.setVisible(pkChanged && hasNullablePKColumn);
		warnNullablePKLabel.setVisible(hasNullablePKColumn);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton checkPKButton;
    private javax.swing.JPanel columnDetailsPanel;
    private javax.swing.JCheckBox columnIsIdentity;
    private javax.swing.JCheckBox columnIsNullable;
    private javax.swing.JCheckBox columnIsVirtual;
    private javax.swing.JTextField columnLength;
    private javax.swing.JTextField columnName;
    private javax.swing.JTextField columnPrec;
    private javax.swing.JTextField columnType;
    private javax.swing.JTextField displayName;
    private javax.swing.JCheckBox excludeFromDeletion;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JTextField nameField;
    private javax.swing.JButton okButton;
    private javax.swing.JCheckBox primaryKey1;
    private javax.swing.JPanel slotPanel;
    private javax.swing.JCheckBox upsertCheckbox;
    private javax.swing.JLabel warnNullablePKLabel;
    private javax.swing.JLabel warnPKChangedLabel;
    private javax.swing.JPanel warnPanel;
    private javax.swing.JPanel warnSeparator;
    // End of variables declaration//GEN-END:variables
	
    private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}
	private static final long serialVersionUID = -3331167410435129849L;
}
