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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;


/**
 * Editor for a list of elements.
 * 
 * @param <T> type of element
 *
 * @author Wisser
 */
public abstract class ListEditor<T> extends javax.swing.JPanel {

	protected List<T> model;
	private final String[] columnNames;
	private final String elementTypeDisplayName;
	private JDialog detailsDialog;
	private boolean needsSave = false;
	private T element;
	private JComponent detailsView;
	private int currentIndex;
	
	/**
	 * @return the model
	 */
	public List<T> getModel() {
		return model;
	}

	/**
	 * @param model the model to set
	 */
	public void setModel(List<T> model) {
		this.model = model;
		columnsTable.setModel(createTableModel());
		adjustColumnsWidth();
		updateEnableState();
	}

	private static final long serialVersionUID = 6636910708235642618L;

	/**
	 * Creates new form ListEditor
	 * @param simple 
	 */
	public ListEditor(String[] columnNames, String elementTypeDisplayName,
			boolean withCopy,
			boolean readonly,
			final boolean simple) {
		this.columnNames = columnNames;
		this.elementTypeDisplayName = elementTypeDisplayName;
		
		initComponents();
		
		if (!withCopy) {
			copyButton.setVisible(false);
		}
		
		if (!readonly) {
			columnsTable.addMouseListener(new MouseAdapter() {
				@Override
				public void mousePressed(MouseEvent me) {
					JTable table =(JTable) me.getSource();
					Point p = me.getPoint();
					int row = table.rowAtPoint(p);
					if (row >= 0) {
						onElementClicked(model.get(row));
						if (me.getClickCount() >= 2) {
							onDoubleClick(model.get(row));
						}
					}
					if (me.getClickCount() >= 2) {
						columnsTable.getSelectionModel().setSelectionInterval(row, row);
						updateButtonActionPerformed(null);
					}
				}
			});
			columnsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		} else {
			columnsTable.setAutoCreateRowSorter(true);
			columnsTable.setEnabled(false);
			upButton.setVisible(false);
			downButton.setVisible(false);
			addButton.setVisible(false);
			updateButton.setVisible(false);
			deleteButton.setVisible(false);
		}
		final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
		final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;
		final Color BGSEL = new Color(0, 200, 255);
		TableCellRenderer renderer = new DefaultTableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table,
					Object value, boolean isSelected, boolean hasFocus,
					int row, int column) {
				Component render = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (!isSelected) {
					render.setBackground(simple? BG1: row % 2 == 0? BG1 : BG2);
				} else {
					render.setBackground(BGSEL);
				}
				if (render instanceof JLabel) {
					((JLabel) render).setToolTipText(value != null? value.toString() : null);
				}
				if (row >= 0 && row < model.size()) {
					Color color = getForegroundColor(model.get(row), column);
					if (color != null) {
						render.setForeground(color);
					} else {
						render.setForeground(Color.BLACK);
					}
				}
				return render;
			}
			private static final long serialVersionUID = 2081014492388534455L;
		};
		columnsTable.setDefaultRenderer(Object.class, renderer);
		columnsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				updateEnableState();
				onSelectionChange();
			}
		});
	}

	abstract protected String getDisplayName(T element);
	abstract protected T copy(T element);
	abstract protected T createNew();
	abstract protected JComponent createDetailsView(T element);
	protected void onElementClicked(T t) {
	}
	protected Dimension detailsViewMinSize() {
		return null;
	}
	protected Dimension detailsViewMaxSize() {
		return null;
	}
	abstract protected void updateFromDetailsView(T element, JComponent detailsView, List<T> model, StringBuilder errorMessage);

	private void updateEnableState() {
		int i = columnsTable.getSelectedRow();
		upButton.setEnabled(i > 0);
		downButton.setEnabled(i >= 0 && i < model.size() - 1);
		updateButton.setEnabled(i >= 0);
		deleteButton.setEnabled(i >= 0);
		updateButton.setEnabled(i >= 0);
		copyButton.setEnabled(i >= 0);
	}

	/**
	 * Creates the model for the columns table.
	 * 
	 * @return the model for the columns table
	 */
	private TableModel createTableModel() {
		DefaultTableModel model = new DefaultTableModel() {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
			private static final long serialVersionUID = -5655810990374489020L;
		};
		for (String columnName: columnNames) {
			model.addColumn(columnName);
		}
		int i = 0;
		for (T element: this.model) {
			model.addRow(toColumnList(element, i++).clone());
		}
		return model;
	}
	
	private void adjustColumnsWidth() {
		DefaultTableModel dtm = (DefaultTableModel) columnsTable.getModel();
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		for (int i = 0; i < columnsTable.getColumnCount(); i++) {
			TableColumn column = columnsTable.getColumnModel().getColumn(i);
			
			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(columnsTable, column.getHeaderValue(), false, false, 0, i);
			int width = comp.getPreferredSize().width;

			for (int line = 0; line < columnsTable.getRowCount(); ++line) {
				comp = columnsTable.getCellRenderer(line, i).getTableCellRendererComponent(columnsTable, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, Math.min(comp.getPreferredSize().width + 16, 100));
				if (line > 2000) {
					break;
				}
			}

			column.setPreferredWidth(width);
		}
	}
	
	/**
	 * Creates a list of column-values for a model element.
	 * 
	 * @param element the model element
	 * @return list of column-values for a model element
	 */
	protected abstract Object[] toColumnList(T element, int index);

	/**
	 * Gets foreground color of the column of a given model element.
	 * 
	 * @param element the element
	 * @param column 
	 * @return foreground color or <code>null</code> for default color
	 */
	protected abstract Color getForegroundColor(T element, int column);

	/**
	 * Reacts on model changes.
	 */
	protected void onModelUpdate() {
	}

	/**
	 * Reacts on selection changes.
	 */
	protected void onSelectionChange() {
	}
	/**
	 * Gets the currently selected element.
	 * 
	 * @return currently selected element or <code>null</code>, if nothing is selected
	 */
	public T getSelectedElement() {
		int i = columnsTable.getSelectedRow();
		if (i >= 0 && i < model.size()) {
			return model.get(i);
		}
		return null;
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
	private void initComponents() {
		java.awt.GridBagConstraints gridBagConstraints;

		detailsPanel = new javax.swing.JPanel();
		slotPanel = new javax.swing.JPanel();
		cancelButton = new javax.swing.JButton();
		okButton = new javax.swing.JButton();
		jPanel2 = new javax.swing.JPanel();
		jScrollPane1 = new javax.swing.JScrollPane();
		columnsTable = new javax.swing.JTable();
		upButton = new javax.swing.JButton();
		downButton = new javax.swing.JButton();
		addButton = new javax.swing.JButton();
		updateButton = new javax.swing.JButton();
		deleteButton = new javax.swing.JButton();
		copyButton = new javax.swing.JButton();

		detailsPanel.setLayout(new java.awt.GridBagLayout());

		slotPanel.setLayout(new java.awt.GridBagLayout());
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridwidth = 2;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		detailsPanel.add(slotPanel, gridBagConstraints);

		cancelButton.setText("Cancel");
		cancelButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				cancelButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 2;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
		detailsPanel.add(cancelButton, gridBagConstraints);

		okButton.setText(" Ok ");
		okButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				okButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
		detailsPanel.add(okButton, gridBagConstraints);

		setLayout(new java.awt.GridBagLayout());

		jPanel2.setLayout(new java.awt.GridBagLayout());

		columnsTable.setShowHorizontalLines(false);
		columnsTable.setShowVerticalLines(false);
		columnsTable.setShowGrid(false);

		columnsTable.setModel(new javax.swing.table.DefaultTableModel(
			new Object [][] {
				{null, null, null, null},
				{null, null, null, null},
				{null, null, null, null},
				{null, null, null, null}
			},
			new String [] {
				"Titel 1", "Titel 2", "Titel 3", "Titel 4"
			}
		));
		jScrollPane1.setViewportView(columnsTable);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridheight = 22;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		jPanel2.add(jScrollPane1, gridBagConstraints);

		upButton.setText("Up");
		upButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				upButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 10;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
		jPanel2.add(upButton, gridBagConstraints);

		downButton.setText("Down");
		downButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				downButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 11;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
		gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
		jPanel2.add(downButton, gridBagConstraints);

		addButton.setText("Add");
		addButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				addButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 3;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
		jPanel2.add(addButton, gridBagConstraints);

		updateButton.setText("Edit");
		updateButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updateButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 4;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
		jPanel2.add(updateButton, gridBagConstraints);

		deleteButton.setText("Delete");
		deleteButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				deleteButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 6;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
		gridBagConstraints.weighty = 1.0;
		gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
		jPanel2.add(deleteButton, gridBagConstraints);

		copyButton.setText("Copy");
		copyButton.addActionListener(new java.awt.event.ActionListener() {
			@Override
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				copyButtonActionPerformed(evt);
			}
		});
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 5;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
		gridBagConstraints.insets = new java.awt.Insets(0, 2, 0, 0);
		jPanel2.add(copyButton, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		add(jPanel2, gridBagConstraints);
	}// </editor-fold>//GEN-END:initComponents

	private void upButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_upButtonActionPerformed
	   int i = columnsTable.getSelectedRow();
	   if (i > 0) {
		   T element = model.get(i);
		   model.set(i, model.get(i - 1));
		   model.set(i - 1, element);
		   setModel(model);
		   columnsTable.getSelectionModel().setSelectionInterval(i - 1, i - 1);
		   needsSave = true;
		   onModelUpdate();
	   }
	}//GEN-LAST:event_upButtonActionPerformed

	private void downButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_downButtonActionPerformed
		int i = columnsTable.getSelectedRow();
		if (i >= 0 && i < model.size() - 1) {
			T element = model.get(i);
			model.set(i, model.get(i + 1));
			model.set(i + 1, element);
			setModel(model);
			columnsTable.getSelectionModel().setSelectionInterval(i + 1, i + 1);
		   needsSave = true;
		   onModelUpdate();
		}
	}//GEN-LAST:event_downButtonActionPerformed

	private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed
		element = createNew();
		currentIndex = -1;
		detailsView = createDetailsView(element);
		detailsDialog = createEditDialog(element, "New " + elementTypeDisplayName, detailsView);
		detailsDialog.setVisible(true);
	}//GEN-LAST:event_addButtonActionPerformed

	private void updateButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_updateButtonActionPerformed
		int i = columnsTable.getSelectedRow();
		if (i >= 0 && i < model.size()) {
			element = copy(model.get(i));
			currentIndex = i;
			detailsView = createDetailsView(element);
			if (detailsView != null) {
				detailsDialog = createEditDialog(element, "Edit " + elementTypeDisplayName, detailsView);
				detailsDialog.setVisible(true);
			}
		}
	}//GEN-LAST:event_updateButtonActionPerformed

	protected void onDoubleClick(T element) {
	}
	
	private JDialog createEditDialog(T element, String titel, JComponent detailsView) {
		Component owner = SwingUtilities.getRoot(this);
		JDialog dialog;
		if (owner instanceof Frame) {
			dialog = new JDialog((Frame) owner, titel, true);
		} else if (owner instanceof Dialog) {
			dialog = new JDialog((Dialog) owner, titel, true);
		} else {
			dialog = new JDialog((Dialog) null, titel, true);
		}
		slotPanel.removeAll();
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		gridBagConstraints.insets = new Insets(4, 4, 4, 4);
		slotPanel.add(detailsView, gridBagConstraints);
		dialog.getContentPane().add(detailsPanel);
		dialog.pack();
		Dimension m = detailsViewMinSize();
		if (m != null) {
			dialog.setSize(Math.max(dialog.getWidth(), m.width), Math.max(dialog.getHeight(), m.height));
		}
		m = detailsViewMaxSize();
		if (m != null) {
			dialog.setSize(Math.min(dialog.getWidth(), m.width), Math.min(dialog.getHeight(), m.height));
		}
		if (owner!= null) {
			dialog.setLocation(
					owner.getLocation().x + owner.getWidth() /2 - dialog.getWidth() /2,
					owner.getLocation().y + owner.getHeight() /2 - dialog.getHeight() /2);
		}
		return dialog;
	}

	private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteButtonActionPerformed
		int i = columnsTable.getSelectedRow();
		if (i >= 0 && i < model.size()) {
			String displayName = getDisplayName(model.get(i));
			if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Delete " + elementTypeDisplayName + (displayName == null? "" : (" \"" + displayName + "\"")) + "?", "Delete " + elementTypeDisplayName, JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
				model.remove(i);
				setModel(model);
			   columnsTable.getSelectionModel().clearSelection();
			   needsSave = true;
			   onModelUpdate();
			}
		}
	}//GEN-LAST:event_deleteButtonActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		detailsDialog.setVisible(false);
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		StringBuilder errorMessage = new StringBuilder();
		updateFromDetailsView(element, detailsView, model, errorMessage);
		if (errorMessage.length() > 0) {
			JOptionPane.showMessageDialog(this, errorMessage.toString(), "Error", JOptionPane.ERROR_MESSAGE);
		} else {
			if (currentIndex >= 0) {
				model.set(currentIndex, element);
			} else {
				model.add(element);
			}
			setModel(model);
			if (currentIndex < 0) {
				columnsTable.getSelectionModel().setSelectionInterval(model.size() - 1, model.size() - 1);
			} else {
				columnsTable.getSelectionModel().setSelectionInterval(currentIndex, currentIndex);
			}
			needsSave = true;
			detailsDialog.setVisible(false);
			onModelUpdate();
		}
	}//GEN-LAST:event_okButtonActionPerformed

	private void copyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyButtonActionPerformed
		int i = columnsTable.getSelectedRow();
		if (i >= 0 && i < model.size()) {
			element = createCopy(model.get(i));
			model.add(i + 1, element);
			setModel(model);
			   columnsTable.getSelectionModel().setSelectionInterval(i + 1, i + 1);
			needsSave = true;
		}
	}//GEN-LAST:event_copyButtonActionPerformed

	protected T createCopy(T t) {
		return copy(t);
	}

	public boolean needsSave() {
		return needsSave;
	}

	public void hideUpAndDownButton() {
		upButton.setVisible(false);
		downButton.setVisible(false);
	}

	public void forUpdateAndDeleteOnly() {
		hideUpAndDownButton();
		addButton.setVisible(false);
		copyButton.setVisible(false);
		updateButton.setVisible(true);
		updateButton.setText("Rename");
	}
	
	public void hideAllButtons() {
		hideUpAndDownButton();
		addButton.setVisible(false);
		copyButton.setVisible(false);
		updateButton.setVisible(false);
		deleteButton.setVisible(false);
	}

	// Variables declaration - do not modify//GEN-BEGIN:variables
	private javax.swing.JButton addButton;
	private javax.swing.JButton cancelButton;
	private javax.swing.JTable columnsTable;
	private javax.swing.JButton copyButton;
	private javax.swing.JButton deleteButton;
	private javax.swing.JPanel detailsPanel;
	private javax.swing.JButton downButton;
	private javax.swing.JPanel jPanel2;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JButton okButton;
	private javax.swing.JPanel slotPanel;
	private javax.swing.JButton upButton;
	private javax.swing.JButton updateButton;
	// End of variables declaration//GEN-END:variables

}
