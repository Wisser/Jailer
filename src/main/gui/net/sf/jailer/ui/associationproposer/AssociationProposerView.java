/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
package net.sf.jailer.ui.associationproposer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.TextArea;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import org.fife.rsta.ui.EscapableDialog;
import org.fife.ui.rtextarea.RTextScrollPane;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.DataModelEditor;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.sqlconsole.SQLPlusSupport;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * Analyzes SQL statements and proposes association definitions. <br>
 * This allows to reverse-engineer the data model based on existing SQL queries. <br><br>
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class AssociationProposerView extends javax.swing.JPanel {
	private int numErrors;
	private RSyntaxTextAreaWithSQLSyntaxStyle editorPane;
	private TextArea checksPane;
	private DefaultTableModel proposalsModel;
	private DefaultTableModel knownModel;
	private final DataModel dataModel;
	private JDialog dialog;
	final AssociationProposer associationProposer;
	private DataModelBasedSQLCompletionProvider provider;
	private final ExecutionContext executionContext;
	private final int timeoutSec;
	
    /**
     * Creates new form ConstraintChecker
     * @param scriptFile file to load, if not <code>null</code>
     */
    public AssociationProposerView(JFrame owner, DataModel dataModel, final File scriptFile, final int timeoutSec, ExecutionContext executionContext) {
        this.dataModel = dataModel;
        this.associationProposer = new AssociationProposer(dataModel);
        this.timeoutSec = timeoutSec;
    	this.executionContext = executionContext;
    	
    	initComponents(); UIUtil.initComponents(this);
    	
    	loadButton.setIcon(UIUtil.scaleIcon(loadButton, loadIcon));
    	closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
        
        editorPane = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			@Override
			protected void runBlock() {
				super.runBlock();
				applyButtonActionPerformed(null);
			}
		};
		
		Font infoFont = jinfoLabe.getFont();
		infoFont = infoFont.deriveFont(infoFont.getStyle(), (int) (infoFont.getSize() * 1.2));
		jinfoLabe.setFont(infoFont);
	
		try {
			provider = new DataModelBasedSQLCompletionProvider(null, dataModel);
			provider.setDefaultClause(SQLCompletionProvider.Clause.WHERE);
			new SQLAutoCompletion(provider, editorPane);
		} catch (Exception e) {
			// ignore
		}
		editorPane.setRows(6);
        editorPane.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				update();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				update();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				update();
			}
			private void update() {
				applyButton.setEnabled(true);
			}
		});
        RTextScrollPane jScrollPane = new RTextScrollPane();
        jScrollPane.setViewportView(editorPane);
        conditionEditorPanel.add(jScrollPane);
        jScrollPane.setLineNumbersEnabled(true);

        checksPane = new TextArea();
        checksPane.setEditable(false);
        JScrollPane jScrollPane2 = new JScrollPane();
        jScrollPane2.setViewportView(checksPane);
        problemTabPanel.add(jScrollPane2);

        statusLabel.setVisible(false);

        proposalTable.setShowVerticalLines(false);
        proposalTable.setShowHorizontalLines(false);
		proposalTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		proposalsModel = new DefaultTableModel(new String[] { " ", "A", "B", "Condition" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return column == 0;
			}
			
			@Override
			public void setValueAt(Object aValue, int row, int column) {
				super.setValueAt(aValue, row, column);
				updateButtonsState();
			}
			
			@Override
            public Class<?> getColumnClass(int columnIndex) {
                if(columnIndex == 0){
                    return Boolean.class;
                }
                return super.getColumnClass(columnIndex);
            }
		};
		final JCheckBox checkBox = new JCheckBox("  ");
		checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
		DefaultCellEditor anEditor = new DefaultCellEditor(checkBox);
		anEditor.setClickCountToStart(1);
		proposalTable.setDefaultEditor(Boolean.class, anEditor);
		
		proposalTable.setModel(proposalsModel);
		
		final TableCellRenderer defaultTableCellRenderer = proposalTable.getDefaultRenderer(String.class);
		TableCellRenderer renderer = new TableCellRenderer() {
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (value instanceof Boolean) {
					JCheckBox checkBox = new JCheckBox("  ");
					checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
					checkBox.setSelected(Boolean.TRUE.equals(value));
					render = checkBox;
				}
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					((JLabel) render).setToolTipText(UIUtil.toHTML(String.valueOf(value), 200));
				}
				return render;
			}
		};
		proposalTable.getColumnModel().getColumn(0).setCellRenderer(renderer);
		proposalTable.setDefaultRenderer(Object.class, renderer);
		proposalTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				updateConditionEditorState();
			}
		});
		proposalTable.setAutoCreateRowSorter(true);
		List<SortKey> keys = new ArrayList<SortKey>();
		keys.add(new SortKey(1, SortOrder.ASCENDING));
		keys.add(new SortKey(2, SortOrder.ASCENDING));
		proposalTable.getRowSorter().setSortKeys(keys);

		knownModel = new DefaultTableModel(new String[] { "A", "B", "Condition" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		knownTable.setModel(knownModel);
		
		knownTable.setAutoCreateRowSorter(true);
		knownTable.setRowSelectionAllowed(false);
		knownTable.setShowVerticalLines(false);
		knownTable.setShowHorizontalLines(false);
		knownTable.setDefaultRenderer(Object.class, renderer);
		keys = new ArrayList<SortKey>();
		keys.add(new SortKey(0, SortOrder.ASCENDING));
		keys.add(new SortKey(1, SortOrder.ASCENDING));
		knownTable.getRowSorter().setSortKeys(keys);

		updateButtonsState();
		updateConditionEditorState();
		
		if (scriptFile != null) {
			loadButton.setVisible(false);
			emptyLineCheckBox.setSelected(true);
			emptyLineCheckBox.setVisible(false);
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					loadSQLScript(scriptFile);
				}
			});
		}
		
        dialog = new EscapableDialog(owner, "Analyze SQL") {
        };
        dialog.setModal(true);
		dialog.getContentPane().add(this);
		dialog.pack();
		UIUtil.setDialogSize(dialog, 800, 600);
		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2);
		UIUtil.fit(dialog);

		dialog.setVisible(true);
    }

    private void updateConditionEditorState() {
		int i = proposalTable.getSelectedRow();
		if (i >= 0) {
			i = proposalTable.getRowSorter().convertRowIndexToModel(i);
			editorPane.setText(String.valueOf(proposalsModel.getValueAt(i, 3)));
			editorPane.setCaretPosition(0);
			editorPane.setEnabled(true);
	        editorPane.setEditable(true);
			applyButton.setEnabled(false);
			if (provider != null) {
				provider.removeAliases();
				Table table1 = newAssociations.get(i).source;
				Table table2 = newAssociations.get(i).destination;
				if (table1 != null) {
					provider.addAlias("A", table1);
				}
				if (table2 != null) {
					provider.addAlias("B", table2);
				}
			}
		} else {
			editorPane.setText("");
			editorPane.setEnabled(false);
	        editorPane.setEditable(false);
			applyButton.setEnabled(false);
		}
	}

    private void applyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_applyButtonActionPerformed
    	int i = proposalTable.getSelectedRow();
		if (i >= 0) {
			i = proposalTable.getRowSorter().convertRowIndexToModel(i);
			proposalsModel.setValueAt(editorPane.getText(), i, 3);
			applyButton.setEnabled(false);
		}
    }//GEN-LAST:event_applyButtonActionPerformed

    private boolean allowButtonUpdates = true;

	private void updateButtonsState() {
		if (allowButtonUpdates) {
			if (proposalsModel.getRowCount() == 0) {
				selectAllButton.setEnabled(false);
				deselectAllButton.setEnabled(false);
				acceptButton.setEnabled(false);
			} else {
				boolean allSelected = true;
				boolean allDeSelected = true;
				boolean hasSelected = false;
				for (int i = 0; i < proposalsModel.getRowCount(); ++i) {
					if (Boolean.TRUE.equals(proposalsModel.getValueAt(i, 0))) {
						hasSelected = true;
						allDeSelected = false;
					} else {
						allSelected = false;
					}
				}
				selectAllButton.setEnabled(!allSelected);
				deselectAllButton.setEnabled(!allDeSelected);
				acceptButton.setEnabled(hasSelected);
			}
		}
	}

	private void loadSQLScript(File sqlFile) {
		if (sqlFile == null) {
	    	String fn = UIUtil.choseFile(null, ".", "Load SQL Script", "", this, false, true);
			if (fn == null) {
				return;
			}
			sqlFile = new File(fn);
		}
		try {
			loadButton.setEnabled(false);
	    	final File file = sqlFile;
			jProgressBar1.setMaximum(1000);
			final boolean emptyLineSeparates = emptyLineCheckBox.isSelected();
			Thread thread = new Thread(new Runnable() {
				@Override
				public void run() {
					int lineNr = 0;
					try {
						FileInputStream inputStream = new FileInputStream(file);
						long fileSize = Math.max(file.length(), 1);
						BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
					    SQLPlusSupport sqlPlusSupport = new SQLPlusSupport();

						String line = null;
						long bytesRead = 0;
						LineReader lineReader = new LineReader(bufferedReader);
						final StringBuffer currentStatement = new StringBuffer();
						while ((line = lineReader.readLine()) != null) {
							++lineNr;
							bytesRead += line.length() + 1;
							line = line.trim();
							if (line.startsWith("--")) {
								continue;
							}
							if (line.endsWith(";") || emptyLineSeparates && line.length() == 0) {
								if (line.length() > 0) {
									currentStatement.append(line.substring(0, line.length() - 1));
								}
								String stmt = currentStatement.toString().trim();
								if (stmt.length() > 0) {
									stmt = 
											stmt
						            		.replaceFirst("(?is)(;\\s*)+$", "")
						            		.replaceAll("((?:(?:;(?: |\\t|\\r)*?(?:--[^\\n]*)?))) ?\\\\([ \\t\\r]*\\n)", "$1$2")
						            		.replaceAll("((?:\\n(?: |\\t|\\r)*?)) ?\\\\([ \\t\\r]*)(?=\\n)", "$1");
									stmt = sqlPlusSupport.replaceVariables(stmt, null);
							        if (!sqlPlusSupport.executeSQLPLusStatement(stmt)) {
										addResult(bytesRead * 1000L / fileSize, null, null, associationProposer.analyze(stmt, lineNr, timeoutSec));
									}
								}
								currentStatement.setLength(0);
							} else {
								currentStatement.append(line + " \n");
							}
						}
						bufferedReader.close();
						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								addResult(1000L, sort(associationProposer.pickUpNewAssociations()), sort(associationProposer.pickUpKnownAssociations()), null);
								jProgressBar1.setValue(1000);
								loadButton.setEnabled(true);
								acceptButton.grabFocus();
							}
							private List<Association> sort(List<Association> associations) {
								Collections.sort(associations, new Comparator<Association>() {
									@Override
									public int compare(Association o1, Association o2) {
										int sourceDiff = o1.source.getName().compareTo(o2.source.getName());
										if (sourceDiff != 0) {
											return sourceDiff;
										}
										int destinationDiff = o1.destination.getName().compareTo(o2.destination.getName());
										if (destinationDiff != 0) {
											return destinationDiff;
										}
										return o1.getUnrestrictedJoinCondition().compareTo(o2.getUnrestrictedJoinCondition());
									}
								});
								return associations;
							}
						});
					} catch (Throwable e) {
						final Throwable fe = e;
						final String title = "Error in file \"" + file.getName() + "\" at line " + lineNr;
						final Object exceptionContext = e instanceof IOException? UIUtil.EXCEPTION_CONTEXT_USER_ERROR : null;
						UIUtil.invokeLater(new Runnable() {
							@Override
							public void run() {
								UIUtil.showException(AssociationProposerView.this, title, fe, exceptionContext);
								loadButton.setEnabled(true);
							}
						});
					}
				}});
			thread.setDaemon(true);
			thread.start();
		} catch (Throwable e) {
			UIUtil.showException(this, "Error", e);
		}
	}

    private List<Association> newAssociations = new ArrayList<Association>();
    private final int MAX_ERRORS = 1000;

    private void addResult(final long progess, final List<Association> associations, final List<Association> knownAssociations, final String error) {
    	UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
		    	if (error != null) {
					++numErrors;
					if (numErrors == MAX_ERRORS) {
						checksPane.append("more...\n");
					} else if (numErrors < MAX_ERRORS) {
						checksPane.append(error + "\n");
					}
		    	}
		    	if (associations != null) {
					for (Association association: associations) {
						newAssociations.add(association);
						proposalsModel.addRow(new Object[] { true, association.source.getName(), association.destination.getName(), association.getUnrestrictedJoinCondition() });
					}
				}
		    	if (knownAssociations != null) {
					for (Association association: knownAssociations) {
						newAssociations.add(association);
						knownModel.addRow(new Object[] { association.source.getName(), association.destination.getName(), association.getUnrestrictedJoinCondition() });
					}
				}
				if (associations != null || knownAssociations != null) {
					statusLabel.setVisible(true);
					statusLabel.setText(proposalsModel.getRowCount() + " Proposals, " + knownModel.getRowCount() + " already known associations, " + numErrors + " Problems");
					adjustTableColumnsWidth();
					if (proposalsModel.getRowCount() == 0 && knownModel.getRowCount() == 0 && numErrors > 0) {
						tabbedPane.setSelectedComponent(problemTabPanel);
					} else if (proposalsModel.getRowCount() == 0 && knownModel.getRowCount() > 0) {
						tabbedPane.setSelectedComponent(knownPanel);
					}
				}
				
		    	jProgressBar1.setValue((int) progess);
		    	updateButtonsState();
			}
		});
	}

	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jProgressBar1 = new javax.swing.JProgressBar();
        statusLabel = new javax.swing.JLabel();
        closeButton = new javax.swing.JButton();
        tabbedPane = new javax.swing.JTabbedPane();
        propsTabPanel = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        proposalTable = new javax.swing.JTable();
        jPanel4 = new javax.swing.JPanel();
        selectAllButton = new javax.swing.JButton();
        deselectAllButton = new javax.swing.JButton();
        acceptButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        panel8 = new javax.swing.JPanel();
        conditionEditorPanel = new javax.swing.JPanel();
        applyButton = new javax.swing.JButton();
        knownPanel = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        knownTable = new javax.swing.JTable();
        problemTabPanel = new javax.swing.JPanel();
        loadButton = new javax.swing.JButton();
        emptyLineCheckBox = new javax.swing.JCheckBox();
        dummyLabel1 = new javax.swing.JLabel();
        jinfoLabe = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jProgressBar1.setStringPainted(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 6;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jProgressBar1, gridBagConstraints);

        statusLabel.setForeground(/* Renaming also in *.form! */ Colors.Color_255_0_0);
        statusLabel.setText("Ready");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 8, 0);
        jPanel1.add(statusLabel, gridBagConstraints);

        closeButton.setText("Close");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        jPanel1.add(closeButton, gridBagConstraints);

        propsTabPanel.setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        proposalTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane1.setViewportView(proposalTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane1, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        selectAllButton.setText("Select all");
        selectAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel4.add(selectAllButton, gridBagConstraints);

        deselectAllButton.setText("Deselect all");
        deselectAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deselectAllButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel4.add(deselectAllButton, gridBagConstraints);

        acceptButton.setText("Accept selected proposals");
        acceptButton.setToolTipText("Merge the selected proposals into the Data Model.");
        acceptButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                acceptButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        jPanel4.add(acceptButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel2.add(jPanel4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 2.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jPanel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        propsTabPanel.add(jPanel3, gridBagConstraints);

        jPanel7.setLayout(new java.awt.GridBagLayout());

        panel8.setBorder(javax.swing.BorderFactory.createTitledBorder("Condition Editor"));
        panel8.setLayout(new java.awt.GridBagLayout());

        conditionEditorPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        panel8.add(conditionEditorPanel, gridBagConstraints);

        applyButton.setText("Apply");
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        panel8.add(applyButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 0.6;
        jPanel7.add(panel8, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        propsTabPanel.add(jPanel7, gridBagConstraints);

        tabbedPane.addTab("Proposals", propsTabPanel);

        knownPanel.setLayout(new java.awt.BorderLayout());

        knownTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane2.setViewportView(knownTable);

        knownPanel.add(jScrollPane2, java.awt.BorderLayout.CENTER);

        tabbedPane.addTab("Known", knownPanel);

        problemTabPanel.setLayout(new java.awt.BorderLayout());
        tabbedPane.addTab("Problems", problemTabPanel);
        problemTabPanel.getAccessibleContext().setAccessibleName("");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(tabbedPane, gridBagConstraints);
        tabbedPane.getAccessibleContext().setAccessibleDescription("");

        loadButton.setText("Load and Analyze SQL Script");
        loadButton.setToolTipText("<html>A SQL Script is a text file that contains SQL-statements. <br>\nEach statement is terminated by a ';' character.<br>\nEmpty lines may or may not separate statements.");
        loadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                loadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 8, 0);
        jPanel1.add(loadButton, gridBagConstraints);

        emptyLineCheckBox.setSelected(true);
        emptyLineCheckBox.setText("Empty line separates statements");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 12, 8, 0);
        jPanel1.add(emptyLineCheckBox, gridBagConstraints);

        dummyLabel1.setForeground(/* Renaming also in *.form! */ Colors.Color_0_105_0);
        dummyLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 7;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(dummyLabel1, gridBagConstraints);

        jinfoLabe.setText("<html>Analyzes SQL statements and proposes association definitions. This allows to reverse-engineer the data model based on existing SQL queries.<hr></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jinfoLabe, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
        dialog.dispose();
    }//GEN-LAST:event_closeButtonActionPerformed

    private void loadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_loadButtonActionPerformed
    	loadSQLScript(null);
    }//GEN-LAST:event_loadButtonActionPerformed

    private void selectAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectAllButtonActionPerformed
    	setProposalsSelected(true);
    }//GEN-LAST:event_selectAllButtonActionPerformed

    private void deselectAllButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deselectAllButtonActionPerformed
    	setProposalsSelected(false);
    }//GEN-LAST:event_deselectAllButtonActionPerformed

    private void acceptButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_acceptButtonActionPerformed
        acceptProposals();
    }//GEN-LAST:event_acceptButtonActionPerformed

	private void setProposalsSelected(boolean selected) {
    	allowButtonUpdates = false;
    	try {
    		for (int i = 0; i < proposalsModel.getRowCount(); ++i) {
    			proposalsModel.setValueAt(selected, i, 0);
    		}
    	} finally {
    		allowButtonUpdates = true;
    		updateButtonsState();
    	}
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton acceptButton;
    private javax.swing.JButton applyButton;
    private javax.swing.JButton closeButton;
    private javax.swing.JPanel conditionEditorPanel;
    private javax.swing.JButton deselectAllButton;
    private javax.swing.JLabel dummyLabel1;
    private javax.swing.JCheckBox emptyLineCheckBox;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JProgressBar jProgressBar1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel jinfoLabe;
    private javax.swing.JPanel knownPanel;
    private javax.swing.JTable knownTable;
    private javax.swing.JButton loadButton;
    private javax.swing.JPanel panel8;
    private javax.swing.JPanel problemTabPanel;
    private javax.swing.JTable proposalTable;
    private javax.swing.JPanel propsTabPanel;
    private javax.swing.JButton selectAllButton;
    private javax.swing.JLabel statusLabel;
    private javax.swing.JTabbedPane tabbedPane;
    // End of variables declaration//GEN-END:variables

	static final LinkedBlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
    static {
    	Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						queue.take().run();
					} catch (Throwable t) {
					}
				}
			}
		});
        thread.setDaemon(true);
        thread.start();
    }

	public void adjustTableColumnsWidth() {
		adjustTableColumnsWidth(proposalTable, true);
		adjustTableColumnsWidth(knownTable, false);
	}

	public void adjustTableColumnsWidth(JTable table, boolean fixFirstColumn) {
		DefaultTableModel dtm = (DefaultTableModel) table.getModel();
		DefaultTableCellRenderer defaultTableCellRenderer = new DefaultTableCellRenderer();
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			Component comp = defaultTableCellRenderer.getTableCellRendererComponent(table, column.getHeaderValue(), false, false, 0, i);
			int width = 1;
			width = Math.max(width, comp.getPreferredSize().width);

			int line = 0;
			for (; line < table.getRowCount(); ++line) {
				comp = table.getCellRenderer(line, i).getTableCellRendererComponent(table, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width);
			}
			column.setPreferredWidth(Math.min(width, 400));
			if (i == 0 && fixFirstColumn) {
				column.setWidth(column.getPreferredWidth());
			}
		}
	}
    
	private static class LineReader {

		private final BufferedReader reader;
		private boolean eofRead = false;
		
		public LineReader(BufferedReader reader) {
			this.reader = reader;
		}

		public String readLine() throws IOException {
			String line = reader.readLine();
			if (line == null && !eofRead) {
				eofRead = true;
				return ";";
			}
			return line;
		}
	}
	
	private boolean accepted = false;
	
    public boolean isAccepted() {
		return accepted;
	}

	private void acceptProposals() {
    	try {
    		List<String> names = new ArrayList<String>();
    		for (int i = 0; i < proposalsModel.getRowCount(); ++i) {
    			if (Boolean.TRUE.equals(proposalsModel.getValueAt(i, 0))) {
    	    		names.add("P_" + Quoting.staticUnquote(String.valueOf(proposalsModel.getValueAt(i, 1))) + "_" + Quoting.staticUnquote(String.valueOf(proposalsModel.getValueAt(i, 2))));
    			} else {
    				names.add("?");
    			}
    		}
    		Map<String, Integer> nameCount = new HashMap<String, Integer>();
    		for (String name: names) {
    			Integer count = nameCount.get(name);
    			if (count == null) {
    				count = 1;
    			} else {
    				++count;
    			}
    			nameCount.put(name, count);
    		}
    		for (int i = 0; i < names.size(); ++i) {
    			String name = names.get(i);
    			int n = 0;
    			while (nameCount.get(name) != null && nameCount.get(name) > 1 || dataModel.namedAssociations.containsKey(name)) {
    				name = names.get(i) + "_" + (++n);
    			}
    			if (n > 0) {
    				nameCount.put(name, 2);
    			}
    			names.set(i, name);
    		}
    		
    		BufferedWriter out = new BufferedWriter(new FileWriter(ModelBuilder.getModelBuilderAssociationsFilename(executionContext)));
    		out.append("\n");
    		AssociationProposer ap = new AssociationProposer(dataModel);
    		int knownCount = 0;
    		int allCount = 0;
    		for (int i = 0; i < proposalsModel.getRowCount(); ++i) {
    			if (Boolean.TRUE.equals(proposalsModel.getValueAt(i, 0))) {
	    			String condition = String.valueOf(proposalsModel.getValueAt(i, 3));
	    			condition = condition.replaceAll(" *\\n", " ");
	    			String fromName = proposalsModel.getValueAt(i, 1).toString();
					String toName = proposalsModel.getValueAt(i, 2).toString();
					Table from = dataModel.getTable(fromName);
					Table to = dataModel.getTable(toName);
					if (from != null && to != null) {
						++allCount;
						Association association = new Association(from, to, false, false, condition, dataModel, false, null);
						if (ap.addAssociation(names.get(i), new Pair<Table, Table>(from, to), association, true)) {
							out.append(
		    					CsvFile.encodeCell(String.valueOf(fromName)) + "; " +
		    					CsvFile.encodeCell(String.valueOf(toName)) + "; ; ; " +
		    					CsvFile.encodeCell(condition) + "; " + names.get(i) + "; " + DataModelEditor.DATA_MODEL_EDITOR_AUTHOR + "\n");
						} else {
							++knownCount;
						}
					}
    			}
    		}
    		out.close();
    		accepted = true;
    		if (knownCount > 0) {
    			JOptionPane.showMessageDialog(dialog, knownCount + " of " + allCount + " associations are already known.", "", JOptionPane.INFORMATION_MESSAGE);
    		}
    		dialog.dispose();
    	} catch (Throwable t) {
    		UIUtil.showException(this, "Error", t);
    	}
	}

	private ImageIcon loadIcon;
	private ImageIcon cancelIcon;
	{
		// load images
		loadIcon = UIUtil.readImage("/load2.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
    }

	// TODO 2
	// TODO write docu for this
	
	// TODO warning if one of the two PKs is only partially covered.
}

