/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.ui.autocomplete.AutoCompletion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil.PLAF;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.databrowser.sqlconsole.MetaDataBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.util.SqlUtil;

/**
 * Query Builder Dialog.
 * 
 * @author Ralf Wisser
 */
public class QueryBuilderDialog extends javax.swing.JDialog {

	/** Creates new form QueryBuilderDialog */
	public QueryBuilderDialog(java.awt.Frame parent) {
		super(parent, true);
		this.sqlTextArea = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			@Override
			protected void runBlock() {
				super.runBlock();
				if (sqlEditButton.isVisible()) {
					sqlEditButton.doClick();
				}
			}
		};
		initComponents(); UIUtil.initComponents(this);
		
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		sqlEditButton.setIcon(UIUtil.scaleIcon(sqlEditButton, sqlConsoleIcon));
		clipboardButton.setIcon(copyIcon);
		
		if (jScrollPane1.getHorizontalScrollBar() != null) {
        	jScrollPane1.getHorizontalScrollBar().setUnitIncrement(16);
        }
        if (jScrollPane1.getVerticalScrollBar() != null) {
        	jScrollPane1.getVerticalScrollBar().setUnitIncrement(16);
        }
        
		JScrollPane jScrollPane2 = new JScrollPane();
		jScrollPane2.setViewportView(sqlTextArea);

		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
		jPanel1.add(jScrollPane2, gridBagConstraints);
		distinctCheckBox.addItemListener(new ItemListener() {
			@Override
			public void itemStateChanged(ItemEvent e) {
				selectDistinct = distinctCheckBox.isSelected();
				updateSQL();
			}
		});
		mlmTextField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void changedUpdate(DocumentEvent e) {
				appendMLM(mlmTextField.getText());
			}

			@Override
			public void insertUpdate(DocumentEvent e) {
				appendMLM(mlmTextField.getText());
			}

			@Override
			public void removeUpdate(DocumentEvent e) {
				appendMLM(mlmTextField.getText());
			}
		});
		sqlEditButton.setVisible(false);
		pack();
		setSize(Math.max(800, getWidth()), 500);
	}
	
	private AutoCompletion autoCompletion = null;
	
	private void initAutoCompletion(DataModel dataModel, Session session, MetaDataSource metaDataSource) {
		if (autoCompletion != null) {
			autoCompletion.uninstall();
			autoCompletion = null;
		}
		try {
			CompletionProvider provider = null;
			if (metaDataSource != null) {
				provider = new MetaDataBasedSQLCompletionProvider(session, metaDataSource);
			} else if (dataModel != null) {
				provider = new DataModelBasedSQLCompletionProvider(session, dataModel);
			}
			
			if (provider != null) {
				autoCompletion = new SQLAutoCompletion(provider, sqlTextArea);
			}
		} catch (SQLException e1) {
		}
	}
	
	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel2 = new javax.swing.JPanel();
        saveButton = new javax.swing.JButton();
        clipboardSingleLineButton = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        mlmTextField = new javax.swing.JTextField();
        clipboardButton = new javax.swing.JButton();
        sqlEditButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jLabel4 = new javax.swing.JLabel();
        jPanel5 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        distinctCheckBox = new javax.swing.JCheckBox();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        jPanel8 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel6 = new javax.swing.JPanel();
        relationshipsPanel = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Query Builder");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        saveButton.setText(" Save ");
        saveButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 2, 2);
        jPanel2.add(saveButton, gridBagConstraints);

        clipboardSingleLineButton.setText(" Copy as Single Line ");
        clipboardSingleLineButton.setToolTipText(" Copy the query as a single line to the clipboard");
        clipboardSingleLineButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clipboardSingleLineButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 2, 4);
        jPanel2.add(clipboardSingleLineButton, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("multi-line continuation  ");
        jLabel1.setToolTipText("multi-line continuation character");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel4.add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 16;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        jPanel4.add(mlmTextField, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 8;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel2.add(jPanel4, gridBagConstraints);

        clipboardButton.setText(" Copy to Clipboard ");
        clipboardButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clipboardButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 2, 2);
        jPanel2.add(clipboardButton, gridBagConstraints);

        sqlEditButton.setText("SQL Console");
        sqlEditButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sqlEditButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 7;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 2, 2);
        jPanel2.add(sqlEditButton, gridBagConstraints);

        cancelButton.setText("Close");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 8;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 2, 4);
        jPanel2.add(cancelButton, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jLabel4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 2, 0);
        getContentPane().add(jPanel2, gridBagConstraints);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText("Select ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(1, 0, 0, 0);
        jPanel5.add(jLabel2, gridBagConstraints);

        distinctCheckBox.setText("distinct");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel5.add(distinctCheckBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(6, 8, 0, 0);
        getContentPane().add(jPanel5, gridBagConstraints);

        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane1.setDoubleBuffered(true);
        jSplitPane1.setOneTouchExpandable(true);

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1), "SQL Query", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, UIUtil.defaultTitleFont()));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel3.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel1.add(jPanel3, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel1);

        jPanel8.setLayout(new java.awt.GridBagLayout());

        jPanel6.setLayout(new java.awt.GridBagLayout());

        relationshipsPanel.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        relationshipsPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 0, 0);
        jPanel6.add(relationshipsPanel, gridBagConstraints);

        jScrollPane1.setViewportView(jPanel6);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel8.add(jScrollPane1, gridBagConstraints);

        jPanel7.setLayout(new java.awt.GridBagLayout());

        jLabel3.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(96, 0, 0, 0);
        jPanel7.add(jLabel3, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        jPanel7.add(jLabel5, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel8.add(jPanel7, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel8);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jSplitPane1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		dispose();
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void saveButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_saveButtonActionPerformed
		String fn = UIUtil.choseFile(null, ".", "Save SQL Query", "", this,
				false, false, false);
		if (fn != null) {
			try {
				PrintWriter out = new PrintWriter(new FileWriter(fn));
				out.print(sqlTextArea.getText());
				out.close();
			} catch (Exception e) {
				UIUtil.showException(this, "Error saving query", e);
			}
		}
	}// GEN-LAST:event_saveButtonActionPerformed

	private void clipboardButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_clipboardButtonActionPerformed
		sqlTextArea.selectAll();
		sqlTextArea.copy();
		sqlTextArea.select(0, 0);
	}// GEN-LAST:event_clipboardButtonActionPerformed

	private void joinAWithBButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_joinAWithBButtonActionPerformed
		createPathQuery(null);
	}// GEN-LAST:event_joinAWithBButtonActionPerformed

	private void clipboardSingleLineButtonActionPerformed(
			java.awt.event.ActionEvent evt) {// GEN-FIRST:event_clipboardSingleLineButtonActionPerformed
		String orig = sqlTextArea.getText();
		sqlTextArea.setText(orig.replaceAll(" *(\n|\r)+ *", " "));
		sqlTextArea.selectAll();
		sqlTextArea.copy();
		sqlTextArea.setText(orig);
		sqlTextArea.select(0, 0);
		sqlTextArea.discardAllEdits();
	}// GEN-LAST:event_clipboardSingleLineButtonActionPerformed

	private void sqlEditButtonActionPerformed(java.awt.event.ActionEvent evt) {// GEN-FIRST:event_sqlEditButtonActionPerformed
	}// GEN-LAST:event_sqlEditButtonActionPerformed

	private Font font = new JLabel("normal").getFont();

	/**
	 * Non-bold font.
	 */
	private Font nonBoldFont = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());

	/**
	 * Subject of query.
	 */
	private Table subject;

	private boolean selectDistinct = false;

	public static enum JoinOperator {
		Join("Join"), LeftJoin("Left Join");

		private final String operator;

		private JoinOperator(String operator) {
			this.operator = operator;
		}

		@Override
		public String toString() {
			return operator;
		}
	}

	/**
	 * Relationship.
	 */
	public static class Relationship {
		public List<Relationship> children = new ArrayList<Relationship>();
		public Relationship parent;
		public String whereClause;
		public String anchorWhereClause;
		public Association anchor;
		public boolean needsAnchor = false;
		public Association association;
		public JoinOperator joinOperator = JoinOperator.Join;

		public int level;
		public List<Relationship> origChildren = null;
		public JTextField aliasTextField;
		public Color originalBGColor;
		public boolean selectColumns;
		public String alias;
		public String aliasSuggestion;
		public Relationship originalParent;

		public List<Association> getPathToRoot() {
			List<Association> path = new ArrayList<Association>();
			path.add(association);
			if (parent != null) {
				path.addAll(parent.getPathToRoot());
			}
			return path;
		}

		public List<Relationship> flatten(int level, Relationship parent,
				boolean withLastPseudoChild) {
			this.level = level;
			this.parent = parent;
			List<Relationship> flat = new ArrayList<Relationship>();
			flat.add(this);
			if (withLastPseudoChild) {
				Relationship lastChild = new Relationship();
				lastChild.parent = this;
				lastChild.level = level + 1;
				flat.add(lastChild);
			}
			for (Relationship child : children) {
				flat.addAll(child.flatten(level + 1, this, withLastPseudoChild));
			}
			return flat;
		}

		public void dump(int level) {
			String indent = "                     ".substring(0, level * 4);
			System.out.println(indent
					+ (association == null ? "" : (association.source.getName()
							+ " -> " + association.destination.getName())));
			System.out.println(indent + anchorWhereClause);
			for (Relationship r : children) {
				r.dump(level + 1);
			}
		}
	}

	private Map<List<Association>, String> originalAnchorSQL = new HashMap<List<Association>, String>();
	private Map<List<Association>, String> originalConditionSQL = new HashMap<List<Association>, String>();
	private Map<List<Association>, Association> originalAnchor = new HashMap<List<Association>, Association>();

	/**
	 * Root relationship.
	 */
	private Relationship rootRelationship;

	private void resetRelationshipsPanel() {
		relationshipsPanel.removeAll();
		UIUtil.initToolTips(relationshipsPanel);

		// Table lastTable = null;
		List<Relationship> relationships = rootRelationship.flatten(0, null,
				true);
		for (int y = 0; y < relationships.size(); ++y) {
			final Relationship relationship = relationships.get(y);

			javax.swing.JLabel label;

			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 0.0;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			if (y == 0) {
				label = new javax.swing.JLabel();
				label.setText(" From  ");
				label.setFont(nonBoldFont);

				relationshipsPanel.add(label, gridBagConstraints);
			} else if (relationship.association != null) {
				JComboBox2 joinCB = new JComboBox2();
				DefaultComboBoxModel aModel = new DefaultComboBoxModel(
						JoinOperator.values());
				joinCB.setModel(aModel);
				joinCB.setSelectedItem(relationship.joinOperator);

				joinCB.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						if (e.getStateChange() == ItemEvent.SELECTED) {
							relationship.joinOperator = (JoinOperator) e
									.getItem();
							resetRelationshipsPanel();
							updateSQL();
						}
					}
				});

				relationshipsPanel.add(joinCB, gridBagConstraints);
			}

			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 3;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
			gridBagConstraints.weightx = 0.0;
			gridBagConstraints.insets = new Insets(0,
					0 + relationship.level * 12, 2, 0);
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			JComboBox2 tableCB = null;
			if (relationship != rootRelationship) {
				tableCB = new JComboBox2() {
					private boolean layingOut = false;

					@Override
					public void doLayout() {
						try {
							layingOut = true;
							super.doLayout();
						} finally {
							layingOut = false;
						}
					}

					@Override
					public Dimension getSize() {
						Dimension sz = super.getSize();
						if (!layingOut) {
							sz.width = Math.max(sz.width,
									super.getPreferredSize().width);
						}
						return sz;
					}

					@Override
					public Dimension getPreferredSize() {
						return new Dimension(Math.min(
								super.getPreferredSize().width, 300),
								super.getPreferredSize().height);
					}

					private static final long serialVersionUID = -6555670830339032571L;
				};
				net.sf.jailer.ui.AutoCompletion.enable(tableCB);
				DefaultComboBoxModel aModel = new DefaultComboBoxModel();
				aModel.addElement("");
				Table lastTable = relationship.parent == rootRelationship ? subject
						: relationship.parent.association.destination;
				for (Association a : lastTable.associations) {
					aModel.addElement(joinTableRender(lastTable, a));
				}
				tableCB.setModel(aModel);
				if (relationship.association != null) {
					tableCB.setSelectedItem(joinTableRender(lastTable,
							relationship.association));
				}
				final Table ft = lastTable;
				tableCB.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						if (e.getStateChange() == ItemEvent.SELECTED) {
							Association sa = null;
							for (Association a : ft.associations) {
								if (joinTableRender(ft, a).equals(e.getItem())) {
									sa = a;
									break;
								}
							}
							if (sa != null) {
								relationship.association = sa;
								relationship.children.clear();
								relationship.anchorWhereClause = originalAnchorSQL
										.get(relationship.getPathToRoot());
								relationship.whereClause = originalConditionSQL
										.get(relationship.getPathToRoot());
								relationship.anchor = originalAnchor
										.get(relationship.getPathToRoot());
								if (relationship.parent != null
										&& !relationship.parent.children
												.contains(relationship)) {
									relationship.parent.children
											.add(relationship);
								}
							} else {
								if (relationship.parent != null) {
									relationship.parent.children
											.remove(relationship);
								}
							}
							resetRelationshipsPanel();
							updateSQL();
						}
					}
				});
				relationshipsPanel.add(tableCB, gridBagConstraints);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				final JLabel joinLabel = new JLabel("  Join");
				tableCB.addComponentListener(new ComponentListener() {
					@Override
					public void componentShown(ComponentEvent e) {
						joinLabel.setVisible(true);
					}
					@Override
					public void componentResized(ComponentEvent e) {
					}
					@Override
					public void componentMoved(ComponentEvent e) {
					}
					@Override
					public void componentHidden(ComponentEvent e) {
						joinLabel.setVisible(false);
					}
				});
				relationshipsPanel.add(joinLabel, gridBagConstraints);
				
				final JLabel minusLabel = new javax.swing.JLabel();
				minusLabel.setText(null);
				minusLabel.setIcon(minusImage);
				minusLabel.setIconTextGap(0);
				minusLabel.setToolTipText("remove this table from query");

				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 2;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.insets = new Insets(0, 4, 0, 0);

				if (UIUtil.plaf != PLAF.FLAT) {
					minusLabel.setBorder(BorderFactory.createSoftBevelBorder(BevelBorder.RAISED));
				} else {
					minusLabel.setOpaque(true);
				}
				final JComboBox2 combobox = tableCB;
				Color bg = minusLabel.getBackground();
				minusLabel.addMouseListener(new java.awt.event.MouseAdapter() {
					@Override
					public void mouseEntered(java.awt.event.MouseEvent evt) {
						if (UIUtil.plaf == PLAF.FLAT) {
							minusLabel.setBackground(UIUtil.BG_FLATMOUSEOVER);
						} else {
							minusLabel.setBorder(BorderFactory.createSoftBevelBorder(BevelBorder.LOWERED));
						}
					}

					@Override
					public void mouseExited(java.awt.event.MouseEvent evt) {
						if (UIUtil.plaf == PLAF.FLAT) {
							minusLabel.setBackground(bg);
						} else {
							minusLabel.setBorder(BorderFactory.createSoftBevelBorder(BevelBorder.LOWERED));
						}
					}

					@Override
					public void mouseClicked(java.awt.event.MouseEvent evt) {
						combobox.setSelectedItem("");
					}
				});

				if (relationship.association != null) {
					relationshipsPanel.add(minusLabel, gridBagConstraints);
				}
			} else {
				label = new javax.swing.JLabel();
				label.setText(subject.getName());
				relationshipsPanel.add(label, gridBagConstraints);
			}

			if (relationship.association == null
					&& relationship != rootRelationship) {
				final JLabel jlabel = new javax.swing.JLabel();
				jlabel.setText(null);
				jlabel.setIconTextGap(0);
				jlabel.setIcon(joinImage);
				jlabel.setToolTipText("join another table");

				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 4;
				gridBagConstraints.gridy = y - 1;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.insets = new Insets(0, 4, 0, 0);
				relationshipsPanel.add(jlabel, gridBagConstraints);

				final JComponent finalTCB = tableCB;
				finalTCB.setVisible(false);
				if (UIUtil.plaf != PLAF.FLAT) {
					jlabel.setBorder(BorderFactory.createSoftBevelBorder(BevelBorder.RAISED));
				} else {
					jlabel.setOpaque(true);
				}
				Color bg = jlabel.getBackground();
				
				jlabel.addMouseListener(new java.awt.event.MouseAdapter() {
					@Override
					public void mouseEntered(java.awt.event.MouseEvent evt) {
						if (UIUtil.plaf == PLAF.FLAT) {
							jlabel.setBackground(UIUtil.BG_FLATMOUSEOVER);
						} else {
							jlabel.setBorder(BorderFactory.createSoftBevelBorder(BevelBorder.LOWERED));
						}
					}

					@Override
					public void mouseExited(java.awt.event.MouseEvent evt) {
						if (UIUtil.plaf == PLAF.FLAT) {
							jlabel.setBackground(bg);
						} else {
							jlabel.setBorder(BorderFactory.createSoftBevelBorder(BevelBorder.LOWERED));
						}
					}

					@Override
					public void mouseClicked(java.awt.event.MouseEvent evt) {
						finalTCB.setVisible(true);
						jlabel.setVisible(false);
					}
				});
			}

			if (relationship.association != null
					|| relationship == rootRelationship) {
				label = new javax.swing.JLabel();
				label.setText("   as ");
				label.setFont(nonBoldFont);

				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 5;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				relationshipsPanel.add(label, gridBagConstraints);

				String alias = "";
				if (relationship.aliasTextField != null) {
					alias = relationship.aliasTextField.getText();
				}
				JTextField aliasField = new JTextField(alias);
				relationship.aliasTextField = aliasField;
				relationship.originalBGColor = aliasField.getBackground();
				aliasField.getDocument().addDocumentListener(
						new DocumentListener() {
							@Override
							public void changedUpdate(DocumentEvent e) {
								checkAliases();
								updateSQL();
							}

							@Override
							public void insertUpdate(DocumentEvent e) {
								checkAliases();
								updateSQL();
							}

							@Override
							public void removeUpdate(DocumentEvent e) {
								checkAliases();
								updateSQL();
							}
						});
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 6;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				relationshipsPanel.add(aliasField, gridBagConstraints);

				final JCheckBox selectColumnsCB = new JCheckBox(
						"select columns");
				selectColumnsCB.setSelected(relationship.selectColumns);

				selectColumnsCB.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						relationship.selectColumns = selectColumnsCB
								.isSelected();
						updateSQL();
					}
				});

				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 8;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 1.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.insets = new Insets(0, 10, 0, 0);
				relationshipsPanel.add(selectColumnsCB, gridBagConstraints);
			}
			// lastTable = relationship == null? null : relationship.association
			// == null? subject : relationship.association.destination;
		}

		JLabel label = new javax.swing.JLabel();
		label.setText("                ");

		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 6;
		gridBagConstraints.gridy = relationships.size() + 1;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weighty = 1.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		relationshipsPanel.add(label, gridBagConstraints);

		checkAliases();
		updateSQL();
		Rectangle r = getBounds();
		r.width--;
		setBounds(r); // force relayouting
		r.width++;
		setBounds(r);
	}

	/**
	 * Gets render of an association for the join-combobox.
	 * 
	 * @param association
	 *            the association
	 * @return render of association
	 */
	private String joinTableRender(Table from, Association association) {
		int n = 0;
		for (Association a : from.associations) {
			if (a.destination == association.destination) {
				++n;
			}
		}
		return datamodel.getDisplayName(association.destination)
				+ (n > 1 ? " on " + association.getName() : "");
	}

	/**
	 * Checks aliases, renders naming conflicts.
	 */
	private void checkAliases() {
		List<Relationship> relationships = rootRelationship.flatten(0, null,
				false);
		for (Relationship a : relationships) {
			a.aliasTextField.setBackground(a.originalBGColor);
		}
		for (Relationship a : relationships) {
			a.aliasSuggestion = null;
			for (Relationship b : relationships) {
				if (a != b) {
					String ta = a.aliasTextField.getText().trim();
					Table tableA = a.association == null ? subject
							: a.association.destination;
					String tb = b.aliasTextField.getText().trim();
					Table tableB = b.association == null ? subject
							: b.association.destination;
					if (ta.length() == 0) {
						ta = unquote(tableA.getUnqualifiedName());
					}
					if (tb.length() == 0) {
						tb = unquote(tableB.getUnqualifiedName());
					}
					if (ta.equalsIgnoreCase(tb)) {
						String as = null;
						String aName = unquote(tableA.getUnqualifiedName());
						for (int i = 1; i <= aName.length(); ++i) {
							as = aName.substring(0, i);
							if (as.endsWith("_")) {
								continue;
							}
							boolean unique = true;
							for (Relationship c : relationships) {
								Table tableC = c.association == null ? subject
										: c.association.destination;
								if (!unquote(tableC.getUnqualifiedName())
										.equalsIgnoreCase(aName)) {
									if (unquote(tableC.getUnqualifiedName())
											.toLowerCase(Locale.ENGLISH).startsWith(
													as.toLowerCase(Locale.ENGLISH))) {
										unique = false;
										break;
									}
								}
							}
							if (unique) {
								break;
							}
						}
						a.aliasSuggestion = as;

						Color bg = new Color(255, 150, 140);
						a.aliasTextField.setBackground(bg);
						b.aliasTextField.setBackground(bg);
					}
				}
			}
		}
	}

	private String unquote(String name) {
		if (!name.isEmpty()) {
			char fc = name.charAt(0);
			if (!Character.isLetterOrDigit(fc) && fc != '_') {
				String fcStr = Character.toString(fc);
				if (name.startsWith(fcStr) && name.endsWith(fcStr)) {
					name = name.substring(1, name.length() - 1);
				}
			}
		}
		return name;
	}

	private String prevSQL = "";

	/**
	 * Updates the SQL query.
	 */
	private void updateSQL() {
		appendMLM("");
		String currentSql = prevSQL; // createSQL(sqlIsSingleLine,
										// qualifyTableNames);
		String sql = sqlTextArea.getText();
		String suffix = "";
		if (sql.startsWith(currentSql)) {
			suffix = sql.substring(currentSql.length());
		}
		sqlTextArea.setText((prevSQL = createSQL(false)) + suffix);
		appendMLM(mlmTextField.getText());
		sqlTextArea.setCaretPosition(0);
		sqlTextArea.discardAllEdits();
	}

	/**
	 * Gets current SQL statement.
	 * 
	 * @return current SQL statement
	 */
	public String getSQL() {
		appendMLM("");
		String sql = sqlTextArea.getText();
		appendMLM(mlmTextField.getText());
		sqlTextArea.setCaretPosition(0);
		return sql;
	}

	/**
	 * Creates SQL query.
	 */
	private String createSQL(boolean singleLine) {
		StringBuffer sql = new StringBuffer("Select "
				+ (selectDistinct ? "distinct " : ""));
		String lf = System.getProperty("line.separator", "\n");
		String tab = "     ";

		List<Relationship> relationships = rootRelationship.flatten(0, null,
				false);
		boolean needsIndent = false;
		for (Relationship r : relationships) {
			if (r.children.size() > 1) {
				needsIndent = true;
				break;
			}
		}

		if (!singleLine) {
			int sa = 0;
			for (int i = 0; i < relationships.size(); ++i) {
				Relationship r = relationships.get(i);
				if (r.selectColumns) {
					++sa;
				}
			}
			if (sa > 1) {
				sql.append(lf + tab);
			}
		}
		boolean selectAll = true;
		boolean fr = true;
		for (int i = 0; i < relationships.size(); ++i) {
			Relationship r = relationships.get(i);
			Table t = r.association == null ? subject
					: r.association.destination;
			r.alias = r.aliasTextField.getText().trim();
			if (r.alias.equals("")) {
				r.alias = t.getName();
			}
			if (r.selectColumns) {
				selectAll = false;
				if (!fr) {
					sql.append(", ");
					if (!singleLine) {
						sql.append(lf + tab);
					}
				}
				fr = false;
				boolean f = true;
				for (Column c : t.getColumns()) {
					if (!f) {
						sql.append(", ");
					}
					f = false;
					sql.append(r.alias + "." + c.name);
				}
			}
		}
		if (selectAll) {
			sql.append("*");
		}
		if (!singleLine) {
			sql.append(lf);
		} else {
			sql.append(" ");
		}
		sql.append("From ");
		// String lastAlias = "";
		for (int i = 0; i < relationships.size(); ++i) {
			Relationship r = relationships.get(i);
			String indent = "";
			if (needsIndent) {
				for (int l = 0; l < r.level; ++l) {
					indent += "  ";
				}
			}
			Table t = r.association == null ? subject
					: r.association.destination;
			if (r.association != null) {
				sql.append(singleLine ? " " : (lf + tab + indent));
				sql.append(r.joinOperator + " ");
			} else if (relationships.size() > 1) {
				sql.append(singleLine ? "" : (lf + tab + indent));
			}
			sql.append(t.getName());
			String alias = r.aliasTextField.getText().trim();
			if (alias.length() > 0) {
				sql.append(" " + alias);
			} else {
				alias = t.getName();
			}
			String lastAlias = "";
			Relationship parent = r.parent;
			if (parent != null) {
				lastAlias = parent.aliasTextField.getText().trim();
				if (lastAlias.length() <= 0) {
					lastAlias = parent.association == null ? subject.getName()
							: parent.association.destination.getName();
				}
			}
			if (r.association != null) {
				String jc;
				if (!r.association.reversed) {
					jc = SqlUtil.replaceAliases(
							SqlUtil.normalizeRestrictionCondition(r.association.getUnrestrictedJoinCondition()),
							lastAlias, alias);
				} else {
					jc = SqlUtil.replaceAliases(
							r.association.getUnrestrictedJoinCondition(),
							alias, lastAlias);
				}
				if (r.joinOperator == JoinOperator.LeftJoin
						&& r.originalParent == null) {
					if (r.whereClause != null) {
						jc = "("
								+ jc
								+ ") and ("
								+ SqlUtil.replaceAliases(r.whereClause,
										r.alias, lastAlias) + ")";
					}
				}
				sql.append(" on " + jc);
			}
			// lastAlias = alias;
		}

		boolean f = true;
		int lines = 0;
		for (int i = 0; i < relationships.size(); ++i) {
			Relationship r = relationships.get(i);
			if (r.whereClause != null) {
				++lines;
			}
		}

		int numConds = 0;
		
		for (int i = 0; i < relationships.size(); ++i) {
			Relationship r = relationships.get(i);
			boolean appendAnd = true;
			if (r.anchorWhereClause != null && r.anchor != null) {
				boolean anchorExists = false;
				for (Relationship c : r.children) {
					if (c.association == r.anchor) {
						anchorExists = true;
						break;
					}
				}
				if (!anchorExists) {
					appendAnd = false;
					++numConds;
				}
			}
			if (appendAnd
					&& r.whereClause != null
					&& (r.joinOperator == JoinOperator.Join || r.originalParent != null)) {
				++numConds;
			}
		}

		for (int i = 0; i < relationships.size(); ++i) {
			Relationship r = relationships.get(i);
			String lastAlias = "";
			Relationship parent = r.originalParent;
			if (parent == null) {
				parent = r.parent;
			}
			if (parent != null) {
				lastAlias = parent.aliasTextField.getText().trim();
				if (lastAlias.length() <= 0) {
					lastAlias = parent.association == null ? subject.getName()
							: parent.association.destination.getName();
				}
			}
			boolean appendAnd = true;
			if (r.anchorWhereClause != null && r.anchor != null) {
				boolean anchorExists = false;
				for (Relationship c : r.children) {
					if (c.association == r.anchor) {
						anchorExists = true;
						break;
					}
				}
				if (!anchorExists) {
					appendAnd = false;
					if (f) {
						sql.append(singleLine ? " " : lf);
						sql.append("Where");
						sql.append(singleLine || lines == 1 ? " " : (lf + tab));
					} else {
						sql.append(singleLine ? " " : (lf + tab));
						sql.append("and ");
					}
					if (!f || numConds != 1) {
						sql.append("(");
					}
					sql.append(SqlUtil.replaceAliases(r.anchorWhereClause, r.alias, lastAlias));
					if (!f || numConds != 1) {
						sql.append(")");
					}
					f = false;
				}
			}
			if (appendAnd
					&& r.whereClause != null
					&& (r.joinOperator == JoinOperator.Join || r.originalParent != null)) {
				if (f) {
					sql.append(singleLine ? " " : lf);
					sql.append("Where");
					sql.append(singleLine || lines == 1 ? " " : (lf + tab));
				} else {
					sql.append(singleLine ? " " : (lf + tab));
					sql.append("and ");
				}
				if (!f || numConds != 1) {
					sql.append("(");
				}
				sql.append(SqlUtil.replaceAliases(r.whereClause, r.alias, lastAlias));
				if (!f || numConds != 1) {
					sql.append(")");
				}
				f = false;
			}
		}

		if (!singleLine) {
			sql.append(" " + lf);
		} else {
			sql.append(" ");
			return sql.toString().replaceAll(" *(\n|\r)+ *", " ");
		}
		String sqlString = sql.toString().trim();
		sqlString += lf;

		return sqlString;
	}

	/**
	 * The data model.
	 */
	private DataModel datamodel;

	/**
	 * Opens the dialog.
	 * 
	 * @param table
	 *            subject of query
	 * @param usePath
	 *            if <code>true</code>, immediately build query based on
	 *            selected path
	 * @param associationsOnPath
	 *            currently selected associations path
	 */
	public void buildQuery(Table table, boolean usePath,
			boolean showJoinButton, List<Association> associationsOnPath,
			List<String> whereClauses, DataModel datamodel, Session session, MetaDataSource metaDataSource, boolean openSQLConsole) {
		this.associationsOnPath = associationsOnPath;
		if (table == null) {
			return;
		}
		initAutoCompletion(datamodel, session, metaDataSource);
		sqlTextArea.setText("");
		mlmTextField.setText("");
		sqlTextArea.discardAllEdits();
		this.datamodel = datamodel;
		subject = table;
		// relationships.clear();
		Relationship firstR = new Relationship();
		rootRelationship = firstR;
		firstR.selectColumns = true;
		// relationships.add(firstR);
		firstR.whereClause = null;
		if (whereClauses != null && whereClauses.size() > 0) {
			firstR.whereClause = whereClauses.get(0);
		}
		if (usePath && !associationsOnPath.isEmpty()) {
			createPathQuery(whereClauses);
		}

		resetRelationshipsPanel();

		List<JTextField> tf = new ArrayList<JTextField>();
		List<String> as = new ArrayList<String>();
		for (Relationship r : rootRelationship.flatten(0, null, false)) {
			if (r.aliasSuggestion != null) {
				tf.add(r.aliasTextField);
				as.add(r.aliasSuggestion);
			}
		}
		Map<String, Integer> counterPerAlias = new HashMap<String, Integer>();
		for (int i = 0; i < tf.size(); ++i) {
			Integer c = counterPerAlias.get(as.get(i));
			if (c == null) {
				c = 1;
			}
			counterPerAlias.put(as.get(i), c + 1);
			tf.get(i).setText(as.get(i) + c);
		}
		checkAliases();

		setLocation(getParent().getX() + (getParent().getWidth() - getWidth())
				/ 2, getParent().getY()
				+ (getParent().getHeight() - getHeight()) / 2);
		UIUtil.fit(this);
		if (sqlEditButton.isVisible()) {
			sqlEditButton.grabFocus();
		}
		if (openSQLConsole) {
			sqlEditButton.doClick();
		}
		else {
			setVisible(true);
		}
	}

	/**
	 * Opens the dialog.
	 * 
	 * @param table
	 *            subject of query
	 * @param root
	 *            root relation
	 * @param openSQLConsole 
	 */
	public void buildQuery(Table table, Relationship root, DataModel datamodel, Session session, MetaDataSource metaDataSource, boolean openSQLConsole) {
		if (table == null) {
			return;
		}
		initAutoCompletion(datamodel, session, metaDataSource);
		sqlTextArea.setText("");
		mlmTextField.setText("");
		this.datamodel = datamodel;
		subject = table;
		rootRelationship = root;
		resetRelationshipsPanel();

		List<JTextField> tf = new ArrayList<JTextField>();
		List<String> as = new ArrayList<String>();
		boolean distinct = false;
		for (Relationship r : rootRelationship.flatten(0, null, false)) {
			if (r.aliasSuggestion != null) {
				tf.add(r.aliasTextField);
				as.add(r.aliasSuggestion);
			}
			if (r.association != null && r.joinOperator != JoinOperator.Join) {
				distinct = true;
			}
			originalAnchorSQL.put(r.getPathToRoot(), r.anchorWhereClause);
			originalConditionSQL.put(r.getPathToRoot(), r.whereClause);
			originalAnchor.put(r.getPathToRoot(), r.anchor);
		}
		Map<String, Integer> counterPerAlias = new HashMap<String, Integer>();
		for (int i = 0; i < tf.size(); ++i) {
			Integer c = counterPerAlias.get(as.get(i));
			if (c == null) {
				c = 1;
			}
			counterPerAlias.put(as.get(i), c + 1);
			tf.get(i).setText(as.get(i) + c);
		}
		checkAliases();

		setLocation(getParent().getX() + (getParent().getWidth() - getWidth())
				/ 2, getParent().getY()
				+ (getParent().getHeight() - getHeight()) / 2);
		UIUtil.fit(this);
		distinctCheckBox.setSelected(distinct);
		if (sqlEditButton.isVisible()) {
			sqlEditButton.grabFocus();
		}
		if (openSQLConsole) {
			sqlEditButton.doClick();
		}
		else {
			setVisible(true);
		}
	}

	private List<Association> associationsOnPath;

	private void createPathQuery(List<String> whereClauses) {
		// relationships.clear();
		Relationship firstR = new Relationship();
		rootRelationship = firstR;
		// relationships.add(firstR);
		firstR.selectColumns = true;
		if (whereClauses != null && whereClauses.size() > 0) {
			firstR.whereClause = whereClauses.get(0);
		}
		subject = associationsOnPath.get(0).source;
		for (int i = 0; i < associationsOnPath.size(); ++i) {
			Association a = associationsOnPath.get(i);
			Relationship r = new Relationship();
			r.association = a;
			if (whereClauses != null && whereClauses.size() > i + 1) {
				r.whereClause = whereClauses.get(i + 1);
			}
			firstR.children.add(r);
			firstR = r;
		}
		resetRelationshipsPanel();
	}

	private String mlm = "";

	private void appendMLM(String mlm) {
		mlm = mlm.trim();
		if (mlm.length() > 1) {
			mlm = mlm.substring(0, 1);
		}
		if (this.mlm.equals(mlm)) {
			return;
		}
		if (this.mlm.length() > 0) {
			String omlm = this.mlm;
			if ("\\|[]()^-$".indexOf(omlm) >= 0) {
				omlm = "\\" + omlm;
			}
			sqlTextArea.setText(sqlTextArea.getText().replaceAll(
					" " + omlm + "([\n\r])", "$1"));
			sqlTextArea.select(0, 0);
		}
		this.mlm = mlm;
		if (mlm.length() > 0) {
			if ("\\".equals(mlm) || "$".equals(mlm)) {
				mlm = "\\" + mlm;
			}
			sqlTextArea.setText(sqlTextArea.getText().replaceAll(
					"([^\n\r;])([\n\r])", "$1 " + mlm + "$2"));
			sqlTextArea.select(0, 0);
		}
		sqlTextArea.discardAllEdits();
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton clipboardButton;
    private javax.swing.JButton clipboardSingleLineButton;
    private javax.swing.JCheckBox distinctCheckBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTextField mlmTextField;
    private javax.swing.JPanel relationshipsPanel;
    private javax.swing.JButton saveButton;
    public javax.swing.JButton sqlEditButton;
    // End of variables declaration//GEN-END:variables

    private final RSyntaxTextArea sqlTextArea;
    
	private ImageIcon joinImage = null;
	private ImageIcon minusImage = null;
	private ImageIcon sqlConsoleIcon = null;
	private ImageIcon copyIcon = null;
	{
		// load image
		joinImage = UIUtil.scaleIcon(UIUtil.readImage("/collapsed.png"), 20, 16);
		minusImage = UIUtil.scaleIcon(UIUtil.readImage("/minus.png"), 20, 16);
	}

	private ImageIcon cancelIcon;
	{
		// load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        sqlConsoleIcon = UIUtil.readImage("/runall.png");
        copyIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png"));
    }
	private static final long serialVersionUID = -2801831496446636545L;
}
