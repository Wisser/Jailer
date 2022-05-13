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
package net.sf.jailer.ui;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.scrollmenu.JScrollMenu;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.util.SqlUtil;

/**
 * Editor for multi-line SQL conditions with parameter support.
 * 
 * @author Ralf Wisser
 */
public class ConditionEditor extends EscapableDialog {

	private boolean ok;
	private ParameterSelector parameterSelector;
	private DataModelBasedSQLCompletionProvider provider;

	/** Creates new form ConditionEditor 
	 */
	public ConditionEditor(JComponent anchor, java.awt.Frame parent, ParameterSelector.ParametersGetter parametersGetter, DataModel dataModel, String altTitel) {
		super(parent, true);
		initComponents();
		this.editorPane = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			@Override
			protected void runBlock() {
				super.runBlock();
				okButtonActionPerformed(null);
			}
		};

		addOnPanel.setVisible(false);

		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
		scalarSQIconToggleButton.setIcon(dropDownIcon);
		scalarSQIconToggleButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				JPopupMenu popupMenu = ConditionEditor.createJoinPopupMenu(table1alias, table1, editorPane);
				UIUtil.fit(popupMenu);
				popupMenu.show(scalarSQIconToggleButton, 0, scalarSQIconToggleButton.getHeight());
				popupMenu.addPropertyChangeListener("visible", new PropertyChangeListener() {
					@Override
					public void propertyChange(PropertyChangeEvent evt) {
						if (Boolean.FALSE.equals(evt.getNewValue())) {
							scalarSQIconToggleButton.setSelected(false);
						}
					}
				});
			}
		});

		JScrollPane jScrollPane2 = new JScrollPane();
		jScrollPane2.setViewportView(editorPane);
		
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 10;
		gridBagConstraints.gridy = 9;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 0;
		JLabel where = new JLabel(" " + (altTitel != null? altTitel : "Where"));
		where.setForeground(new Color(0, 0, 255));
		jPanel1.add(where, gridBagConstraints);
		
		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 10;
		gridBagConstraints.gridy = 10;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		jPanel1.add(jScrollPane2, gridBagConstraints);
		jScrollPane2.setViewportView(editorPane);
		
		if (dataModel != null) {
			provider = new DataModelBasedSQLCompletionProvider(null, dataModel);
			provider.setDefaultClause(SQLCompletionProvider.Clause.WHERE);
			new SQLAutoCompletion(provider, editorPane);
		}

		if (anchor != null) {
			Point p = new Point(0, 0);
			SwingUtilities.convertPointToScreen(p, anchor);
			setLocation((int) p.getX(), (int) p.getY());
		} else {
			setLocation(400, 150);
		}
		setSize(660, 400);
		UIUtil.fit(this);
		
		if (parametersGetter != null) {
			paramsPanel.add(parameterSelector = new ParameterSelector(this, editorPane, parametersGetter));
		} else {
			paramsPanel.setVisible(false);
		}
	}
	
	/**
	 * Opens a drop-down box which allows the user to select columns for restriction definitions.
	 */
	private void openColumnDropDownBox(JLabel label, String alias, Table table) {
		JPopupMenu popup = new JScrollPopupMenu();
		List<String> columns = new ArrayList<String>();
		
		for (Column c: table.getColumns()) {
			columns.add(alias + "." + c.name);
		}
		if (addPseudoColumns) {
			columns.add("");
			columns.add(alias + ".$IS_SUBJECT");
			columns.add(alias + ".$DISTANCE");
			columns.add("$IN_DELETE_MODE");
			columns.add("NOT $IN_DELETE_MODE");
		}
		
		for (final String c: columns) {
			if (c.equals("")) {
				popup.add(new JSeparator());
				continue;
			}
			JMenuItem m = new JMenuItem(c);
			m.addActionListener(new ActionListener () {
				@Override
				public void actionPerformed(ActionEvent e) {
					if (editorPane.isEnabled()) {
						if (editorPane.isEditable()) {
							editorPane.replaceSelection(c);
						}
					}
				}
			});
			popup.add(m);
		}
		UIUtil.fit(popup);
		popup.show(label, 0, label.getHeight());
	}
	
	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        paramsPanel = new javax.swing.JPanel();
        addOnPanel = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        toSubQueryButton = new javax.swing.JButton();
        scalarSQIconToggleButton = new javax.swing.JToggleButton();
        jLabel2 = new javax.swing.JLabel();
        jSeparator1 = new javax.swing.JSeparator();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        paramsPanel.setMinimumSize(new java.awt.Dimension(150, 0));
        paramsPanel.setLayout(new javax.swing.BoxLayout(paramsPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(paramsPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(addOnPanel, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel3.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        jPanel3.add(cancelButton, gridBagConstraints);

        toSubQueryButton.setText("Convert to Subquery");
        toSubQueryButton.setToolTipText("<html>Converts condition into a subquery.<br> This allows to add joins with related tables or limiting clauses etc. </html>");
        toSubQueryButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                toSubQueryButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 24);
        jPanel3.add(toSubQueryButton, gridBagConstraints);

        scalarSQIconToggleButton.setText("Scalar Subquery...");
        scalarSQIconToggleButton.setToolTipText("Inserts a scalar query for a column of a neighboring table.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel3.add(scalarSQIconToggleButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 2);
        jPanel1.add(jPanel3, gridBagConstraints);

        jLabel2.setForeground(new java.awt.Color(128, 128, 128));
        jLabel2.setText(" ctrl-space for code completion");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jLabel2, gridBagConstraints);

        jSeparator1.setOrientation(javax.swing.SwingConstants.VERTICAL);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 19;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(jSeparator1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		ok = true;
		setVisible(false);
	}//GEN-LAST:event_okButtonActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		ok = false;
		setVisible(false);
	}//GEN-LAST:event_cancelButtonActionPerformed

    private void toSubQueryButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_toSubQueryButtonActionPerformed
        if (table1alias != null && table1 != null) {
        	String condition = editorPane.getText();
        	String subAlias = table1alias + "_SUB";
    		
        	if ("T".equalsIgnoreCase(table1alias)) {
        		condition = SqlUtil.replaceAlias(condition, subAlias);
        	} else if ("A".equalsIgnoreCase(table1alias)) {
        		condition = SqlUtil.replaceAliases(condition, subAlias, "B");
        	} else {
        		return;
        	}
        	StringBuilder prefix = new StringBuilder();
        	StringBuilder suffix = new StringBuilder();
        	StringBuilder pkCond = new StringBuilder();
        	
        	for (Column pk: table1.primaryKey.getColumns()) {
        		if (pkCond.length() > 0) {
        			pkCond.append(" and ");
        		}
        		pkCond.append(subAlias + "." + pk.name + "=" + table1alias + "." + pk.name);
        	}
        	
        	if (table1.primaryKey.getColumns().size() == 1) {
        		prefix.append(table1alias + "." + table1.primaryKey.getColumns().get(0).name + " in (\n    Select " + subAlias + "." + table1.primaryKey.getColumns().get(0).name + " From " + table1.getName() + " " + subAlias + " \n    Where\n        ");
        		suffix.append("\n)");
        	} else {
        		prefix.append("exists(\n    Select * From " + table1.getName() + " " + subAlias + " \n    Where (\n        ");
        		suffix.append("\n        ) and " + pkCond + ")");
        	}
        	editorPane.beginAtomicEdit();
        	editorPane.setText(prefix + condition + suffix);
        	editorPane.setCaretPosition(prefix.length() + condition.length());
        	editorPane.endAtomicEdit();
        	editorPane.grabFocus();
        	toSubQueryButton.setEnabled(false);
        }
    }//GEN-LAST:event_toSubQueryButtonActionPerformed

	private Table table1, table2;
	private String table1alias, table2alias;
	private boolean addPseudoColumns;
	
	/**
	 * Edits a given condition.
	 * 
	 * @param condition the condition
	 * @return new condition or <code>null</code>, if user canceled the editor
	 */
	public String edit(JComponent anchor, String condition, String table1label, String table1alias, Table table1, String table2label, String table2alias, Table table2, boolean addPseudoColumns, boolean addConvertSubqueryButton) {
		if (Pattern.compile("\\bselect\\b", Pattern.CASE_INSENSITIVE|Pattern.DOTALL).matcher(condition).find()) {
			condition = new BasicFormatterImpl().format(condition);
		}
		
		if (table2 != null || table1 == null) {
			scalarSQIconToggleButton.setVisible(false);
		} else {
			scalarSQIconToggleButton.setVisible(true);
		}
		
		scalarSQIconToggleButton.setEnabled(table1 != null && !table1.associations.isEmpty());

		this.table1 = table1;
		this.table2 = table2;
		this.table1alias = table1alias;
		this.table2alias = table2alias;
		this.addPseudoColumns = addPseudoColumns;
		toSubQueryButton.setVisible(addConvertSubqueryButton);
		toSubQueryButton.setEnabled(true);
		if (table1 != null && (table1.primaryKey == null || table1.primaryKey.getColumns() == null|| table1.primaryKey.getColumns().isEmpty())) {
			toSubQueryButton.setEnabled(false);
		}
		if (Pattern.compile("(exists|in)\\s*\\(\\s*select", Pattern.CASE_INSENSITIVE|Pattern.DOTALL).matcher(condition).find()) {
			toSubQueryButton.setEnabled(false);
		}
		ok = false;
		editorPane.setText(condition);
		editorPane.setCaretPosition(0);
		editorPane.discardAllEdits();

		if (parameterSelector != null) {
			parameterSelector.updateParameters();
		}
		if (provider != null) {
			provider.removeAliases();
			if (table1 != null) {
				provider.addAlias(table1alias, table1);
			}
			if (table2 != null) {
				provider.addAlias(table2alias, table2);
			}
		}
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				editorPane.grabFocus();
			}
		});
		if (anchor != null) {
			Point p = new Point(0, 0);
			SwingUtilities.convertPointToScreen(p, anchor);
			setLocation((int) p.getX(), (int) p.getY());
			setSize(660, 400);
			UIUtil.fit(this);
		}
		setVisible(true);
		if (ok && condition.equals(editorPane.getText())) {
			ok = false;
		}
		return ok? removeSingleLineComments(editorPane.getText()).replaceFirst("(?is)^\\s*where\\b\\s*", "").replaceAll("\\n(\\r?) *", " ").replaceAll(";\\s*$", "").replace('\n', ' ').replace('\r', ' ') : null;
	}

	/**
	 * Removes single line comments.
	 * 
	 * @param statement
	 *            the statement
	 * 
	 * @return statement the statement without comments and literals
	 */
	private String removeSingleLineComments(String statement) {
		Pattern pattern = Pattern.compile("('(?:[^']*'))|(/\\*.*?\\*/)|(\\-\\-.*?(?=\n|$))", Pattern.DOTALL);
		Matcher matcher = pattern.matcher(statement);
		boolean result = matcher.find();
		StringBuffer sb = new StringBuffer();
		if (result) {
			do {
				if (matcher.group(3) == null) {
					matcher.appendReplacement(sb, "$0");
					result = matcher.find();
					continue;
				}
				int l = matcher.group(0).length();
				matcher.appendReplacement(sb, "");
				if (matcher.group(1) != null) {
					l -= 2;
					sb.append("'");
				}
				while (l > 0) {
					--l;
					sb.append(' ');
				}
				if (matcher.group(1) != null) {
					sb.append("'");
				}
				result = matcher.find();
			} while (result);
		}
		matcher.appendTail(sb);
		return sb.toString();
	}

	public void setLocationAndFit(Point pos) {
		setLocation(pos);
//		UIUtil.fit(this);
        try {
            // Get the size of the screen
            Rectangle2D dim = UIUtil.getScreenBounds();
            int hd = (int) (getY() - (dim.getHeight() - 80));
            if (hd > 0) {
                setLocation(getX(), Math.max(getY() - hd, (int) dim.getY()));
            }
        } catch (Throwable t) {
            // ignore
        }
	}

	public static JPopupMenu createJoinPopupMenu(final String alias, Table table, final RSyntaxTextAreaWithSQLSyntaxStyle editor) {
		JPopupMenu popupMenu = new JPopupMenu();
		ImageIcon redDotIconScaled = UIUtil.scaleIcon(editor, redDotIcon);
		ImageIcon blueDotIconScaled = UIUtil.scaleIcon(editor, blueDotIcon);
		ImageIcon greenDotIconScaled = UIUtil.scaleIcon(editor, greenDotIcon);
		
		Map<Table, Integer> destCount = new HashMap<Table, Integer>();
		for (Association a: table.associations) {
			if (!destCount.containsKey(a.destination)) {
				destCount.put(a.destination, 1);
			} else {
				destCount.put(a.destination, destCount.get(a.destination) + 1);
			}
		}
		Map<String, Association> namedAssocs = new TreeMap<String, Association>();
		for (Association a: table.associations) {
			Integer cnt = destCount.get(a.destination);
			namedAssocs.put(a.getDataModel().getDisplayName(a.destination) + (cnt != null && cnt > 1? (" (" + a.getName() + ")") : ""), a);
		}
		for (final Entry<String, Association> e: namedAssocs.entrySet()) {
			ImageIcon icon;
			if (e.getValue().isInsertDestinationBeforeSource()) {
				icon = redDotIconScaled;
			} else if (e.getValue().isInsertSourceBeforeDestination()) {
				icon = greenDotIconScaled;
			} else {
				icon = blueDotIconScaled;
			}
			JMenu menu = new JScrollMenu(e.getKey());
			menu.setIcon(icon);
			popupMenu.add(menu);
			List<Column> cols = new ArrayList<Column>(e.getValue().destination.getColumns());
			Collections.sort(cols, new Comparator<Column>() {
				@Override
				public int compare(Column o1, Column o2) {
					return o1.name.compareToIgnoreCase(o2.name);
				}
			});
			for (final Column col: cols) {
				JMenuItem item = new JMenuItem(col.name);
				menu.add(item);
				item.addActionListener(new ActionListener() {
					@Override
					public void actionPerformed(ActionEvent ev) {
						String condition;
						if (!e.getValue().reversed) {
							condition = SqlUtil.replaceAliases(e.getValue().getUnrestrictedJoinCondition(), alias, e.getValue().destination.getUnqualifiedName());
						} else {
							condition = SqlUtil.replaceAliases(e.getValue().getUnrestrictedJoinCondition(), e.getValue().destination.getUnqualifiedName(), alias);
						}
						editor.replaceSelection(
								"(Select " + col.name + " from " + e.getValue().destination.getName() + 
								" Where " + condition + ")");
					}
				});
			}
		}
		return popupMenu;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JPanel addOnPanel;
    private javax.swing.JButton cancelButton;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel paramsPanel;
    private javax.swing.JToggleButton scalarSQIconToggleButton;
    private javax.swing.JButton toSubQueryButton;
    // End of variables declaration//GEN-END:variables
	
	private static Icon dropDownIcon;
	private static ImageIcon redDotIcon;
	private static ImageIcon blueDotIcon;
	private static ImageIcon greenDotIcon;
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
		dropDownIcon = UIUtil.readImage("/dropdown.png");
		redDotIcon = UIUtil.readImage("/reddot.gif");
		blueDotIcon = UIUtil.readImage("/bluedot.gif");
		greenDotIcon = UIUtil.readImage("/greendot.gif");
	}
	
	public final RSyntaxTextAreaWithSQLSyntaxStyle editorPane;
	
	private static final long serialVersionUID = -5169934807182707970L;

}
