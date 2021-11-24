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

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.scrollmenu.JScrollPopupMenu;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.syntaxtextarea.DataModelBasedSQLCompletionProvider;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.SQLAutoCompletion;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;
import net.sf.jailer.ui.util.SizeGrip;
import net.sf.jailer.util.SqlUtil;

/**
 * Editor for multi-line SQL conditions with parameter support.
 * 
 * @author Ralf Wisser
 */
public abstract class NonModalConditionEditor extends EscapableDialog {

	private boolean ok;
	private boolean escaped;
	private ParameterSelector parameterSelector;
	private DataModelBasedSQLCompletionProvider provider;

	/** Creates new form ConditionEditor 
	 * @param withPseudoColumns */
	public NonModalConditionEditor(java.awt.Frame parent, ParameterSelector.ParametersGetter parametersGetter, boolean withPseudoColumns, DataModel dataModel) {
		super(parent, false);
		init(parametersGetter, dataModel, withPseudoColumns);
	}

	/** Creates new form ConditionEditor */
	public NonModalConditionEditor(Dialog parent, ParameterSelector.ParametersGetter parametersGetter, DataModel dataModel) {
		super(parent, false);
		init(parametersGetter, dataModel, false);
	}

	@SuppressWarnings("serial")
	private void init(ParameterSelector.ParametersGetter parametersGetter, DataModel dataModel, final boolean withPseudoColumns) {
		setUndecorated(true);
		initComponents();

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

		addWindowFocusListener(new WindowFocusListener() {
			@Override
			public void windowLostFocus(WindowEvent e) {
				if (parameterSelector != null && !parameterSelector.hasOpenDialog()) {
					ok = !escaped;
					setVisible(false);
				}
			}
			@Override
			public void windowGainedFocus(WindowEvent e) {
			}
		});
		
		addComponentListener(new ComponentListener() {
			@Override
			public void componentShown(ComponentEvent e) {
			}
			@Override
			public void componentResized(ComponentEvent e) {
			}
			@Override
			public void componentMoved(ComponentEvent e) {
			}
			@Override
			public void componentHidden(ComponentEvent e) {
				if (ok && initialCondition.equals(editorPane.getText())) {
					ok = false;
				}
				consume(ok? removeSingleLineComments(editorPane.getText()).replaceAll("\\n(\\r?) *", " ").replace('\n', ' ').replace('\r', ' ').replaceFirst("(?is)^\\s*where\\b\\s*", "") : null);
			}
		});

		this.editorPane = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			@Override
			protected void runBlock() {
				super.runBlock();
				okButtonActionPerformed(null);
			}
			@Override
			protected boolean withFindAndReplace() {
				return false;
			}
		};
		JScrollPane jScrollPane2 = new JScrollPane();
		jScrollPane2.setViewportView(editorPane);
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
		clearButton.setIcon(UIUtil.scaleIcon(clearButton, clearIcon));
		editorPane.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				clearButton.setEnabled(!editorPane.getText().isEmpty());
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				clearButton.setEnabled(!editorPane.getText().isEmpty());
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				clearButton.setEnabled(!editorPane.getText().isEmpty());
			}
		});
		clearButton.addActionListener(e -> {
			editorPane.setText("");
			ok = true;
			setVisible(false);
		});
		
		JPanel corner = new SizeGrip();
		gripPanel.add(corner);

		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 10;
		gridBagConstraints.gridy = 10;
		gridBagConstraints.gridheight = 10;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		jPanel1.add(jScrollPane2, gridBagConstraints);
		jScrollPane2.setViewportView(editorPane);
		
		if (dataModel != null) {
			provider = new DataModelBasedSQLCompletionProvider(null, dataModel) {
				@Override
				protected List<String> getColumns(Table table, long timeOut, JComponent waitCursorSubject) {
					List<String> columns = super.getColumns(table, timeOut, waitCursorSubject);
					if (withPseudoColumns) {
						columns.add("$DISTANCE");
						columns.add("$IS_SUBJECT");
					}
					return columns;
				}
			};
			provider.setDefaultClause(SQLCompletionProvider.Clause.WHERE);
			sqlAutoCompletion = new SQLAutoCompletion(provider, editorPane);
		}
		
		setLocation(400, 150);
		setSize(600, 200);
		
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
        jPanel2 = new javax.swing.JPanel();
        jPanel3 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        scalarSQIconToggleButton = new javax.swing.JToggleButton();
        toSubQueryButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        jPanel4 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        gripPanel = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        paramsPanel.setMinimumSize(new java.awt.Dimension(150, 0));
        paramsPanel.setLayout(new javax.swing.BoxLayout(paramsPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(paramsPanel, gridBagConstraints);

        addOnPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 20;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 4);
        jPanel1.add(addOnPanel, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(jPanel2, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        jPanel3.add(okButton, gridBagConstraints);

        scalarSQIconToggleButton.setText("Scalar Subquery...");
        scalarSQIconToggleButton.setToolTipText("Inserts a scalar query for a column of a neighboring table.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        jPanel3.add(scalarSQIconToggleButton, gridBagConstraints);

        toSubQueryButton.setText("to Subquery");
        toSubQueryButton.setToolTipText("<html>Converts condition into a subquery.<br> This allows to add joins with related tables or limiting clauses etc. </html>");
        toSubQueryButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                toSubQueryButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        jPanel3.add(toSubQueryButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
        jPanel3.add(cancelButton, gridBagConstraints);

        clearButton.setText("Clear");
        clearButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clearButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
        jPanel3.add(clearButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel3, gridBagConstraints);

        jPanel4.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jPanel4.setLayout(new java.awt.GridBagLayout());

        jLabel2.setText("<html>  <i>Ctrl+Space</i> for code completion. <i>Ctrl+Enter</i> for Ok. <i>Esc</i> for Cancel.</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel4.add(jLabel2, gridBagConstraints);

        gripPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        jPanel4.add(gripPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jPanel4, gridBagConstraints);

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
		escaped = true;
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

    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearButtonActionPerformed
    }//GEN-LAST:event_clearButtonActionPerformed

	private Table table1, table2;
	private String table1alias, table2alias;
	private boolean addPseudoColumns;
	private String initialCondition;
	
	/**
	 * Edits a given condition.
	 * @param locator 
	 * 
	 * @param condition the condition
	 * @return new condition or <code>null</code>, if user canceled the editor
	 */
	public void edit(JComponent locator, String condition, String table1label, String table1alias, Table table1, String table2label, String table2alias, Table table2, boolean addPseudoColumns, boolean addConvertSubqueryButton) {
		if (condition.length() > 60 || Pattern.compile("\\bselect\\b", Pattern.CASE_INSENSITIVE|Pattern.DOTALL).matcher(condition).find()) {
			condition = new BasicFormatterImpl().format(condition);
		}

		if (table2 != null || table1 == null) {
			scalarSQIconToggleButton.setVisible(false);
		} else {
			scalarSQIconToggleButton.setVisible(true);
			scalarSQIconToggleButton.setIcon(dropDownIcon);
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
		escaped = false;
		editorPane.setText(condition);
//		editorPane.setCaretPosition(0);
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
		initialCondition = condition;
		
		if (locator != null) {
			Point locationLocation = locator.getLocationOnScreen();
        	Point location;
        	location = new Point(locationLocation.x, locationLocation.y);
        	setLocationAndFit(location);
		}
		
		setVisible(true);
	}

	@Override
	protected void escapePressed() {
		escaped = true;
		super.escapePressed();
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
		setLocation(new Point(pos.x + 1, pos.y + 1));
		UIUtil.fit(this);
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

    // Variables declaration - do not modify//GEN-BEGIN:variables
    public javax.swing.JPanel addOnPanel;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton clearButton;
    private javax.swing.JPanel gripPanel;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel paramsPanel;
    private javax.swing.JToggleButton scalarSQIconToggleButton;
    private javax.swing.JButton toSubQueryButton;
    // End of variables declaration//GEN-END:variables
	
	private Icon dropDownIcon;
	{
		// load images
		dropDownIcon = UIUtil.readImage("/dropdown.png");
	}

	public RSyntaxTextAreaWithSQLSyntaxStyle editorPane;
	private SQLAutoCompletion sqlAutoCompletion;

	public void observe(final JTextField textfield, final Consumer<String> open) {
		textfield.addMouseListener(new MouseAdapter() {
			final long MAX_PTIME_DIFF = 250;
			Long pTime;
			@Override
			public void mousePressed(MouseEvent e) {
				pTime = System.currentTimeMillis();
			}
			@Override
			public void mouseReleased(MouseEvent e) {
				if (pTime != null && System.currentTimeMillis() - pTime <= MAX_PTIME_DIFF && e.getX() < textfield.getWidth() - 10) {
					UIUtil.invokeLater(1, () -> openOnTextfield(textfield, open, false));
				}
			}
		});

		@SuppressWarnings("serial")
		Action a = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openOnTextfield(textfield, open, true);
			}

		};
		
		KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, InputEvent.CTRL_DOWN_MASK);
		InputMap im = textfield.getInputMap();
		im.put(ks, a);
		ActionMap am = textfield.getActionMap();
		am.put(a, a);
	}

	private void openOnTextfield(final JTextField textfield, final Consumer<String> open, boolean doCompletion) {
		String origText = textfield.getText();
		int origPos = textfield.getCaretPosition();
		String caretMarker;
		for (int suffix = 0; ; suffix++) {
			caretMarker = "CARET" + suffix;
			if (!origText.contains(caretMarker)) {
				break;
			}
		}
		try {
			open.accept(origText.substring(0, origPos) + caretMarker + origText.substring(origPos));
		} catch (Exception e) {
			e.printStackTrace();
		}
		String text = editorPane.getText();
		int i = text.indexOf(caretMarker);
		if (i >= 0) {
			editorPane.setText(text.substring(0, i) + text.substring(i + caretMarker.length()));
			editorPane.setCaretPosition(i);
		}
		if (doCompletion) {
			UIUtil.invokeLater(1, new Runnable() {
				@Override
				public void run() {
					sqlAutoCompletion.doCompletion();
				}
			});
		}
	}

	protected abstract void consume(String cond);
	
	private static final long serialVersionUID = -5169934807182707970L;

	private static ImageIcon clearIcon;
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        clearIcon = UIUtil.readImage("/clear.png");
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

}
