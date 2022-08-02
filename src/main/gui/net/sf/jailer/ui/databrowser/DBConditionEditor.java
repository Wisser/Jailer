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
package net.sf.jailer.ui.databrowser;

import java.awt.GridBagConstraints;
import java.awt.Point;
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
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.UIUtil;
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
public abstract class DBConditionEditor extends EscapableDialog {

	private boolean ok;
	private boolean escaped;
	private DataModelBasedSQLCompletionProvider provider;
	private DocumentListener clearButtonDocListener;

	/** Creates new form ConditionEditor */
	public DBConditionEditor(java.awt.Frame parent, DataModel dataModel) {
		super(parent, false);
		setUndecorated(true);
		initComponents();
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
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
				ok = !escaped;
				setVisible(false);
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
				consume(ok? UIUtil.toSingleLineSQL(editorPane.getText().replaceFirst("(?is)^\\s*where\\b\\s*", "")) : null);
				if (clearButtonDocListener != null) {
					editorPane.getDocument().removeDocumentListener(clearButtonDocListener);
					clearButtonDocListener = null;
				}
				if (getEditorPanesCache() != null) {
					getEditorPanesCache().add(editorPane);
				}
			}
		});
		
		editorPaneScrollPane = new JScrollPane();

		clearButton.setIcon(UIUtil.scaleIcon(clearButton, clearIcon));
		clearButton.addActionListener(e -> {
			editorPane.setText("");
			ok = true;
			setVisible(false);
		});
		

		JPanel corner = new SizeGrip();
		gripPanel.add(corner);

		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridwidth = 100;
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		jPanel1.add(editorPaneScrollPane, gridBagConstraints);
		
		if (getEditorPanesCache() != null && getEditorPanesCache().isEmpty()) {
			LinkedList<RSyntaxTextArea> editors = new LinkedList<RSyntaxTextArea>();
			while (editors.size() < 2) {
				createEditorPane(editorPaneScrollPane, dataModel);
				editors.add(editorPane);
			}
			getEditorPanesCache().addAll(editors);
		}
		
		setLocation(400, 150);
		setSize(400, 140);
	}

	@Override
	protected void escapePressed() {
		escaped = true;
		super.escapePressed();
	}

	private JScrollPane editorPaneScrollPane;

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel4 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        toSubQueryButton = new javax.swing.JButton();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        clearButton = new javax.swing.JButton();
        scalarSQIconToggleButton = new javax.swing.JToggleButton();
        jPanel3 = new javax.swing.JPanel();
        gripPanel = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jPanel1.setBorder(new javax.swing.border.LineBorder(java.awt.Color.lightGray, 1, true));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

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
        gridBagConstraints.gridheight = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(toSubQueryButton, gridBagConstraints);

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 16);
        jPanel2.add(cancelButton, gridBagConstraints);

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
        jPanel2.add(clearButton, gridBagConstraints);

        scalarSQIconToggleButton.setText("Scalar Subquery...");
        scalarSQIconToggleButton.setToolTipText("Inserts a scalar query for a column of a neighboring table.");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel2.add(scalarSQIconToggleButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 100;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(jPanel2, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        gripPanel.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 100;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
        jPanel3.add(gripPanel, gridBagConstraints);

        jLabel2.setText("<html>  <i>Ctrl+Space</i> for code completion. <i>Ctrl+Enter</i> for Ok. <i>Esc</i> for Cancel.</html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jPanel3, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(jPanel1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel4, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		ok = true;
		setVisible(false);
	}//GEN-LAST:event_okButtonActionPerformed

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

    private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
        escaped = true;
        setVisible(false);
    }//GEN-LAST:event_cancelButtonActionPerformed

    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearButtonActionPerformed
    }//GEN-LAST:event_clearButtonActionPerformed

	private Table table1, table2;
	private String table1alias, table2alias;
	private boolean addPseudoColumns;
	private String initialCondition;
	
	/**
	 * Edits a given condition.
	 * 
	 * @param condition the condition
	 * @return new condition or <code>null</code>, if user canceled the editor
	 */
	public void edit(String condition, String table1label, String table1alias, Table table1, String table2label, String table2alias, Table table2, boolean addPseudoColumns, boolean addConvertSubqueryButton, DataModel dataModel) {
		if (!isVisible() && condition.length() > 60 || Pattern.compile("(\\bselect\\b)|(^\\s*\\()", Pattern.CASE_INSENSITIVE|Pattern.DOTALL).matcher(condition).find()) {
			condition = new BasicFormatterImpl().format(condition);
		}
		this.table1 = table1;
		this.table2 = table2;
		this.table1alias = table1alias;
		this.table2alias = table2alias;
		this.addPseudoColumns = addPseudoColumns;

		if (table2 != null || table1 == null) {
			scalarSQIconToggleButton.setVisible(false);
		} else {
			scalarSQIconToggleButton.setVisible(true);
			scalarSQIconToggleButton.setIcon(dropDownIcon);
		}

		scalarSQIconToggleButton.setEnabled(table1 != null && !table1.associations.isEmpty());

		createEditorPane(editorPaneScrollPane, dataModel);

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
		editorPane.setCaretPosition(0);
		editorPane.discardAllEdits();

		editorPane.setAnimateBracketMatching(false);
		
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
		setVisible(true);
	}

	public void setLocationAndFit(Point pos, int maxXW) {
		setLocation(new Point(pos.x + 1, pos.y + 2));
		UIUtil.fit(this);
        try {
            // Get the size of the screen
            Rectangle2D dim = UIUtil.getScreenBounds();
            int hd = (int) (getY() - (dim.getHeight() - 80));
            if (hd > 0) {
                setLocation(getX(), Math.max(getY() - hd, (int) dim.getY()));
            }
            int maxX = maxXW - getWidth();
    		setLocation(Math.max((int) dim.getX(), Math.min(maxX, getX())), getY());
        } catch (Throwable t) {
            // ignore
        }
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton clearButton;
    private javax.swing.JPanel gripPanel;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JButton okButton;
    private javax.swing.JToggleButton scalarSQIconToggleButton;
    private javax.swing.JButton toSubQueryButton;
    // End of variables declaration//GEN-END:variables

    public RSyntaxTextArea editorPane;
    protected abstract List<RSyntaxTextArea> getEditorPanesCache();
	
    private SQLAutoCompletion sqlAutoCompletion;

	static class RSyntaxTextArea extends RSyntaxTextAreaWithSQLSyntaxStyle {
		DBConditionEditor conditionEditor;
		DataModelBasedSQLCompletionProvider provider;
		SQLAutoCompletion sqlAutoCompletion;
		public RSyntaxTextArea() {
			super(false, false);
		}
		@Override
		protected void runBlock() {
			super.runBlock();
			conditionEditor.okButtonActionPerformed(null);
		}
		@Override
		protected boolean withFindAndReplace() {
			return false;
		}
	};

	private void createEditorPane(JScrollPane scrollPane, DataModel dataModel) {
		RSyntaxTextArea editor;
		if (getEditorPanesCache() == null || getEditorPanesCache().isEmpty()) {
			editor = new RSyntaxTextArea();
			if (dataModel != null) {
				provider = new DataModelBasedSQLCompletionProvider(null, dataModel);
				provider.setDefaultClause(SQLCompletionProvider.Clause.WHERE);
				sqlAutoCompletion = new SQLAutoCompletion(provider, editor);
			}
		} else {
			editor = getEditorPanesCache().remove(0);
			provider = editor.provider;
			sqlAutoCompletion = editor.sqlAutoCompletion;
		}
		editor.conditionEditor = this;
		editor.provider = provider;
		editor.sqlAutoCompletion = sqlAutoCompletion;
		editorPane = editor;
		scrollPane.setViewportView(editorPane);
		clearButtonDocListener = new DocumentListener() {
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
		};
		editorPane.getDocument().addDocumentListener(clearButtonDocListener);
	}
	
	public static void initialObserve(final JTextField textfield, Consumer<String> open, Runnable openAndDoCompletion) {
		textfield.addMouseListener(new MouseAdapter() {
			final long MAX_PTIME_DIFF = 250;
			Long pTime;
			boolean inProgress = false;
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON1) {
					pTime = System.currentTimeMillis();
				}
			}
			@Override
			public void mouseReleased(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON1) {
					if (pTime != null && System.currentTimeMillis() - pTime <= MAX_PTIME_DIFF && e.getX() < textfield.getWidth() - 10) {
						openSQLEditor(textfield, open);
					}
				}
			}
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getButton() == MouseEvent.BUTTON1) {
					openSQLEditor(textfield, open);
				}
			}
			private void openSQLEditor(final JTextField textfield, Consumer<String> open) {
				if (!inProgress && textfield.isEnabled()) {
					inProgress = true;
					UIUtil.invokeLater(1, () -> {
						try {
							open.accept(textfield.getText());
						} finally {
							inProgress = false;
						}
					});
				}
			}
		});
		
		InputMap im = textfield.getInputMap();
		@SuppressWarnings("serial")
		Action a = new AbstractAction() {
			boolean done = false;
			@Override
			public void actionPerformed(ActionEvent e) {
				if (!done && textfield.isEnabled()) {
					done = true;
					openAndDoCompletion.run();
				}
			}
		};
		KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, InputEvent.CTRL_DOWN_MASK);
		im.put(ks, a);
		ActionMap am = textfield.getActionMap();
		am.put(a, a);
	}

	public void observe(final JTextField textfield, final Consumer<String> open) {
		InputMap im = textfield.getInputMap();
		@SuppressWarnings("serial")
		Action a = new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				doCompletion(textfield, open, true);
			}
		};
		KeyStroke ks = KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, InputEvent.CTRL_DOWN_MASK);
		im.put(ks, a);
		ActionMap am = textfield.getActionMap();
		am.put(a, a);
	}
	
	public void doCompletion(final JTextField textfield, final Consumer<String> open, boolean withCompletion) {
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
		if (withCompletion) {
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

	private Icon dropDownIcon;
	{
		// load images
		dropDownIcon = UIUtil.readImage("/dropdown.png");
	}
	
	private static ImageIcon clearIcon;
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	    clearIcon = UIUtil.readImage("/clear.png");
	}

}
