/*
 * Copyright 2007 the original author or authors.
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import com.sun.jndi.url.iiopname.iiopnameURLContextFactory;

import net.sf.jailer.Configuration;
import net.sf.jailer.DDLCreator;
import net.sf.jailer.ScriptFormat;
import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.database.TemporaryTableScope;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * "Data Export" Dialog.
 *
 * @author Ralf Wisser
 */
public class ExportDialog extends javax.swing.JDialog {

	/**
	 * true iff ok-button was clicked.
	 */
	boolean isOk = false;
	
	/**
	 * Xml/Sql switch.
	 */
	private ScriptFormat scriptFormat;
	
	/**
	 * Restricted data model.
	 */
	private final DataModel dataModel;
	
	/**
	 * Previous subject condition.
	 */
	private static String previousSubjectCondition;

	/**
	 * Previous initial subject condition.
	 */
	private static String previousInitialSubjectCondition;
	
	/**
	 * Display name for default schema.
	 */
	private static String DEFAULT_SCHEMA = "<default>";
	
	/**
	 * Schema mapping fields.
	 */
	private Map<String, JTextField> schemaMappingFields = new HashMap<String, JTextField>();
	
	/**
	 * The form field setting.
	 */
	private Settings theSettings;
	
    /** Creates new form DbConnectionDialog */
    public ExportDialog(java.awt.Frame parent, DataModel dataModel, net.sf.jailer.datamodel.Table subject, String subjectCondition, StatementExecutor statementExecutor) {
        super(parent, true);
        this.dataModel = dataModel;
        initComponents();
        setModal(true);
        setLocation(100, 150);
        Map<String, JTextField> fields = new HashMap<String, JTextField>();
        fields.put("insert", insert);
        fields.put("delete", delete);
        fields.put("threads", threads);
        fields.put("rowsPerThread", rowsPerThread);
        
        scriptFormat = ScriptFormat.SQL;
        try {
        	scriptFormat = ScriptFormat.valueOf(dataModel.getExportModus());
        } catch (Exception e) {
		}
        
        upsertCheckbox.setEnabled(ScriptFormat.SQL.equals(scriptFormat));
        rowsPerThread.setEnabled(ScriptFormat.SQL.equals(scriptFormat));

    	Map<JTextField, String> defaults = new HashMap<JTextField, String>();

    	if (!ScriptFormat.SQL.equals(scriptFormat)) {
        	schemaMappingPanel.setVisible(false);
        } else {
        	schemaMappingPanel.setVisible(true);
        	initSchemaMapping(dataModel, fields, defaults);
        }
        
        theSettings = new Settings(".exportdata.ui", fields);
        selectInsert.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.choseFile(null, ".", scriptFormat.getFileChooserTitle(), scriptFormat.getFileExtension(), ExportDialog.this, true, false);
                if (fn != null) {
                    insert.setText(fn);
                }
            }
        });
        selectDelete.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.choseFile(null, ".", "SQL Delete Script", ".sql", ExportDialog.this, true, true);
                if (fn != null) {
                    delete.setText(fn);
                }
            }
        });
        theSettings.restore("default");
    	for (JTextField field: defaults.keySet()) {
    		if (field.getText().length() == 0) {
    			field.setText(defaults.get(field));
    		}
    	}
        if (threads.getText().length() == 0) {
        	threads.setText("1");
        }
        if (rowsPerThread.getText().length() == 0) {
        	rowsPerThread.setText("50");
        }
        
        subjectTable.setText(subject.getName());
        if (subjectCondition.equals(previousInitialSubjectCondition)) {
        	where.setText(previousSubjectCondition);
        } else {
        	where.setText(subjectCondition);
        }
        
        initScopeButtons(statementExecutor);

        pack();
        UIUtil.initPeer();
        setVisible(true);
        try {
			if (initScopeButtonThread != null) {
				initScopeButtonThread.join();
			}
		} catch (InterruptedException e1) {
		}
		initScopeButtonThread = null;
        if (isOk) {
        	previousInitialSubjectCondition = subjectCondition;
        	previousSubjectCondition = where.getText();
        }
	}

    private Thread initScopeButtonThread;
    
    private void initScopeButtons(final StatementExecutor statementExecutor) {
    	synchronized (this) {
	    	scopeGlobal.setSelected(true);
	    	scopeSession.setSelected(false);
	    	scopeSession.setEnabled(false);
	    	scopeTransaction.setSelected(false);
	    	scopeTransaction.setEnabled(false);
	    	jButton1.setEnabled(false);
		}
    	
    	initScopeButtonThread = new Thread(new Runnable() {
			public void run() {
		    	Configuration configuration = Configuration.forDbms(statementExecutor);
		    	if (configuration.sessionTemporaryTableManager != null) {
		    		SwingUtilities.invokeLater(new Runnable() {
						public void run() {
				    		synchronized (ExportDialog.this) {
				    			scopeSession.setEnabled(true);
				    		}
						}
		    		});
					try {
						statementExecutor.reconnect();
						DDLCreator.createDDL(statementExecutor, TemporaryTableScope.SESSION_LOCAL);
			    		SwingUtilities.invokeLater(new Runnable() {
							public void run() {
					    		synchronized (ExportDialog.this) {
							    	scopeGlobal.setSelected(false);
						    		scopeSession.setSelected(true);
					    		}
							}
			    		});
					} catch (Exception e) {
					}
		    	}
		    	if (configuration.transactionTemporaryTableManager != null) {
		    		SwingUtilities.invokeLater(new Runnable() {
						public void run() {
				    		synchronized (ExportDialog.this) {
				    			scopeTransaction.setEnabled(true);
				    		}
						}
		    		});
					try {
						statementExecutor.reconnect();
						DDLCreator.createDDL(statementExecutor, TemporaryTableScope.TRANSACTION_LOCAL);
			    		SwingUtilities.invokeLater(new Runnable() {
							public void run() {
					    		synchronized (ExportDialog.this) {
									scopeGlobal.setSelected(false);
									scopeSession.setSelected(false);
									scopeTransaction.setSelected(true);
					    		}
							}
			    		});
					} catch (Exception e) {
					}
		    	}
	    		SwingUtilities.invokeLater(new Runnable() {
					public void run() {
			    		synchronized (ExportDialog.this) {
			    			jButton1.setEnabled(true);
			    		}
					}
	    		});
			}
    	});
    	initScopeButtonThread.start();
	}

	/**
     * Initializes the schema mapping panel.
     * 
     * @param dataModel the data model
     * @param fields to put newly created text fields into
     * @param defaults to put default values for newly created text fields into
     */
    private void initSchemaMapping(DataModel dataModel, Map<String, JTextField> fields, Map<JTextField, String> defaults) {
    	Set<String> distinctSchemas = new HashSet<String>();
    	
    	for (Table table: dataModel.getTables()) {
    		String schema = table.getSchema(DEFAULT_SCHEMA);
    		distinctSchemas.add(schema);
    	}
    	
    	List<String> sortedSchemaList = new ArrayList<String>(distinctSchemas);
    	Collections.sort(sortedSchemaList);
    	
    	int y = 0;
    	for (String schema: sortedSchemaList) {
    		JLabel a = new JLabel(schema);
    		a.setFont(new java.awt.Font("Dialog", 0, 12));
            java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 1;
            gridBagConstraints.gridy = y;
    		schemaMappingPanel.add(a, gridBagConstraints);
    		JLabel b = new JLabel(" into schema ");
            gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 2;
            gridBagConstraints.gridy = y;
    		schemaMappingPanel.add(b, gridBagConstraints);
    		JTextField c = new JTextField(schema);
            fields.put("schema-" + schema, c);
            defaults.put(c, schema);
            schemaMappingFields.put(schema, c);
    		gridBagConstraints = new java.awt.GridBagConstraints();
            gridBagConstraints.gridx = 3;
            gridBagConstraints.gridy = y;
            gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
            schemaMappingPanel.add(c, gridBagConstraints);
    		y++;
    	}
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        jPanel1 = new javax.swing.JPanel();
        schemaMappingPanel = new javax.swing.JPanel();
        jLabel13 = new javax.swing.JLabel();
        jLabel14 = new javax.swing.JLabel();
        jLabel15 = new javax.swing.JLabel();
        where = new javax.swing.JTextField();
        exportLabel = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        insert = new javax.swing.JTextField();
        selectInsert = new javax.swing.JButton();
        delete = new javax.swing.JTextField();
        selectDelete = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        threads = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        rowsPerThread = new javax.swing.JTextField();
        jLabel17 = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        upsertCheckbox = new javax.swing.JCheckBox();
        explain = new javax.swing.JCheckBox();
        jLabel1 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        subjectTable = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        scopeGlobal = new javax.swing.JRadioButton();
        jLabel16 = new javax.swing.JLabel();
        scopeSession = new javax.swing.JRadioButton();
        scopeTransaction = new javax.swing.JRadioButton();
        jLabel9 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Data Export");
        getContentPane().setLayout(new java.awt.CardLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        schemaMappingPanel.setLayout(new java.awt.GridBagLayout());

        jLabel13.setText(" Import rows from schema ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        schemaMappingPanel.add(jLabel13, gridBagConstraints);

        jLabel14.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel14.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        schemaMappingPanel.add(jLabel14, gridBagConstraints);

        jLabel15.setText("                          ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 200;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        schemaMappingPanel.add(jLabel15, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(schemaMappingPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanel1.add(where, gridBagConstraints);

        exportLabel.setText(" Into*");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(exportLabel, gridBagConstraints);

        jLabel3.setText(" Generate delete-script* ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel3, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 45;
        jPanel1.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" Threads ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(6, 0, 0, 0);
        jPanel1.add(jLabel6, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel1.add(insert, gridBagConstraints);

        selectInsert.setFont(new java.awt.Font("Dialog", 0, 10));
        selectInsert.setText("...");
        selectInsert.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                selectInsertActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        jPanel1.add(selectInsert, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel1.add(delete, gridBagConstraints);

        selectDelete.setFont(new java.awt.Font("Dialog", 0, 10));
        selectDelete.setText("...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 40;
        jPanel1.add(selectDelete, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        threads.setMinimumSize(new java.awt.Dimension(44, 19));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(threads, gridBagConstraints);

        jLabel10.setText("  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jLabel10, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel3.add(rowsPerThread, gridBagConstraints);

        jLabel17.setText("           ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        jPanel3.add(jLabel17, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jPanel3, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jButton1.setText("Export Data");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        jPanel2.add(jButton1, gridBagConstraints);

        jLabel2.setFont(new java.awt.Font("Dialog", 0, 12));
        jLabel2.setText(" * '.zip' extension for compressed files");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel2.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        upsertCheckbox.setText("generate Upsert-statements (overwrite) for all rows");
        upsertCheckbox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 44;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        jPanel1.add(upsertCheckbox, gridBagConstraints);

        explain.setText("explain");
        explain.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 45;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 4, 0);
        jPanel1.add(explain, gridBagConstraints);

        jLabel1.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jLabel1, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        jPanel1.add(jLabel4, gridBagConstraints);

        jLabel8.setText(" Working table scope");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel8, gridBagConstraints);

        jLabel7.setText(" Export from");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jLabel7, gridBagConstraints);

        jLabel11.setText(" Where");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanel1.add(jLabel11, gridBagConstraints);

        jPanel4.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 0, 0));

        subjectTable.setFont(new java.awt.Font("Dialog", 0, 12));
        subjectTable.setText("jLabel11");
        jPanel4.add(subjectTable);

        jLabel12.setText("  as T");
        jPanel4.add(jLabel12);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jPanel4, gridBagConstraints);

        buttonGroup1.add(scopeGlobal);
        scopeGlobal.setText("Global");
        scopeGlobal.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                scopeGlobalActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel1.add(scopeGlobal, gridBagConstraints);

        jLabel16.setText(" Rows per statement");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 51;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(8, 0, 0, 0);
        jPanel1.add(jLabel16, gridBagConstraints);

        buttonGroup1.add(scopeSession);
        scopeSession.setText("Session");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 56;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel1.add(scopeSession, gridBagConstraints);

        buttonGroup1.add(scopeTransaction);
        scopeTransaction.setText("Transaction");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 57;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel1.add(scopeTransaction, gridBagConstraints);

        jLabel9.setText("           ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 58;
        jPanel1.add(jLabel9, gridBagConstraints);

        getContentPane().add(jPanel1, "card2");

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void selectInsertActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectInsertActionPerformed
    }//GEN-LAST:event_selectInsertActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        for (JTextField f: schemaMappingFields.values()) {
        	if (f.getText().trim().length() == 0) {
        		f.setText(DEFAULT_SCHEMA);
        	}
        }
    	theSettings.save("default");
        
        if (insert.getText().trim().length() == 0) {
        	exportLabel.setForeground(Color.RED);
        } else {
        	isOk = true;
        	setVisible(false);
        }
    }//GEN-LAST:event_jButton1ActionPerformed

    private void scopeGlobalActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_scopeGlobalActionPerformed
    }//GEN-LAST:event_scopeGlobalActionPerformed
    
    public boolean isOk() {
		return isOk;
	}
    
    /**
     * Fills field content into cli-args.
     * 
     * @param args the argument-list to fill
     */
    public void fillCLIArgs(List<String> args) {
    	
    	args.add("-e");
    	args.add(insert.getText());
    	if (delete.getText().trim().length() > 0) {
    		args.add("-d");
    		args.add(delete.getText().trim());
    	}
    	if (explain.isSelected()) {
    		args.add("-explain");
    	}
    	if (upsertCheckbox.isSelected()) {
    		args.add("-upsert-only");
    	}
    	try {
    		int nt = Integer.parseInt(threads.getText().trim());
    		if (nt > 0 && nt <= 100) {
    			args.add("-threads");
    			args.add("" + nt);
    		}
    	} catch (Exception e) {
    	}
    	try {
    		int nt = Integer.parseInt(rowsPerThread.getText().trim());
    		if (nt > 0) {
    			args.add("-entities");
    			args.add("" + nt);
    		}
    	} catch (Exception e) {
    	}
    	
    	args.add("-where");
    	args.add(where.getText());

    	args.add("-format");
    	args.add(scriptFormat.toString());
    	if (ScriptFormat.XML.equals(scriptFormat)) {
    		args.add("-xml");
    		args.add("-xml-root");
    		args.add(dataModel.getXmlSettings().rootTag);
    		args.add("-xml-date");
    		args.add(dataModel.getXmlSettings().datePattern);
    		args.add("-xml-timestamp");
    		args.add(dataModel.getXmlSettings().timestampPattern);
    	}
    	
    	StringBuilder schemaMapping = new StringBuilder();
    	for (String schema: schemaMappingFields.keySet()) {
    		String to = schemaMappingFields.get(schema).getText().trim();
    		if (to.equals(DEFAULT_SCHEMA)) {
    			to = "";
    		}
    		if (schemaMapping.length() > 0) {
    			schemaMapping.append(",");
    		}
    		schemaMapping.append((schema.equals(DEFAULT_SCHEMA)? "" : schema) + "=" + to);
    	}
		if (schemaMapping.length() > 0) {
			args.add("-schemamapping");
			args.add(schemaMapping.toString());
		}
    }
    

    public TemporaryTableScope getTemporaryTableScope() {
    	if (scopeSession.isSelected()) {
    		return TemporaryTableScope.SESSION_LOCAL;
    	}
    	if (scopeTransaction.isSelected()) {
    		return TemporaryTableScope.TRANSACTION_LOCAL;
    	}
    	return TemporaryTableScope.GLOBAL;
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JTextField delete;
    public javax.swing.JCheckBox explain;
    private javax.swing.JLabel exportLabel;
    private javax.swing.JTextField insert;
    private javax.swing.JButton jButton1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JTextField rowsPerThread;
    private javax.swing.JPanel schemaMappingPanel;
    private javax.swing.JRadioButton scopeGlobal;
    private javax.swing.JRadioButton scopeSession;
    private javax.swing.JRadioButton scopeTransaction;
    private javax.swing.JButton selectDelete;
    private javax.swing.JButton selectInsert;
    private javax.swing.JLabel subjectTable;
    private javax.swing.JTextField threads;
    private javax.swing.JCheckBox upsertCheckbox;
    private javax.swing.JTextField where;
    // End of variables declaration//GEN-END:variables
    
}
