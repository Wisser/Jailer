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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;

/**
 * "Connect with DB" dialog.
 *
 * @author Wisser
 */
public class DbConnectionDialog extends javax.swing.JDialog {

	boolean isConnected = false;
	private Settings theSettings;
	
	/**
	 * Gets connection to DB.
	 * 
	 * @return <code>true</code> if connection succeeded
	 */
	public boolean connect(String reason) {
		setTitle((reason == null? "" : (reason + " - ")) + "Connect.");
		setVisible(true);
		return isConnected;
	}
	
    /** Creates new form DbConnectionDialog */
    public DbConnectionDialog(java.awt.Frame parent) {
        super(parent, true);
        initComponents();
        setLocation(100, 150);
        Map<String, JTextField> fields = new HashMap<String, JTextField>();
        fields.put("password", password);
        fields.put("dbUrl", dbUrl);
        fields.put("dbUser", user);
        fields.put("driver", driverClass);
        fields.put("jar1", jar1);
        fields.put("jar2", jar2);
        theSettings = new Settings(".connect.ui", fields);
        if (theSettings.isNew) {
        	init(theSettings);
        }
        ComboBoxModel aModel = new DefaultComboBoxModel(theSettings.getSettingNames());
        jComboBox1.setEditable(true);
        jComboBox1.setModel(aModel);
        if (theSettings.currentSetting != null) {
            theSettings.restore(theSettings.currentSetting);
            for (Object item: theSettings.getSettingNames()) {
                if (theSettings.currentSetting.equals(item)) {
                    jComboBox1.setSelectedItem(item);
                }
            }
        }
        jComboBox1.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + jComboBox1.getSelectedItem();
                theSettings.restore(name);
            }
        });
        jComboBox1.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + jComboBox1.getSelectedItem();
                theSettings.restore(name);
            }
        });
        loadJar1.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.choseFile(null, ".", "Jdbc Driver", ".jar", DbConnectionDialog.this, true, true);
                if (fn != null) {
                    jar1.setText(fn);
                }
            }
        });
        loadJar2.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.choseFile(null, ".", "Jdbc Driver", ".jar", DbConnectionDialog.this, true, true);
                if (fn != null) {
                    jar2.setText(fn);
                }
            }
        });
        if (theSettings.isNew) {
        	jComboBox1.setSelectedIndex(0);
        }
        pack();
        setSize((int)(getSize().width * 1.2), getSize().height + 8);
	}
    
    /**
     * Initializes JDBC-settings.
     * 
     * @param theSettings settings to initialize
     */
    private void init(Settings theSettings) {
		try {
			CsvFile drivers = new CsvFile(new File(new File("config"), "driverlist.csv"));
			List<Line> lines = new ArrayList<Line>(drivers.getLines());
			Collections.sort(lines, new Comparator<Line>() {
				public int compare(Line o1, Line o2) {
					return o1.cells.get(0).compareTo(o2.cells.get(0));
				}
			});
			for (Line line: lines) {
				if (line.cells.get(0).length() > 0) {
					dbUrl.setText(line.cells.get(1));
					driverClass.setText(line.cells.get(2));
					theSettings.save(line.cells.get(0));
				}
			}
		} catch (Exception e) {
		}
	}

	/** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc=" Erzeugter Quelltext ">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jComboBox1 = new javax.swing.JComboBox();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jar1 = new javax.swing.JTextField();
        loadJar1 = new javax.swing.JButton();
        jar2 = new javax.swing.JTextField();
        loadJar2 = new javax.swing.JButton();
        driverClass = new javax.swing.JTextField();
        dbUrl = new javax.swing.JTextField();
        user = new javax.swing.JTextField();
        password = new javax.swing.JTextField();
        jPanel2 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jLabel10 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();

        getContentPane().setLayout(new java.awt.CardLayout());

        setTitle("Connect with DB");
        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText(" Settings ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel1, gridBagConstraints);

        jLabel2.setText(" JDBC Driver JAR ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel2, gridBagConstraints);

        jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Eintrag 1", "Eintrag 2", "Eintrag 3", "Eintrag 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jComboBox1, gridBagConstraints);

        jLabel3.setText(" JDBC Driver JAR 2 ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel3, gridBagConstraints);

        jLabel4.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridy = 15;
        jPanel1.add(jLabel4, gridBagConstraints);

        jLabel5.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 45;
        jPanel1.add(jLabel5, gridBagConstraints);

        jLabel6.setText(" Driver-Class");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel6, gridBagConstraints);

        jLabel7.setText(" DB-URL");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel7, gridBagConstraints);

        jLabel8.setText(" User");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 70;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel8, gridBagConstraints);

        jLabel9.setText(" Password");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel9, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jar1, gridBagConstraints);

        loadJar1.setFont(new java.awt.Font("Dialog", 0, 10));
        loadJar1.setText("...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 20;
        jPanel1.add(loadJar1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(jar2, gridBagConstraints);

        loadJar2.setFont(new java.awt.Font("Dialog", 0, 10));
        loadJar2.setText("...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 40;
        jPanel1.add(loadJar2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 50;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(driverClass, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 60;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(dbUrl, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 70;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(user, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 80;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(password, gridBagConstraints);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jButton1.setText("Connect");
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

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jPanel2, gridBagConstraints);

        jLabel10.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        jPanel1.add(jLabel10, gridBagConstraints);

        jLabel11.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 10;
        jPanel1.add(jLabel11, gridBagConstraints);

        getContentPane().add(jPanel1, "card2");

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
    	isConnected = false;
    	String name = "" + jComboBox1.getSelectedItem();
        theSettings.save(name);
        
        String d1 = jar1.getText().trim();
        String d2 = jar2.getText().trim();
        if (d1.length() == 0) {
            d1 = null;
        }
        if (d2.length() == 0) {
            d2 = null;
        }
        try {
            StatementExecutor.setClassLoaderForJdbcDriver(addJarToClasspath(d1, d2));
        } catch (Exception e) {
            UIUtil.showException(this, "Error loading driver jars", e);
            return;
        }
        
        try {
	        if (StatementExecutor.classLoaderForJdbcDriver != null) {
	            Driver d = (Driver)Class.forName(driverClass.getText(), true, StatementExecutor.classLoaderForJdbcDriver).newInstance();
	            DriverManager.registerDriver(new StatementExecutor.DriverShim(d));
	        } else {
	            Class.forName(driverClass.getText());
	        }
	        Connection con = DriverManager.getConnection(dbUrl.getText(), user.getText(), password.getText());
	        con.close();
	        isConnected = true;
	        setVisible(false);
        } catch (Exception e) {
            UIUtil.showException(this, "Cannot connect with DB", e);
            return;
        }
        
    }//GEN-LAST:event_jButton1ActionPerformed
    
    /**
     * Adds one or two jars to classpath.
     * 
     * @param jarName1 filename of jar 1
     * @param jarName2 filename of jar 2
     */
    private URLClassLoader addJarToClasspath(String jarName1, String jarName2) throws Exception {
        URL[] urls;
        if (jarName1 == null) {
            if (jarName2 == null) {
                return null;
            }
            jarName1 = jarName2;
            jarName2 = null;
        }
        System.out.println("add '" + jarName1 + "' to classpath");
        if (jarName2 == null) {
            urls = new URL[] {new URL("file", null, jarName1)};
        } else {
            System.out.println("add '" + jarName2 + "' to classpath");
            urls = new URL[] {new URL("file", null, jarName1), new URL("file", null, jarName2)};
        }
        URLClassLoader urlLoader = new URLClassLoader(urls);
        return urlLoader;
    }

    /**
     * Selects the DB-schema to for introspection.
     * 
     * @return the DB-schema to for introspection
     */
	public String selectDBSchema(Component parent) throws Exception {
		StatementExecutor statementExecutor = new StatementExecutor(driverClass.getText(), dbUrl.getText(), user.getText(), password.getText());
		List<String> schemas = JDBCMetaDataBasedModelElementFinder.getSchemas(statementExecutor, user.getText());
		statementExecutor.shutDown();
		if (schemas.size() == 1) {
			return schemas.get(0);
		}
		if (schemas.isEmpty()) {
			return null;
		}
		for (String s: schemas) {
			if (s.equalsIgnoreCase(user.getText().trim())) {
				return s;
			}
		}
		String s = (String) JOptionPane.showInputDialog(parent, "Select schema to introspect", "Schema", JOptionPane.QUESTION_MESSAGE, null, schemas.toArray(), schemas.get(0));
		if (s == null) {
			return "";
		}
		return s;
	}
    
    /**
     * Adds jailer cli-arguments for DB connection.
     * 
     * @param args the arg-list to add arguments to
     */
	public void addDbArgs(List<String> args) {
		args.add(driverClass.getText());
		args.add(dbUrl.getText());
		args.add(user.getText());
		args.add(password.getText());
	}
    
    // Variablendeklaration - nicht modifizieren//GEN-BEGIN:variables
    public javax.swing.JTextField dbUrl;
    public javax.swing.JTextField driverClass;
    private javax.swing.JButton jButton1;
    private javax.swing.JComboBox jComboBox1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
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
    private javax.swing.JTextField jar1;
    private javax.swing.JTextField jar2;
    private javax.swing.JButton loadJar1;
    private javax.swing.JButton loadJar2;
    public javax.swing.JTextField password;
    public javax.swing.JTextField user;
    // Ende der Variablendeklaration//GEN-END:variables

}
