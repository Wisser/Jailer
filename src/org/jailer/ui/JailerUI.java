package org.jailer.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.jailer.Jailer;

public class JailerUI extends javax.swing.JFrame {

    private JPanel jContentPane = null;
    private JTextField jdbcDriver = null;
    private JLabel jLabel = null;
    private JLabel jLabel1 = null;
    private JLabel jLabel2 = null;
    private JLabel jLabel3 = null;
    private JLabel jLabel31 = null;
    private JLabel jLabel32 = null;
    private JLabel jLabel33 = null;
    private JTextField dbURL = null;
    private JTextField dbUser = null;
    private JTextField dbPassword = null;
    private JTextField extractionModel = null;
    private JTextField insertScript = null;
    private JTextField deleteScript = null;
    private JButton jButton = null;
    private JLabel jLabel4 = null;
    private JComboBox settings = null;
    private JButton OK = null;
    private JLabel jLabel5 = null;
    private JLabel jLabel51 = null;
    
    private Settings theSettings;
    
    /**
     * This method initializes 
     * 
     */
    public JailerUI() {
    	super();
    	initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setSize(new Dimension(491, 267));
        this.setContentPane(getJContentPane());
        this.setTitle("Jailer " + Jailer.VERSION);
        getSettings().setEditable(true);
        Map<String, JTextField> fields = new HashMap<String, JTextField>();
        fields.put("password", getDbPassword());
        fields.put("dbUrl", getDbURL());
        fields.put("dbUser", getDbUser());
        fields.put("driver", getJdbcDriver());
        fields.put("insert", getInsertScript());
        fields.put("delete", getDeleteScript());
        fields.put("extranctionModel", getExtractionModel());
        theSettings = new Settings(".export.ui", fields);
        ComboBoxModel aModel = new DefaultComboBoxModel(theSettings.getSettingNames());
        getSettings().setModel(aModel);
        if (theSettings.currentSetting != null) {
            theSettings.restore(theSettings.currentSetting);
            for (Object item: theSettings.getSettingNames()) {
                if (theSettings.currentSetting.equals(item)) {
                    getSettings().setSelectedItem(item);
                }
            }
        }
        getSettings().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + getSettings().getSelectedItem();
                theSettings.restore(name);
            }
        });
        getJButton().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + getSettings().getSelectedItem();
                theSettings.save(name);
                ComboBoxModel aModel = new DefaultComboBoxModel(theSettings.getSettingNames());
                getSettings().setModel(aModel);
                getSettings().setSelectedItem(name);
            }
        });
        getOK().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = "" + getSettings().getSelectedItem();
                theSettings.save(name);
            }
        });
        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }
    
    /**
     * This method initializes jContentPane	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getJContentPane() {
        if (jContentPane == null) {
            GridBagConstraints gridBagConstraints21 = new GridBagConstraints();
            gridBagConstraints21.gridx = 1;
            gridBagConstraints21.gridy = 1;
            jLabel51 = new JLabel();
            jLabel51.setText("   ");
            GridBagConstraints gridBagConstraints20 = new GridBagConstraints();
            gridBagConstraints20.gridx = 3;
            gridBagConstraints20.gridy = 5;
            jLabel5 = new JLabel();
            jLabel5.setText("   ");
            GridBagConstraints gridBagConstraints19 = new GridBagConstraints();
            gridBagConstraints19.gridx = 2;
            gridBagConstraints19.fill = GridBagConstraints.HORIZONTAL;
            gridBagConstraints19.gridy = 10;
            gridBagConstraints19.insets = new Insets(10, 10, 0, 0);
            GridBagConstraints gridBagConstraints18 = new GridBagConstraints();
            gridBagConstraints18.fill = GridBagConstraints.BOTH;
            gridBagConstraints18.gridy = 0;
            gridBagConstraints18.weightx = 1.0;
            gridBagConstraints18.gridx = 1;
            GridBagConstraints gridBagConstraints17 = new GridBagConstraints();
            gridBagConstraints17.gridx = 0;
            gridBagConstraints17.anchor = GridBagConstraints.WEST;
            gridBagConstraints17.fill = GridBagConstraints.HORIZONTAL;
            gridBagConstraints17.gridy = 0;
            gridBagConstraints17.insets = new Insets(0, 10, 0, 0);
            jLabel4 = new JLabel();
            jLabel4.setText("Setting");
            GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
            gridBagConstraints13.gridx = 2;
            gridBagConstraints13.anchor = GridBagConstraints.WEST;
            gridBagConstraints13.gridy = 0;
            gridBagConstraints13.insets = new Insets(0, 10, 0, 0);
            GridBagConstraints gridBagConstraints12 = new GridBagConstraints();
            gridBagConstraints12.fill = GridBagConstraints.BOTH;
            gridBagConstraints12.gridy = 9;
            gridBagConstraints12.weightx = 1.0;
            gridBagConstraints12.gridx = 1;
            gridBagConstraints12.gridwidth = 2;
            GridBagConstraints gridBagConstraints111 = new GridBagConstraints();
            gridBagConstraints111.fill = GridBagConstraints.BOTH;
            gridBagConstraints111.gridy = 8;
            gridBagConstraints111.weightx = 1.0;
            gridBagConstraints111.gridx = 1;
            gridBagConstraints111.gridwidth = 2;
            GridBagConstraints gridBagConstraints10 = new GridBagConstraints();
            gridBagConstraints10.fill = GridBagConstraints.BOTH;
            gridBagConstraints10.gridy = 7;
            gridBagConstraints10.weightx = 1.0;
            gridBagConstraints10.gridx = 1;
            gridBagConstraints10.gridwidth = 2;
            GridBagConstraints gridBagConstraints9 = new GridBagConstraints();
            gridBagConstraints9.fill = GridBagConstraints.BOTH;
            gridBagConstraints9.gridy = 5;
            gridBagConstraints9.weightx = 1.0;
            gridBagConstraints9.gridx = 1;
            gridBagConstraints9.gridwidth = 2;
            GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
            gridBagConstraints8.fill = GridBagConstraints.BOTH;
            gridBagConstraints8.gridy = 4;
            gridBagConstraints8.weightx = 1.0;
            gridBagConstraints8.gridx = 1;
            gridBagConstraints8.gridwidth = 2;
            GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
            gridBagConstraints7.fill = GridBagConstraints.BOTH;
            gridBagConstraints7.gridy = 3;
            gridBagConstraints7.weightx = 1.0;
            gridBagConstraints7.gridx = 1;
            gridBagConstraints7.gridwidth = 2;
            GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
            gridBagConstraints6.gridx = 0;
            gridBagConstraints6.anchor = GridBagConstraints.WEST;
            gridBagConstraints6.ipadx = 8;
            gridBagConstraints6.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints6.gridy = 9;
            jLabel33 = new JLabel();
            jLabel33.setText("delete-script");
            GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
            gridBagConstraints5.gridx = 0;
            gridBagConstraints5.anchor = GridBagConstraints.WEST;
            gridBagConstraints5.ipadx = 8;
            gridBagConstraints5.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints5.gridy = 8;
            jLabel32 = new JLabel();
            jLabel32.setText("insert-script");
            GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
            gridBagConstraints4.gridx = 0;
            gridBagConstraints4.anchor = GridBagConstraints.WEST;
            gridBagConstraints4.ipadx = 8;
            gridBagConstraints4.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints4.gridy = 7;
            jLabel31 = new JLabel();
            jLabel31.setText("extraction model");
            GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
            gridBagConstraints3.gridx = 0;
            gridBagConstraints3.anchor = GridBagConstraints.WEST;
            gridBagConstraints3.ipadx = 8;
            gridBagConstraints3.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints3.gridy = 5;
            jLabel3 = new JLabel();
            jLabel3.setText("DB Password");
            GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
            gridBagConstraints2.gridx = 0;
            gridBagConstraints2.anchor = GridBagConstraints.WEST;
            gridBagConstraints2.ipadx = 8;
            gridBagConstraints2.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints2.gridy = 4;
            jLabel2 = new JLabel();
            jLabel2.setText("DB User");
            GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
            gridBagConstraints11.gridx = 0;
            gridBagConstraints11.anchor = GridBagConstraints.WEST;
            gridBagConstraints11.ipadx = 8;
            gridBagConstraints11.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints11.gridy = 3;
            jLabel1 = new JLabel();
            jLabel1.setText("DB URL");
            GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
            gridBagConstraints1.gridx = 0;
            gridBagConstraints1.anchor = GridBagConstraints.WEST;
            gridBagConstraints1.ipadx = 8;
            gridBagConstraints1.insets = new Insets(0, 10, 0, 0);
            gridBagConstraints1.gridy = 2;
            jLabel = new JLabel();
            jLabel.setText("JDBC Driver");
            GridBagConstraints gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.fill = GridBagConstraints.BOTH;
            gridBagConstraints.gridy = 2;
            gridBagConstraints.weightx = 1.0;
            gridBagConstraints.anchor = GridBagConstraints.WEST;
            gridBagConstraints.gridx = 1;
            gridBagConstraints.gridwidth = 2;
            jContentPane = new JPanel();
            jContentPane.setLayout(new GridBagLayout());
            jContentPane.add(getJdbcDriver(), gridBagConstraints);
            jContentPane.add(jLabel, gridBagConstraints1);
            jContentPane.add(jLabel1, gridBagConstraints11);
            jContentPane.add(jLabel2, gridBagConstraints2);
            jContentPane.add(jLabel3, gridBagConstraints3);
            jContentPane.add(jLabel31, gridBagConstraints4);
            jContentPane.add(jLabel32, gridBagConstraints5);
            jContentPane.add(jLabel33, gridBagConstraints6);
            jContentPane.add(getDbURL(), gridBagConstraints7);
            jContentPane.add(getDbUser(), gridBagConstraints8);
            jContentPane.add(getDbPassword(), gridBagConstraints9);
            jContentPane.add(getExtractionModel(), gridBagConstraints10);
            jContentPane.add(getInsertScript(), gridBagConstraints111);
            jContentPane.add(getDeleteScript(), gridBagConstraints12);
            jContentPane.add(getJButton(), gridBagConstraints13);
            jContentPane.add(jLabel4, gridBagConstraints17);
            jContentPane.add(getSettings(), gridBagConstraints18);
            jContentPane.add(getOK(), gridBagConstraints19);
            jContentPane.add(jLabel5, gridBagConstraints20);
            jContentPane.add(jLabel51, gridBagConstraints21);
        }
        return jContentPane;
    }

    /**
     * This method initializes jdbcDriver	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getJdbcDriver() {
        if (jdbcDriver == null) {
            jdbcDriver = new JTextField();
        }
        return jdbcDriver;
    }

    /**
     * This method initializes dbURL	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getDbURL() {
        if (dbURL == null) {
            dbURL = new JTextField();
        }
        return dbURL;
    }

    /**
     * This method initializes dbUser	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getDbUser() {
        if (dbUser == null) {
            dbUser = new JTextField();
        }
        return dbUser;
    }

    /**
     * This method initializes dbPassword	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getDbPassword() {
        if (dbPassword == null) {
            dbPassword = new JTextField();
        }
        return dbPassword;
    }

    /**
     * This method initializes extractionModel	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getExtractionModel() {
        if (extractionModel == null) {
            extractionModel = new JTextField();
        }
        return extractionModel;
    }

    /**
     * This method initializes insertScript	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getInsertScript() {
        if (insertScript == null) {
            insertScript = new JTextField();
        }
        return insertScript;
    }

    /**
     * This method initializes deleteScript	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getDeleteScript() {
        if (deleteScript == null) {
            deleteScript = new JTextField();
        }
        return deleteScript;
    }

    /**
     * This method initializes jButton	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getJButton() {
        if (jButton == null) {
            jButton = new JButton();
            jButton.setText("Save Settings");
        }
        return jButton;
    }

    /**
     * This method initializes settings	
     * 	
     * @return javax.swing.JComboBox	
     */
    private JComboBox getSettings() {
        if (settings == null) {
            settings = new JComboBox();
        }
        return settings;
    }

    /**
     * This method initializes OK	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getOK() {
        if (OK == null) {
            OK = new JButton();
            OK.setText("Export");
        }
        return OK;
    }

    public static void main(String args[]) throws FileNotFoundException, IOException {
        JailerUI jailerUI = new JailerUI();
        jailerUI.setVisible(true);
    }

}  //  @jve:decl-index=0:visual-constraint="10,10"
