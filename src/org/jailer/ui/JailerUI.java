package org.jailer.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowStateListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.URL;
import java.net.URLClassLoader;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import org.jailer.Jailer;
import org.jailer.database.StatementExecutor;

public class JailerUI extends JDialog {

    private JPanel jPanel = null;
    private JLabel jLabel = null;
    private JLabel jLabel1 = null;
    private JTextField driverJar = null;
    private JTextField secDriverJar = null;
    private JButton selectDriverJar = null;
    private JButton selectSecDriverJar = null;
    private JButton OK = null;
    private JPanel jPanel1 = null;
    private JLabel jLabel2 = null;
    private JLabel jLabel21 = null;
    private JTextArea jTextArea = null;
    private JLabel jLabel22 = null;
    private JLabel jLabel23 = null;
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
        this.setSize(new Dimension(510, 243));
        this.setContentPane(getJPanel());
        this.setTitle("Jailer " + Jailer.VERSION);
        setLocation(200, 200);
        getSelectDriverJar().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.chooseFile(".", "Jdbc Driver", ".jar", JailerUI.this);
                if (fn != null) {
                    getDriverJar().setText(fn);
                }
            }
        });
        getSelectSecDriverJar().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String fn = UIUtil.chooseFile(".", "Jdbc Driver", ".jar", JailerUI.this);
                if (fn != null) {
                    getSecDriverJar().setText(fn);
                }
            }
        });
        getOK().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                store();
                setVisible(false);
            }
        });
        restore();
        setModal(true);
        setVisible(true);
        String d1 = getDriverJar().getText().trim();
        String d2 = getSecDriverJar().getText().trim();
        if (d1.length() == 0) {
            d1 = null;
        }
        if (d2.length() == 0) {
            d2 = null;
        }
        try {
            StatementExecutor.setClassLoaderForJdbcDriver(addJarToClasspath(d1, d2));
        } catch (Exception e1) {
            e1.printStackTrace();
        }
    }

    private static final String CONFIG_FILE = ".cp.ui";

    private void store() {
        try {
            File file = new File(CONFIG_FILE);
            file.delete();
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            File file = new File(CONFIG_FILE);
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(file));
            out.writeObject(getDriverJar().getText());
            out.writeObject(getSecDriverJar().getText());
            out.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void restore() {
        if (new File(CONFIG_FILE).exists()) {
            try {
                ObjectInputStream in = new ObjectInputStream(new FileInputStream(CONFIG_FILE));
                getDriverJar().setText((String) in.readObject());
                getSecDriverJar().setText((String) in.readObject());
                in.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * This method initializes jPanel	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getJPanel() {
        if (jPanel == null) {
            GridBagConstraints gridBagConstraints17 = new GridBagConstraints();
            gridBagConstraints17.gridx = 2;
            gridBagConstraints17.gridy = 6;
            jLabel23 = new JLabel();
            jLabel23.setText(" ");
            GridBagConstraints gridBagConstraints16 = new GridBagConstraints();
            gridBagConstraints16.gridx = 0;
            gridBagConstraints16.gridy = 1;
            jLabel22 = new JLabel();
            jLabel22.setText(" ");
            GridBagConstraints gridBagConstraints15 = new GridBagConstraints();
            gridBagConstraints15.fill = GridBagConstraints.BOTH;
            gridBagConstraints15.gridy = 1;
            gridBagConstraints15.weightx = 1.0;
            gridBagConstraints15.weighty = 1.0;
            gridBagConstraints15.gridwidth = 3;
            gridBagConstraints15.gridx = 1;
            GridBagConstraints gridBagConstraints13 = new GridBagConstraints();
            gridBagConstraints13.gridx = 5;
            gridBagConstraints13.gridy = 2;
            jLabel21 = new JLabel();
            jLabel21.setText(" ");
            GridBagConstraints gridBagConstraints12 = new GridBagConstraints();
            gridBagConstraints12.gridx = 1;
            gridBagConstraints12.gridy = 4;
            jLabel2 = new JLabel();
            jLabel2.setText(" ");
            GridBagConstraints gridBagConstraints11 = new GridBagConstraints();
            gridBagConstraints11.gridx = 2;
            gridBagConstraints11.weightx = 1.0;
            gridBagConstraints11.anchor = GridBagConstraints.EAST;
            gridBagConstraints11.gridwidth = 3;
            gridBagConstraints11.gridy = 5;
            GridBagConstraints gridBagConstraints10 = new GridBagConstraints();
            gridBagConstraints10.gridx = -1;
            gridBagConstraints10.anchor = GridBagConstraints.EAST;
            gridBagConstraints10.gridy = -1;
            gridBagConstraints10.insets = new Insets(8, 0, 4, 0);
            GridBagConstraints gridBagConstraints8 = new GridBagConstraints();
            gridBagConstraints8.gridx = 4;
            gridBagConstraints8.gridy = 3;
            GridBagConstraints gridBagConstraints7 = new GridBagConstraints();
            gridBagConstraints7.gridx = 4;
            gridBagConstraints7.gridy = 2;
            GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
            gridBagConstraints3.fill = GridBagConstraints.BOTH;
            gridBagConstraints3.gridy = 3;
            gridBagConstraints3.weightx = 1.0;
            gridBagConstraints3.gridx = 2;
            GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
            gridBagConstraints2.fill = GridBagConstraints.BOTH;
            gridBagConstraints2.gridy = 2;
            gridBagConstraints2.weightx = 1.0;
            gridBagConstraints2.gridx = 2;
            GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
            gridBagConstraints1.gridx = 1;
            gridBagConstraints1.anchor = GridBagConstraints.WEST;
            gridBagConstraints1.gridy = 3;
            jLabel1 = new JLabel();
            jLabel1.setText("sec. Jdbc-Driver Jar ");
            GridBagConstraints gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.gridx = 1;
            gridBagConstraints.anchor = GridBagConstraints.WEST;
            gridBagConstraints.gridy = 2;
            jLabel = new JLabel();
            jLabel.setText("Jdbc-Driver Jar");
            jPanel = new JPanel();
            jPanel.setLayout(new GridBagLayout());
            jPanel.add(jLabel, gridBagConstraints);
            jPanel.add(jLabel1, gridBagConstraints1);
            jPanel.add(getDriverJar(), gridBagConstraints2);
            jPanel.add(getSecDriverJar(), gridBagConstraints3);
            jPanel.add(getSelectDriverJar(), gridBagConstraints7);
            jPanel.add(getSelectSecDriverJar(), gridBagConstraints8);
            jPanel.add(getJPanel1(), gridBagConstraints11);
            jPanel.add(jLabel2, gridBagConstraints12);
            jPanel.add(jLabel21, gridBagConstraints13);
            jPanel.add(getJTextArea(), gridBagConstraints15);
            jPanel.add(jLabel22, gridBagConstraints16);
            jPanel.add(jLabel23, gridBagConstraints17);
        }
        return jPanel;
    }

    /**
     * This method initializes driverJar	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getDriverJar() {
        if (driverJar == null) {
            driverJar = new JTextField();
        }
        return driverJar;
    }

    /**
     * This method initializes secDriverJar	
     * 	
     * @return javax.swing.JTextField	
     */
    private JTextField getSecDriverJar() {
        if (secDriverJar == null) {
            secDriverJar = new JTextField();
        }
        return secDriverJar;
    }

    /**
     * This method initializes selectDriverJar	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getSelectDriverJar() {
        if (selectDriverJar == null) {
            selectDriverJar = new JButton();
            selectDriverJar.setText("...");
        }
        return selectDriverJar;
    }

    /**
     * This method initializes selectSecDriverJar	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getSelectSecDriverJar() {
        if (selectSecDriverJar == null) {
            selectSecDriverJar = new JButton();
            selectSecDriverJar.setText("...");
        }
        return selectSecDriverJar;
    }

    /**
     * This method initializes OK	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getOK() {
        if (OK == null) {
            OK = new JButton();
            OK.setText("   Ok   ");
        }
        return OK;
    }

    /**
     * This method initializes jPanel1	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getJPanel1() {
        if (jPanel1 == null) {
            jPanel1 = new JPanel();
            jPanel1.setLayout(new GridBagLayout());
            jPanel1.add(getOK());
        }
        return jPanel1;
    }

    /**
     * This method initializes jTextArea	
     * 	
     * @return javax.swing.JTextArea	
     */
    private JTextArea getJTextArea() {
        if (jTextArea == null) {
            jTextArea = new JTextArea();
            jTextArea.setText(
                    "\nSelect the Jdbc-Driver Jar file(s).\n\n"
                    + "A Jdbc Driver is a client-side adaptor needed to access the DBMS.\n" +
                      "Jdbc Drivers are usually shipped in a '*.jar' file.\nSome drivers need a second Jar-file (a license-file for example).");
            jTextArea.setWrapStyleWord(true);
            jTextArea.setLineWrap(true);
            jTextArea.setOpaque(false);
            jTextArea.setEditable(false);
        }
        return jTextArea;
    }

    public static void main(String args[]) throws FileNotFoundException, IOException {
        new JailerUI();
        new MainUI();
    }

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

}  //  @jve:decl-index=0:visual-constraint="10,10"
