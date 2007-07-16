package org.jailer.ui;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.jailer.Jailer;
import org.jailer.modelbuilder.ModelBuilder;
import org.jailer.util.PrintUtil;

public class AcceptDatamodelUI extends JFrame {

    private JPanel jContentPane = null;
    private CsvFileView tables = null;
    private JScrollPane jScrollPane1 = null;
    private CsvFileView associations = null;
    private CsvFileView builderTables = null;
    private CsvFileView BuilderAssociations = null;
    private JScrollPane jScrollPane = null;
    private JScrollPane jScrollPane2 = null;
    private JScrollPane jScrollPane3 = null;
    private JLabel jLabel = null;
    private JLabel jLabel1 = null;
    private JLabel jLabel11 = null;
    private JLabel jLabel12 = null;
    private JButton AcceptAll = null;
    /**
     * This method initializes 
     * 
     */
    public AcceptDatamodelUI() {
    	super();
    	initialize();
    }

    /**
     * This method initializes this
     * 
     */
    private void initialize() {
        this.setSize(new Dimension(750, 550));
        this.setContentPane(getJContentPane());
        
        setLocation(50, 50);
        
        setTitle("Jailer " + Jailer.VERSION + " Datamodel");
        loadModelFiles();
        
        getAcceptAll().addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    accept();
                    loadModelFiles();
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });
    }

    private void loadModelFiles() {
        try {
            getTables().setContent(PrintUtil.loadFile("datamodel" + File.separator + "table.csv"));
            getAssociations().setContent(PrintUtil.loadFile("datamodel" + File.separator + "association.csv"));
            getBuilderTables().setContent(PrintUtil.loadFile("datamodel" + File.separator + "model-builder-table.csv"));
            getBuilderAssociations().setContent(PrintUtil.loadFile("datamodel" + File.separator + "model-builder-association.csv"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * This method initializes jContentPane	
     * 	
     * @return javax.swing.JPanel	
     */
    private JPanel getJContentPane() {
        if (jContentPane == null) {
            GridBagConstraints gridBagConstraints6 = new GridBagConstraints();
            gridBagConstraints6.gridx = 1;
            gridBagConstraints6.anchor = GridBagConstraints.EAST;
            gridBagConstraints6.gridy = 5;
            GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
            gridBagConstraints5.gridx = 0;
            gridBagConstraints5.anchor = GridBagConstraints.WEST;
            gridBagConstraints5.gridy = 3;
            jLabel12 = new JLabel();
            jLabel12.setText("newly found tables (datamodel/model-builder-table.csv)");
            GridBagConstraints gridBagConstraints4 = new GridBagConstraints();
            gridBagConstraints4.gridx = 1;
            gridBagConstraints4.anchor = GridBagConstraints.WEST;
            gridBagConstraints4.gridy = 3;
            jLabel11 = new JLabel();
            jLabel11.setText("newly found associations (datamodel/model-builder-association.csv)");
            GridBagConstraints gridBagConstraints31 = new GridBagConstraints();
            gridBagConstraints31.gridx = 1;
            gridBagConstraints31.anchor = GridBagConstraints.WEST;
            gridBagConstraints31.gridy = 1;
            jLabel1 = new JLabel();
            jLabel1.setText("known associations (datamodel/association.csv)");
            GridBagConstraints gridBagConstraints21 = new GridBagConstraints();
            gridBagConstraints21.gridx = 0;
            gridBagConstraints21.anchor = GridBagConstraints.WEST;
            gridBagConstraints21.gridy = 1;
            jLabel = new JLabel();
            jLabel.setText("known tables (datamodel/table.csv)");
            GridBagConstraints gridBagConstraints3 = new GridBagConstraints();
            gridBagConstraints3.fill = GridBagConstraints.BOTH;
            gridBagConstraints3.weighty = 1.0;
            gridBagConstraints3.weightx = 1.0;
            gridBagConstraints3.gridx = 1;
            gridBagConstraints3.gridy = 4;
            gridBagConstraints3.insets = new Insets(2, 2, 2, 2);
            GridBagConstraints gridBagConstraints2 = new GridBagConstraints();
            gridBagConstraints2.fill = GridBagConstraints.BOTH;
            gridBagConstraints2.weighty = 1.0;
            gridBagConstraints2.gridx = 0;
            gridBagConstraints2.gridy = 4;
            gridBagConstraints2.weightx = 1.0;
            gridBagConstraints2.insets = new Insets(2, 2, 2, 2);
            GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
            gridBagConstraints1.fill = GridBagConstraints.BOTH;
            gridBagConstraints1.weighty = 1.0;
            gridBagConstraints1.weightx = 1.0;
            gridBagConstraints1.gridx = 0;
            gridBagConstraints1.gridy = 2;
            gridBagConstraints1.insets = new Insets(2, 2, 2, 2);
            GridBagConstraints gridBagConstraints = new GridBagConstraints();
            gridBagConstraints.fill = GridBagConstraints.BOTH;
            gridBagConstraints.weighty = 1.0;
            gridBagConstraints.weightx = 1.0;
            gridBagConstraints.gridx = 1;
            gridBagConstraints.gridy = 2;
            gridBagConstraints.insets = new Insets(2, 2, 2, 2);
            jContentPane = new JPanel();
            jContentPane.setLayout(new GridBagLayout());
            jContentPane.add(getJScrollPane1(), gridBagConstraints);
            jContentPane.add(getJScrollPane(), gridBagConstraints1);
            jContentPane.add(getJScrollPane2(), gridBagConstraints2);
            jContentPane.add(getJScrollPane3(), gridBagConstraints3);
            jContentPane.add(jLabel, gridBagConstraints21);
            jContentPane.add(jLabel1, gridBagConstraints31);
            jContentPane.add(jLabel11, gridBagConstraints4);
            jContentPane.add(jLabel12, gridBagConstraints5);
            jContentPane.add(getAcceptAll(), gridBagConstraints6);
        }
        return jContentPane;
    }

    /**
     * This method initializes tables	
     * 	
     * @return org.jailer.ui.CsvFileView	
     */
    private CsvFileView getTables() {
        if (tables == null) {
            tables = new CsvFileView();
        }
        return tables;
    }

    /**
     * This method initializes jScrollPane1	
     * 	
     * @return javax.swing.JScrollPane	
     */
    private JScrollPane getJScrollPane1() {
        if (jScrollPane1 == null) {
            jScrollPane1 = new JScrollPane();
            jScrollPane1.setViewportView(getAssociations());
        }
        return jScrollPane1;
    }

    /**
     * This method initializes associations	
     * 	
     * @return org.jailer.ui.CsvFileView	
     */
    private CsvFileView getAssociations() {
        if (associations == null) {
            associations = new CsvFileView();
        }
        return associations;
    }

    /**
     * This method initializes builderTables	
     * 	
     * @return org.jailer.ui.CsvFileView	
     */
    private CsvFileView getBuilderTables() {
        if (builderTables == null) {
            builderTables = new CsvFileView();
        }
        return builderTables;
    }

    /**
     * This method initializes BuilderAssociations	
     * 	
     * @return org.jailer.ui.CsvFileView	
     */
    private CsvFileView getBuilderAssociations() {
        if (BuilderAssociations == null) {
            BuilderAssociations = new CsvFileView();
        }
        return BuilderAssociations;
    }

    /**
     * This method initializes jScrollPane	
     * 	
     * @return javax.swing.JScrollPane	
     */
    private JScrollPane getJScrollPane() {
        if (jScrollPane == null) {
            jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getTables());
        }
        return jScrollPane;
    }

    /**
     * This method initializes jScrollPane2	
     * 	
     * @return javax.swing.JScrollPane	
     */
    private JScrollPane getJScrollPane2() {
        if (jScrollPane2 == null) {
            jScrollPane2 = new JScrollPane();
            jScrollPane2.setViewportView(getBuilderTables());
        }
        return jScrollPane2;
    }

    /**
     * This method initializes jScrollPane3	
     * 	
     * @return javax.swing.JScrollPane	
     */
    private JScrollPane getJScrollPane3() {
        if (jScrollPane3 == null) {
            jScrollPane3 = new JScrollPane();
            jScrollPane3.setViewportView(getBuilderAssociations());
        }
        return jScrollPane3;
    }

    /**
     * This method initializes AcceptAll	
     * 	
     * @return javax.swing.JButton	
     */
    private JButton getAcceptAll() {
        if (AcceptAll == null) {
            AcceptAll = new JButton();
            AcceptAll.setText("Accept All");
        }
        return AcceptAll;
    }

    private void accept() throws Exception {
        writeFile("datamodel/table.csv", PrintUtil.loadFile("datamodel/table.csv") + "\n" + PrintUtil.loadFile("datamodel/model-builder-table.csv", true));
        writeFile("datamodel/association.csv", PrintUtil.loadFile("datamodel/association.csv") + "\n" + PrintUtil.loadFile("datamodel/model-builder-association.csv", true));
        ModelBuilder.resetFiles();
    }

    /**
     * Writes content into a file.
     * 
     * @param content the content
     * @param fileName the name of the file
     */
    private static void writeFile(String fileName, String content) throws IOException {
        PrintWriter out = new PrintWriter(new FileOutputStream(fileName));
        out.print(content);
        out.close();
    }

}  //  @jve:decl-index=0:visual-constraint="15,41"
