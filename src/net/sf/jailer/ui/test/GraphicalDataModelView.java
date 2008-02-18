package net.sf.jailer.ui.test;

import java.awt.Component;

import javax.swing.JTextArea;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Renders a {@link DataModel} as graph.
 */
public class GraphicalDataModelView extends javax.swing.JFrame {
    
    /** 
     * Creates new form GraphicalDataModelView 
     */
    public GraphicalDataModelView() {
        setDefaultCloseOperation(3);
        try {
			getContentPane().add(createModelView(new DataModel()));
		} catch (Exception e) {
			e.printStackTrace();
		}
		setLocation(100, 100);
        pack();
    }
    
    /**
     * Creates the graph view component.
     * 
     * @param dataModel the model to render
     * @return the graph view component
     */
    private Component createModelView(DataModel dataModel) {
    	JTextArea view = new JTextArea();
    	
    	for (Table table: dataModel.getTables()) {
    		view.append("Table " + table.getName() + "\n");
    		for (Association association: table.associations) {
    			String type;
    			if (association.isIgnored()) {
    				type = "Ignored";
    			} else if (association.isInsertDestinationBeforeSource()) {
    				type = "Dependency";
    			} else if (association.isInsertSourceBeforeDestination()) {
    				type = "Inverse dependency";
    			} else {
        			type = "Association";
    			}
        		view.append("  " + type + " to " + association.destination.getName() + " on " + association.getJoinCondition() + "\n");
    		}
    	}
    	
		return view;
	}

	/**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new GraphicalDataModelView().setVisible(true);
            }
        });
    }
    
}
