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

import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.SwingUtilities;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.DbConnectionDialog.ConnectionInfo;
import net.sf.jailer.util.ClasspathUtil;

/**
 * Asks for an alternative connection if the database analyzers connection has
 * insufficient privileges to analyze a certain schema.
 *
 * @author Ralf Wisser
 */
public class PrivilegedSessionProviderDialog extends javax.swing.JDialog {

	public static class Provider implements JDBCMetaDataBasedModelElementFinder.PrivilegedSessionProvider {
		private final Frame parent;
		private Set<String> tabu = new HashSet<String>();
		
		public Provider(Frame parent) {
			synchronized(this) {
				this.parent = parent;
			}
		}

		@Override
		public Session askForSessionWithPermissionToReadSchema(final Session session, final Table view, final String schema, final String tableName,
				final ExecutionContext executionContext) {
			final Frame theParent;
			synchronized(this) {
				theParent = parent;
			}
			if (tabu.contains(schema)) {
				return null;
			}
			final AtomicReference<Session> newSession = new AtomicReference<>();
			try {
				SwingUtilities.invokeAndWait(new Runnable() {
					@Override
					public void run() {
						InfoBar infoBar = new InfoBar("Insufficient Privileges",
								"Need to analyze schema \"" + schema + "\" because\n" +
								"view \"" + view.getName() + "\" is based on table \"" + schema + "." + tableName +"\".\n" +
								"but that's not possible with the current connection.\n ",
								"Please choose an alternative connection with the required privileges."
								);
						final PrivilegedSessionProviderDialog dialog = new PrivilegedSessionProviderDialog(theParent);
						ConnectionInfo ci = new ConnectionInfo();
						final DbConnectionDialog connectionDialog = new DbConnectionDialog(theParent, JailerVersion.APPLICATION_NAME, infoBar, executionContext, true, false) {
							@Override
							protected boolean isAssignedToDataModel(String dataModelFolder) {
								return true;
							}
						};
						final DbConnectionDetailsEditor detailsEditor = new DbConnectionDetailsEditor(theParent, DbConnectionDialog.jdbcHelpURL, false, false, infoBar, true, true) {
							@Override
							protected void onClose(boolean ok, ConnectionInfo info) {
								dialog.setVisible(false);
								if (ok) {	
									try {
										BasicDataSource dataSource = UIUtil.createBasicDataSource(this, info.driverClass,
												info.url, info.user,
												info.password, 0, ClasspathUtil.toURLArray(info.jar1, info.jar2, info.jar3, info.jar4));
										newSession.set(new Session(dataSource, dataSource.dbms, executionContext.getIsolationLevel()));
									} catch (Exception e) {
										UIUtil.showException(theParent, "Error", e);
									}
								}
							}
							@Override
							protected void onSelect() {
								if (connectionDialog.connect("Analyze Database")) {
									try {
										setDetails((ConnectionInfo) connectionDialog.currentConnection.clone());
									} catch (CloneNotSupportedException e) {
										setDetails(connectionDialog.currentConnection);
									}
								}
							}
						};
						ci.alias = "(temporary)";
						ci.driverClass = session.driverClassName;
						ci.url = session.dbUrl;
						ci.user = schema;
						detailsEditor.setDetails(ci);
						dialog.detailsContainerPanel.add(detailsEditor.getContentPane());
						detailsEditor.password.grabFocus();
						dialog.pack();
						dialog.setSize(Math.max(500, dialog.getWidth()), dialog.getHeight());
						UIUtil.setInitialWindowLocation(dialog, parent, 100, 150);
						dialog.setVisible(true);
					}
				});
				
				Session result = newSession.get();
				if (result == null) {
					tabu.add(schema);
				}
				return result;
			} catch (InvocationTargetException e) {
				throw new RuntimeException(e);
			} catch (InterruptedException e) {
				// ignore
			}
			
			return null;
		}
	}

    /**
     * Creates new form PrivilegedSessionProvider
     */
    public PrivilegedSessionProviderDialog(Frame parent) {
        super(parent, true);
        initComponents();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        detailsContainerPanel = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        detailsContainerPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(detailsContainerPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jPanel1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel detailsContainerPanel;
    private javax.swing.JPanel jPanel1;
    // End of variables declaration//GEN-END:variables
}
