/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/GUIForms/JPanel.java to edit this template
 */
package net.sf.jailer.ui.ddl_script_generator;

import java.awt.Component;
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.ListCellRenderer;
import javax.swing.WindowConstants;

import org.fife.rsta.ui.EscapableDialog;

import liquibase.CatalogAndSchema;
import liquibase.LabelExpression;
import liquibase.Liquibase;
import liquibase.command.CommandScope;
import liquibase.command.core.GenerateChangelogCommandStep;
import liquibase.command.core.helpers.DbUrlConnectionArgumentsCommandStep;
import liquibase.command.core.helpers.PreCompareCommandStep;
import liquibase.database.Database;
import liquibase.database.DatabaseConnection;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.diff.compare.CompareControl;
import liquibase.exception.CommandExecutionException;
import liquibase.exception.DatabaseException;
import liquibase.exception.LiquibaseException;
import liquibase.resource.FileSystemResourceAccessor;
import liquibase.structure.DatabaseObject;
import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.ui.DbConnectionDialog;
import net.sf.jailer.ui.Environment;
import net.sf.jailer.ui.FileView;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.DataBrowser;
import net.sf.jailer.ui.syntaxtextarea.BasicFormatterImpl;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.ui.util.UISettings;
import net.sf.jailer.util.LogUtil;

/**
 * DDL script generator dialog.
 * 
 * @author Wisser
 */
@SuppressWarnings("serial")
public abstract class DDLScriptGeneratorPanel extends javax.swing.JPanel {

	public static final String TITLE = "Generate DDL Script";
	public static final String TOOLTIP = "Generate a DDL script that creates the database objects (CREATE TABLE, VIEW etc.)";

	/**
     * Creates new form DDLScriptGeneratorPanel
     */
    public DDLScriptGeneratorPanel() {
        initComponents(); UIUtil.initComponents(this);

        okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
        closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
        
        fileFindButton.setIcon(loadIcon);
        UIUtil.setTrailingComponent(scriptFileTextField, fileFindButton);
    }
    
	private void initTargetDBMS(Session session) {
		DefaultComboBoxModel<DBMS> aModel = new DefaultComboBoxModel<DBMS>(DBMS.values());
		DBMS sourceDBMS = session.dbms;
		if (DBMS.forDBMS(null).equals(sourceDBMS)) {
			aModel.addElement(sourceDBMS);
		}
		dbmsComboBox.setModel(aModel);
		dbmsComboBox.setRenderer(new DefaultListCellRenderer() {
			ListCellRenderer renderer = dbmsComboBox.getRenderer();

			@SuppressWarnings("rawtypes")
			@Override
			public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				Component render = renderer.getListCellRendererComponent(list,
						value instanceof DBMS ? ((DBMS) value).getDisplayName() : value, index, isSelected,
						cellHasFocus);
				if (render instanceof JLabel && value instanceof DBMS) {
					String logoUrl = "/dbmslogo/" + ((DBMS) value).getIcon();
					logoUrl = UIUtil.urlOrSmallIconUrl(logoUrl);
					if (logoUrl == null) {
						((JLabel) render).setIcon(null);
						((JLabel) render).setIconTextGap(4);
					} else {
						ImageIcon scaleIcon = UIUtil.scaleIcon((JLabel) render, UIUtil.readImage(logoUrl, false), 1.2);
						((JLabel) render).setIcon(scaleIcon);
						((JLabel) render)
								.setIconTextGap(Math.max(4, scaleIcon == null ? 0 : 36 - scaleIcon.getIconWidth()));
					}
				}
				return render;
			}
		});
		dbmsComboBox.setSelectedItem(sourceDBMS);
		dbmsComboBox.setMaximumRowCount(20);
	}
	
	private Window owner;
	
	public static void open(Window owner, String preselectedSchema, DataModel dataModel, Session session, ExecutionContext executionContext) {
		EscapableDialog dialog = owner instanceof Dialog? new EscapableDialog((Dialog) owner) {
		} : new EscapableDialog((Frame) owner) {
		};
		DDLScriptGeneratorPanel ddlScriptGeneratorPanel = new DDLScriptGeneratorPanel() {
			@Override
			protected void close() {
				dialog.setVisible(false);
			}
		};
		ddlScriptGeneratorPanel.owner = owner;
		ddlScriptGeneratorPanel.session = session;
    	ddlScriptGeneratorPanel.initTargetDBMS(session);
    	ddlScriptGeneratorPanel.statusLabel.setText((" "));
    	ddlScriptGeneratorPanel.statusLabel2.setVisible(false);
    	ddlScriptGeneratorPanel.statusLabelCancelled.setVisible(false);
    	
		if (executionContext.getCurrentModelSubfolder() != null) {
			ddlScriptGeneratorPanel.scriptFileTextField.setText((preselectedSchema != null? preselectedSchema : executionContext.getCurrentModelSubfolder()) + "-ddl.sql");
		}
		
		dialog.getContentPane().add(ddlScriptGeneratorPanel);
		dialog.setModal(true);
		dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		dialog.setTitle(TITLE);
		dialog.pack();
		dialog.setSize(Math.max(400, dialog.getWidth()), dialog.getHeight()); 

		UIUtil.fit(dialog);
		dialog.setLocation(owner.getX() + owner.getWidth() / 2 - dialog.getWidth() / 2, owner.getY() + owner.getHeight() / 2 - dialog.getHeight() / 2);
		
		dialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowOpened(WindowEvent e) {
				try {
					if (!ddlScriptGeneratorPanel.readSchemas(preselectedSchema, session, dataModel, dialog)) {
						dialog.setVisible(false);
					}
				} catch (Exception ex) {
					UIUtil.showException(dialog, "Error", ex);
					dialog.setVisible(false);
				}
			}
			@Override
			public void windowClosing(WindowEvent e) {
				ddlScriptGeneratorPanel.cancel();
			}
			@Override
			public void windowClosed(WindowEvent e) {
				ddlScriptGeneratorPanel.cancel();
			}
		});
		
		dialog.setVisible(true);
		dialog.dispose();
    }
	
	private void cancel() {
		if (!cancelled.get()) {
			cancelled.set(true);
			cancelled = new AtomicBoolean();
			if (out != null) {
				try {
					out.close();
				} catch (Throwable t) {
					// ignore
				}
				out = null;
			}
			DatabaseConnection conn = connection.get();
			if (conn != null) {
				Thread thread = new Thread(() -> {
					try {
						conn.close();
					} catch (Exception ex) {
						// ignore
					}
				}, "generate-ddl-cleanup");
				thread.setDaemon(true);
				thread.start();
			}
		}
	}

	private AtomicReference<DatabaseConnection> connection = new AtomicReference<>();
	private PrintWriter out;
	private AtomicBoolean cancelled = new AtomicBoolean();
    
    private boolean doGenerate(String fileName, AtomicBoolean cancelled) {
    	cancelled.set(false);
    	UIUtil.invokeLater(() -> {
    		statusLabel.setText(("Analyzing schema (might take a while)"));
       		statusLabel2.setVisible(true);
       		statusLabelCancelled.setVisible(false);
    	});
    	UISettings.s15 += 100000;
    	Liquibase liquibase = null;
    	String dbmsName = null;
		File baseDir = Configuration.getInstance().createTempFile();
		File changeLogFile = new File(baseDir.getPath() + ".xml");
		FileSystemResourceAccessor resourceAccessor = new FileSystemResourceAccessor(baseDir.getParent());
		String shortName;
		try {
			connection.set(new JdbcConnection(session.createNewConnection()));
			Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(connection.get());
			liquibase = new Liquibase(changeLogFile.getPath(), resourceAccessor, database);
			shortName = database.getShortName();
			Object dbms = dbmsComboBox.getSelectedItem();
			if (dbms instanceof DBMS && ((DBMS) dbms).getLiquibaseProductName() != null) {
				if (session.dbms != dbms) {
					dbmsName = ((DBMS) dbms).getDisplayName();
					shortName = ((DBMS) dbms).getLiquibaseProductName();
				}
			}
		    generateChangeLog(liquibase, database);
		} catch (Exception e1) {
			if (!cancelled.get()) {
				UIUtil.showException(this, "Error", e1);
			}
			return false;
		} finally {
			if (liquibase != null) {
				try {
					liquibase.close();
				} catch (LiquibaseException e1) {
					// ignore
				}
			}
		}
		
		File databaseChangeLogFile = new File(baseDir.getPath() + ".csv");
		String outputSchema = null;
		try {
		    if (changeLogFile.exists()) {
				liquibase = new Liquibase(changeLogFile.getName(), resourceAccessor, DatabaseFactory.getInstance().openConnection(
						"offline:" + shortName + "?changeLogFile=" + databaseChangeLogFile.getPath()
						+ "&outputLiquibaseSql=NONE",
						session.getSchema(), "", "", null, resourceAccessor));
				
				liquibase.getDatabase().setOutputDefaultSchema(false);
				if (!targetSchemaTextField.getText().trim().isEmpty()) {
					liquibase.getDatabase().setOutputDefaultSchema(true);
					liquibase.getDatabase().setDefaultSchemaName(outputSchema = targetSchemaTextField.getText().trim());
				}
	
				String host = "";
				try {
					host = "@" + InetAddress.getLocalHost().getHostName();
				} catch (UnknownHostException e) {
				}
				out = new PrintWriter(new File(fileName));
				String schema = String.valueOf(schemaComboBox.getSelectedItem());
				
				out.println("-- DDL of " + (schema.isEmpty()? "default schema" : "schema \"" + schema + "\"") + (outputSchema == null? "" : " (Target schema: \"" + outputSchema + "\")") + (dbmsName == null? "" : " for DBMS \"" + dbmsName + "\""));
				out.println("-- generated by Jailer " + JailerVersion.VERSION + ", " + new Date() + " from " + System.getProperty("user.name") + host);
				out.println();
				
				Writer output = new Writer() {
					AtomicInteger count = new AtomicInteger(0);
					StringBuilder currentLine = new StringBuilder();
					Pattern typeRe = Pattern.compile("(\\bcreate\\b.*\\btable\\b)|(\\bcreate\\b.*\\bindex\\b)|(\\bcreate\\b.*\\bview\\b)|(\\balter\\b)", Pattern.CASE_INSENSITIVE);
					int type = -1;
					boolean prevLineHadSemicolon = true;
					AtomicBoolean uiPending = new AtomicBoolean(false);
					@Override
					public void write(char[] cbuf, int off, int len) throws IOException {
						try {
							doWrite(cancelled, cbuf, off, len);
						} catch (Throwable t) {
							if (!cancelled.get()) {
								throw t;
							}
						}
					}
					private void doWrite(AtomicBoolean cancelled, char[] cbuf, int off, int len) {
						for (int i = 0; i < len; ++i) {
							char c = cbuf[i + off];
							if (c == '\n') {
								String line = currentLine.toString();
								if (!line.startsWith("--") && !line.equals("GO") && !line.isEmpty()) {
									int newType = -1;
									Matcher matcher = typeRe.matcher(line);
									if (matcher.find()) {
										if (matcher.group(1) != null) {
											newType = 1;
										} if (matcher.group(2) != null) {
											newType = 2;
										} if (matcher.group(3) != null) {
											newType = 3;
										} if (matcher.group(4) != null) {
											newType = 4;
										}
									}
									if (type >= 0 && newType >= 0 && type != newType) {
										out.println();
									}
									type = newType;
									boolean thisLineHadSemicolon = line.endsWith(";");
									if (prevLineHadSemicolon && thisLineHadSemicolon) {
										line = new BasicFormatterImpl().format(line).replace("\r", "").replace("\n", UIUtil.LINE_SEPARATOR);
									}
									prevLineHadSemicolon = thisLineHadSemicolon;
									out.println(line);
									out.println();
									count.incrementAndGet();
									if (!uiPending.get()) {
										uiPending.set(true);
										UIUtil.invokeLater(() -> {
											statusLabel.setText(count.get() + " Statements written");
											uiPending.set(false);
										});
									}
								}
								currentLine.setLength(0);
							} else if (c != '\r') {
								currentLine.append(c);
							}
						}
						if (cancelled.get()) {
							throw new RuntimeException("cancelled");
						}
					}
					@Override
					public void flush() throws IOException {
						out.flush();
					}
					@Override
					public void close() throws IOException {
						write("\n");
						out.close();
					}
				};
				liquibase.update(null, new LabelExpression(), output, false);
			    liquibase.close();
		    } else {
		    	JOptionPane.showMessageDialog(this, "Schema is empty.");
	    		return false;
		    }
		} catch (Exception e1) {
			if (!cancelled.get()) {
				UIUtil.showException(this, "Error", e1);
			}
			return false;
		} finally {
		    changeLogFile.delete();
			databaseChangeLogFile.delete();					
			if (liquibase != null) {
				try {
					liquibase.close();
				} catch (LiquibaseException e1) {
					// ignore
				}
			}
		}
		return true;
	}

	public void generateChangeLog(Liquibase liquibase, Database database)
			throws DatabaseException, CommandExecutionException {
		String defaultSchemaName = String.valueOf(schemaComboBox.getSelectedItem());
		CatalogAndSchema catalogAndSchema = new CatalogAndSchema(database.getDefaultCatalogName(), defaultSchemaName);
		Set<Class<? extends DatabaseObject>> finalCompareTypes = null;
		CompareControl compareControl = new CompareControl(new CompareControl.SchemaComparison[]{
		        new CompareControl.SchemaComparison(catalogAndSchema, catalogAndSchema)
		}, finalCompareTypes);
		
		new CommandScope(GenerateChangelogCommandStep.COMMAND_NAME[0])
		        .addArgumentValue(GenerateChangelogCommandStep.CHANGELOG_FILE_ARG, liquibase.getChangeLogFile())
		        .addArgumentValue(PreCompareCommandStep.COMPARE_CONTROL_ARG, compareControl)
		        .addArgumentValue(DbUrlConnectionArgumentsCommandStep.DATABASE_ARG, liquibase.getDatabase())
		        .addArgumentValue(PreCompareCommandStep.SNAPSHOT_TYPES_ARG, null)
		        .setOutput(System.out)
		        .execute();
	}

	private String defaultSchema;
	private Session session;
	
	protected boolean readSchemas(String preselectedSchema, Session session, DataModel dataModel, EscapableDialog dialog) {
		AtomicBoolean ok = new AtomicBoolean(false);
		final ConcurrentTaskControl concurrentTaskControl = new ConcurrentTaskControl(
				dialog, "Retrieving schema info...") {
			@Override
			protected void onError(Throwable error) {
				UIUtil.showException(this, "Error", error);
				closeWindow();
			}
			@Override
			protected void onCancellation() {
				closeWindow();
			}
		};
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				if (concurrentTaskControl.master != null) {
					concurrentTaskControl.master.cancelButton.setText("Cancel");
				}
			}
		});

		final List<String> schemaNames = Collections.synchronizedList(new ArrayList<String>());
		
		ConcurrentTaskControl.openInModalDialog(dialog, concurrentTaskControl, new ConcurrentTaskControl.Task() {
			@Override
			public void run() throws Throwable {
				DatabaseConnection c = null;
				try {
					c = new JdbcConnection(session.createNewConnection());
					Database database = DatabaseFactory.getInstance().findCorrectDatabaseImplementation(c);
					defaultSchema = database.getDefaultSchemaName();
				} finally {
					if (c != null) {
						try {
							c.close();
						} catch (Exception e) {
							LogUtil.warn(e);
						}
					}
				}
				List<String> schemas = new ArrayList<String>();
				schemas.addAll(JDBCMetaDataBasedModelElementFinder.getSchemas(session, session.getSchema()));
				synchronized (schemaNames) {
					schemaNames.addAll(schemas);
				}
				schemas.addAll(JDBCMetaDataBasedModelElementFinder.getCatalogsWithSchemas(session));
				ok.set(true);
				UIUtil.invokeLater(new Runnable() {
					@Override
					public void run() {
						if (concurrentTaskControl.master.isShowing()) {
							concurrentTaskControl.closeWindow();
						}
					}
				});
			}
		}, "Retrieving schema info...");
		
		if (ok.get()) {
			synchronized (schemaNames) {
				if (defaultSchema == null) {
					defaultSchema = session.getSchema();
				}
				if (defaultSchema == null) {
					defaultSchema = "";
				}
				String selected = defaultSchema;
				if (!schemaNames.contains(defaultSchema)) {
					schemaNames.add(defaultSchema);
				}
				schemaNames.sort(String::compareToIgnoreCase);
				Set<String> dmSchemas = new HashSet<>();
				for (Table table: dataModel.getTables()) {
					dmSchemas.add(table.getSchema(defaultSchema).toUpperCase());
				}
				for (Iterator<String> it = schemaNames.iterator(); it.hasNext();) {
					String schema = it.next();
					if (dmSchemas.contains(schema.toUpperCase())) {
						if (!dmSchemas.contains(selected)) {
							selected = schema;
						}
						break;
					}
				}
				schemaComboBox.setModel(new DefaultComboBoxModel<>(schemaNames.toArray(new String[0])));
				schemaComboBox.setSelectedItem(preselectedSchema != null && schemaNames.contains(preselectedSchema)? preselectedSchema : defaultSchema);
			}
		}
		return ok.get();
    }

	protected abstract void close();

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        scriptFileTextField = new javax.swing.JTextField();
        fileFindButton = new javax.swing.JButton();
        jLabel4 = new javax.swing.JLabel();
        targetSchemaTextField = new javax.swing.JTextField();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        schemaComboBox = new javax.swing.JComboBox<>();
        dbmsComboBox = new javax.swing.JComboBox<>();
        statusLabel = new javax.swing.JLabel();
        statusLabel2 = new javax.swing.JLabel();
        statusLabelCancelled = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        closeButton = new javax.swing.JButton();
        okButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();

        setLayout(new java.awt.GridBagLayout());

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLabel1.setText("Script file  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jLabel1, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel1.add(scriptFileTextField, gridBagConstraints);

        fileFindButton.setText("jButton1");
        fileFindButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                fileFindButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.insets = new java.awt.Insets(4, 2, 0, 0);
        jPanel1.add(fileFindButton, gridBagConstraints);

        jLabel4.setText("Schema ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jLabel4, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel1.add(targetSchemaTextField, gridBagConstraints);

        jLabel5.setText("Target schema ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 14;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jLabel5, gridBagConstraints);

        jLabel6.setText("Target DBMS  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 16;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jLabel6, gridBagConstraints);

        schemaComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel1.add(schemaComboBox, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 16;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 0);
        jPanel1.add(dbmsComboBox, gridBagConstraints);

        statusLabel.setFont(statusLabel.getFont().deriveFont(statusLabel.getFont().getSize()+4f));
        statusLabel.setForeground(new java.awt.Color(0, 102, 0));
        statusLabel.setText("Analyzing schema (might take a while)  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 4, 8, 0);
        jPanel1.add(statusLabel, gridBagConstraints);

        statusLabel2.setFont(statusLabel2.getFont().deriveFont(statusLabel2.getFont().getSize()+4f));
        statusLabel2.setForeground(new java.awt.Color(0, 102, 0));
        statusLabel2.setText("Running...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 8, 0);
        jPanel1.add(statusLabel2, gridBagConstraints);

        statusLabelCancelled.setFont(statusLabelCancelled.getFont().deriveFont(statusLabelCancelled.getFont().getSize()+4f));
        statusLabelCancelled.setForeground(java.awt.Color.red);
        statusLabelCancelled.setText("Cancelled");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 40;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 8, 0);
        jPanel1.add(statusLabelCancelled, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 4);
        add(jPanel1, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        closeButton.setText("Cancel");
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 4);
        jPanel3.add(closeButton, gridBagConstraints);

        okButton.setText("Generate Script");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHEAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 0, 2);
        jPanel3.add(okButton, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        jPanel3.add(jLabel2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 20;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(jPanel3, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed
    	if (okButton.isEnabled()) {
    		close();
    	} else {
       		statusLabelCancelled.setVisible(true);
       		toggleEnable(true);
    		cancel();
    	}
    }//GEN-LAST:event_closeButtonActionPerformed

    private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
    	String fn = scriptFileTextField.getText().trim();
    	if (fn.isEmpty()) {
    		JOptionPane.showMessageDialog(this, "Missing file name.");
    		return;
    	}
		String fileName = toFileName(fn);
		
		toggleEnable(false);
		
        Thread thread = new Thread(() -> {
        	Throwable throwable = null;
        	AtomicBoolean cancelled = new AtomicBoolean();
        	this.cancelled = cancelled;
        	try {
        		if (doGenerate(fileName, cancelled)) {
					UIUtil.invokeLater(() -> {
	        			close();
					});
        			UIUtil.invokeLater(4, () -> {
        				try {
        					if (owner instanceof DataBrowser) {
        						((DataBrowser) owner).loadSQLScriptFile(new File(fileName));
        					} else {
        						new FileView(owner, owner, fileName, true);
        					}
						} catch (Throwable t) {
							UIUtil.showException(DDLScriptGeneratorPanel.this, "Error", t);
						}
        			});
        		}
        	} catch (Throwable t) {
        		if (!cancelled.get()) {
        			throwable = t;
        		}
        	}
        	final Throwable finalThrowable = throwable;
        	UIUtil.invokeLater(() -> {
        		statusLabel.setText(" ");
        		statusLabel2.setVisible(false);
        		statusLabelCancelled.setVisible(cancelled.get());
        		if (finalThrowable != null) {
        			UIUtil.showException(DDLScriptGeneratorPanel.this, "Error", finalThrowable);
        		} else {
        			toggleEnable(true);
        		}
        	});
        }, "generate-ddl");
        thread.setDaemon(true);
        thread.start();
    }//GEN-LAST:event_okButtonActionPerformed

	private void toggleEnable(boolean enable) {
		Set<JComponent> comps = new HashSet<>();
		comps.add(fileFindButton);
		comps.add(dbmsComboBox);
		comps.add(schemaComboBox);
		comps.add(scriptFileTextField);
		comps.add(targetSchemaTextField);
		comps.add(okButton);
		
		comps.forEach(c -> c.setEnabled(enable));
		if (enable) {
			statusLabel.setText(" ");
			statusLabel2.setVisible(false);    		
		}
	}

    private void fileFindButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_fileFindButtonActionPerformed
    	String fn = UIUtil.choseFile(null, ".", TITLE, ".sql", this, true, false, false);
		if (fn != null) {
			scriptFileTextField.setText(Environment.makeRelative(fn));
		}
    }//GEN-LAST:event_fileFindButtonActionPerformed

	private String toFileName(String f) {
		if (!new File(f).isAbsolute()) {
			return Environment.newFile(f).getAbsolutePath();
		}
		return f;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    javax.swing.JButton closeButton;
    private javax.swing.JComboBox<DBMS> dbmsComboBox;
    private javax.swing.JButton fileFindButton;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JButton okButton;
    private javax.swing.JComboBox<String> schemaComboBox;
    private javax.swing.JTextField scriptFileTextField;
    private javax.swing.JLabel statusLabel;
    private javax.swing.JLabel statusLabel2;
    private javax.swing.JLabel statusLabelCancelled;
    private javax.swing.JTextField targetSchemaTextField;
    // End of variables declaration//GEN-END:variables
    private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	private static ImageIcon loadIcon;
	
	static {
		// load images
		okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        loadIcon = UIUtil.readImage("/load2.png");
	}
}

