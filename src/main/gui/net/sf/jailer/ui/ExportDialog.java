/*
 * Copyright 2007 - 2023 Ralf Wisser.
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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import com.formdev.flatlaf.FlatClientProperties;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.BasicDataSource;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.SqlException;
import net.sf.jailer.database.WorkingTableScope;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.PrimaryKeyFactory;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ddl.DDLCreator;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.util.ConcurrentTaskControl;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.Quoting;

/**
 * Data Export Dialog.
 *
 * @author Ralf Wisser
 */
public abstract class ExportDialog extends javax.swing.JDialog {

	/**
	 * true iff ok-button was clicked.
	 */
	boolean isOk = false;

	/**
	 * Xml/Sql switch.
	 */
	public final ScriptFormat scriptFormat;

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
	 * Labels of schema mapping fields.
	 */
	private Map<String, JLabel> schemaMappingLabels = new HashMap<String, JLabel>();

	/**
	 * Source-schema mapping fields.
	 */
	private Map<String, JTextField> sourceSchemaMappingFields = new HashMap<String, JTextField>();

	/**
	 * Delete-schema mapping fields.
	 */
	private Map<String, JTextField> deleteSchemaMappingFields = new HashMap<String, JTextField>();

	/**
	 * The form field setting.
	 */
	private Settings theSettings;

	/**
	 * The subject table.
	 */
	private final Table subject;
	private final List<AdditionalSubject> additionalSubjects;

	private ParameterEditor parameterEditor;
	private final List<String> initialArgs;
	private final String user;
	private final String password;
	private final String subjectCondition;
	private final String settingsContext;
	private final String settingsContextSecondaryKey;
	private final DBMS sourceDBMS;
	private final DbConnectionDialog dbConnectionDialog;

	private String[] schemaComboboxModel;

	private static boolean lastConfirmInsert = false;
	private final String extractionModelFileName;
	private final String jmFile;
	private final String tmpFileName;
	private final ExecutionContext executionContext;
	private final StringBuilder defaultExportFileName;
	private final StringBuilder databaseProductName = new StringBuilder();

	private static final Object NO_SCHEMA_INFO = new String("");
	private static final String NO_SCHEMA_INFO_LABEL = "<html><i>no further schema information</i></html>";

	/** Creates new form DbConnectionDialog
	 */
	public ExportDialog(java.awt.Frame parent, final DataModel dataModel, final Table subject, String subjectCondition, List<AdditionalSubject> additionalSubjects, final Session session, List<String> initialArgs, String user, String password, boolean showCmd, DbConnectionDialog dbConnectionDialog, String extractionModelFileName, String jmFile, String tmpFileName, StringBuilder defaultExportFileName, ExecutionContext executionContext) {
		super(parent, true);
		this.executionContext = executionContext;
		this.extractionModelFileName = extractionModelFileName;
		this.jmFile = jmFile;
		this.tmpFileName = tmpFileName;
		this.subjectCondition = subjectCondition;
		this.dataModel = dataModel;
		this.subject = subject;
		this.initialArgs = new ArrayList<String>(initialArgs);
		this.user = user;
		this.password = password;
		this.settingsContext = session.dbUrl;
		this.settingsContextSecondaryKey = session.getSchema();
		this.sourceDBMS = session.dbms;
		this.dbConnectionDialog = dbConnectionDialog;
		this.additionalSubjects = additionalSubjects;
		this.defaultExportFileName = defaultExportFileName;

		try {
			UIUtil.setWaitCursor(parent);

			initComponents(); UIUtil.initComponents(this);
			copyButton.setIcon(copyIcon);
			
			cliArea.setDocument(new DefaultStyledDocument());
			
			jButton1.setIcon(runIcon);
			cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
			
			if (jScrollPane2.getHorizontalScrollBar() != null) {
	        	jScrollPane2.getHorizontalScrollBar().setUnitIncrement(16);
	        }
	        if (jScrollPane2.getVerticalScrollBar() != null) {
	        	jScrollPane2.getVerticalScrollBar().setUnitIncrement(16);
	        }

	        CancellationHandler.reset(null);

			if (!showCmd) {
				commandLinePanel.setVisible(false);
			}

			final ConcurrentTaskControl concurrentTaskControl = new ConcurrentTaskControl(
					this, "Retrieving schema info...") {
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
						concurrentTaskControl.master.cancelButton.setText("continue without info");
					}
				}
			});

			final List<String> schemaInfo = Collections.synchronizedList(new ArrayList<String>());
			final List<String> schemaNames = Collections.synchronizedList(new ArrayList<String>());
			final AtomicBoolean schemaInfoRead = new AtomicBoolean(false);
			StringBuilder defaultSchemaSB = new StringBuilder();
			
			ConcurrentTaskControl.openInModalDialog(parent, concurrentTaskControl,
					new ConcurrentTaskControl.Task() {
						@Override
						public void run() throws Throwable {
							List<String> schemas = new ArrayList<String>();
							schemas.addAll(JDBCMetaDataBasedModelElementFinder.getSchemas(session, session.getSchema()));
							synchronized (schemaNames) {
								schemaNames.addAll(schemas);
								defaultSchemaSB.append(JDBCMetaDataBasedModelElementFinder.getDefaultSchema(session, session.getSchema(), schemaNames.isEmpty()? null : schemaNames));
							}
							schemas.addAll(JDBCMetaDataBasedModelElementFinder.getCatalogsWithSchemas(session));
							synchronized (schemaInfo) {
								schemaInfo.addAll(schemas);
								schemaInfoRead.set(true);
							}
							databaseProductName.append(session.getMetaData().getDatabaseProductName());
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

			unknownDBMSWarning(session);
			
			String defaultSchema;
			synchronized (schemaNames) {
				defaultSchema = defaultSchemaSB.toString();
			}
			List<String> allSchemas;
			synchronized (schemaInfo) {
				allSchemas = new ArrayList<String>(schemaInfo);
				if (!schemaInfoRead.get()) {
					allSchemas.add((String) NO_SCHEMA_INFO);
				}
			}
			initWorkingTableSchemaBox(session, allSchemas, defaultSchema);
			initIFMTableSchemaBox(session, allSchemas, defaultSchema);

			try {
				JTextField c = (JTextField) workingTableSchemaComboBox.getEditor().getEditorComponent();
				c.getDocument().addDocumentListener(new DocumentListener() {
					@Override
					public void removeUpdate(DocumentEvent e) {
						update();
					}
					@Override
					public void insertUpdate(DocumentEvent e) {
						update();
					}
					@Override
					public void changedUpdate(DocumentEvent e) {
						update();
					}
					private void update() {
						updateCLIArea();
					}
				});
			} catch (ClassCastException e) {
				// ignore
			}

			parameterEditor = new ParameterEditor(parent);
			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 0;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			gridBagConstraints.weightx = 1.0;
			gridBagConstraints.weighty = 1.0;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			parameterPanel.add(parameterEditor.createPane(dataModel.getParameters(subjectCondition, additionalSubjects)), gridBagConstraints);

			ScriptFormat theScriptFormat = ScriptFormat.SQL;
			try {
				theScriptFormat = ScriptFormat.valueOf(dataModel.getExportModus());
			} catch (Exception e) {
			}

			scriptFormat = theScriptFormat;
			toLabel.setText(scriptFormat.getDisplayName());

			setModal(true);
			UIUtil.setInitialWindowLocation(this, parent, 100, 50);
			Map<String, JComponent> fields = new HashMap<String, JComponent>();
			if (defaultExportFileName == null) {
				fields.put("insert" + scriptFormat.name(), insert);
			} else {
				insert.setText(defaultExportFileName.toString());
			}
			fields.put("threads", threads);
			fields.put("rowsPerThread", rowsPerThread);
			fields.put("rowLimit", rowLimit);
			fields.put("unicode", unicode);
			fields.put("sorted", sortedCheckBox);
			fields.put("insertIncrementally", insertIncrementally);
			fields.put("independentWorkingTables", independentWorkingTables);
			fields.put("isolationLevel", isolationLevelComboBox);
			fields.put("targetDBMS", targetDBMSComboBox);
			fields.put("upsertCheckbox", upsertCheckbox);
			fields.put("scopeLocal", scopeLocal);
			fields.put("scopeGlobal", scopeGlobal);
			fields.put("scopeSession", scopeSession);
			fields.put("orderByPK", orderByPKCheckbox);
			fields.put("rowidPK", rowidPK);
			fields.put("rowidBoth", rowidBoth);
			fields.put("rowidRowid", rowidRowid);
			fields.put("transactional", transactional);
			fields.put("delete", delete);
			fields.put("localTempDir", localTempDirTextField);

			for (Map.Entry<String, JTextField> e: parameterEditor.textfieldsPerParameter.entrySet()) {
				fields.put("$" + e.getKey(), e.getValue());
			}

			try {
				JTextField c;
				c = (JTextField) workingTableSchemaComboBox.getEditor().getEditorComponent();
				fields.put("workingTableSchema", c);
			} catch (ClassCastException e) {
				// ignore
			}
			try {
				JTextField c;
				c = (JTextField) iFMTableSchemaComboBox.getEditor().getEditorComponent();
				fields.put("iFMTableSchema", c);
			} catch (ClassCastException e) {
				// ignore
			}

			confirmInsert.setSelected(lastConfirmInsert);
			if (scriptFormat == ScriptFormat.INTRA_DATABASE) {
				exportLabel.setText(" Receipt*");
				jLabel3.setVisible(false);
				delete.setVisible(false);
				browseDeleteButton.setVisible(false);
			} else {
				confirmInsert.setVisible(false);
			}

			orderByPKCheckbox.setEnabled(ScriptFormat.SQL.equals(scriptFormat));
			orderByPKCheckbox.setVisible(ScriptFormat.SQL.equals(scriptFormat));

			sortedCheckBox.setEnabled(ScriptFormat.SQL.equals(scriptFormat) || ScriptFormat.INTRA_DATABASE.equals(scriptFormat) || ScriptFormat.DBUNIT_FLAT_XML.equals(scriptFormat) || ScriptFormat.LIQUIBASE_XML.equals(scriptFormat));
			sortedCheckBox.setSelected(true);
			upsertCheckbox.setEnabled(ScriptFormat.SQL.equals(scriptFormat) || ScriptFormat.INTRA_DATABASE.equals(scriptFormat));
			rowsPerThread.setEnabled(ScriptFormat.SQL.equals(scriptFormat));
			insertIncrementally.setEnabled(session.dbms.getLimitTransactionSize().getLimit() != 0);

			Map<JTextField, String> defaults = new HashMap<JTextField, String>();

			if (ScriptFormat.INTRA_DATABASE.equals(scriptFormat)) {
				jLabel8.setVisible(false);
				jPanel8.setVisible(false);
			}

			if ((!ScriptFormat.SQL.equals(scriptFormat)) && (!ScriptFormat.INTRA_DATABASE.equals(scriptFormat)) && (!ScriptFormat.DBUNIT_FLAT_XML.equals(scriptFormat)) && !ScriptFormat.LIQUIBASE_XML.equals(scriptFormat)) {
				schemaMappingPanel.setVisible(false);
				schemaMappingLabelPanel.setVisible(false);
			} else {
				schemaMappingPanel.setVisible(true);
				schemaMappingLabelPanel.setVisible(true);
				initSchemaMapping(dataModel, fields, defaults);
			}
			if ((!ScriptFormat.SQL.equals(scriptFormat)) && (!ScriptFormat.DBUNIT_FLAT_XML.equals(scriptFormat)) && !ScriptFormat.LIQUIBASE_XML.equals(scriptFormat)) {
				deleteSchemaMappingPanel.setVisible(false);
				deleteSchemaMappingLabelPanel.setVisible(false);
			} else {
				deleteSchemaMappingPanel.setVisible(true);
				deleteSchemaMappingLabelPanel.setVisible(true);
				initDeleteSchemaMapping(dataModel, fields, defaults);
			}
			initSourceSchemaMapping(dataModel, fields, defaults);
			initIsolationLevel(session);
			initScopeButtons(session);
			initTargetDBMS(session);
			
			if (session.dbms.getRowidName() == null) {
				rowidPK.setSelected(true);
				rowidRowid.setEnabled(false);
				rowidBoth.setEnabled(false);
			} else {
				rowidRowid.setText(rowidRowid.getText().replaceFirst("(<i>).*(</i>)", "$1" + session.dbms.getRowidName() + "$2"));
				rowidBoth.setText(rowidBoth.getText().replaceFirst("(<i>).*(</i>)", "$1" + session.dbms.getRowidName() + "$2"));
			}

			theSettings = new Settings(Environment.newFile(".exportdata.ui").getPath(), fields) {
				@Override
				protected void onModification(JComponent component) {
					boolean defaultValue = component == sortedCheckBox;
					Color bg = new Color(255, 255, 210);
					for (JCheckBox comp: new JCheckBox[] {
							unicode,
							sortedCheckBox,
							insertIncrementally,
							independentWorkingTables,
							upsertCheckbox,
							orderByPKCheckbox,
							transactional
					}) {
						if (component == comp) {
							if (comp.isSelected() != defaultValue) {
								comp.setBackground(bg);
							}
						}
					}
				}
			};

			theSettings.restore(settingsContext, settingsContextSecondaryKey);
			
			FocusListener fl = new FocusListener() {
				@Override
				public void focusLost(FocusEvent e) {
					if (e.getComponent() == insert || e.getComponent() == delete) {
						if (!((JTextField) e.getComponent()).getText().trim().isEmpty()) {
							((JTextField) e.getComponent()).setText(toFileName(((JTextField) e.getComponent()).getText().trim()));
						}
					}
				}
				@Override
				public void focusGained(FocusEvent e) {
				}
			};
			insert.addFocusListener(fl);
			delete.addFocusListener(fl);
			
			try {
				JTextField c;
				c = (JTextField) iFMTableSchemaComboBox.getEditor().getEditorComponent();
				if ("Item 1".equals(c.getText())) {
					c.setText(DEFAULT_SCHEMA);
				}

			} catch (ClassCastException e) {
				// ignore
			}

			for (JTextField field: defaults.keySet()) {
				if (field.getText().length() == 0) {
					field.setText(defaults.get(field));
				}
			}

			insert.setText(UIUtil.correctFileSeparator(insert.getText()));

			if (scriptFormat == ScriptFormat.INTRA_DATABASE && insert.getText().trim().length() == 0) {
				insert.setText("receipt.txt");
			}

			if (scriptFormat == ScriptFormat.INTRA_DATABASE) {
				for (Map.Entry<String, JTextField> e: schemaMappingFields.entrySet()) {
					if (e.getKey().equals(e.getValue().getText())) {
						e.getValue().setText("");
					}
				}
			}

			if (threads.getText().length() == 0) {
				threads.setText("4");
			}
			if (rowsPerThread.getText().length() == 0) {
				rowsPerThread.setText("50");
			}

			if (additionalSubjects.isEmpty()) {
				additSubsLabel.setVisible(false);
				additSubsLabelTitel.setVisible(false);
			} else {
				StringBuilder sb = new StringBuilder();
				int ll = 0;
				for (AdditionalSubject as: additionalSubjects) {
					if (sb.length() > 0) {
						sb.append(", ");
					}
					sb.append(as.getSubject().getName());
					ll += as.getSubject().getName().length();
					if (ll > 120) {
						ll = 0;
						sb.append("\n");
					}
				}
				final int MAX = 60;
				if (sb.length() > MAX) {
					additSubsLabel.setToolTipText(UIUtil.toHTML(sb.toString(), 0));
					additSubsLabel.setText(sb.toString().substring(0, MAX) + "...");
				} else {
					additSubsLabel.setText(sb.toString());
				}
			}

			subjectTable.setText(subject.getName());
			if (subjectCondition.equals(previousInitialSubjectCondition)) {
				where.setText((previousSubjectCondition));
			} else {
				where.setText((subjectCondition));
			}

			browseInsertButton.setIcon(loadIcon);
			browseDeleteButton.setIcon(loadIcon);
			browseLocalTempDirButton.setIcon(loadIcon);
			
			Consumer<JTextField> addClearButton = tf -> {
				JButton clear = new JButton(null, UIUtil.scaleIcon(tf, clearIcon));
				clear.setToolTipText("Clear");
				clear.addActionListener(e -> {
					tf.setText("");
				});
				UIUtil.setLeadingComponent(tf, clear);
			};
			// 
			addClearButton.accept(insert);
			addClearButton.accept(delete);
			addClearButton.accept(localTempDirTextField);
			
			UIUtil.setTrailingComponent(insert, browseInsertButton);
			UIUtil.setTrailingComponent(delete, browseDeleteButton);
			UIUtil.setTrailingComponent(localTempDirTextField, browseLocalTempDirButton);
			
			String lstToolTip = "<html>The folder where the local database will be stored.<br>Default temp folder is used if this field is empty.</html>";
			localTempDirLabel.setToolTipText(lstToolTip);
			localTempDirTextField.setToolTipText(lstToolTip);

			localTempDirTextField.putClientProperty(FlatClientProperties.PLACEHOLDER_TEXT, new File(Configuration.getInstance().getTempFileFolder()).getAbsolutePath());

			DocumentListener dl = new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateCLIArea();
				}
			};
			ActionListener al = new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					workingTableSchemaLabel.setVisible(!scopeLocal.isSelected());
					workingTableSchemaComboBox.setVisible(!scopeLocal.isSelected());
					localTempDirLabel.setVisible(scopeLocal.isSelected());
					localTempDirPanel.setVisible(scopeLocal.isSelected());
					updateCLIArea();
				}
			};
			al.actionPerformed(null);
			where.getDocument().addDocumentListener(dl);
			insert.getDocument().addDocumentListener(dl);
			delete.getDocument().addDocumentListener(dl);
			threads.getDocument().addDocumentListener(dl);
			localTempDirTextField.getDocument().addDocumentListener(dl);
			rowsPerThread.getDocument().addDocumentListener(dl);
			rowLimit.getDocument().addDocumentListener(dl);
			upsertCheckbox.addActionListener(al);
			insertIncrementally.addActionListener(al);
			independentWorkingTables.addActionListener(al);
			transactional.addActionListener(al);
			unicode.addActionListener(al);
			sortedCheckBox.addActionListener(al);
			orderByPKCheckbox.addActionListener(al);
			scopeGlobal.addActionListener(al);
			scopeSession.addActionListener(al);
			scopeLocal.addActionListener(al);
			rowidPK.addActionListener(al);
			rowidBoth.addActionListener(al);
			rowidRowid.addActionListener(al);
			for (JTextField field: parameterEditor.textfieldsPerParameter.values()) {
				field.getDocument().addDocumentListener(dl);
			}

			Dimension preferredSize = where.getPreferredSize();
			preferredSize.width = 10;
			where.setPreferredSize(preferredSize);

			final ConditionEditor subjectConditionEditor = new ConditionEditor(null, null, null, dataModel, null);
			subjectConditionEditor.setTitle("Subject condition");
			openWhereEditor.setIcon(conditionEditorIcon);
			openWhereEditor.setText(null);
			openWhereEditor.addMouseListener(new java.awt.event.MouseAdapter() {
				@Override
				public void mouseReleased(MouseEvent e) {
					mouseClicked(e);
				}
				@Override
				public void mouseClicked(java.awt.event.MouseEvent evt) {
					String cond = subjectConditionEditor.edit(where, where.getText(), "Subject", "T", subject, null, null, null, false, true);
					if (cond != null) {
						if (!where.getText().equals((cond))) {
							where.setText((cond));
						}
						openWhereEditor.setIcon(conditionEditorSelectedIcon);
					}
				}

				@Override
				public void mouseEntered(java.awt.event.MouseEvent evt) {
					openWhereEditor.setIcon(conditionEditorSelectedIcon);
				}
				@Override
				public void mouseExited(java.awt.event.MouseEvent evt) {
					openWhereEditor.setIcon(conditionEditorIcon);
			   }
			});
			
			UIUtil.setLeadingComponent(where, openWhereEditor);

			workingTableSchemaComboBox.setEnabled(!scopeLocal.isSelected());
			
			pack();
			cliArea.setBorder(threads.getBorder());
			updateCLIArea();
			setSize(700, getSize().height);
			cliArea.setPreferredSize(new Dimension(16, 16));
			placeholder.setVisible(false);
			placeholder1.setVisible(false);
			UIUtil.fit(this);
		} finally {
			UIUtil.resetWaitCursor(parent);
		}
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowOpened(WindowEvent e) {
				jButton1.grabFocus();
				super.windowOpened(e);
			}
		});
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

	private void unknownDBMSWarning(Session session) {
		if (DBMS.forDBMS(null) == session.dbms) {
			final String title = "Unknown DBMS";
			unknownDBMSLabel.setIcon(warnIcon);
			unknownDBMSButton.setIcon(warnIcon);
			String message = "Jailer is not configured for DBMS \"" + databaseProductName + "\".";
			unknownDBMSLabel.setText(message);
			unknownDBMSButton.addActionListener(e -> {
				JOptionPane.showMessageDialog(this,
					message + "\n" +
					"The results may not be optimal.\nFor assistance please contact:\n" + 
					"\n" + 
					"Help desk: https://github.com/Wisser/Jailer/issues\n" +
					"(or https://sourceforge.net/p/jailer/discussion )\n" + 
					"Mail: rwisser@users.sourceforge.net",
					title, JOptionPane.WARNING_MESSAGE);
			});
		} else {
			unknownDBMSPanel.setVisible(false);
		}
	}

	@SuppressWarnings({ "unchecked" })
	private void initIsolationLevel(Session session) {
		final Map<String, String> levels = new TreeMap<String, String>();
		levels.put(String.valueOf(Connection.TRANSACTION_READ_COMMITTED), "Read committed");
		levels.put(String.valueOf(Connection.TRANSACTION_READ_UNCOMMITTED), "Read uncommitted");
		levels.put(String.valueOf(Connection.TRANSACTION_REPEATABLE_READ), "Repeatable read");
		levels.put(String.valueOf(Connection.TRANSACTION_SERIALIZABLE), "Serializable");
		int dLevel = Connection.TRANSACTION_NONE;
		try {
			Object prop = session.getSessionProperty(ExportDialog.class, "TransactionIsolation");
			if (prop != null) {
				dLevel = ((Number) prop).intValue();
			} else {
				dLevel = session.getConnection().getTransactionIsolation();
			}
		} catch (SQLException e) {
			// ignore
		}
		String dd = levels.get(String.valueOf(dLevel));
		if (dd == null) {
			dd = "None";
		}
		levels.put(String.valueOf(Connection.TRANSACTION_NONE), "Default (" + dd + ")");
		isolationLevelComboBox.setModel(new DefaultComboBoxModel<String>(levels.keySet().toArray(new String[0])));
		isolationLevelComboBox.setRenderer(new DefaultListCellRenderer() {
			ListCellRenderer renderer = isolationLevelComboBox.getRenderer();
			@SuppressWarnings("rawtypes")
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				return renderer.getListCellRendererComponent(list, levels.get(value), index, isSelected, cellHasFocus);
			}
		});
		isolationLevelComboBox.setSelectedItem(String.valueOf(Connection.TRANSACTION_READ_UNCOMMITTED));
		isolationLevelComboBox.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				updateCLIArea();
			}
		});
	}

	@SuppressWarnings({ "unchecked" })
	private void initTargetDBMS(Session session) {
		resetTargetButton.setVisible(false);
		resetTargetButton.setBackground(new Color(255, 255, 210));
		resetTargetButton.setIcon(resetIcon);
		if (scriptFormat == ScriptFormat.SQL) {
			targetDBMSComboBox.setModel(new DefaultComboBoxModel<DBMS>(DBMS.values()));
			targetDBMSComboBox.setRenderer(new DefaultListCellRenderer() {
				ListCellRenderer renderer = targetDBMSComboBox.getRenderer();
				@SuppressWarnings("rawtypes")
				@Override
				public Component getListCellRendererComponent(JList list,
						Object value, int index, boolean isSelected,
						boolean cellHasFocus) {
					return renderer.getListCellRendererComponent(list,
							value instanceof DBMS? ((DBMS) value).getDisplayName() : value, index, isSelected,
							cellHasFocus);
				}
			});
			targetDBMSComboBox.setSelectedItem(sourceDBMS);
			resetTargetButton.addActionListener(e -> targetDBMSComboBox.setSelectedItem(sourceDBMS));

			Runnable updateResetButton = () -> {
				if (sourceDBMS == targetDBMSComboBox.getSelectedItem()) {
					resetTargetButton.setVisible(false);
				} else {
					resetTargetButton.setVisible(true);
				}
			};
			updateResetButton.run();
			targetDBMSComboBox.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					updateCLIArea();
					updateResetButton.run();
				}
			});
			targetDBMSComboBox.setMaximumRowCount(20);
		} else {
			targetDBMSLabel.setVisible(false);
			targetDBMSComboBox.setVisible(false);
		}
	}

	@SuppressWarnings("unchecked")
	private void initIFMTableSchemaBox(Session session, List<String> allSchemas, String defaultSchema) {
		boolean hasImportFilter = false;
		for (Table table: dataModel.getTables()) {
			for (Column column: table.getColumns()) {
				if (column.getFilter() != null && !column.getFilter().isApplyAtExport()) {
					hasImportFilter  = true;
					break;
				}
			}
			if (hasImportFilter) {
				break;
			}
		}
		if (!hasImportFilter) {
			iFMTPanel.setVisible(false);
			iFMTableSchemaComboBox.setVisible(false);
			return;
		}
		List<String> schemas = new ArrayList<String>();
		schemas.add(DEFAULT_SCHEMA);
		schemas.addAll(allSchemas);
		schemas.remove(defaultSchema);
		quoteSchemas(schemas, session);
		String[] ifmComboboxModel = schemas.toArray(new String[0]);
		setComboboxRenderer(iFMTableSchemaComboBox);
		iFMTableSchemaComboBox.setModel(new DefaultComboBoxModel(ifmComboboxModel));
		iFMTableSchemaComboBox.setSelectedItem(DEFAULT_SCHEMA);
		iFMTableSchemaComboBox.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				updateCLIArea();
			}
		});
		try {
			JTextField c = (JTextField) iFMTableSchemaComboBox.getEditor().getEditorComponent();
			c.getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateCLIArea();
				}
			});
		} catch (ClassCastException e) {
			// ignore
		}
	}

	@SuppressWarnings("unchecked")
	private void initWorkingTableSchemaBox(Session session, List<String> allSchemas, String defaultSchema) {
		List<String> schemas = new ArrayList<String>();
		schemas.add(DEFAULT_SCHEMA);
		schemas.addAll(allSchemas);
		schemas.remove(defaultSchema);
		quoteSchemas(schemas, session);
		schemaComboboxModel = schemas.toArray(new String[0]);
		setComboboxRenderer(workingTableSchemaComboBox);
		workingTableSchemaComboBox.setModel(new DefaultComboBoxModel(schemaComboboxModel));
		workingTableSchemaComboBox.setSelectedItem(DEFAULT_SCHEMA);
		workingTableSchemaComboBox.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				updateCLIArea();
			}
		});
	}

	private void quoteSchemas(List<String> schemas, Session session) {
		List<String> result = new ArrayList<String>();
		Quoting quoting;
		try {
			quoting = Quoting.getQuoting(session);
		} catch (SQLException e) {
			e.printStackTrace();
			return;
		}
		for (String schema: schemas) {
			if (DEFAULT_SCHEMA.equals(schema) || schema.equals(NO_SCHEMA_INFO)) {
				result.add(schema);
			} else {
				int iDot = schema.indexOf('.');
				if (iDot > 0) {
					String catalog = schema.substring(0, iDot);
					result.add(quoting.quote(catalog) + "." + quoting.quote(schema.substring(iDot + 1)));
				} else {
					result.add(quoting.quote(schema));
				}
			}
		}
		schemas.clear();
		schemas.addAll(result);
	}

	private void updateCLIArea() {
		for (JTextField cb: deleteSchemaMappingFields.values()) {
			cb.setEnabled(delete.isVisible() && delete.getText().trim().length() > 0);
		}

		List<String> args = new ArrayList<String>(initialArgs);
		if (args.size() > 0) {
			if (extractionModelFileName != null) {
				args.set(0, new File(extractionModelFileName).getAbsolutePath());
			} else {
				args.set(0, "<extraction model>");
			}
		}
		fillCLIArgs(args);
		String cmd = "sh jailer.sh";
		if (System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).startsWith("windows")) {
			cmd = "jailer.bat";
		}
		String cli = cmd + UIUtil.createCLIArgumentString(user, password, args, executionContext);
		cliArea.setSize(8, 8);
		cliArea.setText(cli);
		cliArea.setCaretPosition(0);
		
		SimpleAttributeSet set = new SimpleAttributeSet();
        ((DefaultStyledDocument) cliArea.getDocument()).setCharacterAttributes(0, cliArea.getText().length(), set, true);
		addStyle("\"\\<password\\>\"", Color.RED);
		addStyle("jailer\\.[^ ]+ ", Color.BLUE);
		addStyle("export|delete|import", Color.BLUE);
	}

	/**
	 * @param color
	 */
	private void addStyle(String reg, Color color) {
		SimpleAttributeSet set;
		Pattern pattern = Pattern.compile(reg);
		Matcher matcher = pattern.matcher(cliArea.getText());
		if (matcher.find()) {
			set = new SimpleAttributeSet();
            StyleConstants.setForeground(set, color);
            ((DefaultStyledDocument) cliArea.getDocument()).setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), set, true);
		}
	}

	private Thread initScopeButtonThread;
	private boolean sessionLocalIsAvailable = false;

	private Set<String> targetSchemaSet = new TreeSet<String>();

	private void initScopeButtons(final Session session) {
		DBMS configuration = session.dbms;
		sessionLocalIsAvailable = configuration.getSessionTemporaryTableManager() != null;

		scopeGlobal.setEnabled(true);
		scopeSession.setEnabled(sessionLocalIsAvailable);
		jButton1.setEnabled(true);
		scopeGlobal.setSelected(true);

		updateCLIArea();
	}

	/**
	 * Initializes the delete-schema mapping panel.
	 *
	 * @param dataModel the data model
	 * @param fields to put newly created text fields into
	 * @param defaults to put default values for newly created text fields into
	 */
	@SuppressWarnings("unchecked")
	private void initDeleteSchemaMapping(DataModel dataModel, Map<String, JComponent> fields, Map<JTextField, String> defaults) {
		Set<String> distinctSchemas = new HashSet<String>();

		Set<String> relevantSchemas = getRelevantSchemas(true);
		for (Table table: dataModel.getTables()) {
			String schema = table.getOriginalSchema(DEFAULT_SCHEMA);
			if (relevantSchemas.contains(schema.equals(DEFAULT_SCHEMA)? "" : schema)) {
				distinctSchemas.add(schema);
			}
		}

		List<String> sortedSchemaList = new ArrayList<String>(distinctSchemas);
		Collections.sort(sortedSchemaList);

		boolean simplified = sortedSchemaList.size() == 1;
		if (simplified) {
			deleteSchemaMappingPanel.setVisible(false);
			deleteSchemaMappingLabelPanel.setVisible(false);
		}

		int y = 0;
		for (String schema: sortedSchemaList) {
			JLabel b = new JLabel("  instead of ");
			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 2;
			gridBagConstraints.gridy = y;
			if (simplified) {
				b.setText(" Delete from schema ");
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = 86;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				jPanel1.add(b, gridBagConstraints);
			} else {
				deleteSchemaMappingPanel.add(b, gridBagConstraints);
			}
			JComboBox2 cb = new JComboBox2();
			cb.setMaximumRowCount(20);
			JComponent ccb = cb;
			setComboboxRenderer(cb);
			cb.setModel(new DefaultComboBoxModel(schemaComboboxModel));
			cb.setEditable(true);
			cb.setSelectedItem(schema);
			JTextField c;
			try {
				c = (JTextField) cb.getEditor().getEditorComponent();
			} catch (ClassCastException e) {
				c = new JTextField(schema);
				ccb = c;
			}
			c.getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateCLIArea();
				}
			});
			fields.put("delschema-" + schema, c);
			defaults.put(c, schema);
			deleteSchemaMappingFields.put(schema, c);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			if (simplified) {
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = 86;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				jPanel1.add(ccb, gridBagConstraints);
			} else {
				deleteSchemaMappingPanel.add(ccb, gridBagConstraints);
			}
			JLabel a = new JLabel(schema);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 3;
			gridBagConstraints.gridy = y;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			deleteSchemaMappingPanel.add(a, gridBagConstraints);
			y++;
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void setComboboxRenderer(javax.swing.JComboBox iFMTableSchemaComboBox2) {
		iFMTableSchemaComboBox2.setRenderer(new DefaultListCellRenderer() {
			ListCellRenderer renderer = iFMTableSchemaComboBox2.getRenderer();
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				return renderer.getListCellRendererComponent(list, value == NO_SCHEMA_INFO? NO_SCHEMA_INFO_LABEL : value, index, isSelected, cellHasFocus);
			}
		});
	}

	/**
	 * Initializes the schema mapping panel.
	 *
	 * @param dataModel the data model
	 * @param fields to put newly created text fields into
	 * @param defaults to put default values for newly created text fields into
	 */
	@SuppressWarnings("unchecked")
	private void initSchemaMapping(DataModel dataModel, Map<String, JComponent> fields, Map<JTextField, String> defaults) {
		Set<String> distinctSchemas = new HashSet<String>();

		Set<String> relevantSchemas = getRelevantSchemas(true);
		for (Table table: dataModel.getTables()) {
			String schema = table.getOriginalSchema(DEFAULT_SCHEMA);
			if (relevantSchemas.contains(schema.equals(DEFAULT_SCHEMA)? "" : schema)) {
				distinctSchemas.add(schema);
			}
		}

		List<String> sortedSchemaList = new ArrayList<String>(distinctSchemas);
		Collections.sort(sortedSchemaList);

		boolean simplified = sortedSchemaList.size() == 1;
		if (simplified) {
			schemaMappingPanel.setVisible(false);
			schemaMappingLabelPanel.setVisible(false);
		}

		int y = 0;
		for (String schema: sortedSchemaList) {
			JLabel a = new JLabel("  instead of " + schema);
			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = y;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			if (simplified) {
				a.setText(" Target schema ");
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = 84;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				jPanel1.add(a, gridBagConstraints);
			} else {
				schemaMappingPanel.add(a, gridBagConstraints);
			}
			JComboBox2 cb = new JComboBox2();
			cb.setMaximumRowCount(20);
			JComponent ccb = cb;
			setComboboxRenderer(cb);
			cb.setModel(new DefaultComboBoxModel(schemaComboboxModel));
			cb.setEditable(true);
			cb.setSelectedItem(schema);
			JTextField c;
			try {
				c = (JTextField) cb.getEditor().getEditorComponent();
			} catch (ClassCastException e) {
				c = new JTextField(schema);
				ccb = c;
			}
			c.getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateCLIArea();
				}
			});
			fields.put("schema-" + schema, c);
			defaults.put(c, schema);
			schemaMappingFields.put(schema, c);
			schemaMappingLabels.put(schema, a);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			if (simplified) {
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = 84;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				jPanel1.add(ccb, gridBagConstraints);
			} else {
				schemaMappingPanel.add(ccb, gridBagConstraints);
			}
			y++;
		}
	}

	/**
	 * Initializes the source schema mapping panel.
	 *
	 * @param dataModel the data model
	 * @param fields to put newly created text fields into
	 * @param defaults to put default values for newly created text fields into
	 */
	@SuppressWarnings("unchecked")
	private void initSourceSchemaMapping(DataModel dataModel, Map<String, JComponent> fields, Map<JTextField, String> defaults) {
		Set<String> distinctSchemas = new HashSet<String>();

		Set<String> relevantSchemas = getRelevantSchemas(true);
		for (Table table: dataModel.getTables()) {
			String schema = table.getOriginalSchema(DEFAULT_SCHEMA);
			if (relevantSchemas.contains(schema.equals(DEFAULT_SCHEMA)? "" : schema)) {
				distinctSchemas.add(schema);
			}
		}

		List<String> sortedSchemaList = new ArrayList<String>(distinctSchemas);
		Collections.sort(sortedSchemaList);

		boolean simplified = sortedSchemaList.size() == 1;
		if (simplified) {
			sourceSchemaMappingPanel.setVisible(false);
			sourceSchemaMappingLabelPanel.setVisible(false);
		}

		int y = 0;
		for (String schema: sortedSchemaList) {
			JLabel b = new JLabel("  instead of ");
			java.awt.GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 2;
			gridBagConstraints.gridy = y;
			if (simplified) {
				b.setText(" Source schema ");
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = 82;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				jPanel1.add(b, gridBagConstraints);
			} else {
				sourceSchemaMappingPanel.add(b, gridBagConstraints);
			}
			JComboBox2 cb = new JComboBox2();
			cb.setMaximumRowCount(20);
			JComponent ccb = cb;
			setComboboxRenderer(cb);
			cb.setModel(new DefaultComboBoxModel(schemaComboboxModel));
			cb.setEditable(true);
			cb.setSelectedItem(schema);
			JTextField c;
			try {
				c = (JTextField) cb.getEditor().getEditorComponent();
			} catch (ClassCastException e) {
				c = new JTextField(schema);
				ccb = c;
			}
			c.getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					updateCLIArea();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					updateCLIArea();
				}
			});
			fields.put("srcschema-" + schema, c);
			defaults.put(c, schema);
			sourceSchemaMappingFields.put(schema, c);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 1;
			gridBagConstraints.gridy = y;
			gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
			if (simplified) {
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = 82;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
				jPanel1.add(ccb, gridBagConstraints);
			} else {
				sourceSchemaMappingPanel.add(ccb, gridBagConstraints);
			}
			JLabel a = new JLabel(schema);
			gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 3;
			gridBagConstraints.gridy = y;
			gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
			sourceSchemaMappingPanel.add(a, gridBagConstraints);
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
        buttonGroup2 = new javax.swing.ButtonGroup();
        jScrollPane2 = new javax.swing.JScrollPane();
        jPanel6 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        sourceSchemaMappingPanel = new javax.swing.JPanel();
        jLabel19 = new javax.swing.JLabel();
        jLabel20 = new javax.swing.JLabel();
        sourceSchemaMappingLabelPanel = new javax.swing.JPanel();
        jLabel33 = new javax.swing.JLabel();
        jLabel34 = new javax.swing.JLabel();
        jLabel42 = new javax.swing.JLabel();
        schemaMappingPanel = new javax.swing.JPanel();
        jLabel14 = new javax.swing.JLabel();
        jLabel15 = new javax.swing.JLabel();
        schemaMappingLabelPanel = new javax.swing.JPanel();
        jLabel36 = new javax.swing.JLabel();
        jLabel37 = new javax.swing.JLabel();
        jLabel43 = new javax.swing.JLabel();
        deleteSchemaMappingPanel = new javax.swing.JPanel();
        jLabel31 = new javax.swing.JLabel();
        deleteSchemaMappingLabelPanel = new javax.swing.JPanel();
        jLabel39 = new javax.swing.JLabel();
        jLabel40 = new javax.swing.JLabel();
        jLabel44 = new javax.swing.JLabel();
        where = new javax.swing.JTextField();
        exportLabel = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        insert = new javax.swing.JTextField();
        delete = new javax.swing.JTextField();
        threads = new javax.swing.JTextField();
        rowsPerThread = new javax.swing.JTextField();
        placeholder = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel8 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jPanel4 = new javax.swing.JPanel();
        subjectTable = new javax.swing.JLabel();
        jLabel12 = new javax.swing.JLabel();
        jLabel16 = new javax.swing.JLabel();
        jPanel8 = new javax.swing.JPanel();
        scopeGlobal = new javax.swing.JRadioButton();
        scopeLocal = new javax.swing.JRadioButton();
        scopeSession = new javax.swing.JRadioButton();
        jLabel1 = new javax.swing.JLabel();
        jLabel26 = new javax.swing.JLabel();
        jLabel27 = new javax.swing.JLabel();
        jLabel21 = new javax.swing.JLabel();
        parameterPanel = new javax.swing.JPanel();
        commandLinePanel = new javax.swing.JPanel();
        jLabel22 = new javax.swing.JLabel();
        jLabel23 = new javax.swing.JLabel();
        jLabel24 = new javax.swing.JLabel();
        jLabel25 = new javax.swing.JLabel();
        copyButton = new javax.swing.JButton();
        cliArea = new javax.swing.JTextPane();
        placeholder1 = new javax.swing.JLabel();
        openWhereEditor = new javax.swing.JLabel();
        additSubsLabel = new javax.swing.JLabel();
        additSubsLabelTitel = new javax.swing.JLabel();
        workingTableSchemaLabel = new javax.swing.JLabel();
        workingTableSchemaComboBox = new javax.swing.JComboBox();
        jLabel17 = new javax.swing.JLabel();
        toLabel = new javax.swing.JLabel();
        targetDBMSLabel = new javax.swing.JLabel();
        iFMTableSchemaComboBox = new javax.swing.JComboBox();
        iFMTPanel = new javax.swing.JPanel();
        jLabel29 = new javax.swing.JLabel();
        jLabel30 = new javax.swing.JLabel();
        jPanel5 = new javax.swing.JPanel();
        sortedCheckBox = new javax.swing.JCheckBox();
        upsertCheckbox = new javax.swing.JCheckBox();
        transactional = new javax.swing.JCheckBox();
        independentWorkingTables = new javax.swing.JCheckBox();
        orderByPKCheckbox = new javax.swing.JCheckBox();
        unicode = new javax.swing.JCheckBox();
        confirmInsert = new javax.swing.JCheckBox();
        insertIncrementally = new javax.swing.JCheckBox();
        browseInsertButton = new javax.swing.JButton();
        browseDeleteButton = new javax.swing.JButton();
        targetDBMSLabel1 = new javax.swing.JLabel();
        jPanel3 = new javax.swing.JPanel();
        isolationLevelComboBox = new javax.swing.JComboBox();
        iLHintLabel = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();
        rowLimit = new javax.swing.JTextField();
        jLabel18 = new javax.swing.JLabel();
        rowidLabel = new javax.swing.JLabel();
        rowidPanel = new javax.swing.JPanel();
        rowidPK = new javax.swing.JRadioButton();
        rowidBoth = new javax.swing.JRadioButton();
        rowidRowid = new javax.swing.JRadioButton();
        resetTargetButton = new javax.swing.JButton();
        targetDBMSPanel = new javax.swing.JPanel();
        targetDBMSComboBox = new javax.swing.JComboBox();
        localTempDirLabel = new javax.swing.JLabel();
        localTempDirPanel = new javax.swing.JPanel();
        localTempDirTextField = new javax.swing.JTextField();
        browseLocalTempDirButton = new javax.swing.JButton();
        jPanel7 = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jButton1 = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        cancelButton = new javax.swing.JButton();
        unknownDBMSPanel = new javax.swing.JPanel();
        unknownDBMSLabel = new javax.swing.JLabel();
        unknownDBMSButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Data Export"); // NOI18N
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jScrollPane2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jPanel6.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel6.setLayout(new java.awt.GridBagLayout());

        jPanel1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        sourceSchemaMappingPanel.setLayout(new java.awt.GridBagLayout());

        jLabel19.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        sourceSchemaMappingPanel.add(jLabel19, gridBagConstraints);

        jLabel20.setText("                          "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 200;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        sourceSchemaMappingPanel.add(jLabel20, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 82;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanel1.add(sourceSchemaMappingPanel, gridBagConstraints);

        sourceSchemaMappingLabelPanel.setLayout(new java.awt.GridBagLayout());

        jLabel33.setText(" Read rows"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        sourceSchemaMappingLabelPanel.add(jLabel33, gridBagConstraints);

        jLabel34.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        sourceSchemaMappingLabelPanel.add(jLabel34, gridBagConstraints);

        jLabel42.setText(" from schema..."); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        sourceSchemaMappingLabelPanel.add(jLabel42, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 82;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(sourceSchemaMappingLabelPanel, gridBagConstraints);

        schemaMappingPanel.setLayout(new java.awt.GridBagLayout());

        jLabel14.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        schemaMappingPanel.add(jLabel14, gridBagConstraints);

        jLabel15.setText("                          "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 200;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        schemaMappingPanel.add(jLabel15, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 84;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanel1.add(schemaMappingPanel, gridBagConstraints);

        schemaMappingLabelPanel.setLayout(new java.awt.GridBagLayout());

        jLabel36.setText(" Insert rows"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        schemaMappingLabelPanel.add(jLabel36, gridBagConstraints);

        jLabel37.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        schemaMappingLabelPanel.add(jLabel37, gridBagConstraints);

        jLabel43.setText(" into schema... "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTH;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        schemaMappingLabelPanel.add(jLabel43, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 84;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(schemaMappingLabelPanel, gridBagConstraints);

        deleteSchemaMappingPanel.setLayout(new java.awt.GridBagLayout());

        jLabel31.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        deleteSchemaMappingPanel.add(jLabel31, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 86;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanel1.add(deleteSchemaMappingPanel, gridBagConstraints);

        deleteSchemaMappingLabelPanel.setLayout(new java.awt.GridBagLayout());

        jLabel39.setText(" Delete rows"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        deleteSchemaMappingLabelPanel.add(jLabel39, gridBagConstraints);

        jLabel40.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        deleteSchemaMappingLabelPanel.add(jLabel40, gridBagConstraints);

        jLabel44.setText(" from schema..."); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        deleteSchemaMappingLabelPanel.add(jLabel44, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 86;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanel1.add(deleteSchemaMappingLabelPanel, gridBagConstraints);

        where.setMaximumSize(new java.awt.Dimension(300, 2147483647));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(where, gridBagConstraints);

        exportLabel.setText(" Into*"); // NOI18N
        exportLabel.setToolTipText("add '.zip' or '.gz' extension for compressed files");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(exportLabel, gridBagConstraints);

        jLabel3.setText(" Generate delete-script* "); // NOI18N
        jLabel3.setToolTipText("add '.zip' or '.gz' extension for compressed files");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 34;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel3, gridBagConstraints);

        jLabel6.setText(" Parallel threads "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 53;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel6, gridBagConstraints);

        insert.setToolTipText("add '.zip' or '.gz' extension for compressed files");
        insert.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 1, 0);
        jPanel1.add(insert, gridBagConstraints);

        delete.setToolTipText("add '.zip' or '.gz' extension for compressed files");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 34;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 1, 0);
        jPanel1.add(delete, gridBagConstraints);

        threads.setMinimumSize(new java.awt.Dimension(44, 19));
        threads.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusLost(java.awt.event.FocusEvent evt) {
                threadsFocusLost(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 53;
        gridBagConstraints.ipadx = 30;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 1, 0);
        jPanel1.add(threads, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 54;
        gridBagConstraints.ipadx = 30;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 1, 0);
        jPanel1.add(rowsPerThread, gridBagConstraints);

        placeholder.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(placeholder, gridBagConstraints);

        jLabel4.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 30;
        jPanel1.add(jLabel4, gridBagConstraints);

        jLabel8.setText(" Working table scope"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 56;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 0, 0);
        jPanel1.add(jLabel8, gridBagConstraints);

        jLabel7.setText(" Export from"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jLabel7, gridBagConstraints);

        jLabel11.setText(" Where"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jLabel11, gridBagConstraints);

        jPanel4.setLayout(new java.awt.GridBagLayout());

        subjectTable.setFont(subjectTable.getFont().deriveFont(subjectTable.getFont().getStyle() | java.awt.Font.BOLD));
        subjectTable.setText("jLabel11"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel4.add(subjectTable, gridBagConstraints);

        jLabel12.setFont(jLabel12.getFont().deriveFont((jLabel12.getFont().getStyle() | java.awt.Font.ITALIC) & ~java.awt.Font.BOLD));
        jLabel12.setText("  as T"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel4.add(jLabel12, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 15;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jPanel4, gridBagConstraints);

        jLabel16.setText(" Rows per statement "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 54;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel16, gridBagConstraints);

        jPanel8.setLayout(new java.awt.GridBagLayout());

        buttonGroup1.add(scopeGlobal);
        scopeGlobal.setText("global tables"); // NOI18N
        scopeGlobal.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                scopeGlobalActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 56;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel8.add(scopeGlobal, gridBagConstraints);

        buttonGroup1.add(scopeLocal);
        scopeLocal.setText("local database");
        scopeLocal.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                scopeLocalActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel8.add(scopeLocal, gridBagConstraints);

        buttonGroup1.add(scopeSession);
        scopeSession.setText("temporary tables    "); // NOI18N
        scopeSession.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                scopeSessionActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 58;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel8.add(scopeSession, gridBagConstraints);

        jLabel1.setForeground(new java.awt.Color(128, 128, 128));
        jLabel1.setText("  (best for single-threaded performance)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 58;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel8.add(jLabel1, gridBagConstraints);

        jLabel26.setForeground(new java.awt.Color(128, 128, 128));
        jLabel26.setText("  (best for multi-threaded performance)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 56;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel8.add(jLabel26, gridBagConstraints);

        jLabel27.setForeground(new java.awt.Color(128, 128, 128));
        jLabel27.setText("  (no update-privilege required)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel8.add(jLabel27, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 56;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 0, 0);
        jPanel1.add(jPanel8, gridBagConstraints);

        jLabel21.setText(" With"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 24;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 0, 0);
        jPanel1.add(jLabel21, gridBagConstraints);

        parameterPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 24;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(parameterPanel, gridBagConstraints);

        commandLinePanel.setLayout(new java.awt.GridBagLayout());

        jLabel22.setText(" Command line"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        commandLinePanel.add(jLabel22, gridBagConstraints);

        jLabel23.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        commandLinePanel.add(jLabel23, gridBagConstraints);

        jLabel24.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        commandLinePanel.add(jLabel24, gridBagConstraints);

        jLabel25.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 3;
        commandLinePanel.add(jLabel25, gridBagConstraints);

        copyButton.setText("Copy to Clipboard"); // NOI18N
        copyButton.setToolTipText("Copy to Clipboard"); // NOI18N
        copyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        commandLinePanel.add(copyButton, gridBagConstraints);

        cliArea.setEditable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridheight = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 8, 0, 0);
        commandLinePanel.add(cliArea, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 95;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(commandLinePanel, gridBagConstraints);

        placeholder1.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        jPanel1.add(placeholder1, gridBagConstraints);

        openWhereEditor.setText("jLabel28");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 18;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(openWhereEditor, gridBagConstraints);

        additSubsLabel.setText(" "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(additSubsLabel, gridBagConstraints);

        additSubsLabelTitel.setText(" Additional Subjects"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(additSubsLabelTitel, gridBagConstraints);

        workingTableSchemaLabel.setText(" Working table schema "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 57;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 12, 0);
        jPanel1.add(workingTableSchemaLabel, gridBagConstraints);

        workingTableSchemaComboBox.setEditable(true);
        workingTableSchemaComboBox.setMaximumRowCount(20);
        workingTableSchemaComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        workingTableSchemaComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                workingTableSchemaComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 57;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 12, 0);
        jPanel1.add(workingTableSchemaComboBox, gridBagConstraints);

        jLabel17.setText(" To"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 19;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(jLabel17, gridBagConstraints);

        toLabel.setText(" To"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 19;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(toLabel, gridBagConstraints);

        targetDBMSLabel.setText(" Target DBMS"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 35;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(targetDBMSLabel, gridBagConstraints);

        iFMTableSchemaComboBox.setEditable(true);
        iFMTableSchemaComboBox.setMaximumRowCount(20);
        iFMTableSchemaComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        iFMTableSchemaComboBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                iFMTableSchemaComboBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 59;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        jPanel1.add(iFMTableSchemaComboBox, gridBagConstraints);

        iFMTPanel.setLayout(new java.awt.GridBagLayout());

        jLabel29.setText(" Import filter-"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        iFMTPanel.add(jLabel29, gridBagConstraints);

        jLabel30.setText(" mapping table schema "); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        iFMTPanel.add(jLabel30, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 59;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(iFMTPanel, gridBagConstraints);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        sortedCheckBox.setText("sort topologically");
        sortedCheckBox.setToolTipText("sort exported rows according to dependencies");
        sortedCheckBox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        sortedCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                sortedCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 37;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(sortedCheckBox, gridBagConstraints);

        upsertCheckbox.setText("upsert/merge statements (overwrite) for all rows"); // NOI18N
        upsertCheckbox.setToolTipText("<html>always generate upsert/merge statements: <br>update in target database existing rows, insert new rows</html>");
        upsertCheckbox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 38;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(upsertCheckbox, gridBagConstraints);

        transactional.setText("transactional"); // NOI18N
        transactional.setToolTipText("<html>Perform export in a single transaction. <br>Caution: the use of more than 1 parallel thread can cause problems.</html>");
        transactional.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        transactional.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                transactionalActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 64;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(transactional, gridBagConstraints);

        independentWorkingTables.setText("independent working tables"); // NOI18N
        independentWorkingTables.setToolTipText("<html>Create working tables that are independent of the extraction model. <br>(Potentially less efficient)</html>");
        independentWorkingTables.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        independentWorkingTables.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                independentWorkingTablesActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 49;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(independentWorkingTables, gridBagConstraints);

        orderByPKCheckbox.setText("order by primary key"); // NOI18N
        orderByPKCheckbox.setToolTipText("<html>Orders the exported rows according to the primary key. This makes the result script deterministic.<br>\n<b>Please take into account that this can affect the performance.</b></html>\n");
        orderByPKCheckbox.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 39;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(orderByPKCheckbox, gridBagConstraints);

        unicode.setText("UTF-8 encoding"); // NOI18N
        unicode.setToolTipText("generated UTF-8 encoded files");
        unicode.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        unicode.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                unicodeActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 45;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(unicode, gridBagConstraints);

        confirmInsert.setText("ask for permission to insert into target schema"); // NOI18N
        confirmInsert.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        confirmInsert.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                confirmInsertActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 46;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(confirmInsert, gridBagConstraints);

        insertIncrementally.setText("limit transaction size"); // NOI18N
        insertIncrementally.setToolTipText("<html>Collects the rows using multiple insert operations with a limited number of rows per operation.<br>Use this option if otherwise the transactions become too big.</html>");
        insertIncrementally.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        insertIncrementally.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertIncrementallyActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 48;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 0, 0);
        jPanel5.add(insertIncrementally, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 37;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 8, 0);
        jPanel1.add(jPanel5, gridBagConstraints);

        browseInsertButton.setText(" Browse..");
        browseInsertButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                browseInsertButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 30;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel1.add(browseInsertButton, gridBagConstraints);

        browseDeleteButton.setText(" Browse..");
        browseDeleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                browseDeleteButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 34;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        jPanel1.add(browseDeleteButton, gridBagConstraints);

        targetDBMSLabel1.setText(" Isolation level"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 36;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel1.add(targetDBMSLabel1, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        isolationLevelComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel3.add(isolationLevelComboBox, gridBagConstraints);

        iLHintLabel.setForeground(new java.awt.Color(128, 128, 128));
        iLHintLabel.setText("  (for all collection and export transactions)");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        jPanel3.add(iLHintLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 36;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jPanel3, gridBagConstraints);

        jLabel13.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 94;
        jPanel1.add(jLabel13, gridBagConstraints);

        rowLimit.setColumns(8);
        rowLimit.setToolTipText("<html>Maximum allowed number of exported rows, if not empty. <br>If this limit is exceeded, the export aborts with an error.<br></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.ipadx = 30;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        jPanel1.add(rowLimit, gridBagConstraints);

        jLabel18.setText("<html>&nbsp;Maximum&nbsp;number<br>&nbsp;of&nbsp;exported&nbsp;rows&nbsp;</html>"); // NOI18N
        jLabel18.setToolTipText("<html>Maximum allowed number of exported rows, if not empty. <br>If this limit is exceeded, the export aborts with an error.<br></html>");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(jLabel18, gridBagConstraints);

        rowidLabel.setText(" Row identification"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 52;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        jPanel1.add(rowidLabel, gridBagConstraints);

        rowidPanel.setLayout(new java.awt.GridBagLayout());

        buttonGroup2.add(rowidPK);
        rowidPK.setSelected(true);
        rowidPK.setText("primary key");
        rowidPK.setToolTipText("Use primary key as row identification of a table's row");
        rowidPK.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rowidPKActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 55;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        rowidPanel.add(rowidPK, gridBagConstraints);

        buttonGroup2.add(rowidBoth);
        rowidBoth.setText("<html>primary key if it exists, else <i>ROWID</i> </html>"); // NOI18N
        rowidBoth.setToolTipText("<html>If this option is selected, tables that do not have a primary key can also be exported.<br>\nPlease note that generating <i>upsert-</i>statements for these tables will still not be possible.</html>");
        rowidBoth.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rowidBothActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 56;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        rowidPanel.add(rowidBoth, gridBagConstraints);

        buttonGroup2.add(rowidRowid);
        rowidRowid.setText("<html><i>ROWID</i> pseudo column</html>"); // NOI18N
        rowidRowid.setToolTipText("<html>If this option is selected, tables that do not have a primary key can also be exported.<br>\nPlease note that generating <i>upsert-</i>statements for these tables will still not be possible.</html>");
        rowidRowid.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rowidRowidActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 58;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        rowidPanel.add(rowidRowid, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 52;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 8, 0);
        jPanel1.add(rowidPanel, gridBagConstraints);

        resetTargetButton.setText("Reset Target DBMS");
        resetTargetButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetTargetButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 35;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        jPanel1.add(resetTargetButton, gridBagConstraints);

        targetDBMSPanel.setLayout(new java.awt.GridBagLayout());

        targetDBMSComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 35;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 0);
        targetDBMSPanel.add(targetDBMSComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 35;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel1.add(targetDBMSPanel, gridBagConstraints);

        localTempDirLabel.setText(" Local storage"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 58;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 12, 0);
        jPanel1.add(localTempDirLabel, gridBagConstraints);

        localTempDirPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        localTempDirPanel.add(localTempDirTextField, gridBagConstraints);

        browseLocalTempDirButton.setText(" Browse..");
        browseLocalTempDirButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                browseLocalTempDirButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 2);
        localTempDirPanel.add(browseLocalTempDirButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 58;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 12, 0);
        jPanel1.add(localTempDirPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(jPanel1, gridBagConstraints);

        jScrollPane2.setViewportView(jPanel6);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jScrollPane2, gridBagConstraints);

        jPanel7.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel7.setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jButton1.setText(" Export Data "); // NOI18N
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 2, 2);
        jPanel2.add(jButton1, gridBagConstraints);

        jLabel2.setFont(jLabel2.getFont().deriveFont(jLabel2.getFont().getStyle() & ~java.awt.Font.BOLD));
        jLabel2.setText(" *  add '.zip' or '.gz' extension for compressed files"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        jPanel2.add(jLabel2, gridBagConstraints);

        cancelButton.setText(" Cancel ");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 2, 6);
        jPanel2.add(cancelButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 100;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        jPanel7.add(jPanel2, gridBagConstraints);

        unknownDBMSPanel.setLayout(new java.awt.GridBagLayout());

        unknownDBMSLabel.setText("jLabel5");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(2, 8, 2, 8);
        unknownDBMSPanel.add(unknownDBMSLabel, gridBagConstraints);

        unknownDBMSButton.setText("Info");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(2, 0, 2, 6);
        unknownDBMSPanel.add(unknownDBMSButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel7.add(unknownDBMSPanel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        getContentPane().add(jPanel7, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
		if (scriptFormat != ScriptFormat.INTRA_DATABASE) {
			for (JTextField f: schemaMappingFields.values()) {
				if (f.getText().trim().length() == 0) {
					f.setText(DEFAULT_SCHEMA);
				}
			}
		}
		for (JTextField f: sourceSchemaMappingFields.values()) {
			if (f.getText().trim().length() == 0) {
				f.setText(DEFAULT_SCHEMA);
			}
		}
		for (JTextField f: deleteSchemaMappingFields.values()) {
			if (f.getText().trim().length() == 0) {
				f.setText(DEFAULT_SCHEMA);
			}
		}

		Color fg = jLabel7.getForeground();
		exportLabel.setForeground(fg);

		boolean err = false;
		if (insert.getText().trim().length() == 0 && (!delete.isVisible() || delete.getText().trim().length() == 0)) {
			exportLabel.setForeground(Color.RED);
			err = true;
		}
		if (scriptFormat == ScriptFormat.INTRA_DATABASE) {
			for (Map.Entry<String, JTextField> e: schemaMappingFields.entrySet()) {
				JLabel label = schemaMappingLabels.get(e.getKey());
				if (label != null) {
					if (e.getValue().getText().trim().length() == 0) {
						label.setForeground(Color.RED);
						err = true;
					} else {
						label.setForeground(fg);
					}
				}
			}
		}
		if (err) {
			JOptionPane.showMessageDialog(this, "Unfilled mandatory fields", "Error", JOptionPane.ERROR_MESSAGE);
		} else {
			if (defaultExportFileName != null) {
				defaultExportFileName.setLength(0);
				defaultExportFileName.append(insert.getText().trim());
			}
			if (rowidPK.isVisible() && rowidPK.isSelected()) {
 				if (!checkForPKs(rowidBoth.isEnabled() && !upsertCheckbox.isSelected() && getDeleteFileName() == null? rowidBoth : null, () -> theSettings.save(settingsContext, settingsContextSecondaryKey))) {
					return;
				}
			} else if (rowidPK.isVisible()) {
				if (getDeleteFileName() != null) {
					JOptionPane.showMessageDialog(this, "Need row identification \"primary key\" to generate a delete script.", "Error", JOptionPane.ERROR_MESSAGE);
					return;
				}
				if (upsertCheckbox.isSelected()) {
					JOptionPane.showMessageDialog(this, "Need row identification \"primary key\" to generate upsert-statements.", "Error", JOptionPane.ERROR_MESSAGE);
					return;
				}
			}
			theSettings.save(settingsContext, settingsContextSecondaryKey);
			UIUtil.setWaitCursor(this);
			boolean cwt;
			try {
				cwt = createWorkingTables();
			} finally {
				UIUtil.resetWaitCursor(this);
			}
			if (cwt) {
				isOk = true;
				lastConfirmInsert = confirmInsert.isSelected();
				setVisible(false);
			}
		}
	}//GEN-LAST:event_jButton1ActionPerformed

	protected abstract boolean checkForPKs(JRadioButton radioButton, Runnable saveSettings);

	private boolean createWorkingTables() {
		List<String> ddlArgs = new ArrayList<String>();
		ddlArgs.add("create-ddl");
		dbConnectionDialog.addDbArgs(ddlArgs);
		ddlArgs.add(tmpFileName != null? tmpFileName : jmFile);
		if (isIndependentWorkingTablesSelected()) {
			ddlArgs.add("-independent-working-tables");
			String delFile = getDeleteFileName();
			if (delFile != null) {
				ddlArgs.add("-d");
				ddlArgs.add(delFile);
			}
		}
		if (isUseRowId()) {
			ddlArgs.add("-use-rowid");
		}
		if (isUseRowIdsOnlyForTablesWithoutPK()) {
			ddlArgs.add("-use-rowid-if-needed");
		}
		if (getWorkingTableSchema() != null) {
			ddlArgs.add("-working-table-schema");
			ddlArgs.add(getWorkingTableSchema());
		}
		ExecutionContext cDDLExecutionContext = new ExecutionContext(executionContext);
		cDDLExecutionContext.setIndependentWorkingTables(isIndependentWorkingTablesSelected());
		cDDLExecutionContext.setUseRowid(isUseRowId());
		cDDLExecutionContext.setUseRowIdsOnlyForTablesWithoutPK(isUseRowIdsOnlyForTablesWithoutPK());

		DDLCreator ddlCreator = new DDLCreator(cDDLExecutionContext);
		BasicDataSource dataSource;
		String hint =
				"Possible solutions:\n" +
				"  - choose working table scope \"local database\"\n" +
				"  - choose another working table schema\n" +
				"  - execute the Jailer-DDL manually (see below)\n ";
		try {
			if (!cDDLExecutionContext.isIndependentWorkingTables()) {
				PrimaryKeyFactory.createUPKScope(tmpFileName != null? tmpFileName : jmFile, cDDLExecutionContext);
			}
			dataSource = UIUtil.createBasicDataSource(this, ddlArgs.get(1), ddlArgs.get(2), ddlArgs.get(3), ddlArgs.get(4), 0, dbConnectionDialog.currentJarURLs());
			String tableInConflict = getTemporaryTableScope().equals(WorkingTableScope.GLOBAL)? UIUtil.getDDLTableInConflict(ddlCreator, ExportDialog.this, dataSource, dataSource.dbms) : null;
			if (tableInConflict != null && getTemporaryTableScope().equals(WorkingTableScope.GLOBAL)) {
				JOptionPane.showMessageDialog(this, "Can't drop table '" + tableInConflict + "' as it is not created by Jailer.\nDrop or rename this table first.", "Error", JOptionPane.ERROR_MESSAGE);
			} else {
				if (!getTemporaryTableScope().equals(WorkingTableScope.GLOBAL) || UIUtil.isDDLUptodate(ddlCreator, ExportDialog.this, dataSource, dataSource.dbms, isUseRowId(), isUseRowIdsOnlyForTablesWithoutPK(), getWorkingTableSchema())) {
					return true;
				} else {
					try {
						DDLCreator.uPKWasTooLong = false;
						boolean result = UIUtil.runJailer(this, ddlArgs, false,
							false, true,
							null, dbConnectionDialog.getUser(), dbConnectionDialog.getPassword(), null,
							null, false, false, true, false, true, null, null, false, executionContext);
						if (DDLCreator.uPKWasTooLong) {
							JOptionPane.showMessageDialog(
									this,
									"Universal Primary Key is too big to be indexed.\nPerformance could suffer.\n\n"
									+ "Solution: Try to reduce the number of primary key columns in the tables.",
									"Universal Primaray Key is big", JOptionPane.INFORMATION_MESSAGE);
						}
						return result;
					} catch (Exception e) {
						Throwable cause = e;
						while (cause != null && !(cause instanceof SqlException) && cause.getCause() != null && cause.getCause() != cause) {
							cause = cause.getCause();
						}
						if (cause instanceof SqlException) {
							SqlException sqlEx = (SqlException) cause;
							UIUtil.showException(this, "Error", new SqlException("Automatic creation of working-tables failed!\n" + hint + "\n\nCause: " + sqlEx.message + "", DDLCreator.lastDDL, null));
						} else {
							UIUtil.showException(this, "Error", e);
						}
					}
				}
			}
		} catch (Exception e) {
			UIUtil.showException(this, "Error", e);
		}
		return false;
	}

	private void scopeGlobalActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_scopeGlobalActionPerformed
	}//GEN-LAST:event_scopeGlobalActionPerformed

	private void copyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyButtonActionPerformed
		cliArea.selectAll();
		cliArea.copy();
		updateCLIArea();
	}//GEN-LAST:event_copyButtonActionPerformed

	private void sortedCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_sortedCheckBoxActionPerformed
	}//GEN-LAST:event_sortedCheckBoxActionPerformed

	private void scopeSessionActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_scopeSessionActionPerformed
	}//GEN-LAST:event_scopeSessionActionPerformed

	private void unicodeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_unicodeActionPerformed
	}//GEN-LAST:event_unicodeActionPerformed

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		dispose();
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void threadsFocusLost(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_threadsFocusLost
		String text = threads.getText().trim();
		if (text.length() > 0) {
			try {
				int n = Integer.parseInt(text);
				if (n > 10000) {
					threads.setText("10000");
				}
			} catch (NumberFormatException e) {
				threads.setText("");
			}
		}
	}//GEN-LAST:event_threadsFocusLost

	private void workingTableSchemaComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_workingTableSchemaComboBoxActionPerformed
	}//GEN-LAST:event_workingTableSchemaComboBoxActionPerformed

	private void scopeLocalActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_scopeLocalActionPerformed
	}//GEN-LAST:event_scopeLocalActionPerformed

	private void confirmInsertActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_confirmInsertActionPerformed
	}//GEN-LAST:event_confirmInsertActionPerformed

	private void iFMTableSchemaComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_iFMTableSchemaComboBoxActionPerformed
	}//GEN-LAST:event_iFMTableSchemaComboBoxActionPerformed

	private void browseInsertButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_browseInsertButtonActionPerformed
		String fn = UIUtil.choseFile(null, ".", scriptFormat.getFileChooserTitle(), scriptFormat.getFileExtension(), ExportDialog.this, true, false);
		if (fn != null) {
			insert.setText(fn);
		}
	}//GEN-LAST:event_browseInsertButtonActionPerformed

	private void browseDeleteButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_browseDeleteButtonActionPerformed
		String fn = UIUtil.choseFile(null, ".", "SQL Delete Script", ".sql", ExportDialog.this, true, false);
		if (fn != null) {
			delete.setText(fn);
		}
	}//GEN-LAST:event_browseDeleteButtonActionPerformed

    private void insertIncrementallyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertIncrementallyActionPerformed
    }//GEN-LAST:event_insertIncrementallyActionPerformed

    private void transactionalActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_transactionalActionPerformed
        iLHintLabel.setVisible(!transactional.isSelected());
    }//GEN-LAST:event_transactionalActionPerformed

    private void independentWorkingTablesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_independentWorkingTablesActionPerformed
    }//GEN-LAST:event_independentWorkingTablesActionPerformed

    private void insertActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertActionPerformed
    }//GEN-LAST:event_insertActionPerformed

    private void rowidRowidActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rowidRowidActionPerformed
    }//GEN-LAST:event_rowidRowidActionPerformed

    private void rowidBothActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rowidBothActionPerformed
    }//GEN-LAST:event_rowidBothActionPerformed

    private void rowidPKActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rowidPKActionPerformed
    }//GEN-LAST:event_rowidPKActionPerformed

    private void resetTargetButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_resetTargetButtonActionPerformed
    }//GEN-LAST:event_resetTargetButtonActionPerformed

    private void browseLocalTempDirButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_browseLocalTempDirButtonActionPerformed
    	JFileChooser fc = new JFileChooser();
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		fc.setDialogTitle("Local database storage folder");
		int returnVal = fc.showOpenDialog(this);
	    if (returnVal == JFileChooser.APPROVE_OPTION) {
	    	localTempDirTextField.setText(fc.getSelectedFile().getAbsolutePath());
	    }
	}//GEN-LAST:event_browseLocalTempDirButtonActionPerformed

	public boolean isOk() {
		return isOk;
	}

	/**
	 * Gets name of delete-script, or <code>null</code>, if there is no delete-script.
	 */
	public String getDeleteFileName() {
		if (delete.isVisible() && delete.getText().trim().length() > 0) {
			return toFileName(delete.getText().trim());
		}
		return null;
	}

	public boolean isIndependentWorkingTablesSelected() {
		return independentWorkingTables.isSelected();
	}

	public boolean insertScripFileNameFieldIsEmpty() {
		return insert.getText().trim().length() == 0;
	}

	public String getInsertFileName() {
		return insert.getText().trim().length() == 0 ? null : insert.getText().trim();
	}

	/**
	 * Fills field content into cli-args.
	 *
	 * @param args the argument-list to fill
	 */
	public void fillCLIArgs(List<String> args) {
		boolean opKnown = false;
		if (insert.getText().trim().length() > 0) {
			args.add(0, "export");
			args.add("-e");
			args.add(toFileName(insert.getText()));
			opKnown = true;
		} else {
			if (delete.isVisible() && delete.getText().trim().length() > 0) {
				args.add(0, "delete");
				opKnown = true;
			}
		}
		copyButton.setEnabled(opKnown);
		if (delete.isVisible() && delete.getText().trim().length() > 0) {
			args.add("-d");
			args.add(toFileName(delete.getText().trim()));
		}
		if (insertIncrementally.isSelected()) {
			args.add("-limit-transaction-size");
		}
		if (isIndependentWorkingTablesSelected()) {
			args.add("-independent-working-tables");
		}
		if (transactional.isSelected()) {
			args.add("-transactional");
		}
		Object isolationLevel = isolationLevelComboBox.getSelectedItem();
		if (isolationLevel != null && !String.valueOf(Connection.TRANSACTION_NONE).equals(isolationLevel)) {
			args.add("-isolation-level");
			args.add(isolationLevel.toString());
		}
		if (unicode.isSelected()) {
			args.add("-UTF8");
		}
		if (upsertCheckbox.isSelected()) {
			args.add("-upsert-only");
		}
		if (!sortedCheckBox.isSelected()) {
			args.add("-no-sorting");
		}
		if (orderByPKCheckbox.isSelected()) {
			args.add("-order-by-pk");
		}
		if (sourceDBMS.getRowidName() != null) {
			if (rowidRowid.isSelected()) {
				args.add("-use-rowid");
			}
			if (rowidBoth.isSelected()) {
				args.add("-use-rowid-if-needed");
			}
		}
		if (scriptFormat == ScriptFormat.SQL) {
			Object selectedItem = targetDBMSComboBox.getSelectedItem();
			if (selectedItem instanceof DBMS) {
				DBMS targetDBMS = (DBMS) selectedItem;
				if (targetDBMS != sourceDBMS) {
					args.add("-target-dbms");
					args.add(targetDBMS.getId());
				}
			}
		}
		try {
			int nt = Integer.parseInt(threads.getText().trim());
			if (nt > 0) {
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
		try {
			long nt = Long.parseLong(rowLimit.getText().trim());
			if (nt > 0) {
				args.add("-row-limit");
				args.add("" + nt);
			}
		} catch (Exception e) {
		}

		if (!where.getText().equals(subjectCondition)) {
			args.add("-where");
			args.add((where.getText()).replace('\n', ' ').replace('\r', ' '));
		}

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
		} else if (scriptFormat.isObjectNotation()) {
			args.add(dataModel.getXmlSettings().rootTag);
			args.add("-date-format");
			args.add(dataModel.getXmlSettings().datePattern);
			args.add("-timestamp-format");
			args.add(dataModel.getXmlSettings().timestampPattern);
		}

		targetSchemaSet.clear();
		StringBuilder schemaMapping = new StringBuilder();
		for (String schema: schemaMappingFields.keySet()) {
			String to = schemaMappingFields.get(schema).getText().trim();
			targetSchemaSet.add(to);
			if (to.equals(DEFAULT_SCHEMA)) {
				to = "";
			}
			if (schemaMapping.length() > 0) {
				schemaMapping.append(",");
			}
			schemaMapping.append((schema.equals(DEFAULT_SCHEMA)? "" : schema) + "=" + to);
		}
		if (!isIdentity(schemaMapping)) {
			args.add("-schemamapping");
			args.add(schemaMapping.toString());
		}

		StringBuilder parameter = new StringBuilder();
		for (String p: parameterEditor.textfieldsPerParameter.keySet()) {
			String v = parameterEditor.textfieldsPerParameter.get(p).getText().trim();
			if (parameter.length() > 0) {
				parameter.append(";");
			}
			parameter.append(p + "=" + CsvFile.encodeCell(v));
		}
		if (parameter.length() > 0) {
			args.add("-parameters");
			args.add(parameter.toString());
		}

		StringBuilder sourceSchemaMapping = new StringBuilder();
		for (String schema: sourceSchemaMappingFields.keySet()) {
			String to = sourceSchemaMappingFields.get(schema).getText().trim();
			if (to.equals(DEFAULT_SCHEMA)) {
				to = "";
			}
			if (sourceSchemaMapping.length() > 0) {
				sourceSchemaMapping.append(",");
			}
			sourceSchemaMapping.append((schema.equals(DEFAULT_SCHEMA)? "" : schema) + "=" + to);
		}
		if (!isIdentity(sourceSchemaMapping)) {
			args.add("-source-schemamapping");
			args.add(sourceSchemaMapping.toString());
		}

		if (delete.isVisible() && delete.getText().trim().length() > 0) {
			StringBuilder deleteSchemaMapping = new StringBuilder();
			for (String schema: deleteSchemaMappingFields.keySet()) {
				String to = deleteSchemaMappingFields.get(schema).getText().trim();
				if (to.equals(DEFAULT_SCHEMA)) {
					to = "";
				}
				if (deleteSchemaMapping.length() > 0) {
					deleteSchemaMapping.append(",");
				}
				deleteSchemaMapping.append((schema.equals(DEFAULT_SCHEMA)? "" : schema) + "=" + to);
			}
			if (!isIdentity(deleteSchemaMapping)) {
				args.add("-deletion-schemamapping");
				args.add(deleteSchemaMapping.toString());
			}
		}

		args.add("-scope");
		args.add(getTemporaryTableScope().toString());

		String schema = (String) workingTableSchemaComboBox.getEditor().getItem();
		if (schema != null && schema.length() > 0 && !schema.equals(DEFAULT_SCHEMA) && !scopeLocal.isSelected()) {
			args.add("-working-table-schema");
			args.add(schema);
		}

		if (scopeLocal.isSelected() && !localTempDirTextField.getText().trim().isEmpty()) {
			args.add("-local-database-storage");
			args.add(localTempDirTextField.getText().trim());
		}
		
		if (iFMTableSchemaComboBox.isVisible()) {
			try {
				JTextField c = (JTextField) iFMTableSchemaComboBox.getEditor().getEditorComponent();
				String ifmItem = c.getText().trim();
				if (ifmItem != null && !"".equals(ifmItem) && !DEFAULT_SCHEMA.equals(ifmItem)) {
					args.add("-import-filter-mapping-table-schema");
					args.add(ifmItem.toString());
				}
			} catch (ClassCastException e) {
				// ignore
			}
		}
	}

	private boolean isIdentity(StringBuilder map) {
		if (map.length() == 0) {
			return true;
		}
		if ("=".contentEquals(map)) {
			return true;
		}
		return false;
	}

	private String toFileName(String f) {
		if (!new File(f).isAbsolute()) {
			return Environment.newFile(f).getAbsolutePath();
		}
		return f;
	}

	private Set<String> getRelevantSchemas(boolean withDelete) {
		Set<Table> closure = closureOfSubjects();

		if (withDelete) {
			Set<Table> border = new HashSet<Table>();
			for (Table table: closure) {
				for (Association a: table.associations) {
					if (!a.reversalAssociation.isIgnored()) {
						border.add(a.destination);
					}
				}
			}
			closure.addAll(border);
		}
		Set<String> relevantSchemas = new HashSet<String>();
		for (Table table: closure) {
			relevantSchemas.add(table.getOriginalSchema(""));
		}
		return relevantSchemas;
	}

	private Set<Table> closureOfSubjects() {
		Set<Table> subjects = new HashSet<Table>();
		subjects.add(subject);
		if (additionalSubjects != null) {
			for (AdditionalSubject as: additionalSubjects) {
				subjects.add(as.getSubject());
			}
		}

		Set<Table> closure = new HashSet<Table>();

		for (Table subject: subjects) {
			Set<Table> toCheck = new HashSet<Table>(subject.closure(closure));
			closure.addAll(toCheck);
		}
		DataModel.addRestrictedDependencyWithNulledFK(closure);
		return closure;
	}

	public Set<String> getTargetSchemaSet() {
		return targetSchemaSet;
	}

	public boolean getConfirmExport() {
		return confirmInsert.isSelected();
	}

	public boolean isUseRowId() {
		return rowidRowid.isSelected();
	}

	public boolean isUseRowIdsOnlyForTablesWithoutPK() {
		return rowidBoth.isSelected();
	}

	public String getWorkingTableSchema() {
		String schema = (String) workingTableSchemaComboBox.getEditor().getItem();
		if (schema.length() > 0 && !schema.equals(DEFAULT_SCHEMA)) {
			return schema;
		}
		return null;
	}

	public WorkingTableScope getTemporaryTableScope() {
		if (scopeLocal.isSelected()) {
			return WorkingTableScope.LOCAL_DATABASE;
		}
		if (scopeSession.isSelected()) {
			return WorkingTableScope.SESSION_LOCAL;
		}
//    	if (scopeTransaction.isSelected()) {
//    		return TemporaryTableScope.TRANSACTION_LOCAL;
//    	}
		return WorkingTableScope.GLOBAL;
	}

	public boolean hasDeleteScript() {
		return delete.isVisible() && delete.getText().trim().length() > 0;
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel additSubsLabel;
    private javax.swing.JLabel additSubsLabelTitel;
    private javax.swing.JButton browseDeleteButton;
    private javax.swing.JButton browseInsertButton;
    private javax.swing.JButton browseLocalTempDirButton;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JButton cancelButton;
    private javax.swing.JTextPane cliArea;
    public javax.swing.JPanel commandLinePanel;
    public javax.swing.JCheckBox confirmInsert;
    private javax.swing.JButton copyButton;
    private javax.swing.JTextField delete;
    private javax.swing.JPanel deleteSchemaMappingLabelPanel;
    private javax.swing.JPanel deleteSchemaMappingPanel;
    private javax.swing.JLabel exportLabel;
    private javax.swing.JPanel iFMTPanel;
    private javax.swing.JComboBox iFMTableSchemaComboBox;
    private javax.swing.JLabel iLHintLabel;
    public javax.swing.JCheckBox independentWorkingTables;
    private javax.swing.JTextField insert;
    public javax.swing.JCheckBox insertIncrementally;
    private javax.swing.JComboBox isolationLevelComboBox;
    private javax.swing.JButton jButton1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel27;
    private javax.swing.JLabel jLabel29;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel30;
    private javax.swing.JLabel jLabel31;
    private javax.swing.JLabel jLabel33;
    private javax.swing.JLabel jLabel34;
    private javax.swing.JLabel jLabel36;
    private javax.swing.JLabel jLabel37;
    private javax.swing.JLabel jLabel39;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel40;
    private javax.swing.JLabel jLabel42;
    private javax.swing.JLabel jLabel43;
    private javax.swing.JLabel jLabel44;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JLabel localTempDirLabel;
    private javax.swing.JPanel localTempDirPanel;
    private javax.swing.JTextField localTempDirTextField;
    private javax.swing.JLabel openWhereEditor;
    private javax.swing.JCheckBox orderByPKCheckbox;
    private javax.swing.JPanel parameterPanel;
    private javax.swing.JLabel placeholder;
    private javax.swing.JLabel placeholder1;
    private javax.swing.JButton resetTargetButton;
    private javax.swing.JTextField rowLimit;
    private javax.swing.JRadioButton rowidBoth;
    private javax.swing.JLabel rowidLabel;
    private javax.swing.JRadioButton rowidPK;
    private javax.swing.JPanel rowidPanel;
    private javax.swing.JRadioButton rowidRowid;
    private javax.swing.JTextField rowsPerThread;
    private javax.swing.JPanel schemaMappingLabelPanel;
    private javax.swing.JPanel schemaMappingPanel;
    private javax.swing.JRadioButton scopeGlobal;
    private javax.swing.JRadioButton scopeLocal;
    private javax.swing.JRadioButton scopeSession;
    private javax.swing.JCheckBox sortedCheckBox;
    public javax.swing.JPanel sourceSchemaMappingLabelPanel;
    public javax.swing.JPanel sourceSchemaMappingPanel;
    private javax.swing.JLabel subjectTable;
    private javax.swing.JComboBox targetDBMSComboBox;
    private javax.swing.JLabel targetDBMSLabel;
    private javax.swing.JLabel targetDBMSLabel1;
    private javax.swing.JPanel targetDBMSPanel;
    private javax.swing.JTextField threads;
    private javax.swing.JLabel toLabel;
    public javax.swing.JCheckBox transactional;
    public javax.swing.JCheckBox unicode;
    private javax.swing.JButton unknownDBMSButton;
    private javax.swing.JLabel unknownDBMSLabel;
    private javax.swing.JPanel unknownDBMSPanel;
    private javax.swing.JCheckBox upsertCheckbox;
    private javax.swing.JTextField where;
    private javax.swing.JComboBox workingTableSchemaComboBox;
    private javax.swing.JLabel workingTableSchemaLabel;
    // End of variables declaration//GEN-END:variables

	private Icon loadIcon;
	private Icon conditionEditorIcon;
	private ImageIcon clearIcon;
	private Icon conditionEditorSelectedIcon;
	private Icon runIcon;
	private Icon resetIcon;
	private ImageIcon cancelIcon;
	private ImageIcon copyIcon;
	private ImageIcon warnIcon;
	
	{
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        clearIcon = UIUtil.readImage("/clear.png");
        loadIcon = UIUtil.readImage("/load2.png");
		conditionEditorIcon = UIUtil.readImage("/edit.png");
		conditionEditorSelectedIcon = UIUtil.readImage("/edit_s.png");
        runIcon = UIUtil.readImage("/run.png");
        resetIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/reset.png"));
        copyIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png"));
        warnIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/wanr.png"));
	}

}


// TODO 1 test: cycle in aggregations
// TODO 1 user option: include (or not) any non-subject top-level object

