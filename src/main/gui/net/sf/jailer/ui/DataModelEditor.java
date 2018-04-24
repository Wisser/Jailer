/*
 * Copyright 2007 - 2018 the original author or authors.
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
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.JailerVersion;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.modelbuilder.KnownIdentifierMap;
import net.sf.jailer.modelbuilder.ModelBuilder;
import net.sf.jailer.util.CsvFile;
import net.sf.jailer.util.CsvFile.Line;
import net.sf.jailer.util.CsvFile.LineFilter;
import net.sf.jailer.util.PrintUtil;

/**
 * Data Model Editor.
 *
 * @author Ralf Wisser
 */
public class DataModelEditor extends javax.swing.JDialog {

	public static final String DATA_MODEL_EDITOR_AUTHOR = "Data Model Editor";

	/**
	 * List of tables.
	 */
	private List<CsvFile.Line> tables;
	
	/**
	 * Maps table names to display names.
	 */
	private Map<String, String> displayNames;
	
	/**
	 * List of associations.
	 */
	private List<CsvFile.Line> associations;
	
	/**
	 * Table- and association definitions from model-finder result files.
	 */
	private List<CsvFile.Line> linesFromModelFinder = new ArrayList<CsvFile.Line>();

	/**
	 * Set of tables with modified columns.
	 */
	private Set<String> modifiedColumnTables = new HashSet<String>();
	
	/**
	 * Columns for each table.
	 */
	private Map<String, CsvFile.Line> columns = new TreeMap<String, Line>();
	
	/**
	 * List of tables to be excluded from deletion.
	 */
	private List<String> excludeFromDeletion = new ArrayList<String>();
	
	/**
	 * <code>true</code> iff model is modified.
	 */
	private boolean needsSave = false;
	
	/**
	 * <code>true</code> iff model is saved.
	 */
	public boolean saved = false;

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;
	
	/** 
	 * Creates new form DataModelEditor.
	 * 
	 * @param toEdit if not null, open table editor for this table immediately
	 * @param assocFilter 
	 * @param tableFilter 
	 */
	public DataModelEditor(java.awt.Frame parent, boolean merge, boolean initiallyDirty, final Table toEdit, LineFilter tableFilter, LineFilter assocFilter, String modelname, String modelnameSuggestion, ExecutionContext executionContext) throws Exception {
		super(parent, true);
		this.executionContext = executionContext;
		
		KnownIdentifierMap knownIdentifierMap = createKnownIdentifierMap();
		
		int newTables = 0;
		int newAssociations = 0;
		
		tables = new CsvFile(new File(DataModel.getTablesFile(executionContext)), tableFilter).getLines();
		for (Iterator<CsvFile.Line> i = tables.iterator(); i.hasNext(); ) {
			CsvFile.Line t = i.next();
			if (t.cells.get(0).length() > 0) {
				String newName = knownIdentifierMap.getTableName(t.cells.get(0));
				if (newName != null && !newName.equals(t.cells.get(0))) {
					i.remove();
					--newTables;
				}
			}
		}
		associations = new CsvFile(new File(DataModel.getAssociationsFile(executionContext)), assocFilter).getLines();
		for (Iterator<CsvFile.Line> i = associations.iterator(); i.hasNext(); ) {
			CsvFile.Line t = i.next();
			if (t.cells.get(0).length() > 0) {
				String source = knownIdentifierMap.getTableName(t.cells.get(0));
				String dest = knownIdentifierMap.getTableName(t.cells.get(1));
				String cond = knownIdentifierMap.getCondition(t.cells.get(4));
				if (source != null) {
					t.cells.set(0, source);
				}
				if (dest != null) {
					t.cells.set(1, dest);
				}
				if (cond != null) {
					t.cells.set(4, cond);
				}
			}
		}

		PrintUtil.loadTableList(excludeFromDeletion, DataModel.getExcludeFromDeletionFile(executionContext));
		displayNames = new TreeMap<String, String>();
		File dnFile = new File(DataModel.getDisplayNamesFile(executionContext));
		if (dnFile.exists()) {
			for (CsvFile.Line dnl: new CsvFile(dnFile).getLines()) {
				displayNames.put(dnl.cells.get(0), dnl.cells.get(1));
			}
		}
		
		File file = new File(DataModel.getColumnsFile(executionContext));
		if (file.exists()) {
			for (CsvFile.Line l: new CsvFile(file).getLines()) {
				columns.put(l.cells.get(0), l);
			}
		}
		
		File modelFinderTablesFile = new File(ModelBuilder.getModelBuilderTablesFilename(executionContext));
		if (merge && modelFinderTablesFile.exists()) {
			List<CsvFile.Line> tablesFromModelFinder = new CsvFile(modelFinderTablesFile).getLines();
			for (Iterator<CsvFile.Line> i = tablesFromModelFinder.iterator(); i.hasNext(); ) {
				CsvFile.Line t = i.next();
				for (CsvFile.Line l: tables) {
					boolean eq = true;
					int n = 0;
					while (n < l.cells.size() && n < t.cells.size()) {
						if (n != 1 // ignore upsert flag 
							&& !l.cells.get(n).equals(t.cells.get(n))) {
							eq = false;
							break;
						}
						if (l.cells.get(n).length() == 0) {
							break;
						}
						++n;
					}
					if (eq) {
						i.remove();
						break;
					}
				}
			}
			linesFromModelFinder.addAll(tablesFromModelFinder);
			for (Iterator<CsvFile.Line> i = tables.iterator(); i.hasNext(); ) {
				CsvFile.Line t = i.next();
				for (CsvFile.Line l: linesFromModelFinder) {
					if (l.cells.get(0).equals(t.cells.get(0))) {
						i.remove();
						break;
					}
				}
			}
			tables.addAll(tablesFromModelFinder);
			newTables += tablesFromModelFinder.size();
		}
		sortLineList(tables, true);
		File modelFinderAssociationsFile = new File(ModelBuilder.getModelBuilderAssociationsFilename(executionContext));
		if (merge && modelFinderAssociationsFile.exists()) {
			Set<String> allNames = new HashSet<String>();
			for (Line as: associations) {
				allNames.add(as.cells.get(5));
			}
			List<CsvFile.Line> associationsFromModelFinder = new CsvFile(modelFinderAssociationsFile).getLines();
			for (Iterator<Line> it = associationsFromModelFinder.iterator(); it.hasNext(); ) {
				if (allNames.contains(it.next().cells.get(5))) {
					it.remove();
				}
			}
			associations.addAll(associationsFromModelFinder);
			linesFromModelFinder.addAll(associationsFromModelFinder);
			newAssociations += associationsFromModelFinder.size();
		}
		
		sortLineList(associations, false);
		initComponents();
		
		String modelpath = executionContext.getQualifiedDatamodelFolder();
		try {
			modelpath = new File(modelpath).getAbsolutePath();
		} catch (Throwable t) {
			// use default modelpath
		}
		locationLabel.setText(modelpath);
		locationLabel.setToolTipText(modelpath);
		
		if (modelnameSuggestion != null) {
			if (DataModel.DEFAULT_NAME.equals(modelname) || "Demo".equals(modelname)) {
				modelname = modelnameSuggestion;
			}
		}
		
		nameTextField.setText(modelname);
		nameTextField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent arg0) {
				markDirty();
			}
			@Override
			public void insertUpdate(DocumentEvent arg0) {
				markDirty();
			}
			@Override
			public void changedUpdate(DocumentEvent arg0) {
				markDirty();
			}
		});
		
		UIUtil.wireComponentWithButton(tablesTable, editTable);
		UIUtil.wireComponentWithButton(associationsTable, editAssociation);
		
		setSize(1000, 700);
		setLocation(100, 32);

		File modelFinderColumnFile = new File(ModelBuilder.getModelBuilderColumnsFilename(executionContext));
		if (merge && modelFinderColumnFile.exists()) {
			for (CsvFile.Line l: new CsvFile(modelFinderColumnFile).getLines()) {
				CsvFile.Line ol = columns.get(l.cells.get(0));
				if (ol == null || !ol.cells.equals(l.cells)) {
					modifiedColumnTables.add(l.cells.get(0));
					markDirty();
				}
				columns.put(l.cells.get(0), l);
			}
		}
		if (merge) {
			info.setText("Found " + newTables + " new tables and " + newAssociations + " new associations");
			if (!linesFromModelFinder.isEmpty()) {
				markDirty();
			}
		}
		info.setVisible(merge);
		updateButtons();
		
		final Color BG_COLOR = new Color(0.8f, 1.0f, 0.7f);
		final Color BG_SELCOLOR = new Color(0.45f, 0.85f, 1.0f);
		final Color BG1 = new Color(255, 255, 255);
		final Color BG2 = new Color(240, 255, 255);

		TableCellRenderer tablesListItemRenderer = new DefaultTableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value,
                    boolean isSelected, boolean hasFocus, int row, int column) {
				boolean fromModelFinder = linesFromModelFinder.contains(value);
				if (!(value instanceof CsvFile.Line)) {
					return super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				}
				CsvFile.Line line = (CsvFile.Line) value;
				String tableName = line.cells.get(0);
				String pk = "";
				for (int i = 2; i < line.length; ++i) {
					if (line.cells.get(i).length() == 0) {
						break;
					}
					if (pk.length() > 0) {
						pk += ", ";
					}
					pk += line.cells.get(i);
				}
				value = column == 0? line.cells.get(0) : (pk.isEmpty()? "" : pk);
				Component render = super.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				render.setForeground(Color.BLACK);
				if (pk.equals("")) {
					render.setForeground(Color.RED);
				}
				if (fromModelFinder || modifiedColumnTables.contains(tableName)) {
					render.setBackground(isSelected? BG_SELCOLOR : BG_COLOR);
				} else {
					render.setBackground(isSelected? BG_SELCOLOR : (row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					((JLabel) render).setToolTipText(UIUtil.toHTML(String.valueOf(value), 100));
				}
				return render;
			}
			private static final long serialVersionUID = -8591324056536900244L;
		};
		
		DefaultTableCellRenderer associationsListItemRenderer = new DefaultTableCellRenderer() {
			@Override
			public Component getTableCellRendererComponent(JTable table, Object value,
                    boolean isSelected, boolean hasFocus, int row, int column) {
				boolean fromModelFinder = linesFromModelFinder.contains(value);
				if (!(value instanceof CsvFile.Line)) {
					return super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				}
				CsvFile.Line line = (CsvFile.Line) value;
				String type = "associates";
				String typeTT = "associates";
				if ("B".equalsIgnoreCase(line.cells.get(2))) {
					type = "depends on";
					typeTT = "depends on (has parent)";
				}
				if ("A".equalsIgnoreCase(line.cells.get(2))) {
					type = "has dependent (has child)";
					typeTT = "has dependent";
				}
				String name = "";
				if (line.cells.get(5).length() > 0) {
					name = line.cells.get(5);
				}
				switch (column) {
				case 0: value = line.cells.get(0); break;
				case 1: value = line.cells.get(1); break;
				case 2: value = line.cells.get(4); break;
				case 3: value = type; break;
				case 4: value = line.cells.get(3); break;
				case 5: value = name; break;
				}
				// value = line.cells.get(0) + " " + type + " " + line.cells.get(1) + " " + name + line.cells.get(3) + " on " + line.cells.get(4);
				Component render = super.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (fromModelFinder) {
					render.setBackground(isSelected? BG_SELCOLOR: BG_COLOR);
				} else {
					render.setBackground(isSelected? BG_SELCOLOR : (row % 2 == 0) ? BG1 : BG2);
				}
				render.setForeground(Color.BLACK);
				if (render instanceof JLabel) {
					if (column == 3) {
						((JLabel) render).setToolTipText(typeTT);
					} else {
						((JLabel) render).setToolTipText(UIUtil.toHTML(String.valueOf(value), 100));
					}
				}
				return render;
			}
			private static final long serialVersionUID = -6057505075587930064L;
		};
	
		tablesTable.setAutoCreateRowSorter(true);
		
		tablesTable.setDefaultRenderer(Object.class, tablesListItemRenderer);
		tablesTable.setModel(createTablesListModel());

		// tablesTable.setRowSelectionAllowed(false);
		tablesTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		tablesTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				updateButtons();
			}
		});
		adjustTableColumnsWidth(tablesTable);

		associationsTable.setDefaultRenderer(Object.class, associationsListItemRenderer);
		
		associationsTable.setAutoCreateRowSorter(true);
//		List<SortKey> keys = new ArrayList<SortKey>();
//		keys.add(new SortKey(1, SortOrder.ASCENDING));
//		keys.add(new SortKey(3, SortOrder.ASCENDING));
//		associationsTable.getRowSorter().setSortKeys(keys);
		associationsTable.setModel(createAssociationsListModel());
		adjustTableColumnsWidth(associationsTable);

		associationsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			@Override
			public void valueChanged(ListSelectionEvent e) {
				updateButtons();
			}
		});

		invalidate();
		if (initiallyDirty) {
			markDirty();
		}
		UIUtil.initPeer();
		
		if (toEdit != null) {
			addWindowListener(new WindowListener() {
				@Override
				public void windowActivated(WindowEvent e) {
				}
				@Override
				public void windowClosed(WindowEvent e) {
				}
				@Override
				public void windowClosing(WindowEvent e) {
				}
				@Override
				public void windowDeactivated(WindowEvent e) {
				}
				@Override
				public void windowDeiconified(WindowEvent e) {
				}
				@Override
				public void windowIconified(WindowEvent e) {
				}
				@Override
				public void windowOpened(WindowEvent e) {
					for (Line l: tables) {
						if (toEdit.getName().equals(l.cells.get(0))) {
							if (new TableEditor(DataModelEditor.this, displayNames, tables, associations, excludeFromDeletion).edit(l, columns)) {
								markDirty();
								repaint();
							}
							break;
						}
					}
				}
			});
		}
		
		if (merge) {
			ModelBuilder.cleanUp(executionContext);
		}
	}

	public void adjustTableColumnsWidth(JTable table) {
		DefaultTableModel dtm = (DefaultTableModel) table.getModel();
		for (int i = 0; i < table.getColumnCount(); i++) {
			TableColumn column = table.getColumnModel().getColumn(i);
			Component comp = table.getDefaultRenderer(String.class).getTableCellRendererComponent(table, column.getHeaderValue(), false, false, 0, i);
			int width = 1;
			width = Math.max(width, comp.getPreferredSize().width);

			int line = 0;
			for (; line < table.getRowCount(); ++line) {
				comp = table.getCellRenderer(line, i).getTableCellRendererComponent(table, dtm.getValueAt(line, i), false, false, line, i);
				width = Math.max(width, comp.getPreferredSize().width);
			}
			column.setPreferredWidth(Math.min(width, 400));
		}
	}

	private KnownIdentifierMap createKnownIdentifierMap() throws IOException {
		KnownIdentifierMap knownIdentifierMap = new KnownIdentifierMap();
		File modelFinderTablesFile = new File(ModelBuilder.getModelBuilderTablesFilename(executionContext));
		if (modelFinderTablesFile.exists()) {
			List<CsvFile.Line> tablesFromModelFinder = new CsvFile(modelFinderTablesFile).getLines();
			for (Iterator<CsvFile.Line> i = tablesFromModelFinder.iterator(); i.hasNext(); ) {
				CsvFile.Line t = i.next();
				knownIdentifierMap.putTableName(t.cells.get(0));
			}
		}
		
		File modelFinderAssociationsFile = new File(ModelBuilder.getModelBuilderAssociationsFilename(executionContext));
		if (modelFinderAssociationsFile.exists()) {
			List<CsvFile.Line> assocsFromModelFinder = new CsvFile(modelFinderAssociationsFile).getLines();
			for (Iterator<CsvFile.Line> i = assocsFromModelFinder.iterator(); i.hasNext(); ) {
				CsvFile.Line t = i.next();
				knownIdentifierMap.putTableName(t.cells.get(4));
			}
		}
		
		return knownIdentifierMap;
	}

	/**
	 * Marks data model as modified.
	 */
	private void markDirty() {
		if (!needsSave) {
			needsSave = true;
			setTitle("*" + getTitle());
		}
	}
	
	/**
	 * Sorts lists of csv-lines.
	 * 
	 * @param lines the list to sort
	 */
	private void sortLineList(List<CsvFile.Line> list, final boolean sortTables) {
		Collections.sort(list, new Comparator<CsvFile.Line> () {
			@Override
			public int compare(CsvFile.Line o1, CsvFile.Line o2) {
				int c1 = linesFromModelFinder.contains(o1)? 0 : 1;
				int c2 = linesFromModelFinder.contains(o2)? 0 : 1;
				if (c1 != c2) {
					return c1 - c2;
				}
				if (sortTables) {
					String pk1 = o1.cells.get(2);
					String pk2 = o2.cells.get(2);
					if (pk1.length() == 0 && pk2.length() > 0) return -1;
					if (pk1.length() > 0 && pk2.length() == 0) return 1;
				}
				for (int i = 0; i < 3; ++i) {
					int r = o1.cells.get(i).compareTo(o2.cells.get(i));
					if (r != 0) {
						return r;
					}
				}
				return 0;
			}
		});
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanel6 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        info = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        locationLabel = new javax.swing.JLabel();
        nameTextField = new javax.swing.JTextField();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel2 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        newTable = new javax.swing.JButton();
        editTable = new javax.swing.JButton();
        deleteTables = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        tablesTable = new javax.swing.JTable();
        jPanel3 = new javax.swing.JPanel();
        jPanel5 = new javax.swing.JPanel();
        newAssociation = new javax.swing.JButton();
        editAssociation = new javax.swing.JButton();
        deleteAssociations = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        associationsTable = new javax.swing.JTable();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Data Model Editor");
        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                formWindowClosing(evt);
            }
        });
        getContentPane().setLayout(new java.awt.GridBagLayout());

        okButton.setText("Ok");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        jPanel6.add(okButton);

        cancelButton.setText("Cancel");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        jPanel6.add(cancelButton);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        getContentPane().add(jPanel6, gridBagConstraints);

        info.setForeground(new java.awt.Color(1, 75, 1));
        info.setText("jLabel1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 4, 4);
        getContentPane().add(info, gridBagConstraints);

        jLabel1.setText(" Name  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel1, gridBagConstraints);

        jLabel2.setText(" Location  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(jLabel2, gridBagConstraints);

        locationLabel.setText(" Location  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        getContentPane().add(locationLabel, gridBagConstraints);

        nameTextField.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 22);
        getContentPane().add(nameTextField, gridBagConstraints);

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Tables"));
        jPanel2.setLayout(new java.awt.GridBagLayout());

        jPanel4.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));

        newTable.setText("Add");
        newTable.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newTableActionPerformed(evt);
            }
        });
        jPanel4.add(newTable);

        editTable.setText("Edit");
        editTable.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editTableActionPerformed(evt);
            }
        });
        jPanel4.add(editTable);

        deleteTables.setText("Delete");
        deleteTables.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteTablesActionPerformed(evt);
            }
        });
        jPanel4.add(deleteTables);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel2.add(jPanel4, gridBagConstraints);

        tablesTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane3.setViewportView(tablesTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel2.add(jScrollPane3, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel2);

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Associations"));
        jPanel3.setLayout(new java.awt.GridBagLayout());

        jPanel5.setLayout(new java.awt.FlowLayout(java.awt.FlowLayout.CENTER, 2, 2));

        newAssociation.setText("Add");
        newAssociation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                newAssociationActionPerformed(evt);
            }
        });
        jPanel5.add(newAssociation);

        editAssociation.setText("Edit");
        editAssociation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                editAssociationActionPerformed(evt);
            }
        });
        jPanel5.add(editAssociation);

        deleteAssociations.setText("Delete");
        deleteAssociations.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteAssociationsActionPerformed(evt);
            }
        });
        jPanel5.add(deleteAssociations);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel3.add(jPanel5, gridBagConstraints);

        associationsTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane1.setViewportView(associationsTable);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel3.add(jScrollPane1, gridBagConstraints);

        jSplitPane1.setRightComponent(jPanel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 0, 0);
        getContentPane().add(jSplitPane1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		save();
	}//GEN-LAST:event_okButtonActionPerformed

	private void formWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_formWindowClosing
		close();
	}//GEN-LAST:event_formWindowClosing

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		close();
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void newAssociationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newAssociationActionPerformed
		List<String> cells = new ArrayList<String>();
		for (int i = 0; i < 100; ++i) {
			cells.add("");
		}
		CsvFile.Line line = new CsvFile.Line("?", cells);
		if (new AssociationEditor(this, tables, associations).edit(line)) {
			associations.add(0, line);
			associationsTable.setModel(createAssociationsListModel());
			repaint();
			markDirty();
		}
	}//GEN-LAST:event_newAssociationActionPerformed

	private void editAssociationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editAssociationActionPerformed
		CsvFile.Line line = null;
		if (associationsTable.getSelectedRow() >= 0) {
			line = associations.get(associationsTable.getRowSorter().convertRowIndexToModel(associationsTable.getSelectedRow()));
		}
		if (line != null) {
			if (new AssociationEditor(this, tables, associations).edit(line)) {
				markDirty();
				repaint();
			}
		}
	}//GEN-LAST:event_editAssociationActionPerformed

	private void newTableActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_newTableActionPerformed
		List<String> cells = new ArrayList<String>();
		cells.add("");
		cells.add("N");
		cells.add("");
		cells.add(DATA_MODEL_EDITOR_AUTHOR);
		for (int i = 0; i < 100; ++i) {
			cells.add("");
		}
		CsvFile.Line line = new CsvFile.Line("?", cells);
		if (new TableEditor(this, displayNames, tables, associations, excludeFromDeletion).edit(line, columns)) {
			tables.add(0, line);
			tablesTable.setModel(createTablesListModel());
			markDirty();
			repaint();
		}
	}//GEN-LAST:event_newTableActionPerformed

	private void editTableActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_editTableActionPerformed
		if (tablesTable.getSelectedRow() >= 0) {
			CsvFile.Line line = tables.get(tablesTable.getRowSorter().convertRowIndexToModel(tablesTable.getSelectedRow()));
			if (new TableEditor(this, displayNames, tables, associations, excludeFromDeletion).edit(line, columns)) {
				markDirty();
				repaint();
			}
		}
	}//GEN-LAST:event_editTableActionPerformed

	private void updateButtons() {
		editTable.setEnabled(tablesTable.getSelectedRowCount() > 0);
		deleteTables.setEnabled(tablesTable.getSelectedRowCount() > 0);
		editAssociation.setEnabled(associationsTable.getSelectedRowCount() > 0);
		deleteAssociations.setEnabled(associationsTable.getSelectedRowCount() > 0);
	}
	
	private void deleteTablesActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteTablesActionPerformed
		Collection<CsvFile.Line> toDelete = new ArrayList<CsvFile.Line>();
		Collection<String> namesOfTablesToDelete = new ArrayList<String>();
		for (int i: tablesTable.getSelectedRows()) {
			Line table = tables.get(tablesTable.getRowSorter().convertRowIndexToModel(i));
			if (table != null) {
				toDelete.add(table);
				namesOfTablesToDelete.add(table.cells.get(0));
			}
		}
		Collection<CsvFile.Line> assToDelete = new HashSet<CsvFile.Line>();
		for (CsvFile.Line t: toDelete) {
			for (CsvFile.Line a: associations) {
				if (a.cells.get(0).equalsIgnoreCase(t.cells.get(0)) || a.cells.get(1).equalsIgnoreCase(t.cells.get(0))) {
					assToDelete.add(a);
				}
			}	
		}
		if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Delete " + toDelete.size() + " tables with\n" + assToDelete.size() + " related associations?", "Delete Table", JOptionPane.YES_NO_OPTION)) {
			tables.removeAll(toDelete);
			for (String k: namesOfTablesToDelete) {
				displayNames.remove(k);
			}
			excludeFromDeletion.removeAll(namesOfTablesToDelete);
			tablesTable.setModel(createTablesListModel());
			associations.removeAll(assToDelete);
			associationsTable.setModel(createAssociationsListModel());
			for (Line l: toDelete) {
				columns.remove(l.cells.get(0));
			}
			markDirty();
		}
	}//GEN-LAST:event_deleteTablesActionPerformed

	private void deleteAssociationsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_deleteAssociationsActionPerformed
		Collection<CsvFile.Line> toDelete = new ArrayList<CsvFile.Line>();
		for (int i: associationsTable.getSelectedRows()) {
			toDelete.add(associations.get(associationsTable.getRowSorter().convertRowIndexToModel(i)));
		}
		associations.removeAll(toDelete);
		associationsTable.setModel(createAssociationsListModel());
		markDirty();
	}//GEN-LAST:event_deleteAssociationsActionPerformed

	/**
	 * Creates model for tables-list component.
	 * 
	 * @return model for tables-list component
	 */
	private TableModel createTablesListModel() {
		DefaultTableModel tablesTableModel = new DefaultTableModel(new Object[] { "Name", "Primary Key" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		for (CsvFile.Line line: tables) {
			tablesTableModel.addRow(new Object[] { line, line });
		}
		return tablesTableModel;
	}

	/**
	 * Creates model for associations-list component.
	 * 
	 * @return model for associations-list component
	 */
	private TableModel createAssociationsListModel() {
		DefaultTableModel associationsTableModel = new DefaultTableModel(new Object[] { "A", "B", "Condition", "Type", "Cardinality", "Name" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		for (CsvFile.Line line: associations) {
			associationsTableModel.addRow(new Object[] { line, line, line, line, line, line });
		}
		return associationsTableModel;
	}
	
	/**
	 * Closes editor.
	 */
	private void close() {
		if (needsSave) {
			if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(this, "Close without save?", "Close", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE)) {
				return;
			}
		}
		setVisible(false);
	}
	
	/**
	 * Saves the model.
	 */
	private void save() {
		try {
			if (needsSave) {
				save(tables, DataModel.getTablesFile(executionContext), "# Name; Upsert; Primary key; ; Author");
				save(associations, DataModel.getAssociationsFile(executionContext), "# Table A; Table B; First-insert; Cardinality; Join-condition; Name; Author");
				save(new ArrayList<Line>(columns.values()), DataModel.getColumnsFile(executionContext), "# Table; Columns");
				saveTableList(excludeFromDeletion, DataModel.getExcludeFromDeletionFile(executionContext));
				saveTableList(Arrays.asList(JailerVersion.VERSION), DataModel.getVersionFile(executionContext));
				saveDisplayNames();
				saveName();
				saved = true;
			}
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
		setVisible(false);
	}
	
	/**
	 * Saves the display names.
	 */
	private void saveDisplayNames() throws FileNotFoundException {
		PrintWriter out = new PrintWriter(DataModel.getDisplayNamesFile(executionContext));
		out.println("# table; display name");
		for (Map.Entry<String, String> e: displayNames.entrySet()) {
			out.println(CsvFile.encodeCell(e.getKey()) + "; " + CsvFile.encodeCell(e.getValue()));
		}
		out.close();
	}

	/**
	 * Saves the model name.
	 */
	private void saveName() throws FileNotFoundException {
		createNameFile(nameTextField.getText(), executionContext);
	}

	/**
	 * Saves the model name.
	 */
	public static void createNameFile(String name, ExecutionContext executionContext) throws FileNotFoundException {
		PrintWriter out = new PrintWriter(DataModel.getModelNameFile(executionContext));
		out.println("# name; last modification");
		out.println(CsvFile.encodeCell(name) + "; " + CsvFile.encodeCell(Long.toString(new Date().getTime())));
		out.close();
	}

	/**
	 * Save a table list.
	 * 
	 * @param tableList list to save
	 * @param fileName the file to save into
	 */
	private void saveTableList(List<String> tableList, String fileName)  throws FileNotFoundException {
		PrintWriter out = new PrintWriter(fileName);
		for (String table: tableList) {
			out.println(table);
		}
		out.close();
	}

	/**
	 * Saves a list of csv-lines.
	 * 
	 * @param lines the lines
	 * @param fileName the file to save into
	 * @param title title line
	 */
	private void save(List<Line> lines, String fileName, String title) throws FileNotFoundException {
		PrintWriter out = new PrintWriter(fileName);
		out.println(title);
		for (Line line: lines) {
			out.println(line);
		}
		out.close();
	}

	/**
	 * Compares current model with the previous one.
	 * 
	 * @return <code>true</code> if something has changed
	 */
	public boolean dataModelHasChanged() throws Exception {
		String tmpFile = DataModel.getDatamodelFolder(executionContext) + File.separator + "tempcsv.tmp";
		boolean result = false;
		save(tables, tmpFile, "#");
		result = !cvsFilesEquals(tmpFile, DataModel.getTablesFile(executionContext));
		if (!result) {
			save(associations, tmpFile, "#");
			result = !cvsFilesEquals(tmpFile, DataModel.getAssociationsFile(executionContext));
		}
		if (!result) {
			save(new ArrayList<Line>(columns.values()), tmpFile, "#");
			result = !cvsFilesEquals(tmpFile, DataModel.getColumnsFile(executionContext));
		}
		if (!result) {
			JOptionPane.showMessageDialog(this, "No changes found.", "Analyze Database", JOptionPane.INFORMATION_MESSAGE);
		}
		new File(tmpFile).delete();
		return result;
	}

	/**
	 * Compares two csv files.
	 * @throws Exception 
	 */
	private boolean cvsFilesEquals(String aFile, String bFile) throws Exception {
		Set<String> a = new TreeSet<String>();
		StringBuilder sb = new StringBuilder();
		for (Line line: new CsvFile(new File(aFile)).getLines()) {
			sb.setLength(0);
			for (int i = 0; i < line.length; ++i) {
				sb.append(line.cells.get(i)).append(';');
			}
			a.add(sb.toString());
		}
		Set<String> b = new TreeSet<String>();
		for (Line line: new CsvFile(new File(bFile)).getLines()) {
			sb.setLength(0);
			for (int i = 0; i < line.length; ++i) {
				sb.append(line.cells.get(i)).append(';');
			}
			b.add(sb.toString());
		}
		return a.equals(b);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTable associationsTable;
    private javax.swing.JButton cancelButton;
    private javax.swing.JButton deleteAssociations;
    private javax.swing.JButton deleteTables;
    private javax.swing.JButton editAssociation;
    private javax.swing.JButton editTable;
    private javax.swing.JLabel info;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JLabel locationLabel;
    private javax.swing.JTextField nameTextField;
    private javax.swing.JButton newAssociation;
    private javax.swing.JButton newTable;
    private javax.swing.JButton okButton;
    private javax.swing.JTable tablesTable;
    // End of variables declaration//GEN-END:variables
	
	private static final long serialVersionUID = -1267039412732180237L;
}
