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
package net.sf.jailer.ui.databrowser;

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

import net.sf.jailer.database.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.ConditionEditor;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.util.Quoting;
import net.sf.jailer.util.SqlUtil;

/**
 * Content UI of a row browser frame (as {@link JInternalFrame}s).
 * Contains a table for rendering rows.
 * 
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public abstract class BrowserContentPane extends javax.swing.JPanel {

	/**
	 * Table to read rows from.
	 */
	private final Table table;

	/**
	 * Parent row, or <code>null</code>.
	 */
	private final Row parentRow;
	
	/**
	 * {@link Association} with parent row, or <code>null</code>.
	 */
	private final Association association;
	
	/**
	 * Rows to render.
	 */
	List<Row> rows = new ArrayList<Row>();
	
	/**
	 * Indexes of highlighted rows.
	 */
	private Set<Integer> highlightedRows = new HashSet<Integer>();
	
	/**
	 * Indexes of primary key columns.
	 */
	private Set<Integer> pkColumns = new HashSet<Integer>();
	
	/**
	 * DB session.
	 */
	private final Session session;
	
	private Quoting quoting;
	
	/**
	 * For concurrent reload of rows.
	 */
	private static final LinkedBlockingQueue<Runnable> runnableQueue = new LinkedBlockingQueue<Runnable>();
	
	/**
	 * Maximum number of concurrent DB connections.
	 */
	private static int MAX_CONCURRENT_CONNECTIONS = 2;

	static {
		// initialize listeners for #runnableQueue
		for (int i = 0; i < MAX_CONCURRENT_CONNECTIONS; ++i) {
			Thread t = new Thread(new Runnable() {
				@Override
				public void run() {
					for (;;) {
						try {
							runnableQueue.take().run();
						} catch (InterruptedException e) {
							// ignore
						}
					}
				}
			}, "browser-" + i);
			t.setDaemon(true);
			t.start();
		}
	}
	
	/**
	 * Constructor.
	 * @param table to read rows from
	 * @param session DB session
	 * @param parentRow parent row
	 * @param association {@link Association} with parent row
	 */
	public BrowserContentPane(final Table table, Session session, Row parentRow, Association association, Frame parentFrame) {
		this.table = table;
		this.session = session;
		this.parentRow = parentRow;
		this.association = association;
		try {
			quoting = new Quoting(session.getMetaData());
		} catch (SQLException e) {
			UIUtil.showException(null, "Error", e);
		}

		initComponents();
		if (association == null) {
			join.setVisible(false);
			on.setVisible(false);
			where.setVisible(false);
			jLabel1.setVisible(false);
			jLabel4.setVisible(false);
			jLabel9.setVisible(false);
			andLabel.setText(" Where ");
		} else {
			join.setText(association.getDataModel().getDisplayName(association.source) + " A");
			on.setText(association.reversed? SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition()) : association.getUnrestrictedJoinCondition());
			where.setText(parentRow.rowId);
			join.setToolTipText(join.getText());
			on.setToolTipText(on.getText());
			where.setToolTipText(where.getText());		
		}
		rowsTable.setShowGrid(false);
		final TableCellRenderer defaultTableCellRenderer = rowsTable.getDefaultRenderer(String.class);
		rowsTable.setDefaultRenderer(Object.class, new TableCellRenderer() {

			final Color BG1 = new Color(255, 255, 255);
			final Color BG2 = new Color(230, 255, 255);
			final Color FG1 = new Color(155, 0, 0);
			final Font font = new JLabel().getFont();
			final Font nonbold = new Font(font.getName(), font.getStyle() & ~Font.BOLD, font.getSize()); 
			final Font bold = new Font(font.getName(), font.getStyle() | Font.BOLD, font.getSize()); 
			
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component render = defaultTableCellRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				final RowSorter<?> rowSorter = rowsTable.getRowSorter();
				if (render instanceof JLabel) {
					((JLabel) render).setBackground((row % 2 == 0) ? BG1 : BG2);
					((JLabel) render).setForeground(pkColumns.contains(rowsTable.convertColumnIndexToModel(column))? FG1 : Color.BLACK);
					try {
						((JLabel) render).setFont(highlightedRows.contains(rowSorter.convertRowIndexToModel(row))? bold : nonbold);
					} catch (Exception e) {
						// ignore
					}
				}
				return render;
			}
		});
		rowsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		rowsTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			private JPopupMenu lastMenu;
			@Override
			public void valueChanged(ListSelectionEvent e) {
				if (rowsTable.getSelectedRow() >= 0) {
					int i = rowsTable.getRowSorter().convertRowIndexToModel(rowsTable.getSelectedRow());
					Row row = rows.get(i);

					if (lastMenu == null || !lastMenu.isVisible()) {
						JPopupMenu popup = createPopupMenu(row, i);
						Rectangle r = rowsTable.getCellRect(rowsTable.getSelectedRow(), rowsTable.getSelectedColumn(), false);
						Rectangle v = rowsTable.getVisibleRect();
						popup.show(rowsTable, Math.max((int) v.getMinX(), (int) r.getMinX()), (int) r.getMaxY() - 2);
						lastMenu = popup;
					}
					
					rowsTable.clearSelection();
				}
			}
		});
		andConditionEditor = new ConditionEditor(parentFrame, null);
		openEditorLabel.setIcon(conditionEditorIcon);
		openEditorLabel.setText(null);
		openEditorLabel.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
					String cond = andConditionEditor.edit(andCondition.getText(), "", "A", table, null, null, null, false);
					if (cond != null) {
						if (!andCondition.getText().equals(ConditionEditor.toSingleLine(cond))) {
							andCondition.setText(ConditionEditor.toSingleLine(cond));
						}
					}
					openEditorLabel.setIcon(conditionEditorSelectedIcon);
			}
			
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				openEditorLabel.setIcon(conditionEditorSelectedIcon);
		    }
            public void mouseExited(java.awt.event.MouseEvent evt) {
            	openEditorLabel.setIcon(conditionEditorIcon);
           }
        });
		updateTableModel();
		reloadRows();
	}
	
	/**
     * Creates popup menu for navigation.
     */
	public JPopupMenu createPopupMenu(final Row row, final int rowIndex) {
		List<String> assList = new ArrayList<String>();
		Map<String, Association> assMap = new HashMap<String, Association>();
		for (Association a: table.associations) {
			int n = 0;
	    	for (Association a2: table.associations) {
	    		if (a.destination == a2.destination) {
	    			++n;
	    		}
	    	}
	    	String name = a.getDataModel().getDisplayName(a.destination) + (n > 1? " on "  + a.getName() : "");
	    	assList.add(name);
	    	assMap.put(name, a);
		}
		Collections.sort(assList);
		
		JPopupMenu popup = new JPopupMenu();
		JMenuItem item = new JMenuItem("to..");
		item.setEnabled(false);
		popup.add(item);
		JMenu current = null;
		int l = 0;
		for (String name: assList) {
			final Association association = assMap.get(name);
			
			if (++l > 30) {
				l = 1;
				JMenu p = new JMenu("more...");
				if (current != null) {
					current.add(p);
				} else {
					popup.add(p);
				}
				current = p;
			}
			
			item = new JMenuItem("  " + name);
			item.addActionListener(new ActionListener () {
				public void actionPerformed(ActionEvent e) {
					highlightedRows.add(rowIndex);
					navigateTo(association, rowIndex, row);
				}
			});
			if (current != null) {
				current.add(item);
			} else {
				popup.add(item);
			}
		}
		if (assList.isEmpty()) {
			item = new JMenuItem("no associations");
			item.setEnabled(false);
			popup.add(item);
		}
		return popup;
	}

	/**
	 * Reloads rows.
	 */
	private void reloadRows() {
		((CardLayout) cardPanel.getLayout()).show(cardPanel, "loading");
		Runnable reloadJob = new Runnable() {
			private List<Row> rows = Collections.synchronizedList(new ArrayList<Row>());
			private Exception exception;
			@Override
			public void run() {
				try {
					reloadRows(rows);
				} catch (SQLException e) {
					synchronized (rows) {
						exception = e;
					}
				}
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						Exception e;
						synchronized (rows) {
							e = exception;
						}
						if (e != null) {
							UIUtil.showException(null, "Error", e);
						} else {
							BrowserContentPane.this.rows.clear();
							BrowserContentPane.this.rows.addAll(rows);
							updateTableModel();
							((CardLayout) cardPanel.getLayout()).show(cardPanel, "table");
						}
					}
				});
			}
		};
		runnableQueue.add(reloadJob);
	}
	
	/**
	 * Reload rows from {@link #table}.
	 * 
	 * @param rows to put the rows into
	 */
	private void reloadRows(final List<Row> rows) throws SQLException {
		String sql = "Select ";
		boolean f = true;
		for (Column column: table.getColumns()) {
			String name = quoting == null? column.name : quoting.quote(column.name);
			sql += (!f? ", " : "") + "B." + name;
			f = false;
		}
		sql += " From ";
		if (association != null) {
			sql += (quoting == null? association.source.getName() : quoting.quote(association.source.getName())) + " A join ";
		}
		sql += (quoting == null? table.getName() : quoting.quote(table.getName())) + " B";
		if (association != null) {
			if (!association.reversed) {
				sql += " on " + association.getUnrestrictedJoinCondition();
			} else {
				sql += " on " + SqlUtil.reversRestrictionCondition(association.getUnrestrictedJoinCondition());
			}
		}
		final Set<String> pkColumnNames = new HashSet<String>();
		f = true;
		String orderBy = "";
		for (Column pk: table.primaryKey.getColumns()) {
			pkColumnNames.add(pk.name);
			orderBy += (f? "" : ", ") + "B." + (quoting == null? pk.name : quoting.quote(pk.name));
			f = false;
		}
		if (parentRow != null) {
			sql += " Where " + parentRow.rowId;
		}
		if (orderBy.length() > 0) {
			sql += " Order by " + orderBy;
		}
		session.executeQuery(sql, new Session.ResultSetReader() {
			
			Map<Integer, Integer> typeCache = new HashMap<Integer, Integer>();

			@Override
			public void readCurrentRow(ResultSet resultSet) throws SQLException {
				int i = 1;
				String rowId = "";
				String v[] = new String[table.getColumns().size()];
				for (Column column: table.getColumns()) {
					String value = "";
					int type = SqlUtil.getColumnType(resultSet, i, typeCache);
					if ((type == Types.BLOB || type == Types.CLOB) && session.dbms != DBMS.SQLITE) {
						Object object = resultSet.getObject(i);
						if (object == null || resultSet.wasNull()) {
							value = "";
						}
						if (object instanceof Blob) {
							value = "Blob";
						}

						if (object instanceof Clob) {
							value = "Clob";
						}
					} else {
						Object o = SqlUtil.getObject(resultSet, i, typeCache);
						if (pkColumnNames.contains(column.name)) {
							String cVal = SqlUtil.toSql(o, session);
			                rowId += (rowId.length() == 0? "" : " and ") + "A." + (quoting == null? column.name : quoting.quote(column.name))
			                	+ "=" + cVal;
						}
		                if (o == null || resultSet.wasNull()) {
							value = "";
						}
						if (o != null) {
							value = o.toString();
						}
					}
					v[i - 1] = value;
					++i;
				}
				rows.add(new Row(rowId, v));
			}
			@Override
			public void close() {
			}
		});
	}

	/**
	 * Updates the model of the {@link #rowsTable}.
	 */
	private void updateTableModel() {
		pkColumns.clear();
		String[] columnNames = new String[table.getColumns().size()];
		final Set<String> pkColumnNames = new HashSet<String>();
		for (Column pk: table.primaryKey.getColumns()) {
			pkColumnNames.add(pk.name);
		}
		for (int i = 0; i < columnNames.length; ++i) {
			columnNames[i] = table.getColumns().get(i).name;
			if (pkColumnNames.contains(columnNames[i])) {
				pkColumns.add(i);
			}
		}
		DefaultTableModel dtm = new DefaultTableModel(columnNames, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		for (Row row : rows) {
			Object[] rowData = new Object[table.getColumns().size()];
			for (int i = 0; i < table.getColumns().size(); ++i) {
				rowData[i] = row.values[i];
			}
			dtm.addRow(rowData);
		}
		rowsTable.setModel(dtm);
		for (int i = 0; i < rowsTable.getColumnCount(); i++) {
            TableColumn column = rowsTable.getColumnModel().getColumn(i);
            int width = (Desktop.BROWSERTABLE_DEFAULT_WIDTH - 14) / rowsTable.getColumnCount();
            
            Component comp = rowsTable.getDefaultRenderer(String.class).
            						getTableCellRendererComponent(
            								rowsTable, column.getHeaderValue(),
            								false, false, 0, i);
            width = Math.max(width, comp.getPreferredSize().width);

            for (int line = 0; line < rowsTable.getRowCount(); ++line) {
	            comp = rowsTable.getCellRenderer(line, i).getTableCellRendererComponent(rowsTable, dtm.getValueAt(line, i), false, false, line, i);
	            width = Math.min(Math.max(width, comp.getPreferredSize().width + 16), 400);
            }
            
            column.setPreferredWidth(width);
        }
		rowsTable.setIntercellSpacing(new Dimension(0, 0));
		rowsCount.setText(" " + rows.size() + " row" + (rows.size() != 1? "s" : ""));
	}

	/**
	 * This method is called from within the constructor to initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is always
	 * regenerated by the Form Editor.
	 */
	// <editor-fold defaultstate="collapsed"
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        buttonGroup1 = new javax.swing.ButtonGroup();
        cardPanel = new javax.swing.JPanel();
        jPanel2 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jPanel1 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        rowsTable = new javax.swing.JTable();
        rowsCount = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        join = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        on = new javax.swing.JLabel();
        where = new javax.swing.JLabel();
        andLabel = new javax.swing.JLabel();
        openEditorLabel = new javax.swing.JLabel();
        andCondition = new javax.swing.JTextField();
        loadButton = new javax.swing.JButton();

        setLayout(new java.awt.GridBagLayout());

        cardPanel.setLayout(new java.awt.CardLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        jLabel2.setFont(new java.awt.Font("DejaVu Sans", 1, 14));
        jLabel2.setForeground(new java.awt.Color(141, 16, 16));
        jLabel2.setText("loading...");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 0);
        jPanel2.add(jLabel2, gridBagConstraints);

        cardPanel.add(jPanel2, "loading");

        jPanel1.setLayout(new java.awt.BorderLayout());

        rowsTable.setAutoCreateRowSorter(true);
        rowsTable.setModel(new javax.swing.table.DefaultTableModel(
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
        rowsTable.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(rowsTable);

        jPanel1.add(jScrollPane1, java.awt.BorderLayout.CENTER);

        rowsCount.setText("jLabel3");
        jPanel1.add(rowsCount, java.awt.BorderLayout.SOUTH);

        cardPanel.add(jPanel1, "table");

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 4;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(cardPanel, gridBagConstraints);

        jLabel1.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel1.setText(" Join ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel1, gridBagConstraints);

        join.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
        join.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(join, gridBagConstraints);

        jLabel4.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel4.setText(" On ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel4, gridBagConstraints);

        jLabel9.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        jLabel9.setText(" Where ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(jLabel9, gridBagConstraints);

        on.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
        on.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(on, gridBagConstraints);

        where.setFont(new java.awt.Font("DejaVu Sans", 0, 14));
        where.setText("jLabel3");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 4;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        add(where, gridBagConstraints);

        andLabel.setFont(new java.awt.Font("DejaVu Sans", 1, 13));
        andLabel.setText(" And  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(andLabel, gridBagConstraints);

        openEditorLabel.setText(" And  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(openEditorLabel, gridBagConstraints);

        andCondition.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(andCondition, gridBagConstraints);

        loadButton.setText("refresh");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 0);
        add(loadButton, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextField andCondition;
    private javax.swing.JLabel andLabel;
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JPanel cardPanel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JLabel join;
    private javax.swing.JButton loadButton;
    private javax.swing.JLabel on;
    private javax.swing.JLabel openEditorLabel;
    private javax.swing.JLabel rowsCount;
    public javax.swing.JTable rowsTable;
    private javax.swing.JLabel where;
    // End of variables declaration//GEN-END:variables

    private ConditionEditor andConditionEditor;
    private Icon conditionEditorIcon;
    private Icon conditionEditorSelectedIcon;
	{
		String dir = "/net/sf/jailer/resource";
		
		// load images
		try {
			conditionEditorIcon = new ImageIcon(getClass().getResource(dir + "/edit.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			conditionEditorSelectedIcon = new ImageIcon(getClass().getResource(dir + "/edit_s.png"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	protected abstract void navigateTo(Association association, int rowIndex, Row row);
    
}
