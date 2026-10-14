/*
 * Copyright 2007 - 2026 Ralf Wisser.
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
package net.sf.jailer.ui.associationdiscovery;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Dialog.ModalityType;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import org.fife.rsta.ui.EscapableDialog;
import org.fife.ui.rtextarea.RTextScrollPane;

import net.coderazzi.filters.Filter;
import net.coderazzi.filters.gui.AutoChoices;
import net.coderazzi.filters.gui.TableFilterHeader;
import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.associationproposer.AssociationProposalWriter;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.util.CancellationHandler;

/**
 * Discovers associations that are not declared as foreign key constraints:
 * proposes candidates based on naming conventions and - optionally - on a scan of the
 * data, verifies each candidate against the database and lets the user accept the
 * confirmed ones into the data model. <br>
 * This allows to reverse-engineer the data model of a database without foreign keys.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class AssociationDiscoveryView extends JPanel {

	private final JFrame owner;
	private final DataModel dataModel;
	private final Session session;
	private final ExecutionContext executionContext;
	private final Object cancellationContext = new Object();

	private final List<AssociationCandidate> proposals = new ArrayList<AssociationCandidate>();

	/**
	 * The {@link AssociationCandidate#key()}s of the proposals.
	 */
	private final Set<String> proposalKeys = new HashSet<String>();

	/**
	 * Minimum fraction of matching rows of the last discovery run.
	 */
	private double runThreshold;

	/**
	 * Match ratios that are less than this above the threshold are considered close to it.
	 */
	private static final double CLOSE_TO_THRESHOLD = 0.05;
	private DefaultTableModel proposalsModel;
	private DefaultTableModel knownModel;
	private JTable proposalTable;
	private JTable knownTable;
	private RSyntaxTextAreaWithSQLSyntaxStyle statementsPane;
	private JTextArea problemsPane;
	private JProgressBar progressBar;
	private JLabel progressLabel;
	private JLabel statusLabel;
	private Color defaultForeground;
	private JCheckBox dataScanCheckBox;
	private JComboBox<String> thresholdComboBox;
	private JButton startButton;
	private JButton rulesButton;
	private JButton acceptButton;
	private JButton acceptAndEditButton;
	private JButton selectAllButton;
	private JButton deselectAllButton;
	private JDialog dialog;

	private volatile boolean cancelled;
	private volatile boolean running;
	private boolean accepted;
	private boolean editRequested;

	/**
	 * Maximum number of statements shown in the "Statements" tab. A run against a large
	 * schema executes far more, and the tab is a diagnostic aid, not a protocol.
	 */
	private static final int MAX_SHOWN_STATEMENTS = 2000;

	/**
	 * Number of rows from which on the column widths are adjusted to the content. Doing it
	 * with the first row would size the columns after a single example.
	 */
	private static final int ADJUST_COLUMNS_AT = 5;

	/**
	 * Label of the button that opens the editor for the naming rules.
	 */
	private static final String RULES_BUTTON_TEXT = "Naming rules...";

	/**
	 * Guards the buffers below. They are filled by the worker thread and read by the event
	 * dispatch thread, so that a run does not put one EDT event per query into the queue
	 * but coalesces bursts into a single update.
	 */
	private final Object bufferLock = new Object();
	private final StringBuilder pendingStatements = new StringBuilder();
	private final StringBuilder pendingProblems = new StringBuilder();
	private final List<AssociationCandidate> pendingProposals = new ArrayList<AssociationCandidate>();
	private final List<AssociationCandidate> pendingKnown = new ArrayList<AssociationCandidate>();
	private String pendingProgressText;
	private int pendingProgressValue;
	private int pendingProgressMaximum;
	private boolean flushScheduled;
	private int shownStatements;
	private boolean proposalColumnsAdjusted;
	private boolean knownColumnsAdjusted;

	/**
	 * Shows a table (e.g. selects it in the extraction model editor), <code>null</code> if there is no such function.
	 */
	private final Consumer<Table> showTable;

	/**
	 * Opens tables A and B of a candidate in the Data Browser, <code>null</code> if there is no such function.
	 */
	private final BiConsumer<Table, Table> browseTables;

	/**
	 * Opens an SQL statement in the SQL Console (statement, execute it), <code>null</code> if there is no such function.
	 */
	private final BiConsumer<List<String>, Boolean> openInSqlConsole;

	/**
	 * The candidates of the "Already known" table, in the order of its model rows.
	 */
	private final List<AssociationCandidate> known = new ArrayList<AssociationCandidate>();

	/**
	 * Creates the view and opens it as a dialog that is modal to the owner's window only
	 * (so that a Data Browser opened from it can be used).
	 *
	 * @param owner the owner frame
	 * @param dataModel the data model
	 * @param session the database session used to profile and verify
	 * @param executionContext the execution context
	 * @param showTable shows a table, <code>null</code> if there is no such function
	 * @param browseTables opens tables A and B of a candidate in the Data Browser, <code>null</code> if there is no such function
	 * @param openInSqlConsole opens SQL statements in the SQL Console (statements, execute them), <code>null</code> if there is no such function
	 */
	public AssociationDiscoveryView(JFrame owner, DataModel dataModel, Session session, ExecutionContext executionContext,
			Consumer<Table> showTable, BiConsumer<Table, Table> browseTables, BiConsumer<List<String>, Boolean> openInSqlConsole) {
		this.owner = owner;
		this.dataModel = dataModel;
		this.session = session;
		this.executionContext = executionContext;
		this.showTable = showTable;
		this.browseTables = browseTables == null? null
				: (a, b) -> withStatus("Opening the Data Browser...", () -> browseTables.accept(a, b));
		this.openInSqlConsole = openInSqlConsole == null? null
				: (statements, execute) -> withStatus("Opening the SQL Console...", () -> openInSqlConsole.accept(statements, execute));

		initUI();
		UIUtil.initComponents(this);

		dialog = new EscapableDialog(owner, "Discover Associations") {
		};
		dialog.setModalityType(ModalityType.DOCUMENT_MODAL);
		dialog.getContentPane().add(this);
		dialog.getRootPane().setDefaultButton(startButton);
		dialog.pack();
		UIUtil.setDialogSize(dialog, 1100, 600);
		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2);
		UIUtil.fit(dialog);
		dialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				cancelled = true;
				CancellationHandler.cancelSilently(cancellationContext);
			}
		});
		dialog.setVisible(true);
	}

	/**
	 * @return <code>true</code> if proposals have been accepted, so that the data model
	 *         has to be reloaded
	 */
	public boolean isAccepted() {
		return accepted;
	}

	/**
	 * @return <code>true</code> if the accepted proposals are to be reviewed in the Data Model Editor
	 *         ("Accept and Edit..."). Otherwise they have already been added to the data model.
	 */
	public boolean isEditRequested() {
		return editRequested;
	}

	private void initUI() {
		setLayout(new GridBagLayout());

		add(createOptionsPanel(), gbc(0, 0, 1, GridBagConstraints.HORIZONTAL, 0));
		add(createTabbedPane(), gbc(0, 1, 1, GridBagConstraints.BOTH, 1));
		add(createProgressPanel(), gbc(0, 2, 1, GridBagConstraints.HORIZONTAL, 0));
		add(createButtonPanel(), gbc(0, 3, 1, GridBagConstraints.HORIZONTAL, 0));
	}

	private GridBagConstraints gbc(int x, int y, int width, int fill, double weighty) {
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = x;
		c.gridy = y;
		c.gridwidth = width;
		c.fill = fill;
		c.weightx = 1;
		c.weighty = weighty;
		c.insets = new Insets(2, 4, 2, 4);
		return c;
	}

	private JPanel createOptionsPanel() {
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(BorderFactory.createEtchedBorder());

		panel.add(new JLabel("Min. matching rows "), optionsGbc(0, 0, 1, 0));
		thresholdComboBox = new JComboBox<String>(new String[] { "100 %", "99 %", "95 %", "90 %", "80 %" });
		thresholdComboBox.setSelectedItem("95 %");
		thresholdComboBox.setToolTipText("A candidate is only proposed if at least this fraction of the rows of the "
				+ "child table has a matching row in the parent table.");
		panel.add(thresholdComboBox, optionsGbc(1, 0, 1, 1));

		dataScanCheckBox = new JCheckBox("Also find candidates without matching column names");
		dataScanCheckBox.setToolTipText("Scan the data for columns whose values are contained in a primary key, "
				+ "even if their names give no hint. Finds keys with cryptic names, but needs considerably more queries. "
				+ "Composite keys are covered too, with a limited number of column combinations per pair of tables.");
		panel.add(dataScanCheckBox, optionsGbc(0, 1, 2, 0));

		rulesButton = new JButton(RULES_BUTTON_TEXT);
		rulesButton.setIcon(UIUtil.scaleIcon(rulesButton, editIcon));
		rulesButton.setToolTipText("Enter the naming conventions of this schema: how a foreign key column is named. "
				+ "The rules are stored with the data model.");
		rulesButton.addActionListener(e -> editNamingRules());
		updateRulesButtonText();
		panel.add(rulesButton, optionsGbc(0, 2, 2, 1));

		startButton = new JButton("Start discovery");
		// the main action of the dialog: bold, so that it catches the eye
		startButton.setFont(startButton.getFont().deriveFont(Font.BOLD));
		startButton.setIcon(UIUtil.scaleIcon(startButton, runIcon));
		startButton.setToolTipText("Search the database for associations: profiles the tables, then verifies "
				+ "every candidate with a query. Closing the dialog cancels a running discovery.");
		startButton.addActionListener(e -> start());
		GridBagConstraints startGbc = optionsGbc(0, 3, 2, 1);
		startGbc.insets = new Insets(6, 4, 2, 4);
		panel.add(startButton, startGbc);

		return panel;
	}

	/**
	 * Constraints for one element of the options panel: left aligned, not stretched.
	 *
	 * @param x the column
	 * @param y the row
	 * @param width the number of columns to span
	 * @param weightx 1 for the last element of a row, so that the free space goes there
	 * @return the constraints
	 */
	private GridBagConstraints optionsGbc(int x, int y, int width, double weightx) {
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = x;
		c.gridy = y;
		c.gridwidth = width;
		c.weightx = weightx;
		c.anchor = GridBagConstraints.WEST;
		c.insets = new Insets(2, 4, 2, 4);
		return c;
	}

	private JTabbedPane createTabbedPane() {
		JTabbedPane tabbedPane = new JTabbedPane();

		proposalsModel = new DefaultTableModel(new String[] { " ", "A", "B", "Condition", "Confidence", "Match", "Evidence" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return column == 0;
			}
			@Override
			public void setValueAt(Object value, int row, int column) {
				super.setValueAt(value, row, column);
				updateButtonsState();
			}
			@Override
			public Class<?> getColumnClass(int columnIndex) {
				if (columnIndex == 0) {
					return Boolean.class;
				}
				if (columnIndex == 4 || columnIndex == 5) {
					return Double.class;
				}
				return super.getColumnClass(columnIndex);
			}
		};
		proposalTable = new JTable(proposalsModel);
		proposalTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		proposalTable.setShowGrid(false);
		JCheckBox checkBox = new JCheckBox("  ");
		checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
		DefaultCellEditor editor = new DefaultCellEditor(checkBox);
		editor.setClickCountToStart(1);
		proposalTable.setDefaultEditor(Boolean.class, editor);
		TableCellRenderer renderer = createRenderer(proposalTable);
		proposalTable.setDefaultRenderer(Object.class, renderer);
		proposalTable.setDefaultRenderer(Double.class, renderer);
		proposalTable.getColumnModel().getColumn(0).setCellRenderer(renderer);
		proposalTable.setAutoCreateRowSorter(true);
		List<SortKey> keys = new ArrayList<SortKey>();
		keys.add(new SortKey(4, SortOrder.DESCENDING));
		proposalTable.getRowSorter().setSortKeys(keys);
		TableFilterHeader filterHeader = new TableFilterHeader();
		filterHeader.setAutoChoices(AutoChoices.ENABLED);
		filterHeader.setTable(proposalTable);
		filterHeader.setMaxVisibleRows(20);
		filterHeader.setRowHeightDelta(2);
		filterHeader.addFilter(textFilter);

		JPanel proposalsPanel = new JPanel(new BorderLayout());
		JLabel info = new JLabel("<html><i>Check the associations you want to add to the data model. "
				+ "The confidence estimates how likely a candidate is a real foreign key. "
				+ "Match: share of the rows of A that have a matching row in B (green: all rows, orange: close to the threshold). "
				+ "Double-click a row (or press Enter) for details, Space checks it, right-click for more.</i></html>");
		info.setBorder(BorderFactory.createEmptyBorder(2, 4, 2, 4));
		JPanel northPanel = new JPanel(new BorderLayout());
		northPanel.add(info, BorderLayout.NORTH);
		JPanel filterPanel = new JPanel(new BorderLayout(4, 0));
		filterPanel.setBorder(BorderFactory.createEmptyBorder(2, 4, 2, 4));
		filterPanel.add(new JLabel("Filter "), BorderLayout.WEST);
		filterField = new JTextField();
		filterField.setToolTipText("Shows only the proposals whose table A, table B or condition contains this text (ignoring case).");
		filterField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void insertUpdate(DocumentEvent e) {
				textFilter.reportFilterUpdatedToObservers();
			}
			@Override
			public void removeUpdate(DocumentEvent e) {
				textFilter.reportFilterUpdatedToObservers();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				textFilter.reportFilterUpdatedToObservers();
			}
		});
		// consumes Enter, which would otherwise press the default button ("Start discovery") and discard the result
		filterField.addActionListener(e -> {});
		filterPanel.add(filterField, BorderLayout.CENTER);
		northPanel.add(filterPanel, BorderLayout.SOUTH);
		proposalsPanel.add(northPanel, BorderLayout.NORTH);
		proposalsPanel.add(new JScrollPane(proposalTable), BorderLayout.CENTER);
		tabbedPane.addTab("Proposals", UIUtil.scaleIcon(tabbedPane, addIcon), proposalsPanel);

		knownModel = new DefaultTableModel(new String[] { "A", "B", "Condition" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		knownTable = new JTable(knownModel);
		knownTable.setRowSelectionAllowed(false);
		knownTable.setShowGrid(false);
		knownTable.setDefaultRenderer(Object.class, createRenderer(knownTable));
		knownTable.setAutoCreateRowSorter(true);
		proposalTable.addMouseListener(new CandidateMouseListener(proposalTable, proposals, true));
		knownTable.addMouseListener(new CandidateMouseListener(knownTable, known, false));
		installKeys();
		tabbedPane.addTab("Already known", UIUtil.scaleIcon(tabbedPane, modelIcon), new JScrollPane(knownTable));

		statementsPane = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false);
		statementsPane.setEditable(false);
		RTextScrollPane statementsScrollPane = new RTextScrollPane();
		statementsScrollPane.setViewportView(statementsPane);
		statementsScrollPane.setLineNumbersEnabled(true);
		tabbedPane.addTab("Statements", UIUtil.scaleIcon(tabbedPane, historyIcon), statementsScrollPane);

		problemsPane = new JTextArea();
		problemsPane.setEditable(false);
		tabbedPane.addTab("Problems", UIUtil.scaleIcon(tabbedPane, warnIcon), new JScrollPane(problemsPane));

		return tabbedPane;
	}

	/**
	 * Keyboard support of the proposals table: Enter opens the details of the selected proposal,
	 * Space checks or unchecks it.
	 */
	@SuppressWarnings("serial")
	private void installKeys() {
		proposalTable.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "showProposalDetails");
		proposalTable.getActionMap().put("showProposalDetails", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				int modelRow = selectedProposalRow();
				if (modelRow >= 0) {
					showDetails(proposals.get(modelRow));
				}
			}
		});
		proposalTable.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, 0), "toggleProposal");
		proposalTable.getActionMap().put("toggleProposal", new AbstractAction() {
			@Override
			public void actionPerformed(ActionEvent e) {
				int modelRow = selectedProposalRow();
				if (modelRow >= 0) {
					if (proposalTable.isEditing()) {
						proposalTable.getCellEditor().cancelCellEditing();
					}
					proposalsModel.setValueAt(!Boolean.TRUE.equals(proposalsModel.getValueAt(modelRow, 0)), modelRow, 0);
					proposalTable.repaint();
				}
			}
		});
	}

	/**
	 * @return the model row of the selected proposal, or <code>-1</code> if none is selected
	 */
	private int selectedProposalRow() {
		int row = proposalTable.getSelectedRow();
		if (row < 0) {
			return -1;
		}
		int modelRow = proposalTable.convertRowIndexToModel(row);
		return modelRow < proposals.size()? modelRow : -1;
	}

	private JTextField filterField;

	/**
	 * Shows only the proposals whose table A, table B or condition contains the text of the filter field.
	 */
	private final Filter textFilter = new Filter() {
		@SuppressWarnings("rawtypes")
		@Override
		public boolean include(Entry entry) {
			String text = filterField == null? "" : filterField.getText().trim().toLowerCase(Locale.ENGLISH);
			if (text.isEmpty()) {
				return true;
			}
			for (int column = 1; column <= 3; ++column) {
				Object value = entry.getValue(column);
				if (value != null && String.valueOf(value).toLowerCase(Locale.ENGLISH).contains(text)) {
					return true;
				}
			}
			return false;
		}
	};

	private TableCellRenderer createRenderer(final JTable table) {
		final TableCellRenderer defaultRenderer = table.getDefaultRenderer(String.class);
		return new TableCellRenderer() {
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				if (value instanceof Double) {
					value = String.format(Locale.US, "%.1f", value) + " %";
				}
				if (table == proposalTable && table.convertColumnIndexToModel(column) == 6 && isMutual(table.convertRowIndexToModel(row))) {
					value = value + "; mutual: also proposed in the reverse direction";
				}
				Component render = defaultRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (value instanceof Boolean) {
					JCheckBox checkBox = new JCheckBox("  ");
					checkBox.setHorizontalAlignment(SwingConstants.RIGHT);
					checkBox.setSelected(Boolean.TRUE.equals(value));
					render = checkBox;
				}
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				if (render instanceof JLabel) {
					((JLabel) render).setToolTipText(value == null? null : UIUtil.toHTML(String.valueOf(value), 200));
				}
				if (table == proposalTable && table.convertColumnIndexToModel(column) == 5) {
					markMatchRatio(table, render, isSelected, row);
				}
				return render;
			}
		};
	}

	/**
	 * Whether a proposal is also proposed in the reverse direction (B to A with the same columns),
	 * which is typical for tables that share their primary key, or for shared IDs.
	 *
	 * @param modelRow the row of the proposal (model index)
	 */
	private boolean isMutual(int modelRow) {
		if (modelRow < 0 || modelRow >= proposals.size()) {
			return false;
		}
		return proposalKeys.contains(proposals.get(modelRow).reverseKey());
	}

	/**
	 * Colors the match cell of a proposal: green if all rows match, orange if the match ratio is close to the threshold.
	 *
	 * @param table the proposals table
	 * @param render the cell's render component
	 * @param isSelected whether the row is selected
	 * @param row the row (view index)
	 */
	private void markMatchRatio(JTable table, Component render, boolean isSelected, int row) {
		int modelRow = table.convertRowIndexToModel(row);
		if (modelRow < 0 || modelRow >= proposals.size()) {
			return;
		}
		AssociationCandidate candidate = proposals.get(modelRow);
		if (candidate.withoutDataCheck || candidate.matchRatio < 0 || candidate.nonNullCount < 0) {
			return;
		}
		boolean all = candidate.orphans == 0;
		boolean close = !all && candidate.matchRatio < runThreshold + CLOSE_TO_THRESHOLD;
		if (!isSelected) {
			if (all) {
				render.setBackground(Colors.Color_220_255_220);
			} else if (close) {
				render.setBackground(Colors.Color_255_230_200);
			}
		}
		if (render instanceof JLabel) {
			long matched = candidate.nonNullCount - Math.max(0, candidate.orphans);
			((JLabel) render).setToolTipText(matched + " of " + candidate.nonNullCount + " rows match"
					+ (close? " (close to the threshold of " + String.format(Locale.US, "%.0f", runThreshold * 100) + " %)" : ""));
		}
	}

	private JPanel createProgressPanel() {
		JPanel panel = new JPanel(new GridBagLayout());
		progressLabel = new JLabel(" ");
		panel.add(progressLabel, gbc(0, 0, 1, GridBagConstraints.HORIZONTAL, 0));
		progressBar = new JProgressBar(0, 1);
		panel.add(progressBar, gbc(0, 1, 1, GridBagConstraints.HORIZONTAL, 0));
		statusLabel = new JLabel(" ");
		statusLabel.setFont(statusLabel.getFont().deriveFont(java.awt.Font.BOLD));
		defaultForeground = statusLabel.getForeground();
		panel.add(statusLabel, gbc(0, 2, 1, GridBagConstraints.HORIZONTAL, 0));
		return panel;
	}

	private JPanel createButtonPanel() {
		JPanel panel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
		selectAllButton = new JButton("Select all");
		selectAllButton.setIcon(UIUtil.scaleIcon(selectAllButton, selectIcon));
		selectAllButton.setToolTipText("Check all proposals that are currently visible, i.e. that are not hidden by a filter.");
		selectAllButton.addActionListener(e -> setProposalsSelected(true));
		panel.add(selectAllButton);
		deselectAllButton = new JButton("Deselect all");
		deselectAllButton.setIcon(UIUtil.scaleIcon(deselectAllButton, clearIcon));
		deselectAllButton.setToolTipText("Uncheck all proposals that are currently visible, i.e. that are not hidden by a filter.");
		deselectAllButton.addActionListener(e -> setProposalsSelected(false));
		panel.add(deselectAllButton);
		acceptButton = new JButton("Accept");
		acceptButton.setIcon(UIUtil.scaleIcon(acceptButton, okIcon));
		acceptButton.setToolTipText("Add the checked associations to the data model.");
		acceptButton.addActionListener(e -> acceptProposals(false));
		panel.add(acceptButton);
		acceptAndEditButton = new JButton("Accept and Edit...");
		acceptAndEditButton.setIcon(UIUtil.scaleIcon(acceptAndEditButton, editIcon));
		acceptAndEditButton.setToolTipText("Open the checked associations in the Data Model Editor, to review them before saving.");
		acceptAndEditButton.addActionListener(e -> acceptProposals(true));
		panel.add(acceptAndEditButton);
		JButton closeButton = new JButton("Close");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
		closeButton.addActionListener(e -> dialog.dispose());
		panel.add(closeButton);
		updateButtonsState();
		return panel;
	}

	/**
	 * Checks or unchecks all proposals that are currently visible, i.e. that are not
	 * hidden by a filter.
	 *
	 * @param selected the new state
	 */
	private void setProposalsSelected(boolean selected) {
		for (int i = 0; i < proposalTable.getRowCount(); ++i) {
			proposalsModel.setValueAt(selected, proposalTable.convertRowIndexToModel(i), 0);
		}
		proposalTable.repaint();
	}

	private void updateButtonsState() {
		int checked = 0;
		for (int i = 0; i < proposalsModel.getRowCount(); ++i) {
			if (Boolean.TRUE.equals(proposalsModel.getValueAt(i, 0))) {
				++checked;
			}
		}
		boolean any = checked > 0;
		String count = any? " (" + checked + ")" : "";
		acceptButton.setText("Accept" + count);
		acceptAndEditButton.setText("Accept and Edit" + count + "...");
		acceptButton.setEnabled(any && !running);
		acceptAndEditButton.setEnabled(any && !running);
		selectAllButton.setEnabled(proposalsModel.getRowCount() > 0);
		deselectAllButton.setEnabled(any);
	}

	/**
	 * Opens the editor for the naming rules of this data model.
	 */
	private void editNamingRules() {
		new NamingRulesEditor(owner, executionContext, NamingRules.load(executionContext));
		updateRulesButtonText();
	}

	/**
	 * Puts the number of rules into the label of the button, so that it is visible without
	 * opening the editor.
	 */
	private void updateRulesButtonText() {
		int count = NamingRules.load(executionContext).getRules().size();
		rulesButton.setText(count > 0? RULES_BUTTON_TEXT + " (" + count + ")" : RULES_BUTTON_TEXT);
	}

	/**
	 * Starts the discovery in the background.
	 */
	private void start() {
		if (running) {
			return;
		}
		running = true;
		cancelled = false;
		proposals.clear();
		proposalKeys.clear();
		proposalsModel.setRowCount(0);
		known.clear();
		knownModel.setRowCount(0);
		statementsPane.setText("");
		problemsPane.setText("");
		synchronized (bufferLock) {
			pendingStatements.setLength(0);
			pendingProblems.setLength(0);
			pendingProposals.clear();
			pendingKnown.clear();
			pendingProgressText = null;
			shownStatements = 0;
		}
		proposalColumnsAdjusted = false;
		knownColumnsAdjusted = false;
		statusLabel.setText(" ");
		statusLabel.setForeground(defaultForeground);
		startButton.setEnabled(false);
		dataScanCheckBox.setEnabled(false);
		thresholdComboBox.setEnabled(false);
		rulesButton.setEnabled(false);
		updateButtonsState();

		final boolean dataScan = dataScanCheckBox.isSelected();
		final double threshold = threshold();
		runThreshold = threshold;
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				try {
					discover(dataScan, threshold);
				} catch (Throwable t) {
					final Throwable error = t;
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							UIUtil.showException(AssociationDiscoveryView.this, "Error", error);
						}
					});
				} finally {
					UIUtil.invokeLater(new Runnable() {
						@Override
						public void run() {
							flush();
							running = false;
							startButton.setEnabled(true);
							dataScanCheckBox.setEnabled(true);
							thresholdComboBox.setEnabled(true);
							rulesButton.setEnabled(true);
							progressLabel.setText(cancelled? "Cancelled." : "Done.");
							updateButtonsState();
						}
					});
				}
			}
		}, "association-discovery");
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * The discovery itself. Runs outside of the event dispatch thread.
	 *
	 * @param dataScan also look for candidates without name evidence
	 * @param threshold minimum fraction of matching rows
	 */
	private void discover(boolean dataScan, double threshold) {
		// the phase the finder is currently in, for its progress messages
		final String[] phase = new String[] { "Matching names..." };
		DiscoveryLog log = new DiscoveryLog() {
			@Override
			public void statement(String sql) {
				synchronized (bufferLock) {
					if (shownStatements < MAX_SHOWN_STATEMENTS) {
						++shownStatements;
						pendingStatements.append(sql + ";\n");
						if (shownStatements == MAX_SHOWN_STATEMENTS) {
							pendingStatements.append("-- only the first " + MAX_SHOWN_STATEMENTS + " statements are shown\n");
						}
						scheduleFlush();
					}
				}
			}
			@Override
			public void problem(String message) {
				synchronized (bufferLock) {
					pendingProblems.append(message + "\n");
					scheduleFlush();
				}
			}
			@Override
			public void progress(int done, int total) {
				AssociationDiscoveryView.this.progress(phase[0] + " (" + done + " of " + total + ")", done, total);
			}
			@Override
			public boolean isCancelled() {
				return cancelled;
			}
		};

		AssociationCandidateFinder finder = new AssociationCandidateFinder(dataModel, NamingRules.load(executionContext));
		List<Table> tables = finder.getTables();
		progress(phase[0], 0, tables.size());
		List<AssociationCandidate> candidates = finder.findByName(log);
		if (cancelled) {
			return;
		}
		phase[0] = "Applying naming rules...";
		candidates.addAll(finder.findByRegexRules(keysOf(candidates), log));
		if (cancelled) {
			return;
		}

		// profile all tables: needed for the confidence and, if the data
		// scan is on, for the pre-filter. Not needed at all if every candidate comes from a
		// rule that is accepted without checking the data
		boolean profilingNeeded = dataScan;
		for (AssociationCandidate candidate: candidates) {
			if (!candidate.withoutDataCheck) {
				profilingNeeded = true;
				break;
			}
		}
		Map<Table, TableProfile> profiles = new HashMap<Table, TableProfile>();
		if (profilingNeeded) {
			TableProfiler profiler = new TableProfiler(session, cancellationContext, log);
			progress("Profiling tables...", 0, tables.size());
			int i = 0;
			for (Table table: tables) {
				if (cancelled) {
					return;
				}
				profiles.put(table, profiler.profile(table));
				progress("Profiling tables... (" + table.getName() + ")", ++i, tables.size());
			}
		}

		if (dataScan && !cancelled) {
			candidates.addAll(finder.findByData(profiles, keysOf(candidates)));
			if (finder.getPartiallyCheckedPairs() > 0) {
				log.problem("For " + finder.getPartiallyCheckedPairs() + " pairs of tables the data scan did not check "
						+ "all possible column combinations. A naming rule would resolve those pairs exactly.");
			}
		}
		if (cancelled) {
			return;
		}

		for (AssociationCandidate known: finder.removeKnown(candidates)) {
			addKnown(known);
		}

		// the most reliable candidates first, so that the proposals appear in that order
		// and a cancelled run has checked the most promising candidates
		Collections.sort(candidates, new Comparator<AssociationCandidate>() {
			@Override
			public int compare(AssociationCandidate o1, AssociationCandidate o2) {
				return o1.evidence.ordinal() - o2.evidence.ordinal();
			}
		});
		int nameCandidates = 0;
		for (AssociationCandidate candidate: candidates) {
			if (candidate.evidence != AssociationCandidate.Evidence.DATA_ONLY) {
				++nameCandidates;
			}
		}

		CandidateVerifier verifier = new CandidateVerifier(session, cancellationContext, profiles, log);
		int statements = 0;
		int checked = 0;
		int rejected = 0;
		int unchecked = 0;
		progress("Verifying candidates...", 0, candidates.size());
		for (AssociationCandidate candidate: candidates) {
			if (cancelled) {
				return;
			}
			if (candidate.withoutDataCheck) {
				// the rule that found it is binding according to the user
				addProposal(candidate);
				++unchecked;
			} else {
				statements += verifier.verify(candidate);
				if (candidate.matchRatio >= threshold) {
					addProposal(candidate);
				} else {
					++rejected;
				}
			}
			++checked;
			progress("Verifying candidates... (" + checked + " of " + candidates.size() + ")", checked, candidates.size());
		}
		status(nameCandidates, candidates.size() - nameCandidates, rejected, statements, unchecked);
	}

	/**
	 * Gets the keys of the given candidates, to recognize duplicates of a later pass.
	 *
	 * @param candidates the candidates
	 * @return their keys
	 */
	private Set<String> keysOf(List<AssociationCandidate> candidates) {
		Set<String> keys = new HashSet<String>();
		for (AssociationCandidate candidate: candidates) {
			keys.add(candidate.key());
		}
		return keys;
	}

	private void progress(String message, int value, int maximum) {
		synchronized (bufferLock) {
			pendingProgressText = message;
			pendingProgressValue = value;
			pendingProgressMaximum = maximum;
			scheduleFlush();
		}
	}

	/**
	 * Requests that the buffered updates are applied on the event dispatch thread.
	 * Must be called while holding the buffer lock. While a flush is still pending,
	 * further updates only add to the buffers, so a burst of queries results in a single
	 * UI update instead of one per query.
	 */
	private void scheduleFlush() {
		if (!flushScheduled) {
			flushScheduled = true;
			UIUtil.invokeLater(new Runnable() {
				@Override
				public void run() {
					flush();
				}
			});
		}
	}

	/**
	 * Applies the buffered updates. Runs on the event dispatch thread.
	 */
	private void flush() {
		String statements;
		String problems;
		List<AssociationCandidate> newProposals;
		List<AssociationCandidate> newKnown;
		String progressText;
		int progressValue;
		int progressMaximum;
		synchronized (bufferLock) {
			flushScheduled = false;
			statements = pendingStatements.toString();
			pendingStatements.setLength(0);
			problems = pendingProblems.toString();
			pendingProblems.setLength(0);
			newProposals = new ArrayList<AssociationCandidate>(pendingProposals);
			pendingProposals.clear();
			newKnown = new ArrayList<AssociationCandidate>(pendingKnown);
			pendingKnown.clear();
			progressText = pendingProgressText;
			pendingProgressText = null;
			progressValue = pendingProgressValue;
			progressMaximum = pendingProgressMaximum;
		}
		if (statements.length() > 0) {
			statementsPane.append(statements);
		}
		if (problems.length() > 0) {
			problemsPane.append(problems);
		}
		for (AssociationCandidate candidate: newKnown) {
			known.add(candidate);
			knownModel.addRow(new Object[] {
					dataModel.getDisplayName(candidate.child),
					dataModel.getDisplayName(candidate.parent),
					candidate.getCondition() });
		}
		for (AssociationCandidate candidate: newProposals) {
			proposals.add(candidate);
			proposalKeys.add(candidate.key());
			proposalsModel.addRow(new Object[] {
					Boolean.valueOf(candidate.evidence != AssociationCandidate.Evidence.DATA_ONLY),
					dataModel.getDisplayName(candidate.child),
					dataModel.getDisplayName(candidate.parent),
					candidate.getCondition(),
					candidate.withoutDataCheck? Double.valueOf(100.0) : Double.valueOf(candidate.confidence),
					// rounded down, so that 100 % is only shown if all rows match
					candidate.withoutDataCheck || candidate.matchRatio < 0? null : Double.valueOf(Math.floor(candidate.matchRatio * 1000) / 10),
					candidate.getEvidenceText() });
		}
		if (!newProposals.isEmpty()) {
			updateButtonsState();
		}
		if (!proposalColumnsAdjusted && proposalsModel.getRowCount() >= ADJUST_COLUMNS_AT) {
			proposalColumnsAdjusted = true;
			UIUtil.adjustTableColumnsWidth(proposalTable, true);
			// the check box column: a little more room than its content needs
			javax.swing.table.TableColumn checkBoxColumn = proposalTable.getColumnModel().getColumn(0);
			checkBoxColumn.setPreferredWidth(checkBoxColumn.getPreferredWidth() + 10);
			checkBoxColumn.setWidth(checkBoxColumn.getPreferredWidth());
		}
		if (!knownColumnsAdjusted && knownModel.getRowCount() >= ADJUST_COLUMNS_AT) {
			knownColumnsAdjusted = true;
			UIUtil.adjustTableColumnsWidth(knownTable, false);
		}
		if (progressText != null) {
			progressLabel.setText(progressText);
			progressBar.setMaximum(progressMaximum == 0? 1 : progressMaximum);
			progressBar.setValue(progressValue);
		}
	}

	/**
	 * Shows the result of a finished run.
	 *
	 * @param nameCandidates number of candidates that had name evidence
	 * @param dataCandidates number of candidates that came from the data scan
	 * @param rejected number of candidates below the threshold
	 * @param statements number of verification queries executed
	 * @param unchecked number of candidates accepted without checking the data
	 */
	private void status(final int nameCandidates, final int dataCandidates, final int rejected, final int statements,
			final int unchecked) {
		UIUtil.invokeLater(new Runnable() {
			@Override
			public void run() {
				flush();
				int candidateCount = nameCandidates + dataCandidates;
				StringBuilder text = new StringBuilder(proposals.size() + " of " + candidateCount + " candidates confirmed");
				text.append(" (" + nameCandidates + " from names, " + dataCandidates + " from the data scan)");
				int known = knownModel.getRowCount();
				if (known > 0) {
					text.append(", " + known + " already known");
				}
				if (rejected > 0) {
					text.append(", " + rejected + " rejected");
				}
//				text.append(", " + statements + " verification queries");
				if (unchecked > 0) {
					text.append(", " + unchecked + " accepted without data check");
				}
				statusLabel.setForeground(Colors.Color_0_80_200);
				statusLabel.setText(text.toString());
			}
		});
	}

	private void addProposal(AssociationCandidate candidate) {
		synchronized (bufferLock) {
			pendingProposals.add(candidate);
			scheduleFlush();
		}
	}

	private void addKnown(AssociationCandidate candidate) {
		synchronized (bufferLock) {
			pendingKnown.add(candidate);
			scheduleFlush();
		}
	}

	/**
	 * Writes the checked proposals into the model builder's associations file.
	 */
	/**
	 * Accepts the checked proposals.
	 *
	 * @param edit if <code>true</code>, they are handed over to the Data Model Editor (by the caller) for review,
	 *        otherwise they are added to the data model directly
	 */
	private void acceptProposals(boolean edit) {
		try {
			List<AssociationProposalWriter.Proposal> accepted = new ArrayList<AssociationProposalWriter.Proposal>();
			for (int i = 0; i < proposalsModel.getRowCount(); ++i) {
				if (Boolean.TRUE.equals(proposalsModel.getValueAt(i, 0))) {
					AssociationCandidate candidate = proposals.get(i);
					accepted.add(new AssociationProposalWriter.Proposal(
							candidate.child.getName(), candidate.parent.getName(), candidate.getCondition()));
				}
			}
			if (accepted.isEmpty()) {
				return;
			}
			if (edit) {
				int[] counts = AssociationProposalWriter.write(accepted, dataModel, executionContext);
				this.accepted = true;
				this.editRequested = true;
				if (counts[1] > 0) {
					JOptionPane.showMessageDialog(dialog, counts[1] + " of " + counts[0] + " associations are already known.", "", JOptionPane.INFORMATION_MESSAGE);
				}
			} else {
				int[] counts = AssociationProposalWriter.addToDataModel(accepted, dataModel, executionContext);
				int added = counts[0] - counts[1];
				this.accepted = added > 0;
				JOptionPane.showMessageDialog(dialog,
						added + (added == 1? " association" : " associations") + " added to the data model."
						+ (counts[1] > 0? "\n" + counts[1] + (counts[1] == 1? " was" : " were") + " already known." : ""),
						"Accept", JOptionPane.INFORMATION_MESSAGE);
			}
			dialog.dispose();
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
	}

	private double threshold() {
		String item = String.valueOf(thresholdComboBox.getSelectedItem()).replace("%", "").trim();
		try {
			return Double.parseDouble(item) / 100.0;
		} catch (NumberFormatException e) {
			return 0.95;
		}
	}

	/**
	 * Opens the details dialog on double-click and the context menu of a row of a table of candidates.
	 */
	private class CandidateMouseListener extends MouseAdapter {
		private final JTable table;
		private final List<AssociationCandidate> candidates;
		private final boolean hasCheckBoxColumn;

		/**
		 * @param table the table
		 * @param candidates the candidates, in the order of the table model rows
		 * @param hasCheckBoxColumn whether column 0 is the check box column (clicking it toggles the check box)
		 */
		CandidateMouseListener(JTable table, List<AssociationCandidate> candidates, boolean hasCheckBoxColumn) {
			this.table = table;
			this.candidates = candidates;
			this.hasCheckBoxColumn = hasCheckBoxColumn;
		}

		private AssociationCandidate candidateAt(MouseEvent e) {
			int row = table.rowAtPoint(e.getPoint());
			if (row < 0) {
				return null;
			}
			int modelRow = table.convertRowIndexToModel(row);
			return modelRow < candidates.size()? candidates.get(modelRow) : null;
		}

		@Override
		public void mouseClicked(MouseEvent e) {
			if (e.getClickCount() == 2 && SwingUtilities.isLeftMouseButton(e)) {
				if (hasCheckBoxColumn && table.convertColumnIndexToModel(table.columnAtPoint(e.getPoint())) == 0) {
					return;
				}
				AssociationCandidate candidate = candidateAt(e);
				if (candidate != null) {
					showDetails(candidate);
				}
			}
		}

		@Override
		public void mousePressed(MouseEvent e) {
			maybeShowPopup(e);
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			maybeShowPopup(e);
		}

		private void maybeShowPopup(MouseEvent e) {
			if (!e.isPopupTrigger()) {
				return;
			}
			AssociationCandidate candidate = candidateAt(e);
			if (candidate == null) {
				return;
			}
			int row = table.rowAtPoint(e.getPoint());
			if (table.getRowSelectionAllowed()) {
				table.setRowSelectionInterval(row, row);
			}
			JPopupMenu popup = new JPopupMenu();
			JMenuItem details = new JMenuItem("Details");
			details.setIcon(UIUtil.scaleIcon(details, detailsIcon));
			details.setToolTipText("Show all details of the association (same as double-click).");
			details.addActionListener(ev -> showDetails(candidate));
			popup.add(details);
			if (openInSqlConsole != null) {
				JMenu sqlConsoleMenu = new JMenu("SQL Console");
				sqlConsoleMenu.setIcon(UIUtil.scaleIcon(sqlConsoleMenu, sqlConsoleIcon));
				sqlConsoleMenu.setToolTipText("Queries about the proposed association, in the SQL Console.");
				for (JMenuItem item: CandidateQueries.createMenuItems(candidate, openInSqlConsole, sqlConsoleIcon)) {
					if (item == null) {
						sqlConsoleMenu.addSeparator();
					} else {
						sqlConsoleMenu.add(item);
					}
				}
				popup.add(sqlConsoleMenu);
			}
			if (showTable != null) {
				popup.addSeparator();
				popup.add(tableItem("Show Table A", tableIcon, "Select table " + candidate.child.getUnqualifiedName() + " in the extraction model editor.", () -> showTable.accept(candidate.child)));
				popup.add(tableItem("Show Table B", tableIcon, "Select table " + candidate.parent.getUnqualifiedName() + " in the extraction model editor.", () -> showTable.accept(candidate.parent)));
			}
			if (browseTables != null) {
				popup.addSeparator();
				popup.add(tableItem("Browse Table A and B", databaseIcon, "Open tables " + candidate.child.getUnqualifiedName() + " and "
						+ candidate.parent.getUnqualifiedName() + " in the Data Browser.", () -> browseTables.accept(candidate.child, candidate.parent)));
			}
			UIUtil.showPopup(table, e.getX(), e.getY(), popup);
		}

		private JMenuItem tableItem(String text, ImageIcon icon, String toolTip, Runnable action) {
			JMenuItem item = new JMenuItem(text);
			item.setIcon(UIUtil.scaleIcon(item, icon));
			item.setToolTipText(toolTip);
			item.addActionListener(ev -> action.run());
			return item;
		}
	}

	/**
	 * Opens the details dialog of a candidate.
	 */
	private void showDetails(AssociationCandidate candidate) {
		// a proposal can be checked for acceptance in the details too
		int proposalRow = -1;
		for (int i = 0; i < proposals.size(); ++i) {
			if (proposals.get(i) == candidate) {
				proposalRow = i;
				break;
			}
		}
		Boolean accept = null;
		Consumer<Boolean> onAccept = null;
		if (proposalRow >= 0) {
			final int row = proposalRow;
			accept = Boolean.TRUE.equals(proposalsModel.getValueAt(row, 0));
			onAccept = selected -> {
				proposalsModel.setValueAt(selected, row, 0);
				proposalTable.repaint();
			};
		}
		new AssociationCandidateDetailsView(dialog, dataModel, candidate, accept, onAccept, openInSqlConsole);
	}

	/**
	 * Shows a status while an action that takes a while runs. The wait cursor alone is not enough:
	 * the main window is blocked by this dialog, and a blocked window always shows the default cursor,
	 * e.g. if the mouse is outside of this dialog after clicking a menu that extends beyond it.
	 *
	 * @param status the status text
	 * @param action the action. It defers its work (invokeLater), the status is reset afterwards
	 */
	private void withStatus(String status, Runnable action) {
		String oldText = statusLabel.getText();
		Color oldForeground = statusLabel.getForeground();
		statusLabel.setForeground(Colors.Color_0_80_200);
		statusLabel.setText(status);
		try {
			action.run();
		} finally {
			UIUtil.invokeLater(() -> {
				// unless the status has changed in the meantime
				if (status.equals(statusLabel.getText())) {
					statusLabel.setText(oldText);
					statusLabel.setForeground(oldForeground);
				}
			});
		}
	}

	private ImageIcon runIcon;
	private ImageIcon selectIcon;
	private ImageIcon clearIcon;
	private ImageIcon okIcon;
	private ImageIcon cancelIcon;
	private ImageIcon addIcon;
	private ImageIcon modelIcon;
	private ImageIcon historyIcon;
	private ImageIcon warnIcon;
	private ImageIcon editIcon;
	private ImageIcon detailsIcon;
	private ImageIcon tableIcon;
	private ImageIcon databaseIcon;
	private ImageIcon sqlConsoleIcon;
	{
		// load images
		runIcon = UIUtil.readImage("/run.png");
		selectIcon = UIUtil.readImage("/select.png");
		clearIcon = UIUtil.readImage("/clear.png");
		okIcon = UIUtil.readImage("/buttonok.png");
		cancelIcon = UIUtil.readImage("/buttoncancel.png");
		addIcon = UIUtil.readImage("/add.png");
		modelIcon = UIUtil.readImage("/model.png");
		historyIcon = UIUtil.readImage("/history.png");
		warnIcon = UIUtil.readImage("/wanr.png");
		editIcon = UIUtil.readImage("/edit.png");
		detailsIcon = UIUtil.readImage("/editdetails.png");
		tableIcon = UIUtil.readImage("/table.png");
		databaseIcon = UIUtil.readImage("/database.png");
		sqlConsoleIcon = UIUtil.readImage("/sqlconsole.png");
	}

}
