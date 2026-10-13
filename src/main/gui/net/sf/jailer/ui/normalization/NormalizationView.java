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
package net.sf.jailer.ui.normalization;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.concurrent.Callable;
import java.util.function.BiConsumer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter.SortKey;
import javax.swing.SortOrder;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import org.fife.rsta.ui.EscapableDialog;

import net.coderazzi.filters.Filter;
import net.coderazzi.filters.gui.AutoChoices;
import net.coderazzi.filters.gui.TableFilterHeader;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.normalization.NormalizationFinding.NormalForm;
import net.sf.jailer.util.CancellationHandler;
import net.sf.jailer.util.LogUtil;

/**
 * Shows possible violations of the normal forms 1NF to 5NF, found by the
 * {@link NormalizationChecker}. <br>
 * If a database session is available, the 2NF/3NF findings are checked against the data
 * in the background, and findings the data does not support are removed.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class NormalizationView extends JPanel {

	private static final int TABLE_COLUMN = 0;
	private static final int NF_COLUMN = 2;
	private static final int SEVERITY_COLUMN = 3;
	private static final int RULE_COLUMN = 4;
	private static final int DATA_CHECK_COLUMN = 5;

	/**
	 * Maximum length of a line of a tooltip.
	 */
	private static final int TOOLTIP_LINE_LENGTH = 100;

	private final DataModel dataModel;
	private final Callable<Session> sessionSupplier;
	private final boolean shutDownSessionOnClose;

	/**
	 * The session, as soon as the supplier has delivered it.
	 */
	private volatile Session session;
	private final BiConsumer<Table, Association> showTable;
	private final BiConsumer<Table, Association> browseTable;
	private final Object cancellationContext = new Object();

	/**
	 * The findings shown in the table, in the order of the rows of the table model.
	 */
	private final List<NormalizationFinding> shownFindings;

	/**
	 * Check box per check (rule name).
	 */
	private final Map<String, JCheckBox> checkBoxes = new LinkedHashMap<String, JCheckBox>();

	private DefaultTableModel findingsModel;
	private LastColumnFillingTable findingsTable;
	private JLabel statusLabel;
	private JLabel rowCountLabel;
	private JPanel progressPanel;
	private JLabel progressLabel;
	private JProgressBar progressBar;
	private JButton copyButton;
	private JDialog dialog;

	private volatile boolean cancelled;

	/**
	 * Checks the data model and shows the result in a non-modal dialog.
	 *
	 * @param owner the owner frame
	 * @param dataModel the data model
	 * @param sessionSupplier supplies the session used to check the findings against the data, <code>null</code> if not connected.
	 *        Called in a background thread, after the dialog has been opened, so it may take a while (e.g. to connect).
	 * @param shutDownSessionOnClose if <code>true</code>, this view shuts the session down when the dialog is closed
	 *        (<code>false</code> for a session that is shared with the caller)
	 * @param showTable shows a table, or selects an association (if not <code>null</code>).
	 *        <code>null</code> if there is no such function
	 * @param browseTable opens a table, and the destination of an association (if not <code>null</code>), in the Data Browser.
	 *        <code>null</code> if there is no such function
	 */
	public NormalizationView(JFrame owner, DataModel dataModel, Callable<Session> sessionSupplier, boolean shutDownSessionOnClose, BiConsumer<Table, Association> showTable, BiConsumer<Table, Association> browseTable) {
		this.dataModel = dataModel;
		this.sessionSupplier = sessionSupplier;
		this.shutDownSessionOnClose = shutDownSessionOnClose;
		this.showTable = showTable;
		this.browseTable = browseTable;
		// filled by the metadata check, which runs in the background (it takes a while for large data models)
		this.shownFindings = new ArrayList<NormalizationFinding>();

		initUI();
		UIUtil.initComponents(this);

		dialog = new EscapableDialog(owner, "Check Normalization") {
			@Override
			protected void escapePressed() {
				// EscapableDialog would only hide it
				dispose();
			}
		};
		// a hidden dialog would neither be reopened nor release its session
		dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		dialog.setModal(false);
		dialog.getContentPane().add(this);
		dialog.pack();
		UIUtil.setDialogSize(dialog, 790, 600);
		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2);
		// the packed size can be far too large (wide columns, long info text)
		UIUtil.fitIntoOwner(dialog);
		dialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				cancel();
				Session s = session;
				if (s != null && shutDownSessionOnClose) {
					shutDown(s);
				}
			}
		});
		dialog.setVisible(true);
		startCheck();
	}

	/**
	 * Runs the metadata check in a background thread, shows the findings
	 * and then starts the check against the data.
	 */
	private void startCheck() {
		progressLabel.setText("Checking the data model...");
		progressBar.setIndeterminate(true);
		progressPanel.setVisible(true);
		revalidate();
		Thread thread = new Thread(() -> {
			List<NormalizationFinding> findings;
			try {
				findings = new NormalizationChecker().check(dataModel);
			} catch (final Throwable t) {
				UIUtil.invokeLater(() -> {
					progressPanel.setVisible(false);
					revalidate();
					UIUtil.showException(NormalizationView.this, "Error", t);
				});
				return;
			}
			UIUtil.invokeLater(() -> {
				showFindings(findings);
				startDataCheck();
			});
		}, "normalization-check");
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * Shows the findings of the metadata check. Runs on the event dispatch thread.
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void showFindings(List<NormalizationFinding> findings) {
		shownFindings.addAll(findings);
		// all rows at once, a single event instead of one per row
		for (NormalizationFinding finding: findings) {
			Vector row = new Vector();
			row.add(dataModel.getDisplayName(finding.table));
			row.add(finding.getColumnNames());
			row.add(finding.normalForm);
			row.add(finding.severity);
			row.add(finding.rule);
			row.add("");
			row.add(finding.explanation);
			findingsModel.getDataVector().add(row);
		}
		findingsModel.fireTableDataChanged();
		updateChecks();
		updateRowCount();
		findingsTable.adjustColumnsWidth();
		copyButton.setEnabled(!shownFindings.isEmpty());
		progressPanel.setVisible(false);
		statusLabel.setText(findings.isEmpty()? "No findings." : " ");
		revalidate();
	}

	/**
	 * @return <code>true</code> if the dialog has not been closed yet
	 */
	public boolean isOpen() {
		return dialog.isDisplayable() && dialog.isVisible();
	}

	/**
	 * Brings the dialog to the front.
	 */
	public void toFront() {
		dialog.toFront();
	}

	/**
	 * Closes the dialog, which cancels the data check and shuts the session down.
	 */
	public void close() {
		dialog.dispose();
	}

	private void initUI() {
		setLayout(new BorderLayout());

		JLabel info = new JLabel("<html><i>Possible violations of the normal forms 1NF to 5NF. "
				+ "The check is heuristic: it uses the metadata of the data model (keys, foreign keys, column names and types). "
				+ "So each finding is a hint that needs to be confirmed. "
				+ "Dependencies that cannot be seen in the metadata (e.g. most BCNF violations) are not found. "
				+ "If connected to a database, the 2NF/3NF findings are checked against the data, "
				+ "and findings the data does not support are removed. "
				+ "Double-click a row for details.</i></html>");
		info.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
		JPanel topPanel = new JPanel(new BorderLayout());
		topPanel.add(info, BorderLayout.NORTH);
		topPanel.add(createChecksPanel(), BorderLayout.CENTER);
		add(topPanel, BorderLayout.NORTH);

		findingsModel = new DefaultTableModel(new String[] { "Table", "Columns", "Normal form", "Severity", "Rule", "Data check", "Explanation" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		findingsTable = new LastColumnFillingTable(findingsModel) {
			/**
			 * The complete text of the cell under the mouse, broken into lines (the explanation is too long for a single line).
			 * Computed on demand, not for every rendering of a cell.
			 */
			@Override
			public String getToolTipText(MouseEvent e) {
				int row = rowAtPoint(e.getPoint());
				int column = columnAtPoint(e.getPoint());
				if (row < 0 || column < 0) {
					return null;
				}
				Object value = getValueAt(row, column);
				if (value == null || "".equals(value)) {
					return null;
				}
				return UIUtil.toHTML(wrap(String.valueOf(value), TOOLTIP_LINE_LENGTH), 0);
			}
		};
		findingsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		findingsTable.setShowGrid(false);
		findingsTable.setDefaultRenderer(Object.class, createRenderer(findingsTable));
		findingsTable.setAutoCreateRowSorter(true);
		List<SortKey> keys = new ArrayList<SortKey>();
		keys.add(new SortKey(NF_COLUMN, SortOrder.ASCENDING));
		keys.add(new SortKey(TABLE_COLUMN, SortOrder.ASCENDING));
		findingsTable.getRowSorter().setSortKeys(keys);
		TableFilterHeader filterHeader = new TableFilterHeader();
		filterHeader.setAutoChoices(AutoChoices.ENABLED);
		filterHeader.setTable(findingsTable);
		filterHeader.setMaxVisibleRows(20);
		filterHeader.setRowHeightDelta(2);
		// the explanations are long and (almost) all different, so a list of choices would be useless and expensive
		filterHeader.getFilterEditor(findingsModel.findColumn("Explanation")).setAutoChoices(AutoChoices.DISABLED);
		filterHeader.addFilter(ruleFilter);
		findingsTable.adjustColumnsWidth();
		findingsTable.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2 && SwingUtilities.isLeftMouseButton(e)) {
					NormalizationFinding finding = findingAt(e);
					if (finding != null) {
						showDetails(finding);
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
				if (e.isPopupTrigger()) {
					int row = findingsTable.rowAtPoint(e.getPoint());
					NormalizationFinding finding = findingAt(e);
					if (finding != null) {
						findingsTable.setRowSelectionInterval(row, row);
						JPopupMenu popup = new JPopupMenu();
						JMenuItem detailsItem = new JMenuItem("Details");
						detailsItem.setIcon(UIUtil.scaleIcon(detailsItem, detailsIcon));
						detailsItem.setToolTipText("Show all details of the finding (same as double-click).");
						detailsItem.addActionListener(ev -> showDetails(finding));
						popup.add(detailsItem);
						if (showTable != null) {
							JMenuItem showTableItem = new JMenuItem("Show Table");
							showTableItem.setIcon(UIUtil.scaleIcon(showTableItem, tableIcon));
							showTableItem.setToolTipText("Select " + finding.getShowTableTarget() + " in the extraction model editor.");
							showTableItem.addActionListener(ev -> showTable(finding));
							popup.add(showTableItem);
						}
						if (browseTable != null) {
							JMenuItem browseTableItem = new JMenuItem("Browse Table");
							browseTableItem.setIcon(UIUtil.scaleIcon(browseTableItem, databaseIcon));
							browseTableItem.setToolTipText("Open " + finding.getBrowseTableTarget() + " in the Data Browser.");
							browseTableItem.addActionListener(ev -> browseTable(finding));
							popup.add(browseTableItem);
						}
						UIUtil.showPopup(findingsTable, e.getX(), e.getY(), popup);
					}
				}
			}
		});
		add(new JScrollPane(findingsTable), BorderLayout.CENTER);

		JPanel bottomPanel = new JPanel(new BorderLayout());

		progressPanel = new JPanel(new BorderLayout(4, 0));
		progressPanel.setBorder(BorderFactory.createEmptyBorder(2, 4, 2, 4));
		progressLabel = new JLabel(" ");
		progressPanel.add(progressLabel, BorderLayout.WEST);
		progressBar = new JProgressBar(0, 1);
		progressPanel.add(progressBar, BorderLayout.CENTER);
		JButton cancelCheckButton = new JButton("Cancel");
		cancelCheckButton.setIcon(UIUtil.scaleIcon(cancelCheckButton, cancelIcon));
		cancelCheckButton.setToolTipText("Stop connecting to or checking against the database. The findings not yet checked remain unchecked.");
		cancelCheckButton.addActionListener(e -> cancel());
		progressPanel.add(cancelCheckButton, BorderLayout.EAST);
		progressPanel.setVisible(false);
		bottomPanel.add(progressPanel, BorderLayout.NORTH);

		JPanel labelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 2));
		rowCountLabel = new JLabel();
		labelPanel.add(rowCountLabel);
		statusLabel = new JLabel(" ");
		statusLabel.setBorder(BorderFactory.createEmptyBorder(0, 12, 0, 0));
		labelPanel.add(statusLabel);
		bottomPanel.add(labelPanel, BorderLayout.WEST);
		updateRowCount();
		findingsTable.getRowSorter().addRowSorterListener(e -> updateRowCount());
		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
		copyButton = new JButton("Copy");
		copyButton.setIcon(UIUtil.scaleIcon(copyButton, copyIcon));
		copyButton.setToolTipText("Copy the findings that are currently visible, i.e. that are not hidden by a filter, to the clipboard.");
		copyButton.addActionListener(e -> copyToClipboard());
		copyButton.setEnabled(!shownFindings.isEmpty());
		buttonPanel.add(copyButton);
		JButton closeButton = new JButton("Close");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
		closeButton.addActionListener(e -> dialog.dispose());
		buttonPanel.add(closeButton);
		bottomPanel.add(buttonPanel, BorderLayout.EAST);
		add(bottomPanel, BorderLayout.SOUTH);
	}

	/**
	 * @return the finding in the row at the position of a mouse event, or <code>null</code>
	 */
	private NormalizationFinding findingAt(MouseEvent e) {
		int row = findingsTable.rowAtPoint(e.getPoint());
		return row < 0? null : shownFindings.get(findingsTable.convertRowIndexToModel(row));
	}

	/**
	 * Opens the details view of a finding.
	 */
	private void showDetails(NormalizationFinding finding) {
		new NormalizationFindingDetailsView(dialog, dataModel, finding,
				showTable == null? null : () -> showTable(finding),
				browseTable == null? null : () -> browseTable(finding));
	}

	/**
	 * Shows the table of a finding.
	 */
	private void showTable(NormalizationFinding finding) {
		showTable.accept(finding.table, finding.association);
	}

	/**
	 * Opens the table(s) of a finding in the Data Browser.
	 */
	private void browseTable(NormalizationFinding finding) {
		UIUtil.invokeLater(() -> browseTable.accept(finding.table, finding.association));
	}

	/**
	 * Creates one check box per check, which shows or hides the findings of the check.
	 */
	private JPanel createChecksPanel() {
		JPanel panel = new JPanel(new GridLayout(0, 4, 8, 0));
		panel.setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createTitledBorder("Checks"), BorderFactory.createEmptyBorder(0, 4, 2, 4)));
		for (Map.Entry<String, NormalForm> rule: NormalizationChecker.RULES.entrySet()) {
			JCheckBox checkBox = new JCheckBox(rule.getKey());
			checkBox.setSelected(true);
			checkBox.setToolTipText("Show the findings of the check \"" + rule.getKey() + "\" (" + rule.getValue() + ")");
			checkBox.addItemListener(e -> ruleFilter.reportFilterUpdatedToObservers());
			checkBoxes.put(rule.getKey(), checkBox);
			JButton helpButton = new JButton();
			helpButton.setIcon(UIUtil.scaleIcon(helpButton, helpIcon, 0.8));
			helpButton.setBorderPainted(false);
			helpButton.setContentAreaFilled(false);
			helpButton.setFocusable(false);
			helpButton.setMargin(new Insets(0, 0, 0, 0));
			helpButton.setToolTipText("What does the check \"" + rule.getKey() + "\" look for?");
			final String ruleName = rule.getKey();
			helpButton.addActionListener(e -> showRuleHelp(ruleName));
			JPanel cell = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
			cell.add(checkBox);
			cell.add(helpButton);
			panel.add(cell);
		}
		return panel;
	}

	/**
	 * Width of the text of the rule definition dialog.
	 */
	private static final int RULE_HELP_WIDTH = 450;

	/**
	 * Shows the definition of a check in a modal dialog.
	 */
	private void showRuleHelp(String rule) {
		final JDialog helpDialog = new EscapableDialog(dialog, NormalizationChecker.RULES.get(rule) + ": " + rule) {
			@Override
			protected void escapePressed() {
				// EscapableDialog would only hide it
				dispose();
			}
		};
		helpDialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		helpDialog.setModalityType(ModalityType.DOCUMENT_MODAL);

		JEditorPane text = new JEditorPane("text/html", "<html>" + NormalizationChecker.RULE_DEFINITIONS.get(rule) + "</html>");
		text.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
		text.setFont(new JLabel().getFont());
		text.setEditable(false);
		text.setBorder(BorderFactory.createEmptyBorder(4, 8, 4, 8));
		// the height the text needs at the given width
		text.setSize(RULE_HELP_WIDTH, Short.MAX_VALUE);
		JScrollPane scrollPane = new JScrollPane(text);
		scrollPane.getViewport().setPreferredSize(new Dimension(RULE_HELP_WIDTH, text.getPreferredSize().height));

		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
		JButton closeButton = new JButton("Close");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
		closeButton.addActionListener(e -> helpDialog.dispose());
		buttonPanel.add(closeButton);

		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
		content.add(scrollPane, BorderLayout.CENTER);
		content.add(buttonPanel, BorderLayout.SOUTH);
		UIUtil.initComponents(content);
		helpDialog.getContentPane().add(content);
		helpDialog.pack();
		helpDialog.setLocation(dialog.getX() + (dialog.getWidth() - helpDialog.getWidth()) / 2, dialog.getY() + (dialog.getHeight() - helpDialog.getHeight()) / 2);
		UIUtil.fitIntoOwner(helpDialog);
		helpDialog.setVisible(true);
	}

	/**
	 * Shows the number of findings per check. A check without findings is unchecked and disabled.
	 */
	private void updateChecks() {
		Map<String, Integer> counts = new HashMap<String, Integer>();
		for (NormalizationFinding finding: shownFindings) {
			Integer count = counts.get(finding.rule);
			counts.put(finding.rule, count == null? 1 : count + 1);
		}
		for (Map.Entry<String, JCheckBox> e: checkBoxes.entrySet()) {
			Integer count = counts.get(e.getKey());
			int n = count == null? 0 : count;
			JCheckBox checkBox = e.getValue();
			checkBox.setText(NormalizationChecker.RULES.get(e.getKey()) + ": " + e.getKey() + " (" + n + ")");
			if (n == 0) {
				checkBox.setSelected(false);
			}
			checkBox.setEnabled(n > 0);
		}
	}

	/**
	 * Hides the findings of the checks whose check box is not selected.
	 */
	private final Filter ruleFilter = new Filter() {
		@SuppressWarnings("rawtypes")
		@Override
		public boolean include(Entry entry) {
			JCheckBox checkBox = checkBoxes.get(entry.getValue(RULE_COLUMN));
			return checkBox == null || checkBox.isSelected();
		}
	};

	/**
	 * Shows the number of rows of the table, and how many of them are visible if a filter is active.
	 */
	private void updateRowCount() {
		int total = findingsModel.getRowCount();
		int visible = findingsTable.getRowCount();
		String rows = total == 1? " row" : " rows";
		rowCountLabel.setText(visible == total? total + rows : visible + " of " + total + rows);
	}

	/**
	 * Breaks a text into lines of at most the given length, at blanks.
	 */
	private static String wrap(String text, int lineLength) {
		StringBuilder sb = new StringBuilder();
		int lineStart = 0;
		for (String word: text.split(" ")) {
			if (sb.length() > lineStart && sb.length() - lineStart + 1 + word.length() > lineLength) {
				sb.append("\n");
				lineStart = sb.length();
			} else if (sb.length() > lineStart) {
				sb.append(" ");
			}
			sb.append(word);
		}
		return sb.toString();
	}

	/**
	 * Cancels the check against the data.
	 */
	private void cancel() {
		cancelled = true;
		CancellationHandler.cancelSilently(cancellationContext);
	}

	/**
	 * Whether the session has been shut down.
	 */
	private boolean sessionShutDown;

	/**
	 * Shuts the session down, once. Called on windowClosed or by the worker thread,
	 * whichever notices the closing of the dialog last.
	 */
	private synchronized void shutDown(Session s) {
		if (!sessionShutDown) {
			sessionShutDown = true;
			try {
				s.shutDown();
			} catch (Throwable t) {
				LogUtil.warn(t);
			}
		}
	}

	/**
	 * Gets the session and checks the 2NF/3NF findings against the data, in a background thread,
	 * if there is a session supplier and anything to check.
	 */
	private void startDataCheck() {
		if (sessionSupplier == null) {
			return;
		}
		if (cancelled) {
			// cancelled while the data model was being checked
			statusLabel.setText("Data check cancelled.");
			return;
		}
		final List<NormalizationFinding> checkable = new ArrayList<NormalizationFinding>();
		for (NormalizationFinding finding: shownFindings) {
			if (finding.isDataCheckable()) {
				checkable.add(finding);
			}
		}
		if (checkable.isEmpty()) {
			return;
		}
		progressLabel.setText("Connecting to the database...");
		progressBar.setIndeterminate(true);
		progressPanel.setVisible(true);
		revalidate();
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				int[] confirmed = new int[1];
				int[] uncheckable = new int[1];
				int[] removed = new int[1];
				String[] connectFailure = new String[1];
				try {
					// connecting may take a while. It cannot be interrupted, but it can be abandoned (Cancel)
					Session s;
					try {
						s = sessionSupplier.call();
					} catch (Throwable t) {
						s = null;
						connectFailure[0] = t.getMessage() == null? t.getClass().getSimpleName() : t.getMessage();
					}
					if (cancelled) {
						if (s != null && shutDownSessionOnClose) {
							shutDown(s);
						}
						return;
					}
					if (s == null) {
						if (connectFailure[0] == null) {
							connectFailure[0] = "no connection";
						}
						return;
					}
					session = s;
					if (cancelled) {
						// closed just now, maybe before the session was known to windowClosed
						if (shutDownSessionOnClose) {
							shutDown(s);
						}
						return;
					}
					final NormalizationDataChecker checker = new NormalizationDataChecker(s, cancellationContext);
					UIUtil.invokeLater(() -> {
						progressBar.setIndeterminate(false);
						progressBar.setMaximum(checkable.size());
						progressBar.setValue(0);
					});
					int done = 0;
					for (final NormalizationFinding finding: checkable) {
						if (cancelled) {
							return;
						}
						final int current = done;
						UIUtil.invokeLater(() -> {
							progressLabel.setText("Checking against the data... (" + (current + 1) + " of " + checkable.size() + ")");
							progressBar.setValue(current);
						});
						final boolean show = checker.check(finding);
						if (cancelled) {
							// the result of a cancelled query is meaningless
							return;
						}
						++done;
						if (!show) {
							++removed[0];
						} else if (finding.dataCheck != null && finding.dataCheck.contains("confirmed")) {
							++confirmed[0];
						} else {
							++uncheckable[0];
						}
						UIUtil.invokeLater(() -> applyDataCheck(finding, show));
					}
				} catch (final Throwable t) {
					UIUtil.invokeLater(() -> UIUtil.showException(NormalizationView.this, "Error", t));
				} finally {
					UIUtil.invokeLater(() -> {
						progressPanel.setVisible(false);
						revalidate();
						if (cancelled) {
							statusLabel.setText("Data check cancelled.");
						} else if (connectFailure[0] != null) {
							statusLabel.setText("Could not connect to the database: " + connectFailure[0]);
						} else {
							statusLabel.setText("Data check: " + confirmed[0] + " confirmed, " + removed[0] + " removed (refuted or too little data)"
									+ (uncheckable[0] > 0? ", " + uncheckable[0] + " not checkable" : "") + ".");
						}
						// the "Data check" column has got its content
						findingsTable.adjustColumnsWidth();
					});
				}
			}
		}, "normalization-data-check");
		thread.setDaemon(true);
		thread.start();
	}

	/**
	 * Shows the result of the check of a finding against the data: updates its row,
	 * or removes it if the data does not support it.
	 */
	private void applyDataCheck(NormalizationFinding finding, boolean show) {
		int row = shownFindings.indexOf(finding);
		if (row < 0) {
			return;
		}
		if (show) {
			findingsModel.setValueAt(finding.severity, row, SEVERITY_COLUMN);
			findingsModel.setValueAt(finding.dataCheck, row, DATA_CHECK_COLUMN);
		} else {
			shownFindings.remove(row);
			findingsModel.removeRow(row);
			copyButton.setEnabled(!shownFindings.isEmpty());
			updateChecks();
		}
		updateRowCount();
	}

	private TableCellRenderer createRenderer(final JTable table) {
		final TableCellRenderer defaultRenderer = table.getDefaultRenderer(String.class);
		return new TableCellRenderer() {
			final Color BG1 = UIUtil.TABLE_BACKGROUND_COLOR_1;
			final Color BG2 = UIUtil.TABLE_BACKGROUND_COLOR_2;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component render = defaultRenderer.getTableCellRendererComponent(table, value, isSelected, false, row, column);
				if (!isSelected) {
					render.setBackground((row % 2 == 0) ? BG1 : BG2);
				}
				return render;
			}
		};
	}

	/**
	 * Copies the visible findings as tab separated text to the clipboard.
	 */
	private void copyToClipboard() {
		StringBuilder sb = new StringBuilder();
		for (int c = 0; c < findingsModel.getColumnCount(); ++c) {
			sb.append(c > 0? "\t" : "").append(findingsModel.getColumnName(c));
		}
		sb.append("\n");
		for (int i = 0; i < findingsTable.getRowCount(); ++i) {
			int row = findingsTable.convertRowIndexToModel(i);
			for (int c = 0; c < findingsModel.getColumnCount(); ++c) {
				sb.append(c > 0? "\t" : "").append(findingsModel.getValueAt(row, c));
			}
			sb.append("\n");
		}
		StringSelection selection = new StringSelection(sb.toString());
		Toolkit.getDefaultToolkit().getSystemClipboard().setContents(selection, selection);
	}

	private ImageIcon copyIcon;
	private ImageIcon tableIcon;
	private ImageIcon databaseIcon;
	private ImageIcon detailsIcon;
	private ImageIcon helpIcon;
	private ImageIcon cancelIcon;
	{
		// load images
		copyIcon = UIUtil.readImage("/copy.png");
		tableIcon = UIUtil.readImage("/table.png");
		databaseIcon = UIUtil.readImage("/database.png");
		detailsIcon = UIUtil.readImage("/editdetails.png");
		helpIcon = UIUtil.readImage("/help.png");
		cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

}
