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
import java.awt.Dialog;
import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.UIUtil;

/**
 * Shows all details of a {@link NormalizationFinding}.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class NormalizationFindingDetailsView extends JPanel {

	/**
	 * Width range of the explanation text, which is wrapped at this width.
	 */
	private static final int MIN_TEXT_WIDTH = 450;
	private static final int MAX_TEXT_WIDTH = 700;

	/**
	 * Maximum number of rows of the columns table that are visible without scrolling.
	 */
	private static final int MAX_VISIBLE_ROWS = 8;

	private final DataModel dataModel;
	private final NormalizationFinding finding;
	private final Runnable showTable;
	private final Runnable browseTable;
	private JDialog dialog;

	/**
	 * Creates the view and opens it as a modal dialog.
	 *
	 * @param owner the owner dialog
	 * @param dataModel the data model
	 * @param finding the finding
	 * @param showTable shows the table (or the foreign key) of the finding, called after this dialog has been closed.
	 *        <code>null</code> if there is no such function
	 * @param browseTable opens the table(s) of the finding in the Data Browser, called after this dialog has been closed.
	 *        <code>null</code> if there is no such function
	 */
	public NormalizationFindingDetailsView(Dialog owner, DataModel dataModel, NormalizationFinding finding, Runnable showTable, Runnable browseTable) {
		this.dataModel = dataModel;
		this.finding = finding;
		this.showTable = showTable;
		this.browseTable = browseTable;

		initUI();
		UIUtil.initComponents(this);

		dialog = new EscapableDialog(owner, "Finding - " + finding.table.getUnqualifiedName()) {
			@Override
			protected void escapePressed() {
				// EscapableDialog would only hide it
				dispose();
			}
		};
		dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		// modal to the main window only, not to Data Browsers
		dialog.setModalityType(ModalityType.DOCUMENT_MODAL);
		dialog.getContentPane().add(this);
		dialog.pack();
		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2);
		UIUtil.fitIntoOwner(dialog);
		dialog.setVisible(true);
	}

	private void initUI() {
		setLayout(new GridBagLayout());
		int y = 0;

		JPanel header = new JPanel(new GridBagLayout());
		int row = 0;
		addField(header, row++, "Table", dataModel.getDisplayName(finding.table));
		addField(header, row++, "Normal form", finding.normalForm.toString());
		addField(header, row++, "Severity", finding.severity.toString());
		addField(header, row++, "Rule", finding.rule);
		if (finding.dataCheck != null) {
			addField(header, row++, "Data check", finding.dataCheck);
		}
		add(header, gbc(y++, GridBagConstraints.HORIZONTAL, 0));

		JTextArea explanation = new JTextArea(finding.explanation);
		explanation.setEditable(false);
		explanation.setLineWrap(true);
		explanation.setWrapStyleWord(true);
		explanation.setFont(new JLabel().getFont());
		explanation.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
		JScrollPane explanationPane = new JScrollPane(explanation);
		explanationPane.setBorder(BorderFactory.createTitledBorder("Explanation"));
		add(explanationPane, gbc(y++, GridBagConstraints.BOTH, 1));

		DefaultTableModel columnsModel;
		if (finding.isDataCheckable()) {
			columnsModel = createModel(new String[] { "Column", "Type", "Original", "Original type" });
			for (Map.Entry<Column, Column> copy: finding.copies.entrySet()) {
				columnsModel.addRow(new Object[] {
						copy.getKey().name,
						NormalizationChecker.typeOf(copy.getKey()),
						finding.parent.getUnqualifiedName() + "." + copy.getValue().name,
						NormalizationChecker.typeOf(copy.getValue()) });
			}
		} else if (finding.fkMapping != null && finding.parent != null) {
			// e.g. a type mismatch: the affected foreign key columns and the columns they reference
			columnsModel = createModel(new String[] { "Column", "Type", "Referenced", "Referenced type" });
			for (Map.Entry<Column, Column> e: finding.fkMapping.entrySet()) {
				if (finding.columns.contains(e.getKey())) {
					columnsModel.addRow(new Object[] {
							e.getKey().name,
							NormalizationChecker.typeOf(e.getKey()),
							finding.parent.getUnqualifiedName() + "." + e.getValue().name,
							NormalizationChecker.typeOf(e.getValue()) });
				}
			}
		} else {
			columnsModel = createModel(new String[] { "Column", "Type", "Nullable" });
			for (Column column: finding.columns) {
				columnsModel.addRow(new Object[] {
						column.name,
						NormalizationChecker.typeOf(column),
						column.isNullable? "yes" : "no" });
			}
		}
		int textWidth = MIN_TEXT_WIDTH;
		if (columnsModel.getRowCount() > 0) {
			LastColumnFillingTable columnsTable = new LastColumnFillingTable(columnsModel);
			columnsTable.setShowGrid(false);
			columnsTable.setDefaultRenderer(Object.class, createRenderer(columnsTable));
			columnsTable.adjustColumnsWidth();
			if (showTable != null) {
				columnsTable.setToolTipText("Double-click a row to select " + finding.getShowTableTarget() + " in the extraction model editor.");
				columnsTable.addMouseListener(new MouseAdapter() {
					@Override
					public void mouseClicked(MouseEvent e) {
						if (e.getClickCount() == 2 && SwingUtilities.isLeftMouseButton(e) && columnsTable.rowAtPoint(e.getPoint()) >= 0) {
							showTable();
						}
					}
				});
			}
			// size for pack(): the table as wide as its columns, as high as its rows (but not too high)
			int tableWidth = 0;
			for (int i = 0; i < columnsTable.getColumnCount(); ++i) {
				tableWidth += columnsTable.getColumnModel().getColumn(i).getPreferredWidth();
			}
			int visibleRows = Math.min(columnsModel.getRowCount(), MAX_VISIBLE_ROWS);
			columnsTable.setPreferredScrollableViewportSize(new Dimension(tableWidth, visibleRows * columnsTable.getRowHeight()));
			textWidth = Math.max(textWidth, tableWidth);
			JPanel columnsPanel = new JPanel(new BorderLayout());
			columnsPanel.setBorder(BorderFactory.createTitledBorder("Columns"));
			columnsPanel.add(new JScrollPane(columnsTable), BorderLayout.CENTER);
			if (finding.fkMapping != null && finding.parent != null) {
				JLabel fkLabel = new JLabel("Foreign key: " + foreignKeyText());
				fkLabel.setBorder(BorderFactory.createEmptyBorder(4, 4, 2, 4));
				columnsPanel.add(fkLabel, BorderLayout.SOUTH);
			}
			add(columnsPanel, gbc(y++, GridBagConstraints.BOTH, 1));
		}

		// size for pack(): the explanation wrapped at the text width, with the height the wrapped text needs
		textWidth = Math.min(textWidth, MAX_TEXT_WIDTH);
		explanation.setSize(textWidth, Short.MAX_VALUE);
		explanationPane.getViewport().setPreferredSize(new Dimension(textWidth, explanation.getPreferredSize().height));

		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
		if (showTable != null) {
			JButton showTableButton = new JButton("Show Table");
			showTableButton.setIcon(UIUtil.scaleIcon(showTableButton, tableIcon));
			showTableButton.setToolTipText("Close this dialog and select " + finding.getShowTableTarget() + " in the extraction model editor.");
			showTableButton.addActionListener(e -> showTable());
			buttonPanel.add(showTableButton);
		}
		if (browseTable != null) {
			JButton browseTableButton = new JButton("Browse Table");
			browseTableButton.setIcon(UIUtil.scaleIcon(browseTableButton, databaseIcon));
			browseTableButton.setToolTipText("Close this dialog and open " + finding.getBrowseTableTarget() + " in the Data Browser.");
			browseTableButton.addActionListener(e -> {
				dialog.dispose();
				browseTable.run();
			});
			buttonPanel.add(browseTableButton);
		}
		JButton closeButton = new JButton("Close");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
		closeButton.addActionListener(e -> dialog.dispose());
		buttonPanel.add(closeButton);
		add(buttonPanel, gbc(y++, GridBagConstraints.HORIZONTAL, 0));
	}

	/**
	 * Closes this dialog and shows the table of the finding.
	 */
	private void showTable() {
		dialog.dispose();
		showTable.run();
	}

	private void addField(JPanel panel, int row, String label, String value) {
		JLabel labelComponent = new JLabel(label + ":");
		labelComponent.setFont(labelComponent.getFont().deriveFont(Font.BOLD));
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = row;
		c.anchor = GridBagConstraints.NORTHWEST;
		c.insets = new Insets(2, 4, 2, 8);
		panel.add(labelComponent, c);
		c = new GridBagConstraints();
		c.gridx = 1;
		c.gridy = row;
		c.weightx = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.NORTHWEST;
		c.insets = new Insets(2, 0, 2, 4);
		panel.add(new JLabel(value), c);
	}

	private GridBagConstraints gbc(int y, int fill, double weighty) {
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = y;
		c.fill = fill;
		c.weightx = 1;
		c.weighty = weighty;
		c.insets = new Insets(2, 4, 2, 4);
		return c;
	}

	private DefaultTableModel createModel(String[] columnNames) {
		return new DefaultTableModel(columnNames, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
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
	 * @return the join condition of the foreign key, e.g. "ORDER_LINE.ORDER_ID = ORDER.ID"
	 */
	private String foreignKeyText() {
		StringBuilder sb = new StringBuilder();
		for (Map.Entry<Column, Column> e: finding.fkMapping.entrySet()) {
			if (sb.length() > 0) {
				sb.append(" and ");
			}
			sb.append(finding.table.getUnqualifiedName() + "." + e.getKey().name + " = " + finding.parent.getUnqualifiedName() + "." + e.getValue().name);
		}
		return sb.toString();
	}

	private ImageIcon cancelIcon;
	private ImageIcon tableIcon;
	private ImageIcon databaseIcon;
	{
		// load images
		cancelIcon = UIUtil.readImage("/buttoncancel.png");
		tableIcon = UIUtil.readImage("/table.png");
		databaseIcon = UIUtil.readImage("/database.png");
	}

}
