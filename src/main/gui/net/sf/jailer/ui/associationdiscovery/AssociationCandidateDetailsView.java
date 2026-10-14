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
import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Dialog;
import java.util.List;
import java.util.Locale;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.normalization.LastColumnFillingTable;

/**
 * Shows all details of an {@link AssociationCandidate}.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class AssociationCandidateDetailsView extends JPanel {

	/**
	 * Width of the text areas.
	 */
	private static final int TEXT_WIDTH = 500;

	/**
	 * Maximum number of rows of the columns table that are visible without scrolling.
	 */
	private static final int MAX_VISIBLE_ROWS = 8;

	private final DataModel dataModel;
	private final AssociationCandidate candidate;
	private final Boolean accept;
	private final Consumer<Boolean> onAccept;
	private final BiConsumer<List<String>, Boolean> openInSqlConsole;
	private JDialog dialog;

	/**
	 * Creates the view and opens it as a dialog that is modal to the owner's window only.
	 *
	 * @param owner the owner dialog
	 * @param dataModel the data model
	 * @param candidate the candidate
	 * @param accept whether the candidate is checked for acceptance, <code>null</code> if it cannot be accepted
	 * @param onAccept called when the user checks or unchecks the candidate for acceptance
	 * @param openInSqlConsole opens SQL statements in the SQL Console (statements, execute them), <code>null</code> if there is no such function
	 */
	public AssociationCandidateDetailsView(Dialog owner, DataModel dataModel, AssociationCandidate candidate,
			Boolean accept, Consumer<Boolean> onAccept, BiConsumer<List<String>, Boolean> openInSqlConsole) {
		this.dataModel = dataModel;
		this.candidate = candidate;
		this.accept = accept;
		this.onAccept = onAccept;
		this.openInSqlConsole = openInSqlConsole;

		initUI();
		UIUtil.initComponents(this);

		dialog = new EscapableDialog(owner, "Association - " + candidate.child.getUnqualifiedName() + " -> " + candidate.parent.getUnqualifiedName()) {
			@Override
			protected void escapePressed() {
				// EscapableDialog would only hide it
				dispose();
			}
		};
		dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		// modal to the owner's window only, not to Data Browsers
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
		addField(header, row++, "Table A", dataModel.getDisplayName(candidate.child));
		addField(header, row++, "Table B", dataModel.getDisplayName(candidate.parent));
		addField(header, row++, "Condition", candidate.getCondition());
		addField(header, row++, "Evidence", candidate.getEvidenceText());
		addField(header, row++, "Confidence", candidate.withoutDataCheck? "100 % (by a naming rule, not checked against the data)"
				: String.format(Locale.US, "%.1f", candidate.confidence) + " %");
		if (candidate.nonNullCount >= 0) {
			long matched = candidate.nonNullCount - Math.max(0, candidate.orphans);
			addField(header, row++, "Rows", matched + " of " + candidate.nonNullCount + " rows match"
					+ (candidate.orphans > 0? " (" + candidate.orphans + " without parent)" : ""));
		}
		add(header, gbc(y++, GridBagConstraints.HORIZONTAL, 0));

		if (accept != null && onAccept != null) {
			JCheckBox acceptCheckBox = new JCheckBox("Accept this association");
			acceptCheckBox.setSelected(accept);
			acceptCheckBox.setToolTipText("Same as the check box in the list of proposals. "
					+ "Checked associations are added with the button \"Accept\" of the dialog \"Discover Associations\".");
			acceptCheckBox.addItemListener(e -> onAccept.accept(acceptCheckBox.isSelected()));
			add(acceptCheckBox, gbc(y++, GridBagConstraints.HORIZONTAL, 0));
		}

		DefaultTableModel columnsModel = new DefaultTableModel(new String[] { "Column A", "Type A", "Column B", "Type B" }, 0) {
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		for (int i = 0; i < candidate.childColumns.size(); ++i) {
			Column a = candidate.childColumns.get(i);
			Column b = i < candidate.parentColumns.size()? candidate.parentColumns.get(i) : null;
			columnsModel.addRow(new Object[] { a.name, typeOf(a), b == null? "" : b.name, b == null? "" : typeOf(b) });
		}
		LastColumnFillingTable columnsTable = new LastColumnFillingTable(columnsModel);
		columnsTable.setShowGrid(false);
		columnsTable.adjustColumnsWidth();
		int tableWidth = 0;
		for (int i = 0; i < columnsTable.getColumnCount(); ++i) {
			tableWidth += columnsTable.getColumnModel().getColumn(i).getPreferredWidth();
		}
		int visibleRows = Math.min(columnsModel.getRowCount(), MAX_VISIBLE_ROWS);
		columnsTable.setPreferredScrollableViewportSize(new Dimension(Math.max(tableWidth, TEXT_WIDTH), visibleRows * columnsTable.getRowHeight()));
		JScrollPane columnsPane = new JScrollPane(columnsTable);
		columnsPane.setBorder(BorderFactory.createTitledBorder("Columns"));
		add(columnsPane, gbc(y++, GridBagConstraints.BOTH, 1));

		if (candidate.verifyStatement != null) {
			JTextArea statement = new JTextArea(candidate.verifyStatement);
			statement.setEditable(false);
			statement.setLineWrap(true);
			statement.setWrapStyleWord(true);
			statement.setFont(new Font(Font.MONOSPACED, Font.PLAIN, new JLabel().getFont().getSize()));
			statement.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
			// the height the wrapped statement needs
			statement.setSize(TEXT_WIDTH, Short.MAX_VALUE);
			JScrollPane statementPane = new JScrollPane(statement);
			statementPane.getViewport().setPreferredSize(new Dimension(TEXT_WIDTH, statement.getPreferredSize().height));
			statementPane.setBorder(BorderFactory.createTitledBorder("Verification (SQL)"));
			add(statementPane, gbc(y++, GridBagConstraints.BOTH, 1));
		}

		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
		if (openInSqlConsole != null) {
			JButton sqlConsoleButton = new JButton("SQL Console");
			sqlConsoleButton.setIcon(UIUtil.scaleIcon(sqlConsoleButton, sqlConsoleIcon));
			sqlConsoleButton.setToolTipText("Queries about the proposed association, in the SQL Console.");
			sqlConsoleButton.addActionListener(e -> {
				JPopupMenu menu = new JPopupMenu();
				for (JMenuItem item: CandidateQueries.createMenuItems(candidate, openInSqlConsole, sqlConsoleIcon)) {
					if (item == null) {
						menu.addSeparator();
					} else {
						menu.add(item);
					}
				}
				UIUtil.showPopup(sqlConsoleButton, 0, sqlConsoleButton.getHeight(), menu);
			});
			buttonPanel.add(sqlConsoleButton);
		}
		JButton closeButton = new JButton("Close");
		closeButton.setIcon(UIUtil.scaleIcon(closeButton, cancelIcon));
		closeButton.addActionListener(e -> dialog.dispose());
		buttonPanel.add(closeButton);
		add(buttonPanel, gbc(y++, GridBagConstraints.HORIZONTAL, 0));
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

	/**
	 * Gets the type of a column including length and precision, like {@link Column#toSQL(String)}
	 * but without the name.
	 */
	private static String typeOf(Column column) {
		if (column.type == null) {
			return "";
		}
		return column.type + (column.length <= 0? "" : column.length == Integer.MAX_VALUE? "(max)" :
			"(" + column.length + (column.precision >= 0? ", " + column.precision : "") + ")");
	}

	private ImageIcon cancelIcon;
	private ImageIcon sqlConsoleIcon;
	{
		// load images
		cancelIcon = UIUtil.readImage("/buttoncancel.png");
		sqlConsoleIcon = UIUtil.readImage("/sqlconsole.png");
	}

}
