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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.ui.ListEditor;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.associationdiscovery.NamingRule.Kind;

/**
 * Editor for the naming rules of a data model: the conventions by which a foreign key
 * column of a child table is named. Every rule is either a template with the placeholders
 * <code>${table}</code> and <code>${pk}</code>, or a regular expression with a group for
 * the parent table.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class NamingRulesEditor extends JPanel {

	/**
	 * Width at which the description wraps. Matches the minimum width of the details view.
	 */
	private static final int HINT_WIDTH = 480;

	private final ExecutionContext executionContext;
	private final NamingRules rules;

	private RuleListEditor listEditor;
	private JCheckBox builtInCheckBox;
	private JComboBox<String> kindComboBox;
	private JTextField patternField;
	private JCheckBox completeByNameCheckBox;
	private JCheckBox withoutDataCheckCheckBox;
	private JPanel detailsPanel;
	private JDialog dialog;
	private boolean ok;

	/**
	 * Creates the editor and opens it as a modal dialog.
	 *
	 * @param owner the owner frame
	 * @param executionContext the execution context, determines the data model folder
	 * @param rules the rules to edit. A copy is edited, the original is left alone
	 */
	public NamingRulesEditor(JFrame owner, ExecutionContext executionContext, NamingRules rules) {
		this.executionContext = executionContext;
		this.rules = rules.copy();
		if (this.rules.isNew() && this.rules.getRules().isEmpty()) {
			// show what the discovery does anyway, as rules that can be edited
			this.rules.getRules().addAll(NamingRules.builtInConventionRules());
		}

		initUI();
		UIUtil.initComponents(this);

		dialog = new EscapableDialog(owner, "Naming Rules") {
		};
		dialog.setModal(true);
		dialog.getContentPane().add(this);
		dialog.pack();
		UIUtil.setDialogSize(dialog, 700, 480);
		dialog.setLocation(owner.getX() + (owner.getWidth() - dialog.getWidth()) / 2, owner.getY() + (owner.getHeight() - dialog.getHeight()) / 2);
		UIUtil.fit(dialog);
		dialog.setVisible(true);
	}

	/**
	 * @return <code>true</code> if the rules have been accepted and stored
	 */
	public boolean wasOk() {
		return ok;
	}

	/**
	 * Gets the edited rules.
	 *
	 * @return the rules
	 */
	public NamingRules getRules() {
		return rules;
	}

	private void initUI() {
		setLayout(new GridBagLayout());

		JLabel info = new JLabel("<html><i>A rule describes how a foreign key column is named. "
				+ "Rules apply to this data model and are stored with it.<br>"
				+ "The list starts out with the built-in conventions, so you can see what the discovery does "
				+ "and adapt it. Name equality, the tolerance towards affixes like <tt>FK_</tt> and the primary "
				+ "key name without the table prefix cannot be written as a rule - those stay behind the check "
				+ "box below.</i></html>");
		info.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
		add(info, gbc(0, 0, GridBagConstraints.HORIZONTAL, 0));

		listEditor = new RuleListEditor();
		listEditor.setModel(new ArrayList<NamingRule>(rules.getRules()));
		add(listEditor, gbc(0, 1, GridBagConstraints.BOTH, 1));

		builtInCheckBox = new JCheckBox("Also use the built-in naming conventions");
		builtInCheckBox.setToolTipText("If checked, the built-in conventions are applied in addition to the rules above: "
				+ "a column named like the primary key, \"<table>_<key>\", \"<table>ID\", singular and plural forms, "
				+ "and the usual affixes. Uncheck it if this schema follows its own systematics and the built-in "
				+ "conventions only add noise.");
		builtInCheckBox.setSelected(rules.isUseBuiltInRules());
		add(builtInCheckBox, gbc(0, 2, GridBagConstraints.HORIZONTAL, 0));

		JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 2));
		JButton okButton = new JButton("Ok");
		okButton.setIcon(UIUtil.scaleIcon(okButton, UIUtil.readImage("/buttonok.png")));
		okButton.addActionListener(e -> store());
		buttonPanel.add(okButton);
		JButton cancelButton = new JButton("Cancel");
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, UIUtil.readImage("/buttoncancel.png")));
		cancelButton.addActionListener(e -> dialog.dispose());
		buttonPanel.add(cancelButton);
		add(buttonPanel, gbc(0, 3, GridBagConstraints.HORIZONTAL, 0));
	}

	private GridBagConstraints gbc(int x, int y, int fill, double weighty) {
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = x;
		c.gridy = y;
		c.fill = fill;
		c.weightx = 1;
		c.weighty = weighty;
		c.anchor = GridBagConstraints.WEST;
		c.insets = new Insets(2, 4, 2, 4);
		return c;
	}

	/**
	 * Takes the rules over and writes them into the data model folder.
	 */
	private void store() {
		try {
			rules.getRules().clear();
			rules.getRules().addAll(listEditor.getModel());
			rules.setUseBuiltInRules(builtInCheckBox.isSelected());
			rules.store(executionContext);
			ok = true;
			dialog.dispose();
		} catch (Throwable t) {
			UIUtil.showException(this, "Error", t);
		}
	}

	/**
	 * Builds the view that edits a single rule.
	 */
	private JPanel createDetailsPanel() {
		if (detailsPanel == null) {
			detailsPanel = new JPanel(new GridBagLayout());
			GridBagConstraints c = new GridBagConstraints();
			c.anchor = GridBagConstraints.WEST;
			c.insets = new Insets(2, 4, 2, 4);

			c.gridx = 0;
			c.gridy = 0;
			detailsPanel.add(new JLabel("Kind "), c);
			kindComboBox = new JComboBox<String>(new String[] { Kind.TEMPLATE.displayName, Kind.REGEX.displayName });
			c.gridx = 1;
			c.weightx = 1;
			c.fill = GridBagConstraints.HORIZONTAL;
			detailsPanel.add(kindComboBox, c);

			c.gridx = 0;
			c.gridy = 1;
			c.weightx = 0;
			c.fill = GridBagConstraints.NONE;
			detailsPanel.add(new JLabel("Pattern "), c);
			patternField = new JTextField();
			c.gridx = 1;
			c.weightx = 1;
			c.fill = GridBagConstraints.HORIZONTAL;
			detailsPanel.add(patternField, c);

			completeByNameCheckBox = new JCheckBox("Remaining key columns match by identical name");
			completeByNameCheckBox.setToolTipText("For a composite key: if this rule matches one of its columns, "
					+ "the remaining ones may be matched by columns of the identical name. That is the usual shape "
					+ "when a key carries a tenant or a version column, which is named alike in both tables. "
					+ "Without effect for a key of a single column.");
			c.gridx = 0;
			c.gridy = 2;
			c.gridwidth = 2;
			c.weightx = 1;
			c.fill = GridBagConstraints.HORIZONTAL;
			detailsPanel.add(completeByNameCheckBox, c);

			// the text is put into a cell of a fixed width, otherwise every paragraph
			// renders as a single endless line and widens the dialog accordingly
			withoutDataCheckCheckBox = new JCheckBox("Accept without checking the data");
			withoutDataCheckCheckBox.setToolTipText("Propose what this rule matches without running a verification "
					+ "query. For a convention you know to be binding, for tables that are empty - where nothing can "
					+ "be confirmed - and for tables too large to check. Such proposals have no matching quota and no "
					+ "cardinality, so their confidence stays empty.");
			c.gridx = 0;
			c.gridy = 3;
			c.gridwidth = 2;
			c.weightx = 1;
			c.fill = GridBagConstraints.HORIZONTAL;
			detailsPanel.add(withoutDataCheckCheckBox, c);

			JLabel hint = new JLabel("<html><table cellpadding=0 cellspacing=0><tr><td width=" + HINT_WIDTH + "><i>"
					+ "<b>Template</b>: the name of the column, with " + NamingRule.TABLE_PLACEHOLDER
					+ " for the parent table (singular and plural forms are tried) and " + NamingRule.PK_PLACEHOLDER
					+ " for its primary key column.<br>"
					+ "Examples: <tt>" + NamingRule.TABLE_PLACEHOLDER + NamingRule.PK_PLACEHOLDER + "</tt>, "
					+ "<tt>" + NamingRule.TABLE_PLACEHOLDER + "_" + NamingRule.PK_PLACEHOLDER + "</tt>, "
					+ "<tt>FK_" + NamingRule.TABLE_PLACEHOLDER + "</tt><br><br>"
					+ "<b>Regular expression</b>: matched against the column name. It needs a group for the parent "
					+ "table, and may have one for the primary key column.<br>"
					+ "Examples: <tt>^FK_(?&lt;table&gt;.+)$</tt>, <tt>^(?&lt;table&gt;.+)_(?&lt;pk&gt;.+)$</tt><br><br>"
					+ "<b>" + NamingRule.PK_PLACEHOLDER + " is one key column, not the whole key.</b> The rule is "
					+ "checked once for <i>every</i> column of the primary key, and each of them needs its own "
					+ "column in the child table. Say <tt>PRODUCT</tt> has the key "
					+ "(<tt>ID</tt>, <tt>VERSION</tt>, <tt>LOCALE</tt>), then "
					+ "<tt>" + NamingRule.TABLE_PLACEHOLDER + "_" + NamingRule.PK_PLACEHOLDER + "</tt> is checked as:"
					+ "<table cellpadding=1 cellspacing=0>"
					+ "<tr><td><tt>ID</tt></td><td>&nbsp;&rarr;&nbsp;</td><td><tt>PRODUCT_ID</tt></td></tr>"
					+ "<tr><td><tt>VERSION</tt></td><td>&nbsp;&rarr;&nbsp;</td><td><tt>PRODUCT_VERSION</tt></td></tr>"
					+ "<tr><td><tt>LOCALE</tt></td><td>&nbsp;&rarr;&nbsp;</td><td><tt>PRODUCT_LOCALE</tt></td></tr>"
					+ "</table>"
					+ "All three have to exist in the child table, otherwise there is no candidate. The same holds "
					+ "for the <tt>pk</tt> group of a regular expression. If only part of the key is qualified that "
					+ "way - the child has <tt>PRODUCT_ID</tt> but shares <tt>VERSION</tt> and <tt>LOCALE</tt> under "
					+ "those very names - switch on the option above: the remaining columns are then taken from "
					+ "columns of the identical name.<br><br>"
					+ "Case and quotes are ignored.<br><br>"
					+ "<b>Accept without checking the data</b> skips the verification query for this rule. Use it for "
					+ "a convention that is binding anyway - and for an empty database, where the check can confirm "
					+ "nothing and would leave you without any proposal at all."
					+ "</i></td></tr></table><br>&nbsp;</html>");
			c.gridx = 0;
			c.gridy = 4;
			c.gridwidth = 2;
			detailsPanel.add(hint, c);
		}
		return detailsPanel;
	}

	/**
	 * The list of rules.
	 */
	private class RuleListEditor extends ListEditor<NamingRule> {

		public RuleListEditor() {
			super(new String[] { "Kind", "Pattern", "Rest by name", "No data check" }, "Naming Rule", true, false);
			hideUpAndDownButton();
		}

		@Override
		protected String getDisplayName(NamingRule element) {
			return element.getPattern();
		}

		@Override
		protected NamingRule copy(NamingRule element) {
			return new NamingRule(element.getKind(), element.getPattern(), element.isCompleteByName(), element.isWithoutDataCheck());
		}

		@Override
		protected NamingRule createNew() {
			// the usual case for a composite key: one column is qualified, the rest - a
			// tenant, a version - is named alike in both tables
			return new NamingRule(Kind.TEMPLATE, "", true, false);
		}

		@Override
		protected JComponent createDetailsView(NamingRule element) {
			JPanel panel = createDetailsPanel();
			kindComboBox.setSelectedItem(element.getKind().displayName);
			patternField.setText(element.getPattern());
			completeByNameCheckBox.setSelected(element.isCompleteByName());
			withoutDataCheckCheckBox.setSelected(element.isWithoutDataCheck());
			return panel;
		}

		@Override
		protected void updateFromDetailsView(NamingRule element, JComponent detailsView,
				List<NamingRule> model, StringBuilder errorMessage) {
			Kind kind = Kind.parse(String.valueOf(kindComboBox.getSelectedItem()));
			String pattern = patternField.getText().trim();
			String error = NamingRule.validate(kind, pattern);
			if (error != null) {
				errorMessage.append(error);
				return;
			}
			element.set(kind, pattern, completeByNameCheckBox.isSelected(), withoutDataCheckCheckBox.isSelected());
		}

		@Override
		protected Object[] toColumnList(NamingRule element, int index) {
			return new String[] { element.getKind().displayName, element.getPattern(),
					element.isCompleteByName()? "yes" : "", element.isWithoutDataCheck()? "yes" : "" };
		}

		@Override
		protected Color getForegroundColor(NamingRule element, int column) {
			return null;
		}

		@Override
		protected Dimension detailsViewMinSize() {
			return new Dimension(500, 10);
		}
	}

}
