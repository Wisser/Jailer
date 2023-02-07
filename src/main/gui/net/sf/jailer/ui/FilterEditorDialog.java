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
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Filter;
import net.sf.jailer.datamodel.FilterSource;
import net.sf.jailer.datamodel.PKColumnFilterSource;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.datamodel.filter_template.Clause;
import net.sf.jailer.datamodel.filter_template.Clause.Predicate;
import net.sf.jailer.datamodel.filter_template.Clause.Subject;
import net.sf.jailer.datamodel.filter_template.FilterTemplate;
import net.sf.jailer.ui.ParameterSelector.ParametersGetter;
import net.sf.jailer.util.Pair;
import net.sf.jailer.util.Quoting;

/**
 * Column filter editor.
 *
 * @author Ralf Wisser
 */
public class FilterEditorDialog extends javax.swing.JDialog {

	/**
	 * Parent frame.
	 */
	private final ExtractionModelFrame parent;

	private boolean isInitialized = false;

	/**
	 * Currently selected table (in closure-table).
	 */
	private Table selectedTable;

	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;

	/**
	 * Make {@link #refresh(Table)} reentrant.
	 */
	private boolean refreshing = false;

	private final class FilterConditionEditor extends ConditionEditor {
		private static final long serialVersionUID = -958818889236629658L;
		JCheckBox checkBox;

		private FilterConditionEditor(JComponent anchor, Frame parent, ParametersGetter parametersGetter) {
			super(anchor, parent, parametersGetter, getDataModel(), "Filter Expression");
		}

		public String edit(JComponent anchor, String condition, String table1label, String table1alias, Table table1, String table2label, String table2alias, Table table2, boolean addPseudoColumns, boolean withColumnsDropDown) {
			if (checkBox == null) {
				 checkBox = new JCheckBox("literal filter");
				 checkBox.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						if (checkBox.isSelected()) {
							if (!editorPane.getText().trim().startsWith(Filter.LITERAL_PREFIX)) {
								editorPane.setText(Filter.LITERAL_PREFIX + " " + editorPane.getText());
							}
						} else {
							if (editorPane.getText().trim().startsWith(Filter.LITERAL_PREFIX)) {
								editorPane.setText(editorPane.getText().trim().substring(Filter.LITERAL_PREFIX.length()).trim());
							}
						}
					}
				 });
				 addOnPanel.setVisible(true);
				 addOnPanel.add(checkBox);
			}
			checkBox.setSelected(condition.trim().startsWith(Filter.LITERAL_PREFIX));
			if (!withColumnsDropDown) {
				return super.edit(anchor, condition, null, null, null, table2label, table2alias, table2, addPseudoColumns, false);
			}
			return super.edit(anchor, condition, table1label, table1alias, table1, table2label, table2alias, table2, addPseudoColumns, false);
		}
	}

	private static class FilterModel {
		public Column column;
		public Table table;
		public boolean isPk;
		public boolean isFk;
	}

	@SuppressWarnings("serial")
	private class ClauseList extends ListEditor<Clause> {

		public ClauseList() {
			super(new String[] { "", "Subject", "Predicate", "Object"}, "Clause", true, false, true);
		}

		@Override
		protected String getDisplayName(Clause element) {
			return element.getSubject().description + " " + element.getPredicate().description + " " + element.getObject();
		}

		@Override
		protected Clause copy(Clause element) {
			return new Clause(element);
		}

		@Override
		protected Clause createNew() {
			return new Clause();
		}

		@Override
		protected JComponent createDetailsView(Clause element) {
			DefaultComboBoxModel<Subject> cm = new DefaultComboBoxModel<Subject>(Subject.values());
			clauseDetailsSubjectComboBox.setModel(cm);
			if (element.getSubject() != null) {
				clauseDetailsSubjectComboBox.setSelectedItem(element.getSubject());
			}
			onClauseDetailsSubjectChanged(element.getSubject());
			clauseDetailsPredicateComboBox.setSelectedItem(element.getPredicate());
			if (element.getPredicate() != null) {
				onClauseDetailsPredicateChanged(element.getPredicate());
			}
			clauseDetailsObjectTextField.setSelectedItem(element.getObject());
			return clauseDetailsPanel;
		}

		@Override
		protected void updateFromDetailsView(Clause element,
				JComponent detailsView, List<Clause> model,
				StringBuilder errorMessage) {
			Predicate selectedPredicate = (Predicate) clauseDetailsPredicateComboBox.getSelectedItem();
			Subject selectedSubject = (Subject) clauseDetailsSubjectComboBox.getSelectedItem();
			String obj = "";
			if (clauseDetailsObjectTextField.getSelectedItem() instanceof String) {
				obj = ((String) clauseDetailsObjectTextField.getSelectedItem()).trim();
			}
			if (selectedPredicate.needsObject) {
				if (obj.trim().length() == 0) {
					errorMessage.append("Object missing");
				} else {
					String validationMessage = selectedPredicate.validateObject(obj);
					if (validationMessage != null) {
						errorMessage.append(validationMessage);
					}
				}
				element.setObject(obj);
			} else {
				element.setObject("");
			}
			element.setPredicate(selectedPredicate);
			element.setSubject(selectedSubject);
		}

		@Override
		protected Object[] toColumnList(Clause element, int index) {
			return new String[] { index > 0? "and" : "", element.getSubject().description, element.getPredicate().description, element.getObject() };
		}

		@Override
		protected Color getForegroundColor(Clause element, int column) {
			return null;
		}

		@Override
		protected Dimension detailsViewMinSize() {
			return new Dimension(400, 1);
		}
	}

	@SuppressWarnings("serial")
	private class TemplateList extends ListEditor<FilterTemplate> {

		public TemplateList() {
			super(new String[] { "Priority", "Name", "Apply At", "Expression"}, "Template", true, false, false);
		}

		@Override
		protected String getDisplayName(FilterTemplate element) {
			return element.getName();
		}

		@Override
		protected FilterTemplate copy(FilterTemplate element) {
			return new FilterTemplate(element);
		}

		@Override
		protected FilterTemplate createNew() {
			FilterTemplate t = new FilterTemplate();
			t.setName(uniqueName("New template"));
			return t;
		}

		@Override
		protected FilterTemplate createCopy(FilterTemplate t) {
			FilterTemplate copy = super.createCopy(t);
			copy.setName(uniqueName("Copy of " + copy.getName()));
			return copy;
		}

		private String uniqueName(String name) {
			boolean unique;
			int i = 1;
			String fullName;
			do {
				unique = true;
				fullName = name + (i > 1? " (" + i + ")" : "");
				for (FilterTemplate t: model) {
					if (fullName.equals(t.getName())) {
						unique = false;
						break;
					}
				}
				++i;
			} while (!unique);
			return fullName;
		}

		private String oldName;

		@Override
		protected JComponent createDetailsView(FilterTemplate element) {
			oldName = element.getName();
			templateDetailsNameField.setText(oldName);
			templatePreValue = element.getExpression();
			if (Filter.EXCLUDED_VALUE.equals(element.getExpression())) {
				templatePreValue = "";
			}
			templateDetailsNewValueField.setText(element.getExpression());
			templateDetailsNewValueField.setEditable(!Filter.EXCLUDED_VALUE.equals(element.getExpression()));
			templateDetailsNewValueField.setEnabled(!Filter.EXCLUDED_VALUE.equals(element.getExpression()));
			templateDetailsTypeField.setText(element.getType() != null? element.getType() : "");
			templateDetailsEnabledCheckBox.setSelected(element.isEnabled());

			ClauseList clauseList = new ClauseList();
			clauseList.setModel(element.getClauses());

			GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
			gridBagConstraints.gridx = 0;
			gridBagConstraints.gridy = 1;
			gridBagConstraints.weightx = 1;
			gridBagConstraints.weighty = 1;
			gridBagConstraints.fill = GridBagConstraints.BOTH;
			templatesDetailsClausePanel.removeAll();
			templatesDetailsClausePanel.add(clauseList, gridBagConstraints);

			templatesDetailsApplyAtComboBox.setSelectedIndex(element.isApplyAtExport()? 0 : 1);

			return templateDetailsPanel;
		}

		@Override
		protected void updateFromDetailsView(FilterTemplate element,
				JComponent detailsView, List<FilterTemplate> model,
				StringBuilder errorMessage) {
			String name = templateDetailsNameField.getText().trim();
			String expression = templateDetailsNewValueField.getText().trim().replaceFirst("\\s*;$", "");
			if (name.length() == 0) {
				errorMessage.append("Name missing");
				return;
			}
			if (expression.length() == 0 || Filter.OLD_VALUE_PROP.equals(expression)) {
				errorMessage.append("New value missing");
				return;
			}
			if (!name.equals(oldName)) {
				for (FilterTemplate t: model) {
					if (t.getName().equals(name)) {
						errorMessage.append("Duplicate name");
						return;
					}
				}
			}

			element.setName(name);
			element.setExpression(expression);
			element.setEnabled(templateDetailsEnabledCheckBox.isSelected());
			String type = templateDetailsTypeField.getText().trim();
			element.setType(type.length() > 0? type : null);
			element.setApplyAtExport(templatesDetailsApplyAtComboBox.getSelectedIndex() == 0);
			if (Filter.EXCLUDED_VALUE.equals(expression)) {
				element.setApplyAtExport(true);
			}
		}

		@Override
		protected Object[] toColumnList(FilterTemplate element, int index) {
			return new String[] { Integer.toString(index + 1), element.getName(), element.isApplyAtExport()? "Export" : "Import", element.getExpression() };
		}

		@Override
		protected Color getForegroundColor(FilterTemplate element, int column) {
			return element.isEnabled()? null : Color.LIGHT_GRAY;
		}

		@Override
		protected void onSelectionChange() {
			refreshTemplatesDerivedList();
		}

		@Override
		protected void onModelUpdate() {
			getDataModel().deriveFilters();
			getDataModel().version++;
			refreshFilterPane();
			parent.extractionModelEditor.refresh(false, false, true, true);
			parent.extractionModelEditor.markDirty();
			parent.extractionModelEditor.resetUndoStack();
		}

		@Override
		protected Dimension detailsViewMaxSize() {
			return new Dimension(400, 500);
		}
	}

	@SuppressWarnings("serial")
	private class DerivedFilterList extends ListEditor<FilterModel> {
		private final Table rootTable;

		public DerivedFilterList(Table rootTable) {
			super(new String[] { "Apply at", "Column", "Type", "Expression", "Derived from"}, "Filter", false, true, true);
			this.rootTable = rootTable;
		}

		@Override
		protected String getDisplayName(FilterModel element) {
			return Quoting.unquotedTableName(element.table, executionContext).toLowerCase(Locale.ENGLISH) + "." + Quoting.staticUnquote(element.column.name).toLowerCase(Locale.ENGLISH);
		}

		@Override
		protected FilterModel copy(FilterModel element) {
			return null;
		}

		@Override
		protected FilterModel createNew() {
			return null;
		}

		@Override
		protected JComponent createDetailsView(FilterModel element) {
			return null;
		}

		@Override
		protected void updateFromDetailsView(FilterModel element,
				JComponent detailsView, List<FilterModel> model,
				StringBuilder errorMessage) {
		}

		@Override
		protected Object[] toColumnList(FilterModel element, int index) {
			String source = "";
			FilterSource fs = element.column.getFilter().getFilterSource();
			if (rootTable == null) {
				if (fs != null) {
					source = fs.getDescription();
				}
			} else if (fs instanceof PKColumnFilterSource) {
				source = ((PKColumnFilterSource) fs).column.name;
			}
			return new String[] {
					element.column.getFilter().isApplyAtExport()? "Export" : "Import",
					getDisplayName(element),
					element.column.type.toLowerCase(Locale.ENGLISH),
					element.column.getFilter().getExpression(),
					source
					};
		}

		@Override
		protected Color getForegroundColor(FilterModel element, int column) {
			return element.isPk? Color.red :
				element.column.getFilter() != null && element.column.getFilter().isDerived()?
						Color.blue : null;
		}

		/**
		 * @see net.sf.jailer.ui.ListEditor#setModel(java.util.List)
		 */
		@Override
		public void setModel(List<FilterModel> model) {
			Collections.sort(model, new Comparator<FilterModel>() {
				@Override
				public int compare(FilterModel o1, FilterModel o2) {
					return Quoting.unquotedTableName(o1.table, executionContext).compareTo(Quoting.unquotedTableName(o2.table, executionContext));
				}
			});
			super.setModel(model);
		}

	}

	private FilterConditionEditor createConditionEditor() {
		return new FilterConditionEditor(null, parent, new ParameterSelector.ParametersGetter() {
			@Override
			public Set<String> getParameters() {
				Set<String> pSet = new TreeSet<String>(parametersGetter.getParameters());
				pSet.add(Filter.OLD_VALUE_PROP_PURE);
				return pSet;
			}
		});
	}

	private final ParameterSelector.ParametersGetter parametersGetter;

	/** Creates new form FilterEditor */
	@SuppressWarnings("serial")
	public FilterEditorDialog(ExtractionModelFrame parent, final ParameterSelector.ParametersGetter parametersGetter, ExecutionContext executionContext) {
		super(parent, true);
		this.executionContext = executionContext;
		this.parametersGetter = parametersGetter;
		this.parent = parent;
		this.conditionEditor = createConditionEditor();
		initComponents();
		
		UIUtil.setLeadingComponent(templateDetailsNewValueField, templatesDetailsMulitlineLabel);
		
		okButton.setIcon(UIUtil.scaleIcon(okButton, okIcon));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, cancelIcon));
		
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
		final JComboBox2 comboBox = clauseDetailsObjectTextField;
        jPanel14.add(StringSearchPanel.createSearchButton(null, comboBox, "Value", null, null, null, null, false, null, true, true, null, false, null), gridBagConstraints);

		ActionListener l = new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				templateDetailsTypeLabel.setVisible(templatesDetailsApplyAtComboBox.getSelectedIndex() == 1);
				templateDetailsTypeField.setVisible(templatesDetailsApplyAtComboBox.getSelectedIndex() == 1);
			}
		};
		l.actionPerformed(null);
		templatesDetailsApplyAtComboBox.addActionListener(l);

		AutoCompletion.enable(tableBox);
		gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.NONE;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        JToggleButton sb;
		jPanel5.add(sb = StringSearchPanel.createSearchButton(this, tableBox, "Find Table", null), gridBagConstraints);
		UIUtil.setLeadingComponent(tableBox, sb);

		final ListCellRenderer tableBoxRenderer = tableBox.getRenderer();
		tableBox.setRenderer(new ListCellRenderer() {
			@Override
			public Component getListCellRendererComponent(JList list,
					Object value, int index, boolean isSelected,
					boolean cellHasFocus) {
				if (value instanceof String) {
					Table table = getDataModel().getTableByDisplayName((String) value);
					if (table != null) {
						int n = 0;
						int ng = 0;
						for (Column c: table.getColumns()) {
							if (c.getFilter() != null) {
								++n;
								if (!c.getFilter().isDerived()) {
									++ng;
								}
							}
						}
						if (n > 0) {
							value = value + " (" + n + (n != ng? "/" + ng : "") + ")";
						}
					}
				}
				return tableBoxRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
			}
		});

		templatesDetailsMulitlineLabel.setText(null);
		templatesDetailsMulitlineLabel.setIcon(conditionEditorIcon);
		templatesDetailsMulitlineLabel.setToolTipText("Open SQL editor");
		templatesDetailsMulitlineLabel.addMouseListener(new java.awt.event.MouseAdapter() {

			private boolean isActive() {
				return !Filter.EXCLUDED_VALUE.equals(templateDetailsNewValueField.getText());
			}

			@Override
			public void mouseReleased(MouseEvent e) {
				mouseClicked(e);
			}

			@Override
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				if (isActive()) {
					mouseExited(evt);
					conditionEditor.setTitle(templateDetailsNameField.getText().trim());
					String cond = conditionEditor.edit(templateDetailsNewValueField, templateDetailsNewValueField.getText(), null, null, null, null, null, null, false, templatesDetailsApplyAtComboBox.getSelectedIndex() == 0);
					if (cond != null) {
						if (!templateDetailsNewValueField.getText().equals((cond))) {
							templateDetailsNewValueField.setText((cond));
						}
						templatesDetailsMulitlineLabel.setIcon(conditionEditorIcon);
					}
				}
			}

			@Override
			public void mouseEntered(java.awt.event.MouseEvent evt) {
				if (isActive()) {
					templatesDetailsMulitlineLabel.setIcon(conditionEditorSelectedIcon);
				}
			}

			@Override
			public void mouseExited(java.awt.event.MouseEvent evt) {
				if (isActive()) {
					templatesDetailsMulitlineLabel.setIcon(conditionEditorIcon);
				}
		   }
		});

		helpComponents.put(Predicate.LIKE, clausePredHelpLike);
		helpComponents.put(Predicate.NOT_LIKE, clausePredHelpLike);
		helpComponents.put(Predicate.MATCHES, clausePredHelpRE);
		helpComponents.put(Predicate.NOT_MATCHES, clausePredHelpRE);
	}

	/**
	 * Gets current data model.
	 *
	 * @return current data model
	 */
	private DataModel getDataModel() {
		if (parent != null) {
			if (parent.extractionModelEditor != null) {
				return parent.extractionModelEditor.dataModel;
			}
		}
		return null;
	}

	/**
	 * Opens the editor.
	 *
	 * @param table the initially selected table
	 */
	public void open(Table table) {
		filterTextfieldsPerColumn.clear();
		typeTextfieldsPerColumn.clear();
		filterApplyAtCBPerColumn.clear();
		if (!isInitialized) {
			int w = 1100, h = 600;
			setSize(w, h);
			Rectangle2D screenBounds = UIUtil.getScreenBounds();
			setLocation(Math.max((int) screenBounds.getX(), parent.getX() + parent.getWidth() / 2 - w / 2),
						Math.max((int) screenBounds.getY(), parent.getY() + parent.getHeight() / 2 - h / 2));
			isInitialized = true;
		}

		this.conditionEditor = createConditionEditor();

		templateList = new TemplateList();
		templateList.setModel(getDataModel().getFilterTemplates());

		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		templatesPane.removeAll();
		templatesPane.add(templateList, gridBagConstraints);

		refresh(table);
		jTabbedPane1.setSelectedIndex(0);

		Set<String> tableNames = new TreeSet<String>();
		Set<String> columnNames = new TreeSet<String>();
		Set<String> typeNames = new TreeSet<String>();
		for (Table tab: getDataModel().getTables()) {
			tableNames.add(Quoting.unquotedTableName(tab, executionContext).toLowerCase(Locale.ENGLISH));
			for (Column column: tab.getColumns()) {
				columnNames.add(Quoting.staticUnquote(column.name).toLowerCase(Locale.ENGLISH));
				typeNames.add(column.type.toLowerCase(Locale.ENGLISH));
			}
		}
		objectsModel.put(Subject.COLUMN_NAME, columnNames.toArray(new String[0]));
		objectsModel.put(Subject.TABLE_NAME, tableNames.toArray(new String[0]));
		objectsModel.put(Subject.TYPE, typeNames.toArray(new String[0]));

		setVisible(true);
	}

	/**
	 * Refreshes the dialog.
	 *
	 * @param tableToSelect the table to select initially or <code>null</code> to keep the current selection
	 */
	public void refresh(Table tableToSelect) {
		if (refreshing) {
			return;
		}
		refreshing = true;
		selectedTable = tableToSelect;

		Vector<String> tableNames = new Vector<String>();
		for (Table table: getDataModel().getTables()) {
			tableNames.add(getDataModel().getDisplayName(table));
		}
		Collections.sort(tableNames, String::compareToIgnoreCase);
		DefaultComboBoxModel model = new DefaultComboBoxModel(tableNames);
		tableBox.setModel(model);
		if (tableToSelect == null && tableNames.size() > 0) {
			tableToSelect = selectedTable = getDataModel().getTableByDisplayName(tableNames.firstElement());
		}
		if (tableToSelect != null) {
			tableBox.setSelectedItem(getDataModel().getDisplayName(tableToSelect));
		} else {
			tableBox.setSelectedItem(0);
		}

		refreshFilterPane();

		refreshing = false;
	}

	private Font font = new JLabel("normal").getFont();

	/**
	 * Bold font.
	 */
	private Font boldFont = font.deriveFont(font.getStyle() | Font.BOLD, font.getSize());

	/**
	 * Non-bold font.
	 */
	private Font nonBoldFont = font.deriveFont(font.getStyle() & ~Font.BOLD, font.getSize());

	/**
	 * Maps columns to the text-field with which to edit the filter expression.
	 */
	private Map<Column, JTextField> filterTextfieldsPerColumn = new IdentityHashMap<Column, JTextField>();

	/**
	 * Maps columns to the text-field with which to edit the filter type.
	 */
	private Map<Column, JTextField> typeTextfieldsPerColumn = new IdentityHashMap<Column, JTextField>();

	/**
	 * Maps columns to the text-field with which to edit the filter expression.
	 */
	private Map<Column, JComboBox2> filterApplyAtCBPerColumn = new IdentityHashMap<Column, JComboBox2>();

	/**
	 * The editor for filter conditions.
	 */
	private FilterConditionEditor conditionEditor;

	private IdentityHashMap<Column, Integer> explicitlySetApplyAtPerColumn = new IdentityHashMap<Column, Integer>();
	private IdentityHashMap<Column, Pair<String, Integer>> excludedPrevValue = new IdentityHashMap<Column, Pair<String, Integer>>();

	/**
	 * Refreshes the filter pane.
	 */
	private void refreshFilterPane() {
		filterPane.removeAll();
		applyButton.setEnabled(false);
		List<Table> tables = new ArrayList<Table>();
		if (selectedTable != null) {
			tables.add(selectedTable);
		} else {
			tables.addAll(getDataModel().getTables());
			Collections.sort(tables);
		}
		int y = 0;
		java.awt.GridBagConstraints gridBagConstraints;
		filterTextfieldsPerColumn.clear();
		typeTextfieldsPerColumn.clear();
		filterApplyAtCBPerColumn.clear();

		Insets is = new Insets(0, 0, 8, 0);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = y;
		gridBagConstraints.gridwidth = 1;
		gridBagConstraints.insets = is;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 0.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		JPanel ae = new JPanel();
		ae.setLayout(new GridBagLayout());
		filterPane.add(ae, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		JLabel label = new JLabel("Apply at ");
		label.setFont(boldFont);
		ae.add(label, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 2;
		gridBagConstraints.gridy = y;
		gridBagConstraints.gridwidth = 2;
		gridBagConstraints.insets = is;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 0.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		label = new JLabel("Column ");
		label.setFont(boldFont);
		filterPane.add(label, gridBagConstraints);

		JPanel p = new JPanel();
		p.setLayout(new GridBagLayout());

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 6;
		gridBagConstraints.gridy = y;
		gridBagConstraints.insets = is;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 0.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		filterPane.add(p, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.insets = is;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		label = new JLabel("Expression ");
		label.setFont(boldFont);
		p.add(label, gridBagConstraints);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = y;
		gridBagConstraints.insets = is;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 0.0;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
		label = new JLabel("Exclude  ");
		label.setToolTipText("Exclude a column from export.");
		label.setFont(boldFont);
		filterPane.add(label, gridBagConstraints);

		++y;

		for (final Table table: tables) {
			for (final Column c: table.getColumns()) {
				String toolTip = c.getFilter() != null && c.getFilter().isDerived()? c.getFilter().getFilterSource().getDescription() : null;
				if (toolTip != null) {
					toolTip = "derived from " + toolTip;
				}
				String filter;
				filter = c.getFilter() != null && !c.getFilter().isDerived()? c.getFilter().getExpression() : null;

				if (filter == null && selectedTable == null) {
					continue;
				}

				if (filter != null && c.getFilter().getAppliedTo() != null) {
					List<String> aTo = c.getFilter().getAppliedTo();
					StringBuilder sb = new StringBuilder("Also applied to " + aTo.size() + " columns: ");
					boolean f = true;
					for (String s: aTo) {
						if (!f) {
							sb.append(", ");
						}
						sb.append(s);
						f = false;
						if (sb.length() > 100) {
							sb.append("...");
							break;
						}
					}
					toolTip = sb.toString();
				}

				final JComboBox2 applyAtCB = new JComboBox2();
				filterApplyAtCBPerColumn.put(c, applyAtCB);
				applyAtCB.setModel(new DefaultComboBoxModel<String>(new String[] { "Export", "Import" }));
				if (c.getFilter() != null) {
					applyAtCB.setSelectedIndex(c.getFilter().isApplyAtExport()? 0 : 1);
				} else {
					Integer explSetApplyAt = explicitlySetApplyAtPerColumn.get(c);
					if (explSetApplyAt != null) {
						applyAtCB.setSelectedIndex(explSetApplyAt);
					} else {
						applyAtCB.setSelectedIndex(0);
					}
				}
				applyAtCB.addItemListener(new java.awt.event.ItemListener() {
					@Override
					public void itemStateChanged(java.awt.event.ItemEvent evt) {
						explicitlySetApplyAtPerColumn.put(c, applyAtCB.getSelectedIndex());
						apply();
					}
				});
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 0;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.insets = new Insets(0, 0, 0, 10);
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				filterPane.add(applyAtCB, gridBagConstraints);

				label = new javax.swing.JLabel();
				if (selectedTable != null) {
					label.setText(" T.");
					label.setFont(nonBoldFont);
					label.setForeground(Color.gray);
					gridBagConstraints = new java.awt.GridBagConstraints();
					gridBagConstraints.gridx = 2;
					gridBagConstraints.gridy = y;
					gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
					gridBagConstraints.weightx = 0.0;
					gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
					filterPane.add(label, gridBagConstraints);
					if (toolTip != null) {
						label.setToolTipText(toolTip);
					}
				}

				javax.swing.JPanel columnPanel = new JPanel();
				columnPanel.setLayout(new java.awt.BorderLayout());

				label = new javax.swing.JLabel();
				final String columnName = (selectedTable == null? " " + table.getUnqualifiedName() + "." : "") + c.name;
				label.setText(columnName);
				label.setFont(filter == null || selectedTable == null? nonBoldFont : boldFont);

				boolean isPK = false;
				for (Column pk: table.primaryKey.getColumns()) {
					if (pk.equals(c)) {
						isPK = true;
						break;
					}
				}

				if (isPK) {
					label.setForeground(Color.RED);
				}
				if (c.getFilter() != null && c.getFilter().isDerived()) {
					label.setForeground(Color.BLUE);
				}
				columnPanel.add(label, java.awt.BorderLayout.WEST);
				if (toolTip != null) {
					label.setToolTipText(toolTip);
				}
				label = new javax.swing.JLabel();
				String type = c.toSQL(null).substring(c.name.length());
				label.setToolTipText(type);
				String typeLabel = type;
				int l = Math.max(Math.min(24 - c.name.length(), 14), 8);
				if (typeLabel.length() > l) {
					typeLabel = typeLabel.substring(0, l) + "..";
				}
				label.setText(" " + typeLabel + " ");
				label.setForeground(Color.GRAY);
				label.setFont(filter == null || selectedTable == null? nonBoldFont : boldFont);

				JTextField typeField = new JTextField();
				typeField.getDocument().addDocumentListener(new DocumentListener() {

					@Override
					public void removeUpdate(DocumentEvent e) {
						refresh();
					}

					@Override
					public void insertUpdate(DocumentEvent e) {
						refresh();
					}

					@Override
					public void changedUpdate(DocumentEvent arg0) {
						refresh();
					}

					private void refresh() {
						applyButton.setEnabled(needsSave());
					}
				});
				typeField.addFocusListener(new FocusListener() {

					@Override
					public void focusLost(FocusEvent e) {
						if (applyButton.isEnabled()) {
							apply();
						}
					}

					@Override
					public void focusGained(FocusEvent e) {
					}
				});
				if (c.getFilter() != null && !c.getFilter().isDerived() && !c.getFilter().isApplyAtExport()) {
					typeField.setText(c.getFilter().getType() == null? type.trim() : c.getFilter().getType());
					typeTextfieldsPerColumn.put(c, typeField);
					columnPanel.add(typeField, java.awt.BorderLayout.EAST);
				} else {
					columnPanel.add(label, java.awt.BorderLayout.EAST);
				}

				if (toolTip != null) {
					label.setToolTipText(toolTip);
				}

				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 3;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				filterPane.add(columnPanel, gridBagConstraints);

				label = new javax.swing.JLabel();
				label.setText(" :=  ");
				label.setFont(nonBoldFont);
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 4;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				filterPane.add(label, gridBagConstraints);
				if (toolTip != null) {
					label.setToolTipText(toolTip);
				}

				final javax.swing.JTextField textField = new javax.swing.JTextField();
				final String initialExpr = filter == null? (c.getFilter() == null? Filter.OLD_VALUE_PROP : (c.getFilter().getExpression())) : (filter);
				final boolean hasFilter = c.getFilter() != null;
				final boolean hasDerivedFilter = c.getFilter() != null && c.getFilter().isDerived();
				final boolean finalIsPk = isPK;
				textField.getDocument().addDocumentListener(new DocumentListener() {

					@Override
					public void removeUpdate(DocumentEvent e) {
						refresh();
					}

					@Override
					public void insertUpdate(DocumentEvent e) {
						refresh();
					}

					@Override
					public void changedUpdate(DocumentEvent arg0) {
						refresh();
					}

					private void refresh() {
						if (!hasFilter && textField.getText().trim().equals(Filter.OLD_VALUE_PROP)) {
							textField.setForeground(Color.gray);
						} else if (hasDerivedFilter && textField.getText().trim().equals(initialExpr.trim())) {
							textField.setForeground(Color.blue);
						} else if (finalIsPk) {
							textField.setForeground(Color.red);
						} else {
							textField.setForeground(Color.black);
						}
						applyButton.setEnabled(needsSave());
					}
				});
				textField.addFocusListener(new FocusListener() {

					@Override
					public void focusLost(FocusEvent e) {
						if (applyButton.isEnabled()) {
							apply();
						}
					}

					@Override
					public void focusGained(FocusEvent e) {
					}
				});
				textField.setText(initialExpr);
				textField.setEditable(!initialExpr.equals(Filter.EXCLUDED_VALUE));
				textField.setEnabled(!initialExpr.equals(Filter.EXCLUDED_VALUE));
				filterTextfieldsPerColumn.put(c, textField);
				if (toolTip != null) {
					textField.setToolTipText(toolTip);
				}

				label = new javax.swing.JLabel();
				label.setText(null);

				label.setIcon(conditionEditorIcon);
				label.setToolTipText("open SQL editor");
				
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 5;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.insets = new Insets(1, 0, 0, 2);
				filterPane.add(label, gridBagConstraints);

				JPanel p2 = new JPanel();
				p2.setLayout(new GridBagLayout());
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 6;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 1.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				gridBagConstraints.insets = new Insets(1, 0, 0, 0);
				filterPane.add(p2, gridBagConstraints);

				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = 1;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 1.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
				p2.add(textField, gridBagConstraints);
				
				UIUtil.setLeadingComponent(textField, label);

				final JCheckBox excluded = new JCheckBox("");
				gridBagConstraints = new java.awt.GridBagConstraints();
				gridBagConstraints.gridx = 1;
				gridBagConstraints.gridy = y;
				gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
				gridBagConstraints.weightx = 0.0;
				gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
				filterPane.add(excluded, gridBagConstraints);

				excluded.setSelected(initialExpr.equals(Filter.EXCLUDED_VALUE));
				excluded.setToolTipText("Exclude this column from export.");

				excluded.addItemListener(new ItemListener() {
					@Override
					public void itemStateChanged(ItemEvent e) {
						if (excluded.isSelected()) {
							String expr = "";
							if (c.getFilter() != null || !textField.getText().equals(Filter.OLD_VALUE_PROP)) {
								if (c.getFilter() == null || !c.getFilter().isDerived()) {
									expr = textField.getText();
								}
							}
							excludedPrevValue.put(c, new Pair<String, Integer>(expr, applyAtCB.getSelectedIndex()));
							textField.setText(Filter.EXCLUDED_VALUE);
							textField.setEditable(false);
							textField.setEnabled(false);
						} else {
							Pair<String, Integer> pv = excludedPrevValue.get(c);
							textField.setEditable(true);
							textField.setEnabled(true);
							if (pv != null) {
								textField.setText(pv.a);
								applyAtCB.setSelectedIndex(pv.b);
							} else {
								textField.setText("");
							}
						}
						apply();
					}
				});

				final JLabel theLabel = label;
				label.addMouseListener(new java.awt.event.MouseAdapter() {

					private boolean isActive() {
						return !Filter.EXCLUDED_VALUE.equals(textField.getText());
					}

					@Override
					public void mouseReleased(MouseEvent e) {
						mouseClicked(e);
					}

					@Override
					public void mouseClicked(java.awt.event.MouseEvent evt) {
						if (isActive()) {
							mouseExited(evt);
							conditionEditor.setTitle(columnName.trim());
							String cond = conditionEditor.edit(textField, textField.getText(), "Table", "T", table, null, null, null, false, applyAtCB.getSelectedIndex() == 0);
							if (cond != null) {
								if (!textField.getText().equals((cond))) {
									textField.setText((cond));
								}
								theLabel.setIcon(conditionEditorIcon);
							}
						}
					}

					@Override
					public void mouseEntered(java.awt.event.MouseEvent evt) {
						if (isActive()) {
							theLabel.setIcon(conditionEditorSelectedIcon);
						}
					}

					@Override
					public void mouseExited(java.awt.event.MouseEvent evt) {
						if (isActive()) {
							theLabel.setIcon(conditionEditorIcon);
						}
				   }

				});

				++y;
			}
		}
		label = new javax.swing.JLabel();
		label.setText(y == 0? " no filter defined" : " "); // lgtm [java/constant-comparison]
		label.setFont(nonBoldFont);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = y++;
		gridBagConstraints.weighty = 0;
		gridBagConstraints.insets = new Insets(0, 0, 8, 0);
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
		filterPane.add(label, gridBagConstraints);

		label = new javax.swing.JLabel();
		label.setText(" ");
		label.setFont(nonBoldFont);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = y++;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.anchor = java.awt.GridBagConstraints.NORTHWEST;
		filterPane.add(label, gridBagConstraints);

		Table root = selectedTable;
		DerivedFilterList derivedFilterList = createDerivedFilterList(root);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		Panel2.removeAll();
		Panel2.add(derivedFilterList, gridBagConstraints);

		derivedFilterList = createDerivedFilterList((Table) null);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		allFiltersPanel.removeAll();
		allFiltersPanel.add(derivedFilterList, gridBagConstraints);

		refreshTemplatesDerivedList();

		validate();
		repaint();
	}

	private void refreshTemplatesDerivedList() {
	  DerivedFilterList derivedFilterList = createDerivedFilterList(templateList.getSelectedElement());
	  GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
	  gridBagConstraints.gridx = 0;
	  gridBagConstraints.gridy = 1;
	  gridBagConstraints.weightx = 1;
	  gridBagConstraints.weighty = 1;
	  gridBagConstraints.fill = GridBagConstraints.BOTH;
	  derivedPanel2.removeAll();
	  derivedPanel2.add(derivedFilterList, gridBagConstraints);
	  derivedPanel2.revalidate();
	}

	private DerivedFilterList createDerivedFilterList(FilterTemplate template) {
		DerivedFilterList derivedFilterList = new DerivedFilterList(null);
		List<FilterModel> derivedFilters = new ArrayList<FilterModel>();
		if (template != null) {
			for (final Table table: getDataModel().getSortedTables()) {
				for (Column c: table.getColumns()) {
					if (c.getFilter() != null) {
						FilterSource filterSource = c.getFilter().getFilterSource();
						if (filterSource instanceof PKColumnFilterSource) {
							Filter source = ((PKColumnFilterSource) filterSource).column.getFilter();
							if (source != null) {
								filterSource = source.getFilterSource();
							}
						}
						if (c.getFilter().isDerived() && filterSource == template) {
							FilterModel m = new FilterModel();
							m.table = table;
							m.column = c;
							for (Column pk : table.primaryKey.getColumns()) {
								if (c.equals(pk)) {
									m.isPk = true;
									break;
								}
							}
							m.isFk = filterSource instanceof PKColumnFilterSource;
							derivedFilters.add(m);
						}
					}
				}
			}
		}
		derivedFilterList.setModel(derivedFilters);
		return derivedFilterList;
	}

	private DerivedFilterList createDerivedFilterList(Table root) {
		DerivedFilterList derivedFilterList = new DerivedFilterList(root);
		List<FilterModel> derivedFilters = new ArrayList<FilterModel>();
		for (final Table table: getDataModel().getSortedTables()) {
			for (Column c: table.getColumns()) {
				if (c.getFilter() != null) {
					if (root == null || c.getFilter().isDerived()) {
						if (root == null
								|| c.getFilter().getFilterSource() != null
								&& c.getFilter().getFilterSource() instanceof PKColumnFilterSource
								&& ((PKColumnFilterSource) c.getFilter()
										.getFilterSource()).table == root) {
							FilterModel m = new FilterModel();
							m.table = table;
							m.column = c;
							for (Column pk : table.primaryKey.getColumns()) {
								if (c.equals(pk)) {
									m.isPk = true;
									break;
								}
							}
							m.isFk = c.getFilter().getFilterSource() != null && c.getFilter().getFilterSource() instanceof PKColumnFilterSource;
							derivedFilters.add(m);
						}
					}
				}
			}
		}
		derivedFilterList.setModel(derivedFilters);
		return derivedFilterList;
	}

	/**
	 * Stores the text-field content.
	 */
	private void storeFilterExpressions() {
		for (Column c: filterTextfieldsPerColumn.keySet()) {
			String newFilter = (filterTextfieldsPerColumn.get(c).getText()).trim().replaceFirst("\\s*;$", "");
			boolean hasDerivedFilter = c.getFilter() != null && c.getFilter().isDerived();
			if (hasDerivedFilter && newFilter.length() == 0) {
				newFilter = Filter.OLD_VALUE_PROP;
			}
			if (newFilter.length() == 0 || c.getFilter() == null && newFilter.trim().equals(Filter.OLD_VALUE_PROP)
					|| hasDerivedFilter && newFilter.trim().equals(c.getFilter().getExpression())) {
				newFilter = null;
			}
			c.setFilter(newFilter == null? null : new Filter(newFilter, null, false, null));
			if (c.getFilter() != null && filterApplyAtCBPerColumn.get(c) != null) {
				c.getFilter().setApplyAtExport(filterApplyAtCBPerColumn.get(c).getSelectedIndex() == 0);
			}
			if (c.getFilter() != null && Filter.EXCLUDED_VALUE.equals(newFilter)) {
				c.getFilter().setApplyAtExport(true);
			}
			if (c.getFilter() != null && typeTextfieldsPerColumn.get(c) != null) {
				String type = typeTextfieldsPerColumn.get(c).getText().trim();
				if (type.length() == 0 || type.equals((c.toSQL(null).substring(c.name.length())).trim())) {
					type = null;
				}
				c.getFilter().setType(type);
			}
		}
		getDataModel().deriveFilters();
		getDataModel().version++;
		parent.extractionModelEditor.refresh(false, false, true, true);
		parent.extractionModelEditor.markDirty();
		parent.extractionModelEditor.resetUndoStack();
	}

	/**
	 * Whether there is unsaved data.
	 */
	private boolean needsSave() {
		for (Column c: filterTextfieldsPerColumn.keySet()) {
			String newFilter = (filterTextfieldsPerColumn.get(c).getText()).trim();
			boolean hasDerivedFilter = c.getFilter() != null && c.getFilter().isDerived();
			if (hasDerivedFilter && newFilter.length() == 0) {
				newFilter = Filter.OLD_VALUE_PROP;
			}
			if (newFilter.length() == 0 || c.getFilter() == null && newFilter.trim().equals(Filter.OLD_VALUE_PROP)
					|| hasDerivedFilter && newFilter.trim().equals(c.getFilter().getExpression())) {
				newFilter = null;
			}
			String filterExpression = c.getFilter() != null && !c.getFilter().isDerived()? c.getFilter().getExpression() : null;
			if (filterExpression == null && newFilter != null) {
				return true;
			}
			if (filterExpression != null) {
				if (!filterExpression.equals(newFilter)) {
					return true;
				}
			}
			if (c.getFilter() != null && typeTextfieldsPerColumn.get(c) != null) {
				String type = typeTextfieldsPerColumn.get(c).getText().trim();
				if (type.length() == 0 || type.equals((c.toSQL(null).substring(c.name.length())).trim())) {
					type = null;
				}
				if (c.getFilter().getType() != null) {
					if (type == null) {
						return true;
					} else {
						if (!type.equals(c.getFilter().getType())) {
							return true;
						}
					}
				} else {
					if (type != null) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/** This method is called from within the constructor to
	 * initialize the form.
	 * WARNING: Do NOT modify this code. The content of this method is
	 * always regenerated by the Form Editor.
	 */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        templateDetailsPanel = new javax.swing.JPanel();
        jLabel6 = new javax.swing.JLabel();
        templateDetailsNameField = new javax.swing.JTextField();
        jLabel7 = new javax.swing.JLabel();
        templateDetailsNewValueField = new javax.swing.JTextField();
        templateDetailsEnabledCheckBox = new javax.swing.JCheckBox();
        templatesDetailsMulitlineLabel = new javax.swing.JLabel();
        templatesDetailsClausePanel = new javax.swing.JPanel();
        jLabel13 = new javax.swing.JLabel();
        jPanel13 = new javax.swing.JPanel();
        templatesDetailsApplyAtComboBox = new JComboBox2();
        templateDetailsTypeLabel = new javax.swing.JLabel();
        templateDetailsTypeField = new javax.swing.JTextField();
        templateDetailsExcludedCheckBox = new javax.swing.JCheckBox();
        clauseDetailsPanel = new javax.swing.JPanel();
        jPanel9 = new javax.swing.JPanel();
        jPanel10 = new javax.swing.JPanel();
        clauseDetailsSubjectComboBox = new JComboBox2();
        clauseDetailsPredicateComboBox = new JComboBox2();
        jPanel14 = new javax.swing.JPanel();
        clauseDetailsObjectTextField = new JComboBox2();
        jPanel11 = new javax.swing.JPanel();
        jLabel9 = new javax.swing.JLabel();
        clausePredHelpRE = new javax.swing.JPanel();
        jLabel10 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();
        clausePredHelpLike = new javax.swing.JPanel();
        jLabel8 = new javax.swing.JLabel();
        jPanel12 = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jEditorPane1 = new javax.swing.JEditorPane();
        jPanel2 = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();
        cancelButton = new javax.swing.JButton();
        jPanel3 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        helpButton = new javax.swing.JButton();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanel5 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        filterScrollPane = new javax.swing.JScrollPane();
        filterPane = new javax.swing.JPanel();
        applyButton = new javax.swing.JButton();
        jLabel5 = new javax.swing.JLabel();
        Panel2 = new javax.swing.JPanel();
        derivedPanel1 = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        tableBox = new JComboBox2();
        jLabel2 = new javax.swing.JLabel();
        jPanel6 = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jSplitPane2 = new javax.swing.JSplitPane();
        jPanel8 = new javax.swing.JPanel();
        templatesPane = new javax.swing.JPanel();
        jLabel11 = new javax.swing.JLabel();
        Panel3 = new javax.swing.JPanel();
        derivedPanel2 = new javax.swing.JPanel();
        jLabel12 = new javax.swing.JLabel();
        allFiltersPanel = new javax.swing.JPanel();

        templateDetailsPanel.setLayout(new java.awt.GridBagLayout());

        jLabel6.setText("Name ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        templateDetailsPanel.add(jLabel6, gridBagConstraints);

        templateDetailsNameField.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        templateDetailsPanel.add(templateDetailsNameField, gridBagConstraints);

        jLabel7.setText("New value :=   ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        templateDetailsPanel.add(jLabel7, gridBagConstraints);

        templateDetailsNewValueField.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 10;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        templateDetailsPanel.add(templateDetailsNewValueField, gridBagConstraints);

        templateDetailsEnabledCheckBox.setText("Enable Template");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        templateDetailsPanel.add(templateDetailsEnabledCheckBox, gridBagConstraints);

        templatesDetailsMulitlineLabel.setText("jLabel8");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 10;
        templateDetailsPanel.add(templatesDetailsMulitlineLabel, gridBagConstraints);

        templatesDetailsClausePanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Condition"));
        templatesDetailsClausePanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 5;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        templateDetailsPanel.add(templatesDetailsClausePanel, gridBagConstraints);

        jLabel13.setText("Apply at ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 19;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        templateDetailsPanel.add(jLabel13, gridBagConstraints);

        jPanel13.setLayout(new java.awt.GridBagLayout());

        templatesDetailsApplyAtComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Export", "Import" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        jPanel13.add(templatesDetailsApplyAtComboBox, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 19;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(2, 4, 4, 0);
        templateDetailsPanel.add(jPanel13, gridBagConstraints);

        templateDetailsTypeLabel.setText("Type  (optional) ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        templateDetailsPanel.add(templateDetailsTypeLabel, gridBagConstraints);

        templateDetailsTypeField.setText("jTextField1");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        templateDetailsPanel.add(templateDetailsTypeField, gridBagConstraints);

        templateDetailsExcludedCheckBox.setText("Excluded from export");
        templateDetailsExcludedCheckBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                templateDetailsExcludedCheckBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 11;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        templateDetailsPanel.add(templateDetailsExcludedCheckBox, gridBagConstraints);

        clauseDetailsPanel.setLayout(new java.awt.GridBagLayout());

        jPanel9.setLayout(new java.awt.GridLayout(1, 0));

        jPanel10.setLayout(new java.awt.GridLayout(1, 2));

        clauseDetailsSubjectComboBox.setMaximumRowCount(22);
        clauseDetailsSubjectComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        clauseDetailsSubjectComboBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                clauseDetailsSubjectComboBoxItemStateChanged(evt);
            }
        });
        jPanel10.add(clauseDetailsSubjectComboBox);

        clauseDetailsPredicateComboBox.setMaximumRowCount(22);
        clauseDetailsPredicateComboBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        clauseDetailsPredicateComboBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                clauseDetailsPredicateComboBoxItemStateChanged(evt);
            }
        });
        jPanel10.add(clauseDetailsPredicateComboBox);

        jPanel9.add(jPanel10);

        jPanel14.setLayout(new java.awt.GridBagLayout());

        clauseDetailsObjectTextField.setEditable(true);
        clauseDetailsObjectTextField.setMaximumRowCount(22);
        clauseDetailsObjectTextField.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        jPanel14.add(clauseDetailsObjectTextField, gridBagConstraints);

        jPanel9.add(jPanel14);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        clauseDetailsPanel.add(jPanel9, gridBagConstraints);

        jPanel11.setLayout(new java.awt.GridBagLayout());

        jLabel9.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.VERTICAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(12, 0, 4, 0);
        jPanel11.add(jLabel9, gridBagConstraints);

        clausePredHelpRE.setLayout(new java.awt.GridBagLayout());

        jLabel10.setText("   Matches regular expression ");
        clausePredHelpRE.add(jLabel10, new java.awt.GridBagConstraints());

        jButton1.setText("Help");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        clausePredHelpRE.add(jButton1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel11.add(clausePredHelpRE, gridBagConstraints);

        clausePredHelpLike.setLayout(new javax.swing.BoxLayout(clausePredHelpLike, javax.swing.BoxLayout.LINE_AXIS));

        jLabel8.setText("    Like  -  \"%\": match any string.   \"_\": match on single character.");
        clausePredHelpLike.add(jLabel8);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        jPanel11.add(clausePredHelpLike, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        clauseDetailsPanel.add(jPanel11, gridBagConstraints);

        jPanel12.setLayout(new java.awt.GridBagLayout());

        jEditorPane1.setEditable(false);
        jEditorPane1.setContentType("text/html"); // NOI18N
        jEditorPane1.setText("<html>\n  <head>\n\n  </head>\n  <body>\n    \n <table border=\"0\" cellpadding=\"1\" cellspacing=\"0\">\n <tr align=\"left\">\n <td bgcolor=\"#CCCCFF\" align=\"left\" >Construct</td>\n <td bgcolor=\"#CCCCFF\" align=\"left\" >Matches</td>\n </tr>\n\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"characters\">Characters</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct characters\"><i>x</i></td>\n     <td headers=\"matches\">The character <i>x</i></td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\\\</tt></td>\n     <td headers=\"matches\">The backslash character</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\0</tt><i>n</i></td>\n     <td headers=\"matches\">The character with octal value <tt>0</tt><i>n</i>\n         (0&nbsp;<tt>&lt;=</tt>&nbsp;<i>n</i>&nbsp;<tt>&lt;=</tt>&nbsp;7)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\0</tt><i>nn</i></td>\n     <td headers=\"matches\">The character with octal value <tt>0</tt><i>nn</i>\n         (0&nbsp;<tt>&lt;=</tt>&nbsp;<i>n</i>&nbsp;<tt>&lt;=</tt>&nbsp;7)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\0</tt><i>mnn</i></td>\n     <td headers=\"matches\">The character with octal value <tt>0</tt><i>mnn</i>\n         (0&nbsp;<tt>&lt;=</tt>&nbsp;<i>m</i>&nbsp;<tt>&lt;=</tt>&nbsp;3,\n         0&nbsp;<tt>&lt;=</tt>&nbsp;<i>n</i>&nbsp;<tt>&lt;=</tt>&nbsp;7)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\x</tt><i>hh</i></td>\n     <td headers=\"matches\">The character with hexadecimal&nbsp;value&nbsp;<tt>0x</tt><i>hh</i></td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>&#92;u</tt><i>hhhh</i></td>\n     <td headers=\"matches\">The character with hexadecimal&nbsp;value&nbsp;<tt>0x</tt><i>hhhh</i></td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>&#92;x</tt><i>{h...h}</i></td>\n     <td headers=\"matches\">The character with hexadecimal&nbsp;value&nbsp;<tt>0x</tt><i>h...h</i>\n         (<a href=\"../../../java/lang/Character.html#MIN_CODE_POINT\"><code>Character.MIN_CODE_POINT</code></a>\n         &nbsp;&lt;=&nbsp;<tt>0x</tt><i>h...h</i>&nbsp;&lt;=&nbsp\n          <a href=\"../../../java/lang/Character.html#MAX_CODE_POINT\"><code>Character.MAX_CODE_POINT</code></a>)</td></tr>\n <tr><td valign=\"top\" headers=\"matches\"><tt>\\t</tt></td>\n     <td headers=\"matches\">The tab character (<tt>'&#92;u0009'</tt>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\n</tt></td>\n     <td headers=\"matches\">The newline (line feed) character (<tt>'&#92;u000A'</tt>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\r</tt></td>\n     <td headers=\"matches\">The carriage-return character (<tt>'&#92;u000D'</tt>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\f</tt></td>\n     <td headers=\"matches\">The form-feed character (<tt>'&#92;u000C'</tt>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\a</tt></td>\n     <td headers=\"matches\">The alert (bell) character (<tt>'&#92;u0007'</tt>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\e</tt></td>\n     <td headers=\"matches\">The escape character (<tt>'&#92;u001B'</tt>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct characters\"><tt>\\c</tt><i>x</i></td>\n     <td headers=\"matches\">The control character corresponding to <i>x</i></td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"classes\">Character classes</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[abc]</tt></td>\n     <td headers=\"matches\"><tt>a</tt>, <tt>b</tt>, or <tt>c</tt> (simple class)</td></tr>\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[^abc]</tt></td>\n     <td headers=\"matches\">Any character except <tt>a</tt>, <tt>b</tt>, or <tt>c</tt> (negation)</td></tr>\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[a-zA-Z]</tt></td>\n     <td headers=\"matches\"><tt>a</tt> through <tt>z</tt>\n         or <tt>A</tt> through <tt>Z</tt>, inclusive (range)</td></tr>\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[a-d[m-p]]</tt></td>\n     <td headers=\"matches\"><tt>a</tt> through <tt>d</tt>,\n      or <tt>m</tt> through <tt>p</tt>: <tt>[a-dm-p]</tt> (union)</td></tr>\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[a-z&&[def]]</tt></td>\n     <td headers=\"matches\"><tt>d</tt>, <tt>e</tt>, or <tt>f</tt> (intersection)</tr>\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[a-z&&[^bc]]</tt></td>\n     <td headers=\"matches\"><tt>a</tt> through <tt>z</tt>,\n         except for <tt>b</tt> and <tt>c</tt>: <tt>[ad-z]</tt> (subtraction)</td></tr>\n <tr><td valign=\"top\" headers=\"construct classes\"><tt>[a-z&&[^m-p]]</tt></td>\n     <td headers=\"matches\"><tt>a</tt> through <tt>z</tt>,\n          and not <tt>m</tt> through <tt>p</tt>: <tt>[a-lq-z]</tt>(subtraction)</td></tr>\n <tr><th>&nbsp;</th></tr>\n\n <tr align=\"left\"><th colspan=\"2\" id=\"predef\">Predefined character classes</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>.</tt></td>\n     <td headers=\"matches\">Any character (may or may not match <a href=\"#lt\">line terminators</a>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>\\d</tt></td>\n     <td headers=\"matches\">A digit: <tt>[0-9]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>\\D</tt></td>\n     <td headers=\"matches\">A non-digit: <tt>[^0-9]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>\\s</tt></td>\n     <td headers=\"matches\">A whitespace character: <tt>[ \\t\\n\\x0B\\f\\r]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>\\S</tt></td>\n     <td headers=\"matches\">A non-whitespace character: <tt>[^\\s]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>\\w</tt></td>\n     <td headers=\"matches\">A word character: <tt>[a-zA-Z_0-9]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct predef\"><tt>\\W</tt></td>\n     <td headers=\"matches\">A non-word character: <tt>[^\\w]</tt></td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"posix\">POSIX character classes</b> (US-ASCII only)<b></th></tr>\n\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Lower}</tt></td>\n     <td headers=\"matches\">A lower-case alphabetic character: <tt>[a-z]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Upper}</tt></td>\n     <td headers=\"matches\">An upper-case alphabetic character:<tt>[A-Z]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{ASCII}</tt></td>\n     <td headers=\"matches\">All ASCII:<tt>[\\x00-\\x7F]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Alpha}</tt></td>\n     <td headers=\"matches\">An alphabetic character:<tt>[\\p{Lower}\\p{Upper}]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Digit}</tt></td>\n     <td headers=\"matches\">A decimal digit: <tt>[0-9]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Alnum}</tt></td>\n     <td headers=\"matches\">An alphanumeric character:<tt>[\\p{Alpha}\\p{Digit}]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Punct}</tt></td>\n     <td headers=\"matches\">Punctuation: One of <tt>!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~</tt></td></tr>\n     \n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Graph}</tt></td>\n     <td headers=\"matches\">A visible character: <tt>[\\p{Alnum}\\p{Punct}]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Print}</tt></td>\n     <td headers=\"matches\">A printable character: <tt>[\\p{Graph}\\x20]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Blank}</tt></td>\n     <td headers=\"matches\">A space or a tab: <tt>[ \\t]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Cntrl}</tt></td>\n     <td headers=\"matches\">A control character: <tt>[\\x00-\\x1F\\x7F]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{XDigit}</tt></td>\n     <td headers=\"matches\">A hexadecimal digit: <tt>[0-9a-fA-F]</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct posix\"><tt>\\p{Space}</tt></td>\n     <td headers=\"matches\">A whitespace character: <tt>[ \\t\\n\\x0B\\f\\r]</tt></td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\">java.lang.Character classes (simple <a href=\"#jcc\">java character type</a>)</th></tr>\n\n <tr><td valign=\"top\"><tt>\\p{javaLowerCase}</tt></td>\n     <td>Equivalent to java.lang.Character.isLowerCase()</td></tr>\n <tr><td valign=\"top\"><tt>\\p{javaUpperCase}</tt></td>\n     <td>Equivalent to java.lang.Character.isUpperCase()</td></tr>\n <tr><td valign=\"top\"><tt>\\p{javaWhitespace}</tt></td>\n     <td>Equivalent to java.lang.Character.isWhitespace()</td></tr>\n <tr><td valign=\"top\"><tt>\\p{javaMirrored}</tt></td>\n     <td>Equivalent to java.lang.Character.isMirrored()</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"unicode\">Classes for Unicode scripts, blocks, categories and binary properties</th></tr>\n * <tr><td valign=\"top\" headers=\"construct unicode\"><tt>\\p{IsLatin}</tt></td>\n     <td headers=\"matches\">A Latin&nbsp;script character (<a href=\"#usc\">script</a>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct unicode\"><tt>\\p{InGreek}</tt></td>\n     <td headers=\"matches\">A character in the Greek&nbsp;block (<a href=\"#ubc\">block</a>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct unicode\"><tt>\\p{Lu}</tt></td>\n     <td headers=\"matches\">An uppercase letter (<a href=\"#ucc\">category</a>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct unicode\"><tt>\\p{IsAlphabetic}</tt></td>\n     <td headers=\"matches\">An alphabetic character (<a href=\"#ubpc\">binary property</a>)</td></tr>\n <tr><td valign=\"top\" headers=\"construct unicode\"><tt>\\p{Sc}</tt></td>\n     <td headers=\"matches\">A currency symbol</td></tr>\n <tr><td valign=\"top\" headers=\"construct unicode\"><tt>\\P{InGreek}</tt></td>\n     <td headers=\"matches\">Any character except one in the Greek block (negation)</td></tr>\n <tr><td valign=\"top\" headers=\"construct unicode\"><tt>[\\p{L}&&[^\\p{Lu}]]&nbsp;</tt></td>\n     <td headers=\"matches\">Any letter except an uppercase letter (subtraction)</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"bounds\">Boundary matchers</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>^</tt></td>\n     <td headers=\"matches\">The beginning of a line</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>$</tt></td>\n     <td headers=\"matches\">The end of a line</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>\\b</tt></td>\n     <td headers=\"matches\">A word boundary</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>\\B</tt></td>\n     <td headers=\"matches\">A non-word boundary</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>\\A</tt></td>\n     <td headers=\"matches\">The beginning of the input</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>\\G</tt></td>\n     <td headers=\"matches\">The end of the previous match</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>\\Z</tt></td>\n     <td headers=\"matches\">The end of the input but for the final\n         <a href=\"#lt\">terminator</a>, if&nbsp;any</td></tr>\n <tr><td valign=\"top\" headers=\"construct bounds\"><tt>\\z</tt></td>\n     <td headers=\"matches\">The end of the input</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"greedy\">Greedy quantifiers</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct greedy\"><i>X</i><tt>?</tt></td>\n     <td headers=\"matches\"><i>X</i>, once or not at all</td></tr>\n <tr><td valign=\"top\" headers=\"construct greedy\"><i>X</i><tt>*</tt></td>\n     <td headers=\"matches\"><i>X</i>, zero or more times</td></tr>\n <tr><td valign=\"top\" headers=\"construct greedy\"><i>X</i><tt>+</tt></td>\n     <td headers=\"matches\"><i>X</i>, one or more times</td></tr>\n <tr><td valign=\"top\" headers=\"construct greedy\"><i>X</i><tt>{</tt><i>n</i><tt>}</tt></td>\n     <td headers=\"matches\"><i>X</i>, exactly <i>n</i> times</td></tr>\n <tr><td valign=\"top\" headers=\"construct greedy\"><i>X</i><tt>{</tt><i>n</i><tt>,}</tt></td>\n     <td headers=\"matches\"><i>X</i>, at least <i>n</i> times</td></tr>\n <tr><td valign=\"top\" headers=\"construct greedy\"><i>X</i><tt>{</tt><i>n</i><tt>,</tt><i>m</i><tt>}</tt></td>\n     <td headers=\"matches\"><i>X</i>, at least <i>n</i> but not more than <i>m</i> times</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"reluc\">Reluctant quantifiers</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct reluc\"><i>X</i><tt>??</tt></td>\n     <td headers=\"matches\"><i>X</i>, once or not at all</td></tr>\n <tr><td valign=\"top\" headers=\"construct reluc\"><i>X</i><tt>*?</tt></td>\n     <td headers=\"matches\"><i>X</i>, zero or more times</td></tr>\n <tr><td valign=\"top\" headers=\"construct reluc\"><i>X</i><tt>+?</tt></td>\n     <td headers=\"matches\"><i>X</i>, one or more times</td></tr>\n <tr><td valign=\"top\" headers=\"construct reluc\"><i>X</i><tt>{</tt><i>n</i><tt>}?</tt></td>\n     <td headers=\"matches\"><i>X</i>, exactly <i>n</i> times</td></tr>\n <tr><td valign=\"top\" headers=\"construct reluc\"><i>X</i><tt>{</tt><i>n</i><tt>,}?</tt></td>\n     <td headers=\"matches\"><i>X</i>, at least <i>n</i> times</td></tr>\n <tr><td valign=\"top\" headers=\"construct reluc\"><i>X</i><tt>{</tt><i>n</i><tt>,</tt><i>m</i><tt>}?</tt></td>\n     <td headers=\"matches\"><i>X</i>, at least <i>n</i> but not more than <i>m</i> times</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"poss\">Possessive quantifiers</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct poss\"><i>X</i><tt>?+</tt></td>\n     <td headers=\"matches\"><i>X</i>, once or not at all</td></tr>\n <tr><td valign=\"top\" headers=\"construct poss\"><i>X</i><tt>*+</tt></td>\n     <td headers=\"matches\"><i>X</i>, zero or more times</td></tr>\n <tr><td valign=\"top\" headers=\"construct poss\"><i>X</i><tt>++</tt></td>\n     <td headers=\"matches\"><i>X</i>, one or more times</td></tr>\n <tr><td valign=\"top\" headers=\"construct poss\"><i>X</i><tt>{</tt><i>n</i><tt>}+</tt></td>\n     <td headers=\"matches\"><i>X</i>, exactly <i>n</i> times</td></tr>\n <tr><td valign=\"top\" headers=\"construct poss\"><i>X</i><tt>{</tt><i>n</i><tt>,}+</tt></td>\n     <td headers=\"matches\"><i>X</i>, at least <i>n</i> times</td></tr>\n <tr><td valign=\"top\" headers=\"construct poss\"><i>X</i><tt>{</tt><i>n</i><tt>,</tt><i>m</i><tt>}+</tt></td>\n     <td headers=\"matches\"><i>X</i>, at least <i>n</i> but not more than <i>m</i> times</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"logical\">Logical operators</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct logical\"><i>XY</i></td>\n     <td headers=\"matches\"><i>X</i> followed by <i>Y</i></td></tr>\n <tr><td valign=\"top\" headers=\"construct logical\"><i>X</i><tt>|</tt><i>Y</i></td>\n     <td headers=\"matches\">Either <i>X</i> or <i>Y</i></td></tr>\n <tr><td valign=\"top\" headers=\"construct logical\"><tt>(</tt><i>X</i><tt>)</tt></td>\n     <td headers=\"matches\">X, as a <a href=\"#cg\">capturing group</a></td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"backref\">Back references</th></tr>\n\n <tr><td valign=\"bottom\" headers=\"construct backref\"><tt>\\</tt><i>n</i></td>\n     <td valign=\"bottom\" headers=\"matches\">Whatever the <i>n</i><sup>th</sup>\n     <a href=\"#cg\">capturing group</a> matched</td></tr>\n\n <tr><td valign=\"bottom\" headers=\"construct backref\"><tt>\\</tt><i>k</i>&lt;<i>name</i>&gt;</td>\n     <td valign=\"bottom\" headers=\"matches\">Whatever the\n     <a href=\"#groupname\">named-capturing group</a> \"name\" matched</td></tr>\n\n <tr><th>&nbsp;</th></tr>\n <tr align=\"left\"><th colspan=\"2\" id=\"quot\">Quotation</th></tr>\n\n <tr><td valign=\"top\" headers=\"construct quot\"><tt>\\</tt></td>\n     <td headers=\"matches\">Nothing, but quotes the following character</td></tr>\n <tr><td valign=\"top\" headers=\"construct quot\"><tt>\\Q</tt></td>\n     <td headers=\"matches\">Nothing, but quotes all characters until <tt>\\E</tt></td></tr>\n <tr><td valign=\"top\" headers=\"construct quot\"><tt>\\E</tt></td>\n     <td headers=\"matches\">Nothing, but ends quoting started by <tt>\\Q</tt></td></tr>\n     \n</table>\n \n    </p>\n  </body>\n</html>\n");
        jScrollPane2.setViewportView(jEditorPane1);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel12.add(jScrollPane2, gridBagConstraints);

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Filter Editor");
        getContentPane().setLayout(new java.awt.GridBagLayout());

        jPanel2.setLayout(new java.awt.GridBagLayout());

        okButton.setText("  OK  ");
        okButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                okButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 4, 2);
        jPanel2.add(okButton, gridBagConstraints);

        cancelButton.setText(" Cancel ");
        cancelButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 11;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 4, 4);
        jPanel2.add(cancelButton, gridBagConstraints);

        jPanel3.setLayout(new java.awt.GridBagLayout());

        jLabel3.setForeground(new java.awt.Color(255, 0, 0));
        jLabel3.setText("Primary key column  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 2, 2);
        jPanel3.add(jLabel3, gridBagConstraints);

        jLabel4.setForeground(new java.awt.Color(0, 0, 255));
        jLabel4.setText("Column with derived filter");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 2, 2, 2);
        jPanel3.add(jLabel4, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 20;
        jPanel2.add(jPanel3, gridBagConstraints);

        helpButton.setText(" Help ");
        helpButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                helpButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 16, 4, 2);
        jPanel2.add(helpButton, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 20;
        gridBagConstraints.gridwidth = 13;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        getContentPane().add(jPanel2, gridBagConstraints);

        jPanel5.setLayout(new java.awt.GridBagLayout());

        jPanel4.setLayout(new java.awt.GridBagLayout());

        jSplitPane1.setContinuousLayout(true);
        jSplitPane1.setOneTouchExpandable(true);
        jSplitPane1.setPreferredSize(new java.awt.Dimension(500, 57));

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Filters"));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        filterScrollPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        filterPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        filterPane.setLayout(new java.awt.GridBagLayout());
        filterScrollPane.setViewportView(filterPane);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 2;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(filterScrollPane, gridBagConstraints);

        applyButton.setText("Apply");
        applyButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                applyButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 6, 0, 0);
        jPanel1.add(applyButton, gridBagConstraints);

        jLabel5.setText("                                                                                                                            ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        jPanel1.add(jLabel5, gridBagConstraints);

        jSplitPane1.setLeftComponent(jPanel1);

        Panel2.setBorder(javax.swing.BorderFactory.createTitledBorder("Derived filters"));
        Panel2.setLayout(new java.awt.GridBagLayout());

        derivedPanel1.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        Panel2.add(derivedPanel1, gridBagConstraints);

        jSplitPane1.setRightComponent(Panel2);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel4.add(jSplitPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel5.add(jPanel4, gridBagConstraints);

        jLabel1.setText("   Table ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        jPanel5.add(jLabel1, gridBagConstraints);

        tableBox.setMaximumRowCount(20);
        tableBox.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        tableBox.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                tableBoxItemStateChanged(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
        gridBagConstraints.ipadx = 40;
        gridBagConstraints.insets = new java.awt.Insets(4, 0, 4, 0);
        jPanel5.add(tableBox, gridBagConstraints);

        jLabel2.setForeground(java.awt.Color.gray);
        jLabel2.setText("as T ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 0, 8);
        jPanel5.add(jLabel2, gridBagConstraints);

        jTabbedPane1.addTab("Filters per Table", jPanel5);

        jPanel6.setLayout(new java.awt.GridBagLayout());

        jPanel7.setLayout(new java.awt.GridBagLayout());

        jSplitPane2.setContinuousLayout(true);
        jSplitPane2.setOneTouchExpandable(true);
        jSplitPane2.setPreferredSize(new java.awt.Dimension(500, 57));

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder("Templates"));
        jPanel8.setLayout(new java.awt.GridBagLayout());

        templatesPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        templatesPane.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel8.add(templatesPane, gridBagConstraints);

        jLabel11.setText("                                                                                                                                                  ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        jPanel8.add(jLabel11, gridBagConstraints);

        jSplitPane2.setLeftComponent(jPanel8);

        Panel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Derived filters"));
        Panel3.setLayout(new java.awt.GridBagLayout());

        derivedPanel2.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        Panel3.add(derivedPanel2, gridBagConstraints);

        jLabel12.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 3;
        Panel3.add(jLabel12, gridBagConstraints);

        jSplitPane2.setRightComponent(Panel3);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel7.add(jSplitPane2, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.gridwidth = 12;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel6.add(jPanel7, gridBagConstraints);

        jTabbedPane1.addTab("Templates", jPanel6);

        allFiltersPanel.setLayout(new java.awt.GridBagLayout());
        jTabbedPane1.addTab("All Filters", allFiltersPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        getContentPane().add(jTabbedPane1, gridBagConstraints);

        pack();
    }// </editor-fold>//GEN-END:initComponents

	private void okButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_okButtonActionPerformed
		if (needsSave()) {
			storeFilterExpressions();
		}
		setVisible(false);
	}//GEN-LAST:event_okButtonActionPerformed

	private void tableBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_tableBoxItemStateChanged
		if (tableBox.getSelectedItem() instanceof String) {
			Table table = getDataModel().getTableByDisplayName((String) tableBox.getSelectedItem());
			if (needsSave()) {
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this, "Store changes?", "", JOptionPane.YES_NO_OPTION)) {
					storeFilterExpressions();
				}
			}
			selectedTable = table;
			refreshFilterPane();
		}
	}//GEN-LAST:event_tableBoxItemStateChanged

	private void cancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelButtonActionPerformed
		setVisible(false);
	}//GEN-LAST:event_cancelButtonActionPerformed

	private void applyButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_applyButtonActionPerformed
		apply();
	}//GEN-LAST:event_applyButtonActionPerformed

	private void apply() {
		storeFilterExpressions();
		refreshFilterPane();
	}

	private void clauseDetailsSubjectComboBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_clauseDetailsSubjectComboBoxItemStateChanged
		if (evt.getItem() instanceof Subject) {
			Subject subject = (Subject) evt.getItem();
			onClauseDetailsSubjectChanged(subject);
		}
	}//GEN-LAST:event_clauseDetailsSubjectComboBoxItemStateChanged

	private void clauseDetailsPredicateComboBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_clauseDetailsPredicateComboBoxItemStateChanged
		if (evt.getItem() instanceof Predicate) {
			Predicate predicate = (Predicate) evt.getItem();
			onClauseDetailsPredicateChanged(predicate);
		}
	}//GEN-LAST:event_clauseDetailsPredicateComboBoxItemStateChanged

	private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
		JDialog dialog = new JDialog(this, "Regular Expression", true);
		dialog.pack();
		dialog.setSize(Math.max(dialog.getWidth(), 600), Math.max(dialog.getHeight(), 700));
		dialog.setLocation(
				getLocation().x + getWidth() /2 - dialog.getWidth() /2,
				getLocation().y + getHeight() /2 - dialog.getHeight() /2);
		dialog.getContentPane().add(jPanel12);
		dialog.setVisible(true);
	}//GEN-LAST:event_jButton1ActionPerformed

	private void helpButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_helpButtonActionPerformed
		JDialog dialog = new JDialog(this, "Filtering", true);
		dialog.pack();
		dialog.setSize(Math.max(dialog.getWidth(), 600), Math.max(dialog.getHeight(), 700));
		dialog.setLocation(
				getLocation().x + getWidth() /2 - dialog.getWidth() /2,
				getLocation().y + getHeight() /2 - dialog.getHeight() /2);

		JPanel helpPanel = new JPanel();
		helpPanel.setLayout(new java.awt.GridBagLayout());

		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setViewportView(helpEditorPane);
		GridBagConstraints gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		helpPanel.add(scrollPane, gridBagConstraints);
		dialog.getContentPane().add(helpPanel);
		dialog.setVisible(true);
	}//GEN-LAST:event_helpButtonActionPerformed

	private String templatePreValue = "";

    private void templateDetailsExcludedCheckBoxItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_templateDetailsExcludedCheckBoxItemStateChanged
    	if (templateDetailsExcludedCheckBox.isSelected()) {
    		templatePreValue = templateDetailsNewValueField.getText();
    		templateDetailsNewValueField.setText(Filter.EXCLUDED_VALUE);
        	templateDetailsNewValueField.setEditable(false);
    		templateDetailsNewValueField.setEnabled(false);
    	} else {
    		templateDetailsNewValueField.setText(templatePreValue);
        	templateDetailsNewValueField.setEditable(true);
    		templateDetailsNewValueField.setEnabled(true);
    	}
    }//GEN-LAST:event_templateDetailsExcludedCheckBoxItemStateChanged

	JEditorPane helpEditorPane = new JEditorPane();
	{
		helpEditorPane.setContentType("text/html"); // NOI18N

		   InputStream is = getClass().getResourceAsStream("/net/sf/jailer/ui/resource/filters.html");
		   if (is != null) {
			   BufferedReader in = new BufferedReader(new InputStreamReader(is)); // lgtm [java/input-resource-leak]
			   StringBuffer sb = new StringBuffer();
			   int c;
			   try {
				while ((c = in.read()) != -1) {
					sb.append((char) c);
				}
				is.close();

				Matcher m = Pattern.compile("(<img\\s+src\\s*=\\s*\")(.*)(\")").matcher(sb.toString());
				sb.setLength(0);
				while (m.find()) {
					m.appendReplacement(sb, "");
					sb.append(m.group(1)).append(getClass().getResource("/net/sf/jailer/ui/resource/" + m.group(2))).append(m.group(3));
				}
				m.appendTail(sb);

				helpEditorPane.setText(sb.toString());
				helpEditorPane.setEditable(false);
			} catch (Exception e) {
				e.printStackTrace();
			}
		   }
	}

	private void onClauseDetailsPredicateChanged(Predicate predicate) {
		clauseDetailsObjectTextField.setVisible(predicate.needsObject);
		jPanel14.setVisible(predicate.needsObject);
		JComponent help = helpComponents.get(predicate);
		for (JComponent component: helpComponents.values()) {
			component.setVisible(false);
		}
		if (help != null) {
			help.setVisible(true);
		}
	}

	private void onClauseDetailsSubjectChanged(Subject subject) {
		List<Predicate> predicates = new ArrayList<Predicate>();
		for (Predicate predicate: Predicate.values()) {
			if (predicate.type.equals(subject.type)) {
				predicates.add(predicate);
			}
		}
		ComboBoxModel cm = new DefaultComboBoxModel(predicates.toArray());
		clauseDetailsPredicateComboBox.setModel(cm);
		Predicate selectedItem = (Predicate) clauseDetailsPredicateComboBox.getSelectedItem();
		if (selectedItem != null) {
			onClauseDetailsPredicateChanged(selectedItem);
		}
		String[] objs = objectsModel.get(subject);
		if (objs == null) {
			cm = new DefaultComboBoxModel<String>();
		} else {
			cm = new DefaultComboBoxModel<String>(objs);
		}
		Object old = clauseDetailsObjectTextField.getSelectedItem();
		clauseDetailsObjectTextField.setModel(cm);
		clauseDetailsObjectTextField.setSelectedItem(old);
	}

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel Panel2;
    private javax.swing.JPanel Panel3;
    private javax.swing.JPanel allFiltersPanel;
    private javax.swing.JButton applyButton;
    private javax.swing.JButton cancelButton;
    private JComboBox2 clauseDetailsObjectTextField;
    private javax.swing.JPanel clauseDetailsPanel;
    private JComboBox2 clauseDetailsPredicateComboBox;
    private JComboBox2 clauseDetailsSubjectComboBox;
    private javax.swing.JPanel clausePredHelpLike;
    private javax.swing.JPanel clausePredHelpRE;
    private javax.swing.JPanel derivedPanel1;
    private javax.swing.JPanel derivedPanel2;
    private javax.swing.JPanel filterPane;
    private javax.swing.JScrollPane filterScrollPane;
    private javax.swing.JButton helpButton;
    private javax.swing.JButton jButton1;
    private javax.swing.JEditorPane jEditorPane1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel10;
    private javax.swing.JPanel jPanel11;
    private javax.swing.JPanel jPanel12;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel14;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JPanel jPanel6;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanel8;
    private javax.swing.JPanel jPanel9;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JSplitPane jSplitPane2;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JButton okButton;
    private JComboBox2 tableBox;
    private javax.swing.JCheckBox templateDetailsEnabledCheckBox;
    private javax.swing.JCheckBox templateDetailsExcludedCheckBox;
    private javax.swing.JTextField templateDetailsNameField;
    private javax.swing.JTextField templateDetailsNewValueField;
    private javax.swing.JPanel templateDetailsPanel;
    private javax.swing.JTextField templateDetailsTypeField;
    private javax.swing.JLabel templateDetailsTypeLabel;
    private JComboBox2 templatesDetailsApplyAtComboBox;
    private javax.swing.JPanel templatesDetailsClausePanel;
    private javax.swing.JLabel templatesDetailsMulitlineLabel;
    private javax.swing.JPanel templatesPane;
    // End of variables declaration//GEN-END:variables
	private TemplateList templateList;


	private Map<Predicate, JComponent> helpComponents = new HashMap<Predicate, JComponent>();
	private Map<Subject, String[]> objectsModel = new HashMap<Subject, String[]>();

	private Icon conditionEditorIcon;
	private Icon conditionEditorSelectedIcon;
	private static ImageIcon okIcon;
	private static ImageIcon cancelIcon;
	{
		// load images
		conditionEditorIcon = UIUtil.readImage("/edit.png");
		conditionEditorSelectedIcon = UIUtil.readImage("/edit_s.png");
		okIcon = UIUtil.readImage("/buttonok.png");
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}
	private static final long serialVersionUID = 7869830170667759018L;
}
