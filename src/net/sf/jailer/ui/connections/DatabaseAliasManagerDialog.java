package net.sf.jailer.ui.connections;

import com.digitprop.tonic.TonicLookAndFeel;
import net.sf.jailer.aliases.database.DatabaseAlias;
import net.sf.jailer.aliases.database.DatabaseAliasManager;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Comparator;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 07.03.2008
 * @time: 21:51:53
 */
public final class DatabaseAliasManagerDialog extends JDialog {

	public static void main(String argv[]) {
		try {
			UIManager.setLookAndFeel(new TonicLookAndFeel());
		} catch (UnsupportedLookAndFeelException exception) {
			JFrame.setDefaultLookAndFeelDecorated(true);
		}
		try {
			File f = new File("config/connections/aliases.xml");
			DatabaseAliasManager.load(f);
		} catch (Throwable throwable) {
			throwable.printStackTrace();
		}
		DatabaseAliasManagerDialog dialog = new DatabaseAliasManagerDialog(null);
		dialog.showDialog();
	}

	/**
	 * Shows the {@code DatabaseAliasManagerDialog}.
	 *
	 * Use this method for simple calls rather than construction.
	 *
	 * @param owner An parent frame for this dialog.
	 */
	public static void showDialog(Frame owner) {
		new DatabaseAliasManagerDialog(owner).showDialog();
	}

	/**
	 * Adds all loaded {@link DatabaseAlias}es, taken via
	 * {@link net.sf.jailer.aliases.database.DatabaseAliasManager#getDatabaseAliases()} method, to this dialog.
	 *
	 * During the adding, a gui forms are created for each of aliases.
	 */
	private void insertEditorsForStoredAliases() {
		DatabaseAlias aliases[] = DatabaseAliasManager.getDatabaseAliases();
		//noinspection ForLoopReplaceableByForEach
		for (int i = 0; i < aliases.length; i++) {
			DatabaseAliasEditor editor = new DatabaseAliasEditor(aliases[i]);
			AliasListItem item = new AliasListItem(editor);
			addAliasListItem(item);
		}
	}

	private DatabaseAliasManagerDialog(Frame owner) {
		super(owner, true);
	}

	private void showDialog() {
		setLocationRelativeTo(null);
		setContentPane(createContentPane());
		pack();
		setAlwaysOnTop(true);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		setTitle("Database Aliases Manager");
		setMinimumSize(getSize());
		insertEditorsForStoredAliases();
		setVisible(true);
	}

	private JComponent createContentPane() {
		Box box = Box.createVerticalBox();
		box.setBorder(BorderFactory.createEmptyBorder(6, 6, 6, 6));
		box.add(createSplitPane());
		box.add(createConfigBox());
		box.add(createButtonsBox());
		return box;
	}

	private JPanel editorPane;
	private JComponent createSplitPane() {
		Box box = Box.createHorizontalBox();
		JSplitPane splitPane = new JSplitPane();
		splitPane.setBorder(BorderFactory.createEmptyBorder());
		splitPane.setDividerSize(12);
		splitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		splitPane.setContinuousLayout(true);
		splitPane.setLeftComponent(createAliasesListPane());
		editorPane = new JPanel(new BorderLayout());
		editorPane.add(Box.createRigidArea(new Dimension(350, 400)), "Center");
		splitPane.setRightComponent(editorPane);
		box.add(splitPane);
		return box;
	}

	private JComponent createAliasesListPane() {
		JPanel panel = new JPanel(new BorderLayout());
		panel.add(createAliasesListButtons(), BorderLayout.NORTH);
		aliasesList = new JPanel(new VFlowLayout());
		JScrollPane scrollPane = new JScrollPane(aliasesList);
		scrollPane.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(2, 0, 0, 0),
				scrollPane.getBorder()));
		panel.add(scrollPane, BorderLayout.CENTER);
		return panel;
	}

	private JComponent createAliasesListButtons() {
		JToolBar toolBar = new JToolBar(JToolBar.HORIZONTAL);
		JButton addButton = new JButton("Add");
		addButton.setBorder(BorderFactory.createEtchedBorder());
		addButton.addActionListener(new OnAddButton());
		toolBar.add(addButton);
		JButton removeButton = new JButton("Remove");
		removeButton.setBorder(BorderFactory.createEtchedBorder());
		removeButton.addActionListener(new OnRemoveButton());
		toolBar.add(removeButton);
		JButton copyButton = new JButton("Copy");
		copyButton.setBorder(BorderFactory.createEtchedBorder());
		copyButton.addActionListener(new OnDuplicateButton());
		toolBar.add(copyButton);
		JButton sortButton = new JButton("Sort");
		sortButton.setBorder(BorderFactory.createEtchedBorder());
		sortButton.addActionListener(new OnSortButton());
		toolBar.add(sortButton);
		return toolBar;
	}

	private JComponent createConfigBox() {
		Box box = Box.createHorizontalBox();
		JCheckBox checkBox = new JCheckBox();
		checkBox.setSelected(true);
		box.add(checkBox);
		box.add(Box.createHorizontalStrut(6));
		JLabel label = new JLabel("Establish test connections every");
		box.add(label);
		box.add(Box.createHorizontalStrut(6));
		JSpinner spinner = new JSpinner(new SpinnerNumberModel(5, 1, 1440, 1));
		spinner.setBorder(BorderFactory.createLineBorder(Color.GRAY));
		box.add(spinner);
		box.add(Box.createHorizontalStrut(6));
		label = new JLabel("minutes.");
		box.add(label);
		box.add(Box.createHorizontalGlue());
		JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		panel.add(box);
		return panel;
	}

	/**
	 * Creates a buttons panel with "OK", "Apply" and "Cancel" buttons.
	 *
	 * This panel fits all width. Buttons are right-aligned.
	 *
	 * @return A created panel.
	 */
	private JComponent createButtonsBox() {
		Box box = Box.createHorizontalBox();
		box.add(Box.createHorizontalGlue());
		JPanel panel = new JPanel(new GridLayout(1, 3, 3, 0));
		JButton okButton = new JButton("OK");
		panel.add(okButton);
		JButton applyButton = new JButton("Apply");
		panel.add(applyButton);
		JButton cancelButton = new JButton("Cancel");
		panel.add(cancelButton);
		box.add(panel);
		return box;
	}

	///////////////
	// Listeners //
	///////////////

	private final class OnAddButton implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			DatabaseAliasEditor editor = new DatabaseAliasEditor();
			AliasListItem item = new AliasListItem(editor);
			addAliasListItem(item);
			setActiveItem(item);
		}
	}

	private final class OnRemoveButton implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			AliasListItem item = getActiveItem();
			if (item == null) {
				return;
			}
			int index = getComponentIndex(item);
			removeAliasListItem(item);
			int componentsCount = aliasesList.getComponentCount();
			index = Math.min(index, componentsCount - 1);
			if (componentsCount != 0) {
				setActiveItem((AliasListItem)aliasesList.getComponent(index));
			} else {
				setActiveItem(null);
			}
		}
		private int getComponentIndex(Component component) {
			Component[] components = aliasesList.getComponents();
			for (int i = 0; i < components.length; i++) {
				if (components[i] == component) {
					return i;
				}
			}
			return -1;
		}
	}

	private final class OnDuplicateButton implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			AliasListItem item = getActiveItem();
			if (item == null) {
				return;
			}
			AliasListItem duplicateItem = item.duplicate();
			addAliasListItem(duplicateItem);
			setActiveItem(duplicateItem);
		}
	}

	private final class OnSortButton implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			Component components[] = aliasesList.getComponents();
			java.util.Arrays.sort(components, new Comparator<Component>() {
				public int compare(Component o1, Component o2) {
					String t1 = ((AliasListItem)o1).getEditor().getTitle();
					String t2 = ((AliasListItem)o2).getEditor().getTitle();
					return t1.compareTo(t2);
				}
			});
			aliasesList.removeAll();
			//noinspection ForLoopReplaceableByForEach
			for (int i = 0; i < components.length; i++) {
				aliasesList.add(components[i]);
			}
			aliasesList.revalidate();
			aliasesList.repaint();
		}
	}

	//////////////////////////
	// Active AliasListItem //
	//////////////////////////

	/**
	 * The active {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}.
	 *
	 * Most of actions such as copying and removing are performed on the active
	 * {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}.
	 */
	private AliasListItem activeItem;

	/**
	 * Returns a selected {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 *
	 * @return A selected {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 */
	@SuppressWarnings({"WeakerAccess"})
	AliasListItem getActiveItem() {
		return activeItem;
	}

	/**
	 * Sets the active {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 * and performs appropriate ui changes.
	 *
	 * @param item Sets the active {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 */
	@SuppressWarnings({"WeakerAccess"})
	void setActiveItem(AliasListItem item) {
		if (activeItem != null) {
			activeItem.setSelected(false);
		}
		activeItem = item;
		if (item != null) {
			item.setSelected(true);
		}
		editorPane.removeAll();
		if (item != null) {
			editorPane.add(item.getEditor());
		} else {
			editorPane.add(Box.createRigidArea(new Dimension(350, 400)), "Center");
		}
		editorPane.revalidate();
		editorPane.repaint();
	}

	///////////////////
	// AliasListItem //
	///////////////////

	/**
	 * The list of the {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}s of this
	 * {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog}.
	 *
	 * Every of this items is associated with appropriate
	 * {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}.
	 *
	 * @see net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem
	 * @see net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor
	 * @see #addAliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem)
	 * @see #removeAliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem)
	 * @see #removeAllAliasListItems()
	 */
	private JComponent aliasesList;

	/**
	 * Carefully adds {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 * to the list, so ui is still valid.
	 *
	 * @param item An {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 * to be added to the items list.
	 *
	 * @see #removeAliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem)
	 * @see #removeAllAliasListItems() 
	 */
	@SuppressWarnings({"WeakerAccess"})
	void addAliasListItem(AliasListItem item) {
		assert (item != null): "Trying to add null-pointer to items list";
		assert (aliasesList != null): "User interface has not been properly initialized";
		aliasesList.add(item);
		aliasesList.revalidate();
		aliasesList.repaint();
	}

	/**
	 * Carefully removes {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 * from the aliases list, so ui is still valid.
	 *
	 * @param item An {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}
	 * to be removed from aliases list.
	 *
	 * @see #addAliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem)
	 * @see #removeAllAliasListItems()
	 */
	@SuppressWarnings({"WeakerAccess"})
	void removeAliasListItem(AliasListItem item) {
		assert (item != null): "Trying to add null-pointer to items list";
		assert (aliasesList != null): "User interface has not been properly initialized";
		aliasesList.remove(item);
		aliasesList.revalidate();
		aliasesList.repaint();
	}

	/**
	 * Removes all {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}s
	 * from the aliases list, so ui is valid and no aliases left in the list.
	 *
	 * @see #addAliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem)
	 * @see #removeAliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem)
	 */
	void removeAllAliasListItems() {
		aliasesList.removeAll();
		aliasesList.revalidate();
		aliasesList.repaint();
	}

	/**
	 * The instance of this class represents a editing database alias in the
	 * right list of the aliases.
	 *
	 * Every {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem} has the link to the
	 * associated {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}
	 * which must be not null.
	 */
	public final class AliasListItem extends Box {

		/**
		 * The {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor} associated with this
		 * {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}. Must be not-null.
		 */
		private final DatabaseAliasEditor editor;

		/**
		 * A label with the title of this {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}.
		 */
		private JLabel title;

		private boolean selected;
		private final AliasListItem self = this;

		/**
		 * Constructs the {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem} with the empty
		 * {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor} associated with it.
		 *
		 * @see net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem#AliasListItem(net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor)
		 */
		public AliasListItem() {
			this(null);
		}

		/**
		 * Constructs the {@link AliasListItem} for the specified
		 * {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}.
		 *
		 * If the {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor} is {@code null}
		 * then a new one will be created.
		 *
		 * @param editor A {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor} to be
		 * associated with this {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.AliasListItem}. If the
		 * editor is {@code null} then a new one will be created.
		 */
		public AliasListItem(DatabaseAliasEditor editor) {
			super(BoxLayout.X_AXIS);
			if (editor == null) {
				this.editor = new DatabaseAliasEditor();
			} else {
				this.editor = editor;
			}
			createUI();
		}

		private void createUI() {
			add(Box.createHorizontalStrut(2));
			title = new JLabel(editor.getTitle());
			add(title);
			add(Box.createHorizontalStrut(2));
			add(Box.createHorizontalGlue());
			addMouseListener(new MouseAdapter() {
				public void mousePressed(MouseEvent event) {
					AliasListItem active = getActiveItem();
					if (active != null) {
						active.setSelected(false);
						active.revalidate();
					}
					setActiveItem(self);
					setSelected(true);
					revalidate();
					repaint();
				}
			});
			addPadding(this, 2, 1, 2, 1);
			editor.setListItem(this);
			revalidate();
		}

		/**
		 * Returns a {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}
		 * associated with this aliases list item.
		 *
		 * @return A {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}
		 * associated with this aliases list item.
		 */
		public DatabaseAliasEditor getEditor() {
			return editor;
		}

		/**
		 * Returns whether this item is selected or not.
		 *
		 * @return {@code true} if this list item is selected and {@code false}
		 * if this item is not selected.
		 */
		public boolean isSelected() {
			return selected;
		}

		/**
		 * Sets the selection state of this list item and updates ui
		 * accordinately.
		 *
		 * @param flag The new value of the selection flag.
		 */
		// todo: extract ui changes to another method.
		public void setSelected(boolean flag) {
			selected = flag;
			if (isSelected()) {
				setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
			} else {
				setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
			}
			addPadding(this, 1, 0, 1, 0);
		}

		/**
		 * Revalidates user interface.
		 *
		 * This method revalidates some showing labels texts and calls default
		 * parent {@link javax.swing.Box#revalidate()} method.
		 */
		public void revalidate() {
			if (editor != null) {
				String str = editor.getTitle();
				if (!str.equals("")) {
					title.setText(editor.getTitle());
				} else {
					title.setText("<html><i color=\"gray\">Untitled Alias</i></html>");
				}
			}
			super.revalidate();
		}

		public AliasListItem duplicate() {
			return new AliasListItem(editor.duplicate());
		}
	}

	/////////////////////////
	// DatabaseAliasEditor //
	/////////////////////////

	/**
	 * This object is a editor panel for
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}.
	 */
	public final class DatabaseAliasEditor extends JPanel {

		/**
		 * The title of the editing
		 * {@link net.sf.jailer.aliases.database.DatabaseAlias}.
		 *
		 * @see #getTitle()
		 */
		private JTextField title;

		/**
		 * The url of the editing
		 * {@link net.sf.jailer.aliases.database.DatabaseAlias}
		 *
		 * @see #getUrl() 
		 */
		private JTextField url;

		/**
		 * A field for the database user.
		 */
		private final JTextField user = new JTextField(25);

		private final JPasswordField password = new JPasswordField(25);
		private final JTextField libraries = new JTextField(20);
		private final JTextField className = new JTextField(20);

		@SuppressWarnings({"FieldCanBeLocal"})
		private JComponent externalDriverPanel;

		@SuppressWarnings({"FieldCanBeLocal"})
		private JCheckBox useExternalDriver = new JCheckBox("Use External Driver");;

		private DatabaseAliasManagerDialog.AliasListItem listItem;

		DatabaseAliasEditor() {
			this(null);
		}

		DatabaseAliasEditor(DatabaseAlias alias) {
			super(new VFlowLayout());
			createUI();
			if (alias != null) {
				title.setText(alias.getName());
				url.setText(alias.getURL());
				user.setText(alias.getUser());
				password.setText(alias.getPassword());
				useExternalDriver.setSelected(alias.isUsingExternalDriver());
				externalDriverPanel.setVisible(useExternalDriver.isSelected());
				if (alias.isUsingExternalDriver()) {
					libraries.setText(alias.getExternalLibrariesAsString());
					className.setText(alias.getDriverClassName());
				}
			}
		}

		/**
		 * Returns the currently entered title for this
		 * {@link net.sf.jailer.aliases.database.DatabaseAlias}
		 *
		 * @return The current name.
		 */
		public String getTitle() {
			assert (title != null): "UI has not been initialized properly";
			return title.getText();
		}

		/**
		 * Returns the currently entered jdbc url of the database.
		 *
		 * The jdbc url consists of 3 parts - <protocol>:<subprotocol>:<subname>. The <protocol> is always "jdbc".
		 * The subprotocol is the string unique to each of the sql servers. Subname is the identifier of database.
		 * The format of this string depends only of driver which is used.
		 *
		 * The more documentation on jdbc url could be found on
		 * <a href="http://java.sun.com/j2se/1.3/docs/guide/jdbc/getstart/connection.html#997649">
		 * http://java.sun.com/j2se/1.3/docs/guide/jdbc/getstart/connection.html#997649</a>
		 *
		 * @return An entered url.
		 */
		public String getUrl() {
			assert (url != null): "UI has not been initialized properly";
			return url.getText();
		}

		/**
		 * Returns the entered user's name.
		 *
		 * @return The entered user name.
		 */
		public String getUser() {
			assert (url != null): "UI has not been initialized properly";
			return user.getText();
		}

		public void setListItem(DatabaseAliasManagerDialog.AliasListItem  listItem) {
			this.listItem = listItem;
		}

		/**
		 * Creates UI for the {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}
		 */
		private void createUI() {
			addMargin(this, 6, 6, 6, 6);
			add(createTitleField());
			add(createURLField());
			add(createUseExternalDriverCheckBox());
			add(externalDriverPanel = createExternalDriverPanel());
			addPadding(user, 2, 2, 2, 2);
			add(createTitledJComponent(user, "User:"));
			addPadding(password, 2, 2, 2, 2);
			add(createTitledJComponent(password, "Password:"));
			add(Box.createVerticalGlue());
		}

		private JComponent createTitleField() {
			title = new JTextField(25);
			addPadding(title, 2, 2, 2, 2);
			title.addKeyListener(new KeyAdapter() {
				public void keyReleased(KeyEvent e) {
					listItem.revalidate();
				}
			});
			return createTitledJComponent(title, "Title:");
		}

		private JComponent createURLField() {
			url = new JTextField(25);
			addPadding(url, 2, 2, 2, 2);
			return createTitledJComponent(url, "JDBC URL:");
		}

		private JComponent createUseExternalDriverCheckBox() {
			Box hbox = Box.createHorizontalBox();
			useExternalDriver.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					externalDriverPanel.setVisible(useExternalDriver.isSelected());
					if (getParent() != null) {
						getParent().validate();
					}
				}
			});
			hbox.add(useExternalDriver);
			hbox.add(Box.createHorizontalGlue());
			return hbox;
		}

		private JComponent createExternalDriverPanel() {
			JPanel panel = new JPanel(new GridLayout(2, 1, 6, 6));
			addPadding(panel, 2, 2, 2, 2);
			Border border = panel.getBorder();
			border = BorderFactory.createCompoundBorder(BorderFactory.createTitledBorder("External Driver"), border);
			panel.setBorder(border);
			panel.add(createLibrariesField());
			panel.add(createClassField());
			return panel;
		}

		private JComponent createLibrariesField() {
			JPanel panel = new JPanel(new VFlowLayout());
			Box hbox = Box.createHorizontalBox();
			hbox.add(new JLabel("Driver Libraries:"));
			hbox.add(Box.createHorizontalGlue());
			panel.add(hbox);
			panel.add(Box.createVerticalStrut(3));
			hbox = Box.createHorizontalBox();
			addPadding(libraries, 2, 2, 2, 2);
			hbox.add(libraries);
			hbox.add(Box.createHorizontalStrut(3));
			JButton selectLibrariesButton = new JButton("Select");
			hbox.add(selectLibrariesButton);
			panel.add(hbox);
			return panel;
		}

		private JComponent createClassField() {
			JPanel panel = new JPanel(new VFlowLayout());
			Box hbox = Box.createHorizontalBox();
			hbox.add(new JLabel("Class Name:"));
			hbox.add(Box.createHorizontalGlue());
			panel.add(hbox);
			panel.add(Box.createVerticalStrut(3));
			hbox = Box.createHorizontalBox();
			addPadding(className, 2, 2, 2, 2);
			hbox.add(className);
			hbox.add(Box.createHorizontalStrut(3));
			panel.add(hbox);
			return panel;
		}

		/**
		 * Duplicates this {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}.
		 *
		 * @return The copy of the {@link net.sf.jailer.ui.connections.DatabaseAliasManagerDialog.DatabaseAliasEditor}.
		 */
		public DatabaseAliasEditor duplicate() {
			DatabaseAliasEditor editor = new DatabaseAliasEditor();
			editor.title.setText(title.getText());
			editor.user.setText(user.getText());
			editor.password.setText(new String(password.getPassword()));
			return editor;
		}

	}


	//////////
	// Util //
	//////////

	private void addMargin(JComponent component, int top, int left, int bottom, int right) {
		Border border = component.getBorder();
		Border margin = BorderFactory.createEmptyBorder(top,left, bottom, right);
		component.setBorder(BorderFactory.createCompoundBorder(margin, border));
	}

	private void addPadding(JComponent component, int top, int left, int bottom, int right) {
		Border border = component.getBorder();
		Border padding = BorderFactory.createEmptyBorder(top,left, bottom, right);
		component.setBorder(BorderFactory.createCompoundBorder(border, padding));
	}

	private JComponent createTitledJComponent(JComponent component, String title) {
		Box box = Box.createVerticalBox();
		Box hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(6));
		hbox.add(new JLabel(title));
		hbox.add(Box.createHorizontalStrut(6));
		hbox.add(Box.createHorizontalGlue());
		box.add(hbox);
		box.add(Box.createVerticalStrut(3));
		hbox = Box.createHorizontalBox();
		hbox.add(component);
		box.add(hbox);
		return box;
	}
}
