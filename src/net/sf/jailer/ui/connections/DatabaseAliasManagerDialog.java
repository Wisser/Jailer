package net.sf.jailer.ui.connections;

import com.digitprop.tonic.TonicLookAndFeel;
import net.sf.jailer.aliases.database.DatabaseAlias;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.event.*;
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
		DatabaseAliasManagerDialog dialog = new DatabaseAliasManagerDialog(null);
		dialog.showDialog();
	}

	/**
	 * Shows the {@code DatabaseAliasManagerDialog}.
	 * Use this method for simple calls rather than construction.
	 *
	 * @param owner An parent frame for this dialog.
	 */
	public static void showDialog(Frame owner) {
		new DatabaseAliasManagerDialog(owner).showDialog();
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

	JPanel editorPane;
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

	private AliasListItem activeItem;

	public AliasListItem getActiveItem() {
		return activeItem;
	}

	public void setActiveItem(AliasListItem item) {
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

	private JComponent aliasesList;

	void addAliasListItem(AliasListItem item) {
		if (item == null) {
			return;
		}
		aliasesList.add(item);
		aliasesList.revalidate();
		aliasesList.repaint();
	}

	void removeAliasListItem(AliasListItem item) {
		if (item == null) {
			return;
		}
		aliasesList.remove(item);
		aliasesList.revalidate();
		aliasesList.repaint();
	}

	void removeAllAliasListItems() {
		aliasesList.removeAll();
		aliasesList.revalidate();
		aliasesList.repaint();
	}

	public final class AliasListItem extends Box {

		private DatabaseAliasEditor editor;
		private JLabel title;
		private boolean selected;
		private AliasListItem self = this;

		public AliasListItem(DatabaseAliasEditor editor) {
			super(BoxLayout.X_AXIS);
			if (editor == null) {
				this.editor = new DatabaseAliasEditor();
			} else {
				this.editor = editor;
			}
			this.title = new JLabel(this.editor.getTitle());
			add(Box.createHorizontalStrut(2));
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
			setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
			addPadding(this, 1, 0, 1, 0);
			this.editor.setListItem(this);
			revalidate();
		}

		public DatabaseAliasEditor getEditor() {
			return editor;
		}

		public boolean isSelected() {
			return selected;
		}

		public void setSelected(boolean flag) {
			selected = flag;
			if (isSelected()) {
				setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
			} else {
				setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
			}
			addPadding(this, 1, 0, 1, 0);
		}

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
	 * This object is editor panel for
	 * {@link net.sf.jailer.aliases.database.DatabaseAlias}.
	 */
	public final class DatabaseAliasEditor extends JPanel {

		/**
		 * The title of the editing
		 * {@link net.sf.jailer.aliases.database.DatabaseAlias}.
		 *
		 * @see #getTitle()
		 */
		private JTextField title = new JTextField(25);

		/**
		 * The url of the editing
		 * {@link net.sf.jailer.aliases.database.DatabaseAlias}
		 *
		 * @see #getUrl();
		 */
		private JTextField url = new JTextField(25);

		/**
		 * A field for the database user.
		 */
		private JTextField user = new JTextField(25);

		private JPasswordField password = new JPasswordField(25);
		private JTextField libraries = new JTextField(20);
		private JTextField className = new JTextField(20);

		@SuppressWarnings({"FieldCanBeLocal"})
		private JComponent externalDriverPanel;

		@SuppressWarnings({"FieldCanBeLocal"})
		private JCheckBox useExternalDriver;

		private DatabaseAliasManagerDialog.AliasListItem listItem;

		DatabaseAliasEditor() {
			this(null);
		}

		DatabaseAliasEditor(DatabaseAlias alias) {
			super(new VFlowLayout());
			createUI();
			if (alias != null) {
				title.setText(alias.getUser() + "@" + alias.getURL());
				url.setText(alias.getURL());
				user.setText(alias.getUser());
				password.setText(alias.getPassword());
				// todo: implement some DA methods
//				useExternalDriver.setSelected(alias.isUseExternalDriver());
//				if (alias.isUseExternalDriver()) {
//					libraries.setText(alias.getExternalLibrariesAsString());
//					libraries.setText(alias.getDriverClassName());
//				}
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
		 * The jdbc url consists of 3 parts -
		 * <protocol>:<subprotocol>:<subname>.
		 * The <protocol> is always "jdbc". The subprotocol is the string unique
		 * to each of the sql servers.
		 * Subname is the identifier of database. The format of this string
		 * depends only of driver which is used.
		 *
		 * The more documentation on jdbc url could be found on
		 * <a href="http://java.sun.com/j2se/1.3/docs/guide/jdbc/getstart/connection.html#997649">
		 * http://java.sun.com/j2se/1.3/docs/guide/jdbc/getstart/connection.html#997649
		 * </a>
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

		private void createUI() {
			addMargin(this, 6, 6, 6, 6);
			addPadding(title, 2, 2, 2, 2);
			add(createTitledJComponent(title, "Title:"));
			title.addKeyListener(new KeyAdapter() {
				public void keyReleased(KeyEvent e) {
					listItem.revalidate();
				}
			});
			addPadding(url, 2, 2, 2, 2);
			add(createTitledJComponent(url, "JDBC URL:"));
			Box hbox = Box.createHorizontalBox();
			useExternalDriver = new JCheckBox("Use External Driver", false);
			hbox.add(useExternalDriver);
			hbox.add(Box.createHorizontalGlue());
			add(hbox);
			externalDriverPanel = createExternalDriverPanel();
			add(externalDriverPanel);
			addPadding(user, 2, 2, 2, 2);
			add(createTitledJComponent(user, "User:"));
			addPadding(password, 2, 2, 2, 2);
			add(createTitledJComponent(password, "Password:"));
			add(Box.createVerticalGlue());
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
