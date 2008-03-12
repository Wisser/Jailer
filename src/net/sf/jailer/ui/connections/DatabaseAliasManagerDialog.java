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
			JFrame.setDefaultLookAndFeelDecorated(true);
		} catch (UnsupportedLookAndFeelException exception) {
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
	 * @author Vladimir "Dair T'arg" Berkutov
	 * @date: 04.03.2008
	 * @time: 22:43:33
	 */
	public final class DatabaseAliasEditor extends JPanel {

		private JTextField title = new JTextField(25);
		private JComboBox serversList = new JComboBox(getServerList());

		private JTextField user = new JTextField(25);
		private JPasswordField password = new JPasswordField(25);

		private DatabaseAliasManagerDialog.AliasListItem listItem;

		DatabaseAliasEditor() {
			this(null);
		}

		DatabaseAliasEditor(DatabaseAlias alias) {
			super(new VFlowLayout());
			createUI();
		}

		public String getTitle() {
			return title.getText();
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
			addPadding(serversList, 2, 2, 2, 2);
			add(createTitledJComponent(serversList, "Server"));
			addPadding(user, 2, 2, 2, 2);
			add(createTitledJComponent(user, "User:"));
			addPadding(password, 2, 2, 2, 2);
			add(createTitledJComponent(password, "Password:"));
			add(Box.createVerticalGlue());
		}

		private String[] getServerList() {
			return new String[] {
				"MySQL",
				"Custom SQL Server"
			};
		}

		public DatabaseAliasEditor duplicate() {
			DatabaseAliasEditor editor = new DatabaseAliasEditor();
			editor.title.setText(title.getText());
			editor.serversList.setSelectedIndex(serversList.getSelectedIndex());
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
