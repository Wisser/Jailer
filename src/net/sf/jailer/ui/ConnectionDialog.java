package net.sf.jailer.ui;

import net.sf.jailer.aliases.*;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.sql.SQLException;

/**
 * {@code ConnectionDialog} performs a way to create
 * {@link net.sf.jailer.aliases.DatabaseAlias} instance and start work with a
 * database.
 *
 * This dialog has 3 obligatory and 2 unobligatory fields. Here is their list:
 * <ul>
 * <li><b>JDBC URL</b> - [required] A java database connection universal resource locator
 * represents a place of a database to which a user is going to connect.</li>
 * <li><b>User</b> - [required] A user name for server access.</li>
 * <li><b>Password</b> - [required] A password for server access.</li>
 * <li><b>External library jarss</b> - [optional] A list of the {@code .jar}
 * files where the driver for the specified url should be looking.</li>
 * <li><b>Class name</b> - [optional] A name of the {@link java.sql.Driver}
 * implementation which instance will be used to connection establishing.
 *
 * For receiving a DatabaseAlias via this dialog just call
 * {@link net.sf.jailer.ui.ConnectionDialog#showDialog(javax.swing.JFrame)}
 * method.
 *
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 29.02.2008
 * @time: 21:27:25
 */
public final class ConnectionDialog extends JDialog {

	/////////////////
	// Call method //
	/////////////////

	/**
	 * Returns a {@link net.sf.jailer.aliases.DatabaseAlias} which properties
	 * have been entered by user.
	 *
	 * The dialog checks properties by getting a {@link java.sql.Driver}
	 * instance and establishing a {@link java.sql.Connection}. Only a valid
	 * {@link net.sf.jailer.aliases.DatabaseAlias} could be returned.
	 *
	 * @param owner An owner {@link javax.swing.JFrame}.
	 * @return {@link net.sf.jailer.aliases.DatabaseAlias} instance or the
	 * {@code null} if the human decided to dismiss this dialog.
	 */
	public static DatabaseAlias showDialog(JFrame owner) {
		return new ConnectionDialog(owner).getDatabaseAlias();
	}

	///////////////////
	// DatabaseAlias //
	///////////////////

	private static DatabaseAlias myLastConnected = null;
	private DatabaseAlias myAlias = null;

	private DatabaseAlias getDatabaseAlias() {
		return myAlias;
	}

	private DatabaseAlias generateDatabaseAlias()
	throws DriverNotFoundException {
		String url = myJDBCURLField.getText();
		String user = myUserField.getText();
		String password = new String(myPasswordField.getPassword());
		if (password.isEmpty()) {
			password = null;
		}
		if (myUseExternalDriverCheckBox.isSelected()) {
			String jars[] = myJarsListField.getText().trim().split(":");
			String className = myDriverClassField.getText();
			JDBCDriverManager.setDriver(JDBCUtil.getSubprotocol(url), jars, className);
			return new DatabaseAliasExternal(url, user, password, jars, className);
		} else {
			return new DatabaseAlias(url, user, password, JDBCDriverManager.getDriverForURL(url));
		}
	}

	/////////////////////
	// GUI Form fields //
	/////////////////////

	private JTextField myJDBCURLField;
	private JTextField myUserField;
	private JPasswordField myPasswordField;
	private JCheckBox myUseExternalDriverCheckBox;
	private JTextField myJarsListField;
	private JTextField myDriverClassField;

	/////////////////
	// Constructor //
	/////////////////
	
	private ConnectionDialog(JFrame owner) {
		super(owner, "Database Connection", true);
		getContentPane().add(createUI());
		addKeyListener(new DialogExitAction(this));
		setAlwaysOnTop(true);
		setResizable(false);
		pack();
		genLocation();
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		setVisible(true);
		if (myLastConnected != null) {
			myJDBCURLField.setText(myLastConnected.getURL());
			myUserField.setText(myLastConnected.getUser());
			myPasswordField.setText(myLastConnected.getPassword());
			if (myLastConnected instanceof DatabaseAliasExternal) {
				myUseExternalDriverCheckBox.setSelected(true);
				myJarsListField.setText(((DatabaseAliasExternal)myLastConnected).getJars());
				myDriverClassField.setText(((DatabaseAliasExternal)myLastConnected).getClassName());
			}
		}
	}

	private void genLocation() {
		int x = (getParent().getWidth() - getWidth()) / 2;
		int y = (getParent().getHeight() - getHeight()) / 2;
		setLocation(x, y);
	}
	private Box createUI() {
		Box box = Box.createVerticalBox();
		box.setBorder(BorderFactory.createEmptyBorder(6, 6, 6, 6));
		box.add(createJDBCURLInputField());
		box.add(Box.createVerticalStrut(6));
		box.add(createUserInputField());
		box.add(Box.createVerticalStrut(6));
		box.add(createPasswordInputField());
		box.add(Box.createVerticalStrut(12));
			Box hbox = Box.createHorizontalBox();
			myUseExternalDriverCheckBox = new JCheckBox("Use non-default driver", false);
			myUseExternalDriverCheckBox.setFocusable(myLastConnected == null || !(myLastConnected instanceof DatabaseAliasExternal));
			hbox.add(myUseExternalDriverCheckBox);
			hbox.add(Box.createHorizontalGlue());
			box.add(hbox);
		box.add(Box.createVerticalStrut(6));
		Box driverSelectionBox = createDriverSelectionBox();
		myUseExternalDriverCheckBox.addChangeListener(new ShowAndHideAction(myUseExternalDriverCheckBox, driverSelectionBox));
		driverSelectionBox.setVisible(false);
		box.add(driverSelectionBox);
		box.add(Box.createVerticalStrut(12));
		box.add(createButtonsBox());
		return box;
	}

	private Box createJDBCURLInputField() {
		Box box = Box.createVerticalBox();
		Box hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(6));
		JLabel label = new JLabel("Jdbc url:");
		hbox.add(label);
		hbox.add(Box.createHorizontalGlue());
		box.add(hbox);
		box.add(Box.createVerticalStrut(3));
		hbox = Box.createHorizontalBox();
		myJDBCURLField = new JTextField(myLastConnected == null ? "" : myLastConnected.getURL());
		myJDBCURLField.setBorder(BorderFactory.createCompoundBorder(myJDBCURLField.getBorder(), BorderFactory.createEmptyBorder(2, 2, 2, 2)));
		myJDBCURLField.addKeyListener(new DialogExitAction(this));
		myJDBCURLField.addKeyListener(new DialogConnectAction(this));
		hbox.add(myJDBCURLField);
//		hbox.add(Box.createHorizontalStrut(4));
//		JButton helpButton = new JButton(new ImageIcon("icons/help.png"));
//		helpButton.setFocusable(false);
//		helpButton.setBorder(null);
//		hbox.add(helpButton);
//		hbox.add(Box.createHorizontalStrut(2));
		box.add(hbox);
		return box;
	}

	private Box createUserInputField() {
		Box box = Box.createVerticalBox();
		Box hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(6));
		JLabel label = new JLabel("User:");
		hbox.add(label);
		hbox.add(Box.createHorizontalGlue());
		box.add(hbox);
		box.add(Box.createVerticalStrut(3));
		hbox = Box.createHorizontalBox();
		myUserField = new JTextField(myLastConnected == null ? "" : myLastConnected.getUser(), 25);
		myUserField.setBorder(BorderFactory.createCompoundBorder(myUserField.getBorder(), BorderFactory.createEmptyBorder(2, 2, 2, 2)));
		myUserField.addKeyListener(new DialogExitAction(this));
		myUserField.addKeyListener(new DialogConnectAction(this));
		hbox.add(myUserField);
		box.add(hbox);
		return box;
	}

	private Box createPasswordInputField() {
		Box box = Box.createVerticalBox();
		Box hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(6));
		JLabel label = new JLabel("Password:");
		hbox.add(label);
		hbox.add(Box.createHorizontalGlue());
		box.add(hbox);
		box.add(Box.createVerticalStrut(3));
		hbox = Box.createHorizontalBox();
		myPasswordField = new JPasswordField(myLastConnected == null ? "" : myLastConnected.getPassword(), 25);
		myPasswordField.setBorder(BorderFactory.createCompoundBorder(myPasswordField.getBorder(), BorderFactory.createEmptyBorder(2, 2, 2, 2)));
		myPasswordField.addKeyListener(new DialogExitAction(this));
		myPasswordField.addKeyListener(new DialogConnectAction(this));
		hbox.add(myPasswordField);
		box.add(hbox);
		return box;
	}

	private Box createDriverSelectionBox() {
		Box box = Box.createVerticalBox();
		box.setBorder(BorderFactory.createTitledBorder("External driver library"));
		box.add(Box.createVerticalStrut(3));
		box.add(createJarsSelectionBox());
		box.add(Box.createVerticalStrut(6));
		box.add(createClassSelectionBox());
		box.add(Box.createVerticalStrut(3));
		return box;
	}

	private Box createJarsSelectionBox() {
		Box box = Box.createVerticalBox();
		Box hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(8));
		JLabel label = new JLabel("Select driver library jar(s):");
		hbox.add(label);
		hbox.add(Box.createHorizontalGlue());
		box.add(hbox);
		box.add(Box.createVerticalStrut(3));
		hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(6));
		myJarsListField = new JTextField();
		if (myLastConnected == null || !(myLastConnected instanceof DatabaseAliasExternal)) {
			myJarsListField.setText("");
		} else {
			myJarsListField.setText(((DatabaseAliasExternal)myLastConnected).getJars());
		}
		myJarsListField.setBorder(BorderFactory.createCompoundBorder(myJarsListField.getBorder(), BorderFactory.createEmptyBorder(2, 2, 2, 2)));
		myJarsListField.addKeyListener(new DialogExitAction(this));
		myJarsListField.addKeyListener(new DialogConnectAction(this));
		hbox.add(myJarsListField);
//		hbox.add(Box.createHorizontalStrut(3));
//		JButton selectJarsButton = new JButton("Select");
//		selectJarsButton.setEnabled(false);
//		selectJarsButton.setFocusable(false);
//		hbox.add(selectJarsButton);
		hbox.add(Box.createHorizontalStrut(6));
		box.add(hbox);
		return box;
	}

	private Box createClassSelectionBox() {
		Box box = Box.createVerticalBox();
		Box hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(8));
		JLabel label = new JLabel("Select driver:");
		hbox.add(label);
		hbox.add(Box.createHorizontalGlue());
		box.add(hbox);
		box.add(Box.createVerticalStrut(3));
		hbox = Box.createHorizontalBox();
		hbox.add(Box.createHorizontalStrut(6));
		myDriverClassField = new JTextField();
		if (myLastConnected == null || !(myLastConnected instanceof DatabaseAliasExternal)) {
			myDriverClassField.setText("");
		} else {
			myDriverClassField.setText(((DatabaseAliasExternal)myLastConnected).getClassName());
		}
		myDriverClassField.setBorder(BorderFactory.createCompoundBorder(myDriverClassField.getBorder(), BorderFactory.createEmptyBorder(2, 2, 2, 2)));
		myDriverClassField.addKeyListener(new DialogExitAction(this));
		myDriverClassField.addKeyListener(new DialogConnectAction(this));
		hbox.add(myDriverClassField);
//		hbox.add(Box.createHorizontalStrut(3));
//		JButton selectClassButton = new JButton("Select");
//		selectClassButton.setEnabled(false);
//		selectClassButton.setFocusable(false);
//		hbox.add(selectClassButton);
		hbox.add(Box.createHorizontalStrut(6));
		box.add(hbox);
		return box;
	}

	private Box createButtonsBox() {
		Box box = Box.createHorizontalBox();
		box.add(Box.createHorizontalGlue());
		JButton myConnectButton = new JButton("Connect");
		myConnectButton.setFocusable(false);
		myConnectButton.addActionListener(new DialogConnectAction(this));
		box.add(myConnectButton);
		box.add(Box.createHorizontalStrut(6));
		JButton myCancelButton = new JButton("Cancel");
		myCancelButton.setFocusable(false);
		myCancelButton.addActionListener(new DialogExitAction(this));
		box.add(myCancelButton);
		return box;
	}

	///////////////
	// Listeners //
	///////////////

	long lastActionTime = 0;
	long minActionTime = 200;
	
	private final class DialogExitAction implements ActionListener, KeyListener {
		private ConnectionDialog myOwner;
		public DialogExitAction(ConnectionDialog owner) {myOwner = owner;}
		public void keyTyped(KeyEvent e) {}
		public void keyPressed(KeyEvent e) {if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {execute();}}
		public void keyReleased(KeyEvent e) {}
		public void actionPerformed(ActionEvent e) {execute();}
		private void execute() {
			if (System.currentTimeMillis() - lastActionTime < minActionTime) return;
			lastActionTime = System.currentTimeMillis();
			myAlias = null;
			myOwner.dispose();
		}
	}


	private final class ShowAndHideAction implements ChangeListener {
		private JCheckBox myFlag;
		private JComponent myComponent;
		public ShowAndHideAction(JCheckBox flag, JComponent component) {myFlag = flag; myComponent = component;}
		public void stateChanged(ChangeEvent e) {
			myComponent.setVisible(myFlag.isSelected());
			pack();
		}
	}


	private final class DialogConnectAction implements ActionListener, KeyListener {
		private ConnectionDialog myOwner;
		public DialogConnectAction(ConnectionDialog owner) {myOwner = owner;}
		public void keyTyped(KeyEvent e) {}
		public void keyPressed(KeyEvent e) {if (e.getKeyCode() == KeyEvent.VK_ENTER) {execute();}}
		public void keyReleased(KeyEvent e) {}
		public void actionPerformed(ActionEvent e) {execute();}
		private void execute() {
			if (System.currentTimeMillis() - lastActionTime < minActionTime) return;
			lastActionTime = System.currentTimeMillis();
			try {
				myAlias = generateDatabaseAlias();
			} catch (Exception exception) {
				myAlias = null;
				JOptionPane.showMessageDialog(myOwner, "Could not find a driver for the specified url");
				return;
			}
			try {
				myAlias.getConnection();
			} catch (SQLException sqlException) {
				myAlias = null;
				JOptionPane.showMessageDialog(myOwner, "Could not establish a test connection");
				return;
			}
			//noinspection EmptyCatchBlock
			try {
				myLastConnected = generateDatabaseAlias();
			} catch (Exception e) {}
			myOwner.dispose();
		}
	}
	
}
