package net.sf.jailer.ui;

import javax.swing.*;

/**
 * The export wizard panel.
 *
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 11.02.2008
 * @time: 0:28:46
 */
public class ExportWizard extends JFrame {

	public static void main(String argv[]) {
		new ExportWizard();
	}

	public ExportWizard() {
		super();
		getContentPane().add(initializeGUI());
		setTitle("Export Wizard");
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		pack();
		setVisible(true);
		setResizable(false);
	}

	private JComponent initializeGUI() {
		Box box = Box.createVerticalBox();
		box.setBorder(BorderFactory.createEmptyBorder(6, 6, 6, 6));
		box.add(Box.createHorizontalStrut(320));
		box.add(createFromPanel());
		box.add(createToPanel());
		return box;
	}

	private JComponent createFromPanel() {
		Box box = Box.createVerticalBox();
		box.setBorder(BorderFactory.createCompoundBorder(
					BorderFactory.createEmptyBorder(0, 0, 6, 0),
					BorderFactory.createTitledBorder("Import From:")));
			box.add(new JLabel("Import from file (.sql or .csv)"));
			box.add(new JLabel("Import from active data"));
		return box;
	}

	private JComponent createToPanel() {
		Box box = Box.createHorizontalBox();
		box.setBorder(BorderFactory.createCompoundBorder(
					BorderFactory.createEmptyBorder(6, 0, 0, 0),
					BorderFactory.createTitledBorder("Import To:")
		));
			JLabel label = new JLabel("Connecton:");
			label.setBorder(BorderFactory.createCompoundBorder(
						BorderFactory.createEmptyBorder(6, 6, 6, 6),
						label.getBorder()
			));
			box.add(label);
			JComboBox comboBox = new JComboBox();
			comboBox.setBorder(BorderFactory.createCompoundBorder(
						BorderFactory.createEmptyBorder(6, 0, 6, 6),
						comboBox.getBorder()
			));
				comboBox.addItem("MySQL");
				comboBox.addItem("-- establish new connection --");
			box.add(comboBox);
		return box;
	}
}
