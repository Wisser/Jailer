/*
 * Copyright 2007 - 2025 Ralf Wisser.
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

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.util.PrintUtil;

/**
 * Renders text files.
 * 
 * @author Ralf Wisser
 */
public class FileView extends javax.swing.JFrame {

	/**
	 * Component for rendering the file content.
	 */
	private JTextArea jTextPane = null;
	
	/**
	 * Scroll-pane around content.
	 */
	private JScrollPane jScrollPane = null;
	
	/**
	 * Constructor. 
	 * 
	 * @param owner the enclosing component.
	 */
	public FileView(Window owner) {
		this.owner = owner;
		this.withSyntaxHighlighting = false;
		this.isXml = false;
		this.isJson = false;
		this.isYaml = false;
		initialize();
	}

	/**
	 * Constructor. 
	 * 
	 * @param owner the enclosing component.
	 * @param file the file to render
	 */
	public FileView(Window owner, Window window, String file, boolean withSyntaxHighlighting) throws FileNotFoundException, IOException {
		this.owner = owner;
		this.withSyntaxHighlighting = withSyntaxHighlighting;
		this.isXml = file.toLowerCase().endsWith(".xml");
		this.isJson = file.toLowerCase().endsWith(".json");
		this.isYaml = file.toLowerCase().endsWith(".yaml") || file.toLowerCase().endsWith(".yml");
		
		File f = new File(file);
		if (f.exists() && f.length() > 65L*1024L*1024L / (file.toLowerCase().endsWith(".zip") || file.toLowerCase().endsWith(".gz")? 5 : 1)) {
			int o = JOptionPane.showOptionDialog(window, "File " + f.getAbsolutePath() + "\nis large (" + (f.length() / 1024 / 1024) + " MB). Loading might fail.", "File is large", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, null, new Object[] { "Open", "Cancel" }, "Open");
			if (o != 0) {
				dispose();
				return;
			}
		}
		try {
			initialize();
			setTitle(f.getAbsolutePath());
			getJTextPane().setText(new PrintUtil().loadFile(f.getPath()));
			if (getJTextPane() instanceof RSyntaxTextAreaWithSQLSyntaxStyle) {
				((RSyntaxTextAreaWithSQLSyntaxStyle) getJTextPane()).discardAllEdits();
			}
			getJTextPane().setCaretPosition(0);
			setVisible(true);
		} catch (OutOfMemoryError oome) {
			JOptionPane.showMessageDialog(window, "File " + f.getAbsolutePath() + "\nis too large to load.", "Out of Memory", JOptionPane.ERROR_MESSAGE);
			dispose();
		} catch (Throwable t) {
			UIUtil.showException(owner, "Error", t);
		}
	}

	private final boolean withSyntaxHighlighting;
	private final boolean isXml;
	private final boolean isJson;
	private final boolean isYaml;
	private final Window owner;
	private static int gCount = 0;
	
	/**
	 * Creates components.
	 */
	private void initialize() {
		this.setSize(new Dimension(800, 1000));
		
		JLabel info = new JLabel("");
		
		JPanel panel = new JPanel(new GridBagLayout());
		GridBagConstraints gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridwidth = 3;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.weighty = 1;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		panel.add(getJScrollPane(), gridBagConstraints);
		
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.insets = new Insets(4, 2, 4, 8);
		JButton copy = new JButton("Copy to Clipboard");
		copy.setIcon(copyIcon);
		copy.addActionListener(e -> {
			String text = getJTextPane().getText();
			UIUtil.setClipboardContent(new StringSelection(text));
			info.setText("Copied");
		});
		panel.add(copy, gridBagConstraints);
		
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 2;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 1;
		gridBagConstraints.insets = new Insets(4, 2, 4, 8);
		panel.add(info, gridBagConstraints);
		
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 3;
		gridBagConstraints.gridy = 2;
		gridBagConstraints.insets = new Insets(4, 2, 4, 2);
		JButton close = new JButton("Close");
		close.addActionListener(e -> { setVisible(false); dispose(); });
		close.setIcon(UIUtil.scaleIcon(close, cancelIcon));
		panel.add(close, gridBagConstraints);
		
		this.setContentPane(panel);
		this.setTitle("Jailer");
		int offset = (++gCount % 6) * 32;
		UIUtil.setInitialWindowLocation(this, owner, 32 + offset, 32 + offset);
		try {
			setIconImage(UIUtil.readImage("/jailer.png").getImage());
		} catch (Throwable t) {
			// ignore
		}
		
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				setVisible(false);
				dispose();
			}
			@Override
			public void windowClosed(WindowEvent e) {
				JTextArea textPane = getJTextPane();
				textPane.setText("");
				if (textPane instanceof RSyntaxTextAreaWithSQLSyntaxStyle) {
					((RSyntaxTextAreaWithSQLSyntaxStyle) textPane).discardAllEdits();
					((RSyntaxTextAreaWithSQLSyntaxStyle) textPane).setDocument(new RSyntaxDocument(null, SyntaxConstants.SYNTAX_STYLE_NONE)); // prevent memory leak
				}
				
				UIUtil.checkTermination();
				Container parent = textPane.getParent();
				if (parent != null) {
					parent.remove(textPane);
				}
			}
		});
	}

	/**
	 * This method initializes jScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane() {
		if (jScrollPane == null) {
			jScrollPane = new JScrollPane();
			jScrollPane.setViewportView(getJTextPane());
		}
		return jScrollPane;
	}

	/**
	 * This method initializes jTextPane	
	 * 	
	 * @return javax.swing.JTextPane	
	 */
	private JTextArea getJTextPane() {
		if (jTextPane == null) {
			RSyntaxTextAreaWithSQLSyntaxStyle rSyntaxTextAreaWithSQLSyntaxStyle = new RSyntaxTextAreaWithSQLSyntaxStyle(
					false, false) {
				protected boolean withModifingMenuItems() {
					return false;
				}
			};
			if (withSyntaxHighlighting) {
				if (isXml) {
					rSyntaxTextAreaWithSQLSyntaxStyle.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_XML);
				}
				if (isJson) {
					rSyntaxTextAreaWithSQLSyntaxStyle.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_JSON_WITH_COMMENTS);
				}
				if (isYaml) {
					rSyntaxTextAreaWithSQLSyntaxStyle.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_YAML);
				}
			} else {
				rSyntaxTextAreaWithSQLSyntaxStyle.setSyntaxEditingStyle(null);
			}
			jTextPane = rSyntaxTextAreaWithSQLSyntaxStyle;
			jTextPane.setEditable(false);
		}
		return jTextPane;
	}

	private static ImageIcon cancelIcon;
	private static ImageIcon copyIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
        copyIcon = UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png"));
	}
	
	private static final long serialVersionUID = -8991473715127153931L;
}  //  @jve:decl-index=0:visual-constraint="10,10"
