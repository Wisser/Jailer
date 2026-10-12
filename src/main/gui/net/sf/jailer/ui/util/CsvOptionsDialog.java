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
package net.sf.jailer.ui.util;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ItemEvent;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.jailer.ui.UIUtil;

/**
 * Lets the user choose the format of a CSV file to be written: a dialect (Excel, RFC 4180, Unix, TSV) or each setting.
 * The settings are kept in the {@link UISettings}.
 *
 * @author Ralf Wisser
 */
@SuppressWarnings("serial")
public class CsvOptionsDialog extends JDialog {

	/**
	 * The format of a CSV file.
	 */
	public static final class CsvFormat {
		/**
		 * The separator of the cells.
		 */
		public final String separator;

		/**
		 * The decimal separator of numbers.
		 */
		public final char decimalSeparator;

		/**
		 * The encoding.
		 */
		public final Charset charset;

		/**
		 * Write a byte order mark at the beginning?
		 */
		public final boolean bom;

		/**
		 * The line break.
		 */
		public final String lineBreak;

		/**
		 * With a header row?
		 */
		public final boolean header;

		/**
		 * Quote all text cells (else only those that need it)?
		 */
		public final boolean quoteAll;

		CsvFormat(String separator, char decimalSeparator, Charset charset, boolean bom, String lineBreak, boolean header, boolean quoteAll) {
			this.separator = separator;
			this.decimalSeparator = decimalSeparator;
			this.charset = charset;
			this.bom = bom;
			this.lineBreak = lineBreak;
			this.header = header;
			this.quoteAll = quoteAll;
		}

		/**
		 * Gets the content of a cell, quoted if it contains the separator, quotes or line breaks (RFC 4180)
		 * or if all text cells are quoted.
		 *
		 * @param content the content
		 * @param isText is it a text (not a number)?
		 */
		public String cell(String content, boolean isText) {
			if (isText && quoteAll && !content.isEmpty()
					|| content.contains(separator) || content.indexOf('"') >= 0 || content.indexOf('\n') >= 0 || content.indexOf('\r') >= 0) {
				return "\"" + content.replace("\"", "\"\"") + "\"";
			}
			return content;
		}

		/**
		 * Gets a number as text, without grouping and with the decimal separator.
		 */
		public String number(BigDecimal number) {
			return number.toPlainString().replace('.', decimalSeparator);
		}

		/**
		 * Gets the cell of a value (as read from the database): numbers without grouping and with the decimal separator,
		 * timestamps without fractional seconds of zero, <code>null</code> as empty cell.
		 */
		public String value(Object value) {
			if (value == null || value == UIUtil.NULL) {
				return "";
			}
			if (value instanceof Double || value instanceof Float) {
				double d = ((Number) value).doubleValue();
				if (Double.isNaN(d) || Double.isInfinite(d)) {
					return cell(String.valueOf(d), true);
				}
				value = new BigDecimal(value.toString());
			}
			if (value instanceof BigDecimal) {
				return cell(number((BigDecimal) value), false);
			}
			if (value instanceof Number) {
				return cell(value.toString(), false);
			}
			if (value instanceof Timestamp) {
				String s = value.toString();
				return cell(s.endsWith(".0") ? s.substring(0, s.length() - 2) : s, true);
			}
			return cell(value.toString(), true);
		}

		/**
		 * Writes a CSV file (with the encoding and, if wanted, a byte order mark).
		 *
		 * @param fileName the file
		 * @param content the content
		 */
		public void write(String fileName, String content) throws IOException {
			try (Writer out = new OutputStreamWriter(new FileOutputStream(fileName), charset)) {
				if (bom) {
					out.write((char) 0xFEFF);
				}
				out.write(content);
			}
		}
	}

	/**
	 * Lets the user choose the CSV file to be written (".csv" is appended if needed).
	 *
	 * @param parent the parent
	 * @return the file name, or <code>null</code> if cancelled
	 */
	public static String chooseFile(Component parent) {
		String fileName = UIUtil.choseFile(null, ".", "CSV File", ".csv", parent, true, false);
		if (fileName != null && !fileName.toLowerCase(Locale.ENGLISH).endsWith(".csv")) {
			fileName += ".csv";
		}
		return fileName;
	}

	private static final String SETTINGS_KEY = "csvExportOptions";
	private static final String TAB = "Tab";
	private static final char LOCALE_DECIMAL_SEPARATOR = DecimalFormatSymbols.getInstance().getDecimalSeparator();
	private static final Charset WINDOWS_1252 = windows1252();

	private static final int EXCEL = 0;
	private static final int RFC_4180 = 1;
	private static final int UNIX = 2;
	private static final int TSV = 3;
	private static final int CUSTOM = 4;
	private static final String COUNTRY = country();
	private static final String EXCEL_SEPARATOR = LOCALE_DECIMAL_SEPARATOR == ',' ? ";" : ",";
	private static final String[] DIALECTS = {
			"Excel (" + COUNTRY + ")",
			"RFC 4180",
			"Unix / LibreOffice",
			"Tab-separated (TSV)",
			"Custom" };
	private static final String[] DIALECT_HINTS = {
			"Excel uses the separator and decimal separator of the regional settings of Windows (here: " + COUNTRY
				+ ", so \"" + EXCEL_SEPARATOR + "\" and \"" + LOCALE_DECIMAL_SEPARATOR + "\"). "
				+ "On a computer with other regional settings Excel may expect other ones (e.g. \"" + (";".equals(EXCEL_SEPARATOR) ? "," : ";")
				+ "\" and \"" + (LOCALE_DECIMAL_SEPARATOR == ',' ? "." : ",") + "\"), so choose RFC 4180 if the file is for such users. "
				+ "The file opens correctly with a double-click, also with umlauts (Excel recognizes UTF-8 only by the byte order mark).",
			"The standard (RFC 4180): comma, CRLF, no byte order mark. "
				+ "Excel needs \"Data > From Text/CSV\" for it, else umlauts and numbers may be read wrongly.",
			"For scripts and tools like Python, pandas, awk: comma, LF, UTF-8 without byte order mark.",
			"Tab-separated values: rarely needs quotes, good for pasting into a spreadsheet.",
			"Own settings." };

	private static final int ENCODING_UTF8 = 0;
	private static final int ENCODING_UTF8_BOM = 1;
	private static final int ENCODING_WINDOWS_1252 = 2;
	private static final int LINE_BREAK_CRLF = 0;
	private static final int LINE_BREAK_LF = 1;
	private static final int DECIMAL_LOCALE = 0;
	private static final int DECIMAL_POINT = 1;

	private final JComboBox<String> dialectComboBox = new JComboBox<>(DIALECTS);
	private final JComboBox<String> separatorComboBox = new JComboBox<>(new String[] { ";", ",", TAB, "|" });
	private final JComboBox<String> decimalComboBox = new JComboBox<>(new String[] { "As in the regional settings (\"" + LOCALE_DECIMAL_SEPARATOR + "\")", "Point (\".\")" });
	private final JComboBox<String> encodingComboBox = new JComboBox<>(new String[] { "UTF-8", "UTF-8 with byte order mark (BOM)", "Windows-1252" });
	private final JComboBox<String> lineBreakComboBox = new JComboBox<>(new String[] { "CRLF (Windows, RFC 4180)", "LF (Unix)" });
	private final JCheckBox headerCheckBox = new JCheckBox("With header row");
	private final JComboBox<String> quoteComboBox = new JComboBox<>(new String[] { "Only if needed", "All text cells" });
	private final JEditorPane hintPane = new JEditorPane();
	private boolean adjusting = false;
	private CsvFormat result;

	private CsvOptionsDialog(Window owner, boolean withHeaderOption) {
		super(owner, "CSV Export Options", Dialog.ModalityType.APPLICATION_MODAL);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		headerCheckBox.setVisible(withHeaderOption); // else the header is chosen elsewhere (the setting is kept)

		separatorComboBox.setEditable(true);
		JPanel fields = new JPanel(new GridBagLayout());
		int y = 0;
		addField(fields, y++, "Dialect", dialectComboBox);
		addField(fields, y++, "Separator", separatorComboBox);
		addField(fields, y++, "Decimal separator", decimalComboBox);
		addField(fields, y++, "Encoding", encodingComboBox);
		addField(fields, y++, "Line break", lineBreakComboBox);
		addField(fields, y++, "Quotes", quoteComboBox);
		addField(fields, y++, "", headerCheckBox);
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y;
		gbc.gridwidth = 2;
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.insets = new Insets(8, 4, 0, 4);
		// a text component (not a label): wraps at the width it gets, with the font and color of a label
		hintPane.setContentType("text/html");
		hintPane.setEditable(false);
		hintPane.setOpaque(false);
		hintPane.setFocusable(false);
		hintPane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true);
		JLabel labelStyle = new JLabel();
		hintPane.setFont(labelStyle.getFont());
		hintPane.setForeground(labelStyle.getForeground());
		hintPane.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
		// the hint determines the width of the dialog (it wraps), and is as high as the longest hint (the size doesn't change)
		int hintWidth = Math.max(200, fields.getPreferredSize().width * 3 / 2 - gbc.insets.left - gbc.insets.right); // 50% wider than the fields need
		int hintHeight = 0;
		for (int dialect = 0; dialect < DIALECTS.length; ++dialect) {
			hintHeight = Math.max(hintHeight, hintHeight(hintText(dialect, true), hintWidth));
		}
		hintPane.setPreferredSize(new Dimension(hintWidth, hintHeight));
		fields.add(hintPane, gbc);

		JButton okButton = new JButton("OK");
		JButton cancelButton = new JButton("Cancel");
		okButton.setIcon(UIUtil.scaleIcon(okButton, UIUtil.readImage("/buttonok.png")));
		cancelButton.setIcon(UIUtil.scaleIcon(cancelButton, UIUtil.readImage("/buttoncancel.png")));
		okButton.addActionListener(e -> {
			result = createFormat();
			storeSettings();
			dispose();
		});
		cancelButton.addActionListener(e -> dispose());
		JPanel buttons = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		buttons.add(okButton);
		buttons.add(cancelButton);
		getRootPane().setDefaultButton(okButton);

		JPanel content = new JPanel(new BorderLayout());
		content.setBorder(BorderFactory.createEmptyBorder(8, 8, 0, 8));
		content.add(fields, BorderLayout.CENTER);
		content.add(buttons, BorderLayout.SOUTH);
		setContentPane(content);

		dialectComboBox.addItemListener(e -> {
			if (e.getStateChange() == ItemEvent.SELECTED && !adjusting) {
				applyDialect(dialectComboBox.getSelectedIndex());
			}
			updateHint();
		});
		for (JComboBox<String> comboBox: asStringComboBoxes()) {
			comboBox.addItemListener(e -> {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					onFieldChanged();
				}
			});
		}
		Component separatorEditor = separatorComboBox.getEditor().getEditorComponent();
		if (separatorEditor instanceof JTextField) {
			((JTextField) separatorEditor).getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void insertUpdate(DocumentEvent e) {
					onFieldChanged();
				}
				@Override
				public void removeUpdate(DocumentEvent e) {
					onFieldChanged();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					onFieldChanged();
				}
			});
		}
		headerCheckBox.addItemListener(e -> onFieldChanged());

		// the hint of each dialect as tooltip in the list of the dialects
		ListCellRenderer<? super String> dialectRenderer = dialectComboBox.getRenderer();
		dialectComboBox.setRenderer((list, value, index, isSelected, cellHasFocus) -> {
			Component render = dialectRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
			if (index >= 0 && render instanceof JComponent) {
				((JComponent) render).setToolTipText(hintText(index, encodingComboBox.getSelectedIndex() == ENCODING_WINDOWS_1252, TOOLTIP_LINE_LENGTH));
			}
			return render;
		});

		restoreSettings();
		updateHint();
		pack();
	}

	/**
	 * Gets the height of the hint label showing a text at a given width.
	 */
	private int hintHeight(String text, int width) {
		hintPane.setText(text);
		hintPane.setSize(width, Short.MAX_VALUE);
		return hintPane.getPreferredSize().height;
	}

	@SuppressWarnings("unchecked")
	private JComboBox<String>[] asStringComboBoxes() {
		return new JComboBox[] { separatorComboBox, decimalComboBox, encodingComboBox, lineBreakComboBox, quoteComboBox };
	}

	private static void addField(JPanel panel, int y, String label, JComponent field) {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = y;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(2, 4, 2, 8);
		panel.add(new JLabel(label), gbc);
		gbc = new GridBagConstraints();
		gbc.gridx = 1;
		gbc.gridy = y;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 1;
		gbc.insets = new Insets(2, 0, 2, 4);
		panel.add(field, gbc);
	}

	/**
	 * Shows the dialog.
	 *
	 * @param parent the parent
	 * @return the format chosen, or <code>null</code> if cancelled
	 */
	public static CsvFormat show(Component parent) {
		return show(parent, true);
	}

	/**
	 * Shows the dialog.
	 *
	 * @param parent the parent
	 * @param withHeaderOption offer the option "With header row"? (else the header is chosen elsewhere)
	 * @return the format chosen, or <code>null</code> if cancelled
	 */
	public static CsvFormat show(Component parent, boolean withHeaderOption) {
		Window owner = parent == null ? null : parent instanceof Window ? (Window) parent : SwingUtilities.getWindowAncestor(parent);
		CsvOptionsDialog dialog = new CsvOptionsDialog(owner, withHeaderOption);
		dialog.setLocationRelativeTo(owner);
		UIUtil.fit(dialog);
		dialog.setVisible(true);
		return dialog.result;
	}

	/**
	 * Sets the fields according to a dialect.
	 */
	private void applyDialect(int dialect) {
		if (dialect == CUSTOM) {
			return;
		}
		adjusting = true;
		try {
			switch (dialect) {
			case EXCEL:
				separatorComboBox.setSelectedItem(EXCEL_SEPARATOR);
				decimalComboBox.setSelectedIndex(DECIMAL_LOCALE);
				encodingComboBox.setSelectedIndex(ENCODING_UTF8_BOM);
				lineBreakComboBox.setSelectedIndex(LINE_BREAK_CRLF);
				break;
			case RFC_4180:
				separatorComboBox.setSelectedItem(",");
				decimalComboBox.setSelectedIndex(DECIMAL_POINT);
				encodingComboBox.setSelectedIndex(ENCODING_UTF8);
				lineBreakComboBox.setSelectedIndex(LINE_BREAK_CRLF);
				break;
			case UNIX:
				separatorComboBox.setSelectedItem(",");
				decimalComboBox.setSelectedIndex(DECIMAL_POINT);
				encodingComboBox.setSelectedIndex(ENCODING_UTF8);
				lineBreakComboBox.setSelectedIndex(LINE_BREAK_LF);
				break;
			case TSV:
				separatorComboBox.setSelectedItem(TAB);
				decimalComboBox.setSelectedIndex(DECIMAL_POINT);
				encodingComboBox.setSelectedIndex(ENCODING_UTF8);
				lineBreakComboBox.setSelectedIndex(LINE_BREAK_LF);
				break;
			default:
				break;
			}
			headerCheckBox.setSelected(true);
			quoteComboBox.setSelectedIndex(0);
		} finally {
			adjusting = false;
		}
		updateHint();
	}

	/**
	 * A setting has been changed by the user: the dialect is then "Custom".
	 */
	private void onFieldChanged() {
		if (adjusting) {
			return;
		}
		if (dialectComboBox.getSelectedIndex() != CUSTOM) {
			adjusting = true;
			try {
				dialectComboBox.setSelectedIndex(CUSTOM);
			} finally {
				adjusting = false;
			}
		}
		updateHint();
	}

	private void updateHint() {
		int dialect = Math.max(0, dialectComboBox.getSelectedIndex());
		boolean windows1252 = encodingComboBox.getSelectedIndex() == ENCODING_WINDOWS_1252;
		hintPane.setText(hintText(dialect, windows1252));
		dialectComboBox.setToolTipText(hintText(dialect, windows1252, TOOLTIP_LINE_LENGTH));
	}

	/**
	 * Maximum length of the lines of the tooltips.
	 */
	private static final int TOOLTIP_LINE_LENGTH = 80;

	/**
	 * Gets the hint about a dialect and the settings.
	 *
	 * @param dialect the dialect
	 * @param windows1252 with the note about the encoding "Windows-1252"?
	 */
	private static String hintText(int dialect, boolean windows1252) {
		return hintText(dialect, windows1252, 0);
	}

	/**
	 * Gets the hint about a dialect and the settings.
	 *
	 * @param dialect the dialect
	 * @param windows1252 with the note about the encoding "Windows-1252"?
	 * @param lineLength if &gt; 0, the lines are wrapped at this length (for a tooltip)
	 */
	private static String hintText(int dialect, boolean windows1252, int lineLength) {
		StringBuilder hint = new StringBuilder("<html>" + wrap(DIALECT_HINTS[dialect], lineLength) + "<br><br>");
		hint.append("<i>Byte order mark (BOM):</i> " + wrap("Excel recognizes UTF-8 only by it. Some tools read it as part of the first column name.", lineLength) + "<br>");
		hint.append("<i>CRLF:</i> " + wrap("required by RFC 4180. Practically all programs also read LF.", lineLength) + "<br>");
		hint.append("<i>Decimal comma:</i> " + wrap("only useful with \";\" or Tab as separator.", lineLength));
		if (windows1252) {
			hint.append("<br><i>Windows-1252:</i> " + wrap("characters it can't represent are written as \"?\".", lineLength));
		}
		hint.append("</html>");
		return hint.toString();
	}

	/**
	 * Converts a text to HTML, wrapping it at word boundaries.
	 *
	 * @param text the text
	 * @param lineLength the maximum length of a line, or 0 for no wrapping
	 */
	private static String wrap(String text, int lineLength) {
		if (lineLength <= 0) {
			return UIUtil.toHTMLFragment(text, 0, true, false); // no "&nbsp;", so that it can wrap
		}
		StringBuilder result = new StringBuilder();
		StringBuilder line = new StringBuilder();
		for (String word: text.split(" ")) {
			if (line.length() > 0 && line.length() + 1 + word.length() > lineLength) {
				result.append(UIUtil.toHTMLFragment(line.toString(), 0, true, false)).append("<br>");
				line.setLength(0);
			}
			if (line.length() > 0) {
				line.append(' ');
			}
			line.append(word);
		}
		result.append(UIUtil.toHTMLFragment(line.toString(), 0, true, false));
		return result.toString();
	}

	private String separator() {
		Object item = separatorComboBox.getEditor().getItem();
		String separator = item == null ? "" : item.toString();
		if (TAB.equals(separator)) {
			return "\t";
		}
		return separator.isEmpty() ? ";" : separator;
	}

	private CsvFormat createFormat() {
		int encoding = encodingComboBox.getSelectedIndex();
		return new CsvFormat(
				separator(),
				decimalComboBox.getSelectedIndex() == DECIMAL_POINT ? '.' : LOCALE_DECIMAL_SEPARATOR,
				encoding == ENCODING_WINDOWS_1252 && WINDOWS_1252 != null ? WINDOWS_1252 : StandardCharsets.UTF_8,
				encoding == ENCODING_UTF8_BOM,
				lineBreakComboBox.getSelectedIndex() == LINE_BREAK_LF ? "\n" : "\r\n",
				headerCheckBox.isSelected(),
				quoteComboBox.getSelectedIndex() == 1);
	}

	/**
	 * Stores the settings in the {@link UISettings} (one line per setting).
	 */
	private void storeSettings() {
		Object separator = separatorComboBox.getEditor().getItem();
		UISettings.store(SETTINGS_KEY, dialectComboBox.getSelectedIndex()
				+ "\n" + (separator == null ? "" : separator)
				+ "\n" + decimalComboBox.getSelectedIndex()
				+ "\n" + encodingComboBox.getSelectedIndex()
				+ "\n" + lineBreakComboBox.getSelectedIndex()
				+ "\n" + headerCheckBox.isSelected()
				+ "\n" + quoteComboBox.getSelectedIndex());
	}

	/**
	 * Restores the settings from the {@link UISettings}, or sets the defaults (dialect "Excel").
	 */
	private void restoreSettings() {
		applyDialect(EXCEL);
		adjusting = true;
		try {
			dialectComboBox.setSelectedIndex(EXCEL);
			Object settings = UISettings.restore(SETTINGS_KEY);
			if (settings instanceof String) {
				String[] values = ((String) settings).split("\n", -1);
				if (values.length == 7) {
					int dialect = Integer.parseInt(values[0]);
					if (dialect >= 0 && dialect < DIALECTS.length) {
						if (dialect != CUSTOM) {
							adjusting = false;
							applyDialect(dialect); // the settings of the dialect (in this locale)
							adjusting = true;
						} else {
							separatorComboBox.setSelectedItem(values[1]);
							setIndex(decimalComboBox, values[2]);
							setIndex(encodingComboBox, values[3]);
							setIndex(lineBreakComboBox, values[4]);
							headerCheckBox.setSelected(Boolean.parseBoolean(values[5]));
							setIndex(quoteComboBox, values[6]);
						}
						dialectComboBox.setSelectedIndex(dialect);
					}
				}
			}
		} catch (RuntimeException e) {
			// invalid settings: defaults
			adjusting = false;
			applyDialect(EXCEL);
			adjusting = true;
			dialectComboBox.setSelectedIndex(EXCEL);
		} finally {
			adjusting = false;
		}
	}

	private static void setIndex(JComboBox<String> comboBox, String index) {
		int i = Integer.parseInt(index);
		if (i >= 0 && i < comboBox.getItemCount()) {
			comboBox.setSelectedIndex(i);
		}
	}

	/**
	 * Gets the country of the regional settings (in English), or "this computer" if there is none.
	 */
	private static String country() {
		String country = Locale.getDefault().getDisplayCountry(Locale.ENGLISH);
		return country == null || country.trim().isEmpty() ? "this computer" : country;
	}

	private static Charset windows1252() {
		try {
			return Charset.forName("windows-1252");
		} catch (RuntimeException e) {
			return null;
		}
	}

}
