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
package net.sf.jailer.ui.databrowser.lob;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Image;
import java.awt.Window;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JToggleButton;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.fife.ui.rtextarea.RTextScrollPane;

import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.util.LogUtil;

/**
 * Renders the content of a BLOB/CLOB cell according to its guessed
 * {@link LobContentType}: images as images, text with syntax highlighting, ZIP
 * archives as an entry list, and anything else as a hexadecimal dump. Offers
 * exporting the content to a file with a type-dependent name/extension.
 *
 * @author Ralf Wisser
 */
public class LobViewerPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	/** Client property marking the dialog created by {@link #showViewer} as the LOB content viewer. */
	public static final String CONTENT_VIEWER_PROPERTY = "net.sf.jailer.lobContentViewer";

	/** Cap for text shown in the viewer (full content is still exported). */
	private static final int TEXT_DISPLAY_CAP = 1_000_000;
	/** Cap for images fully loaded into memory. */
	public static final long IMAGE_MAX = 64L * 1024 * 1024;
	/** Cap for the hex dump. */
	private static final long HEX_MAX = 1024 * 1024;
	/** Cap on the decompressed size of a single-file archive drilled into for preview. */
	private static final long ARCHIVE_PREVIEW_MAX = 64L * 1024 * 1024;
	/** Cap for PDFs loaded into memory for inline rendering. */
	private static final long PDF_MAX = 128L * 1024 * 1024;
	/** Cap on the cumulative size of the text-bearing parts read from a DOCX/XLSX/PPTX package. */
	private static final long OFFICE_PREVIEW_MAX = 32L * 1024 * 1024;

	/** Minimum viewer-dialog size, so tiny content still gets a usable window. */
	private static final int MIN_WIDTH = 420;
	private static final int MIN_HEIGHT = 300;

	private LobContent content;
	private RSyntaxTextAreaWithSQLSyntaxStyle textArea;
	private PdfViewPanel pdfPanel;
	private BufferedImage image;
	private String textForCopy;
	/** The text as read, shown when "Format" is off. */
	private String originalText;
	/** The formatted text, computed on first use of "Format". */
	private String formattedText;
	/** Whether the text shown has been cut at {@link #TEXT_DISPLAY_CAP}. */
	private boolean textCapped;
	private volatile boolean modalChildOpen;

	/** Whether a modal child (e.g. the save file chooser) is currently open; suppresses focus-loss auto-close. */
	boolean isModalChildOpen() {
		return modalChildOpen;
	}

	public LobViewerPanel() {
		super(new BorderLayout());
		setOpaque(true);
	}

	/**
	 * Sets the content and (re)builds the view.
	 */
	public void setContent(LobContent content) {
		this.content = content;
		rebuild();
	}

	/**
	 * Releases the RSyntaxTextArea document to avoid a memory leak (see
	 * {@code FileView}). Call when the hosting window closes.
	 */
	public void dispose() {
		if (textArea != null) {
			textArea.setText("");
			textArea.discardAllEdits();
			textArea.setDocument(new RSyntaxDocument(null, SyntaxConstants.SYNTAX_STYLE_NONE));
			textArea = null;
		}
		if (pdfPanel != null) {
			pdfPanel.dispose();
			pdfPanel = null;
		}
	}

	private void rebuild() {
		removeAll();
		dispose();
		image = null;
		textForCopy = null;
		originalText = null;
		formattedText = null;
		textCapped = false;
		// A LOB that holds a single compressed file (a GZIP stream, or a ZIP with
		// exactly one entry) is shown decompressed: unwrap it and render the inner
		// file as if it were the content. Failure leaves the archive itself shown.
		if (content.getType().category == LobContentType.Category.ARCHIVE && content.hasFullContent()) {
			try {
				LobContent inner = LobContentSupport.unwrapSingleFileArchive(content, ARCHIVE_PREVIEW_MAX);
				if (inner != null) {
					content = inner;
				}
				// Office Open XML packages (DOCX/XLSX/PPTX) are always multi-entry ZIPs, so they are
				// never unwrapped above; checked separately (and also covers a single-file-archive
				// that just unwrapped to one, e.g. a GZIP- or ZIP-wrapped Office document).
				if (content.getType() == LobContentType.ZIP) {
					LobContent office = OfficeTextExtractor.extractPreview(content, OFFICE_PREVIEW_MAX);
					if (office != null) {
						content = office;
					}
				}
			} catch (Throwable t) {
				// keep the original archive content
				LogUtil.warn(t);
			}
		}
		add(buildInfoBar(), BorderLayout.NORTH);
		add(buildBody(), BorderLayout.CENTER);
		add(buildToolBar(), BorderLayout.SOUTH);
		revalidate();
		repaint();
	}

	private JComponent buildInfoBar() {
		JPanel bar = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 4));
		bar.add(new JLabel(content.getType().displayName + "  ·  " + humanSize(content.getLength(), content.isText())));
		String notice = content.getNotice();
		if (notice != null && !notice.isEmpty()) {
			JLabel n = new JLabel(notice);
			n.setBorder(BorderFactory.createEmptyBorder(0, 12, 0, 0));
			bar.add(n);
		}
		return bar;
	}

	private JComponent buildBody() {
		try {
			switch (content.getType().category) {
			case IMAGE:
				return buildImage();
			case TEXT:
				return buildText();
			case PDF:
				return buildPdf();
			case ARCHIVE:
				if (content.getType() == LobContentType.ZIP) {
					return buildZip();
				}
				if (content.getPreviewText() != null) {
					return buildOfficePreview();
				}
				return messagePanel(content.getType().displayName + " (" + humanSize(content.getLength(), false)
						+ ").\nUse „Save to file…“ to extract it.");
			case BINARY:
			default:
				return buildHex();
			}
		} catch (Throwable t) {
			LogUtil.warn(t);
			return messagePanel("Could not render the content: " + t.getMessage());
		}
	}

	private JComponent buildImage() throws IOException {
		image = content.readImage(IMAGE_MAX);
		if (image == null) {
			String notice = content.getLength() > IMAGE_MAX
					? "Image too large to preview (over " + humanSize(IMAGE_MAX, false)
						+ ") — showing a hexadecimal dump; use „Save to file…“ for the full image."
					: "The image could not be rendered — showing a hexadecimal dump instead.";
			JPanel panel = new JPanel(new BorderLayout());
			panel.add(buildHex(), BorderLayout.CENTER);
			JLabel noticeLabel = new JLabel(notice);
			noticeLabel.setBorder(BorderFactory.createEmptyBorder(4, 10, 4, 10));
			noticeLabel.setForeground(Colors.Color_128_128_128);
			panel.add(noticeLabel, BorderLayout.NORTH);
			return panel;
		}
		JLabel label = new JLabel(new javax.swing.ImageIcon(image));
		label.setHorizontalAlignment(JLabel.CENTER);
		return scrollPane(label);
	}

	private JComponent buildText() throws IOException {
		// getTextForDisplay caps the read itself (avoids loading more than the cap
		// from a large file-backed LOB); a result reaching exactly the cap is treated
		// as capped, so the notice is shown even in the rare case the content's true
		// length is exactly the cap (pre-existing, harmless imprecision).
		String text = content.getTextForDisplay(TEXT_DISPLAY_CAP);
		return buildTextView(text, text.length() >= TEXT_DISPLAY_CAP, content.getType());
	}

	/**
	 * Renders the extracted plain-text preview of a DOCX/XLSX/PPTX package
	 * (see {@link LobContent#getPreviewText()}). Unlike {@link #buildText()},
	 * the underlying payload stays the original Office file - only the on-screen
	 * body and "Copy to clipboard" use the preview text; "Save to file…"/"Open
	 * externally" keep exporting the real file.
	 */
	private JComponent buildOfficePreview() {
		String full = content.getPreviewText();
		boolean capped = full.length() > TEXT_DISPLAY_CAP;
		String text = capped ? full.substring(0, TEXT_DISPLAY_CAP) : full;
		return buildTextView(text, capped, null);
	}

	/**
	 * Shared body for any text-like rendering: a read-only {@code RSyntaxTextArea}
	 * showing the (already capped) <code>text</code>, with a notice if
	 * <code>capped</code>. Also sets {@link #textForCopy} for the "Copy to
	 * clipboard" button.
	 *
	 * @param type the type of the text, or <code>null</code> for plain text
	 */
	private JComponent buildTextView(String text, boolean capped, LobContentType type) {
		String syntaxStyle = type != null? type.syntaxStyle : null;
		textArea = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			private static final long serialVersionUID = 1L;
			@Override
			protected boolean withModifingMenuItems() {
				return false;
			}
		};
		textArea.setSyntaxEditingStyle(syntaxStyle != null ? syntaxStyle : SyntaxConstants.SYNTAX_STYLE_NONE);
		textForCopy = text;
		originalText = text;
		textCapped = capped;
		textArea.setText(textForCopy);
		textArea.setEditable(false);
		textArea.setCaretPosition(0);
		if (!capped && (type == LobContentType.JSON || type == LobContentType.XML)) {
			// formatted right away, so that the viewer is also big enough for the formatted text
			// (and "Format" just has to show it)
			try {
				formattedText = type == LobContentType.JSON? LobFormatter.formatJson(text) : LobFormatter.formatXml(text);
			} catch (Exception e) {
				// not well-formed - said when "Format" is used
				formattedText = null;
			}
		}
		boolean wrap = longestLineLength(text) > WRAP_THRESHOLD;
		if (wrap || formattedText != null) {
			setViewSize(text, formattedText, wrap);
		}
		JScrollPane textScrollPane;
		if (wrap) {
			// a long line (e.g. JSON without line breaks) would make the viewer very wide and
			// very flat; wrapped, columns and rows give the viewer a sensible size instead
			textArea.setLineWrap(true);
			textArea.setWrapStyleWord(false);
			// line numbers make the wrapping visible: a continuation row has no number of its own
			RTextScrollPane rTextScrollPane = new RTextScrollPane();
			rTextScrollPane.setViewportView(textArea);
			rTextScrollPane.setLineNumbersEnabled(true);
			rTextScrollPane.getVerticalScrollBar().setUnitIncrement(SCROLL_UNIT);
			rTextScrollPane.getHorizontalScrollBar().setUnitIncrement(SCROLL_UNIT);
			textScrollPane = rTextScrollPane;
		} else {
			textScrollPane = scrollPane(textArea);
		}
		if (!capped) {
			return textScrollPane;
		}
		// the on-screen text was capped - tell the user (the full content is still exported)
		JPanel panel = new JPanel(new BorderLayout());
		panel.add(textScrollPane, BorderLayout.CENTER);
		JLabel notice = new JLabel("Showing the first " + String.format("%,d", TEXT_DISPLAY_CAP)
				+ " characters — use „Save to file…“ for the full content.");
		notice.setBorder(BorderFactory.createEmptyBorder(4, 10, 4, 10));
		notice.setForeground(Colors.Color_128_128_128);
		panel.add(notice, BorderLayout.SOUTH);
		return panel;
	}

	/** Lines longer than this are wrapped. */
	private static final int WRAP_THRESHOLD = 200;
	/** Maximum width of the viewer in columns if its size is set from the text. */
	private static final int WRAP_COLUMNS = 120;
	/** Minimum width of the viewer in columns if its size is set from the text. */
	private static final int MIN_COLUMNS = 40;
	/** Maximum height of the viewer in rows if its size is set from the text. */
	private static final int WRAP_MAX_ROWS = 40;
	/** Minimum height of the viewer in rows if its size is set from the text. */
	private static final int WRAP_MIN_ROWS = 5;

	/**
	 * Sets columns and rows of the text area, and with it the initial size of the viewer,
	 * so that both the text and its formatted version fit.
	 *
	 * @param text the text
	 * @param formatted the formatted text, or <code>null</code>
	 * @param wrap whether lines are wrapped
	 */
	private void setViewSize(String text, String formatted, boolean wrap) {
		int longest = longestLineLength(text);
		if (formatted != null) {
			longest = Math.max(wrap? 0 : longest, longestLineLength(formatted));
		}
		int columns = Math.max(MIN_COLUMNS, Math.min(WRAP_COLUMNS, longest));
		int rows = wrappedRows(text, wrap? columns : Integer.MAX_VALUE);
		if (formatted != null) {
			rows = Math.max(rows, wrappedRows(formatted, wrap? columns : Integer.MAX_VALUE));
		}
		textArea.setColumns(columns);
		textArea.setRows(Math.max(WRAP_MIN_ROWS, Math.min(WRAP_MAX_ROWS, rows)));
	}

	private static int longestLineLength(String text) {
		int longest = 0;
		int length = 0;
		for (int i = 0; i < text.length(); ++i) {
			char c = text.charAt(i);
			if (c == '\n' || c == '\r') {
				length = 0;
			} else if (++length > longest) {
				longest = length;
			}
		}
		return longest;
	}

	/**
	 * Estimates the number of rows the text takes when wrapped at the given number of
	 * columns. Counting stops at {@link #WRAP_MAX_ROWS}.
	 *
	 * @param columns the columns, {@link Integer#MAX_VALUE} for no wrapping
	 */
	private static int wrappedRows(String text, int columns) {
		int rows = 0;
		int length = 0;
		for (int i = 0; i <= text.length() && rows < WRAP_MAX_ROWS; ++i) {
			if (i == text.length() || text.charAt(i) == '\n') {
				rows += Math.max(1, (int) ((length + (long) columns - 1) / columns));
				length = 0;
			} else if (text.charAt(i) != '\r') {
				++length;
			}
		}
		return rows;
	}

	private JComponent buildHex() throws IOException {
		byte[] bytes = content.readAllBytes(HEX_MAX);
		return new HexViewPanel(bytes, content.getLength());
	}

	private JComponent buildPdf() throws IOException {
		if (content.getLength() >= 0 && content.getLength() > PDF_MAX) {
			return messagePanel("PDF document (" + humanSize(content.getLength(), false)
					+ ") is too large to preview inline (over " + humanSize(PDF_MAX, false)
					+ ") — use „Save to file…“ to open it externally.");
		}
		try {
			byte[] bytes = content.readAllBytes(PDF_MAX);
			pdfPanel = new PdfViewPanel(bytes);
			return pdfPanel;
		} catch (Throwable t) {
			// e.g. encrypted / malformed PDF, out of memory, or (defensively) a
			// missing PDFBox class at runtime - fall back to the save-externally hint.
			LogUtil.warn(t);
			return messagePanel("Could not render the PDF (" + t.getMessage()
					+ ").\nUse „Save to file…“ to open it externally.");
		}
	}

	private JComponent buildZip() throws IOException {
		DefaultTableModel model = new DefaultTableModel(new Object[] { "Name", "Size", "Compressed", "Modified" }, 0) {
			private static final long serialVersionUID = 1L;
			@Override
			public boolean isCellEditable(int row, int column) {
				return false;
			}
		};
		List<Object[]> rows = new ArrayList<Object[]>();
		try (ZipInputStream zis = new ZipInputStream(content.openBinaryStream())) {
			ZipEntry e;
			while ((e = zis.getNextEntry()) != null && rows.size() < 100000) {
				rows.add(new Object[] {
						e.getName(),
						e.getSize() >= 0 ? e.getSize() : "?",
						e.getCompressedSize() >= 0 ? e.getCompressedSize() : "?",
						e.getTime() >= 0 ? new java.util.Date(e.getTime()) : "" });
				zis.closeEntry();
			}
		}
		for (Object[] row : rows) {
			model.addRow(row);
		}
		JTable table = new JTable(model);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		table.setRowSelectionAllowed(false);
		table.setColumnSelectionAllowed(false);
		table.setCellSelectionEnabled(false);
		sizeColumnsToContent(table);
		return scrollPane(table);
	}

	/** Max width the (first) name column is allowed to claim, so a long path does not blow out the dialog. */
	private static final int NAME_COLUMN_MAX = 400;

	/**
	 * Sizes each column's preferred width to fit its header and cell content
	 * (measured via the cell renderers), clamping the leading name column.
	 */
	private static void sizeColumnsToContent(JTable table) {
		for (int c = 0; c < table.getColumnCount(); c++) {
			javax.swing.table.TableColumn column = table.getColumnModel().getColumn(c);
			Component header = table.getTableHeader().getDefaultRenderer().getTableCellRendererComponent(
					table, column.getHeaderValue(), false, false, -1, c);
			int width = header.getPreferredSize().width;
			for (int r = 0; r < table.getRowCount(); r++) {
				Component cell = table.getCellRenderer(r, c).getTableCellRendererComponent(
						table, table.getValueAt(r, c), false, false, r, c);
				width = Math.max(width, cell.getPreferredSize().width);
			}
			width += 16; // padding
			if (c == 0) {
				width = Math.min(width, NAME_COLUMN_MAX);
			}
			column.setPreferredWidth(width);
		}
	}

	/** Wheel/track scroll increment for the viewer's scroll panes. */
	private static final int SCROLL_UNIT = 16;

	/**
	 * Wraps a view in a {@link JScrollPane} with a sensible unit increment, so
	 * mouse-wheel scrolling of non-{@code Scrollable} views (e.g. an image
	 * {@link JLabel}) does not crawl at the 1-pixel default.
	 */
	private static JScrollPane scrollPane(Component view) {
		JScrollPane sp = new JScrollPane(view);
		sp.getVerticalScrollBar().setUnitIncrement(SCROLL_UNIT);
		sp.getHorizontalScrollBar().setUnitIncrement(SCROLL_UNIT);
		return sp;
	}

	private JComponent messagePanel(String message) {
		JPanel p = new JPanel(new BorderLayout());
		javax.swing.JTextArea ta = new javax.swing.JTextArea(message);
		ta.setEditable(false);
		ta.setLineWrap(true);
		ta.setWrapStyleWord(true);
		ta.setBorder(BorderFactory.createEmptyBorder(12, 12, 12, 12));
		p.add(ta, BorderLayout.CENTER);
		return p;
	}

	/** Button icons (loaded once; scaled to the label font height). */
	private static final ImageIcon SAVE_ICON = loadButtonIcon("/export.png");
	private static final ImageIcon OPEN_EXTERNAL_ICON = loadButtonIcon("/showinnewwindow.png");
	private static final ImageIcon COPY_ICON = loadButtonIcon("/copy.png");
	private static final ImageIcon CLOSE_ICON = loadButtonIcon("/Cancel.png");
	private static final ImageIcon FORMAT_ICON = loadButtonIcon("/formatsql.png");

	private static ImageIcon loadButtonIcon(String resource) {
		try {
			return UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage(resource, false));
		} catch (Throwable t) {
			return null;
		}
	}

	private JComponent buildToolBar() {
		JPanel toolBar = new JPanel(new BorderLayout());
		JPanel bar = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
		toolBar.add(bar, BorderLayout.CENTER);
		JButton save = new JButton("Save to file…");
		save.setIcon(SAVE_ICON);
		save.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				saveToFile();
			}
		});
		bar.add(save);

		JButton openExternal = new JButton("Open externally");
		openExternal.setIcon(OPEN_EXTERNAL_ICON);
		openExternal.setToolTipText("Open this content in the operating system's default application");
		openExternal.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				openExternally();
			}
		});
		bar.add(openExternal);

		final LobContentType type = content.getType();
		if (textArea != null && (type == LobContentType.JSON || type == LobContentType.XML)) {
			final JToggleButton format = new JToggleButton("Format");
			format.setIcon(FORMAT_ICON);
			if (textCapped) {
				format.setEnabled(false);
				format.setToolTipText("Only the first " + String.format("%,d", TEXT_DISPLAY_CAP)
						+ " characters are shown; formatting needs the complete content.");
			} else {
				format.setToolTipText("Show the " + type.displayName + " content indented (the content itself is not changed)");
			}
			format.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					toggleFormat(format, type);
				}
			});
			// on the left, apart from the buttons acting on the content as a whole
			JPanel left = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 4));
			left.add(format);
			toolBar.add(left, BorderLayout.WEST);
		}

		if (textForCopy != null || image != null) {
			JButton copy = new JButton("Copy to clipboard");
			copy.setIcon(COPY_ICON);
			copy.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					copyToClipboard();
				}
			});
			bar.add(copy);
		}

		JButton close = new JButton("Close");
		close.setIcon(CLOSE_ICON);
		close.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				Window w = SwingUtilities.getWindowAncestor(LobViewerPanel.this);
				if (w != null) {
					w.dispose();
				}
			}
		});
		bar.add(close);
		return toolBar;
	}

	/**
	 * Switches between the formatted and the original JSON/XML text. Only the view and
	 * "Copy to clipboard" are affected, "Save to file…" still exports the original content.
	 */
	private void toggleFormat(JToggleButton button, LobContentType type) {
		if (textArea == null || originalText == null) {
			return;
		}
		if (!button.isSelected()) {
			showText(originalText);
			return;
		}
		if (formattedText == null) {
			UIUtil.setWaitCursor(this);
			try {
				formattedText = type == LobContentType.JSON? LobFormatter.formatJson(originalText) : LobFormatter.formatXml(originalText);
			} catch (Exception e) {
				button.setSelected(false);
				modalChildOpen = true;
				try {
					javax.swing.JOptionPane.showMessageDialog(this,
							"The content is not well-formed " + type.displayName + ":\n" + e.getMessage(),
							"Format", javax.swing.JOptionPane.WARNING_MESSAGE);
				} finally {
					modalChildOpen = false;
				}
				return;
			} finally {
				UIUtil.resetWaitCursor(this);
			}
		}
		showText(formattedText);
	}

	private void showText(String text) {
		textForCopy = text;
		textArea.setText(text);
		textArea.setCaretPosition(0);
	}

	private void saveToFile() {
		String base = LobFilenames.baseName(content);
		String ext = content.getType().extension;
		modalChildOpen = true;
		try {
			String fn = UIUtil.choseFile(new File(base + ext), ".", "Save content", ext, this, false, false);
			if (fn == null) {
				return;
			}
			content.exportTo(new File(fn));
		} catch (IOException ex) {
			UIUtil.showException(this, "Error", ex);
		} finally {
			modalChildOpen = false;
		}
	}

	private void openExternally() {
		if (!Desktop.isDesktopSupported() || !Desktop.getDesktop().isSupported(Desktop.Action.OPEN)) {
			javax.swing.JOptionPane.showMessageDialog(this,
					"Your system does not support opening files in an external application.",
					"Warning", javax.swing.JOptionPane.WARNING_MESSAGE);
			return;
		}
		try {
			File tempDir = Configuration.getInstance().createTempFile();
			tempDir.mkdirs();
			File file = new File(tempDir, LobFilenames.baseName(content) + content.getType().extension);
			content.exportTo(file);
			file.deleteOnExit();
			tempDir.deleteOnExit();
			Desktop.getDesktop().open(file);
		} catch (IOException | RuntimeException ex) {
			UIUtil.showException(this, "Error", ex);
		}
	}

	private void copyToClipboard() {
		if (textForCopy != null) {
			UIUtil.setClipboardContent(new StringSelection(textForCopy));
		} else if (image != null) {
			UIUtil.setClipboardContent(new ImageTransferable(image));
		}
	}

	public static String humanSize(long length, boolean chars) {
		if (length < 0) {
			return "unknown size";
		}
		if (chars) {
			return String.format("%,d", length) + (length == 1 ? " char" : " chars");
		}
		if (length < 1024) {
			return length + " B";
		}
		double v = length;
		String[] units = { "KB", "MB", "GB", "TB" };
		int i = -1;
		do {
			v /= 1024;
			i++;
		} while (v >= 1024 && i < units.length - 1);
		return String.format("%.1f %s", v, units[i]);
	}

	/**
	 * Opens a modeless viewer dialog for the given content, sized relative to
	 * the owner window.
	 */
	public static void showViewer(Component parent, LobContent content) {
		if (content == null) {
			return;
		}
		Window owner = parent == null ? null : SwingUtilities.getWindowAncestor(parent);
		final JDialog dialog;
		if (owner instanceof Frame) {
			dialog = new JDialog((Frame) owner);
		} else if (owner instanceof Dialog) {
			dialog = new JDialog((Dialog) owner);
		} else {
			dialog = new JDialog();
		}
		dialog.setModalityType(Dialog.ModalityType.MODELESS);
		dialog.setTitle("Content viewer");
		dialog.getRootPane().putClientProperty(CONTENT_VIEWER_PROPERTY, Boolean.TRUE);
		final LobViewerPanel viewer = new LobViewerPanel();
		viewer.setContent(content);
		dialog.getContentPane().add(viewer, BorderLayout.CENTER);
		dialog.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				viewer.dispose();
				// bring the owner (the details view) to front and focus it whenever the viewer
				// closes - including the focus-loss auto-close, where focus already went elsewhere.
				final Window owner = dialog.getOwner();
				if (owner != null && owner.isDisplayable()) {
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							owner.toFront();
							owner.requestFocus();
						}
					});
				}
			}
		});
		dialog.addWindowFocusListener(new WindowAdapter() {
			@Override
			public void windowLostFocus(WindowEvent e) {
				if (!dialog.isShowing()) {
					return; // the dialog is being disposed (Close/ESC/X) — not a real focus-away
				}
				if (viewer.isModalChildOpen()) {
					return; // a file chooser / modal child is in progress
				}
				for (Window w = e.getOppositeWindow(); w != null; w = w.getOwner()) {
					if (w == dialog) {
						return; // focus went to a window owned by the viewer (child dialog / file chooser)
					}
				}
				dialog.dispose();
			}
		});
		dialog.getRootPane().registerKeyboardAction(e -> dialog.dispose(),
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Size the dialog to the content's own preferred size (e.g. an image's
		// natural dimensions, a short text's extent), then keep both the initial
		// size and position within the top-level parent window (the DataBrowser /
		// SQL-console frame) so the viewer never overhangs its borders.
		dialog.pack();
		Dimension pref = dialog.getSize();
		// the ultimate owner is the top-level frame; a small intermediate owner
		// (e.g. a details-view popup) must not let the viewer spill past the frame.
		Window top = owner;
		if (top != null) {
			while (top.getOwner() != null) {
				top = top.getOwner();
			}
		}
		java.awt.Rectangle bounds;
		if (top != null && top.isShowing()) {
			bounds = top.getBounds();
			if (top.getGraphicsConfiguration() != null) {
				bounds = bounds.intersection(top.getGraphicsConfiguration().getBounds());
			}
		} else {
			// parentless fallback: 90% of the screen
			java.awt.Rectangle screen;
			if (owner != null && owner.getGraphicsConfiguration() != null) {
				screen = owner.getGraphicsConfiguration().getBounds();
			} else {
				screen = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
			}
			bounds = new java.awt.Rectangle(screen.x, screen.y,
					Math.round(screen.width * 0.9f), Math.round(screen.height * 0.9f));
		}
		int w = Math.min(pref.width + 28, bounds.width);
		int h = Math.min(pref.height + 8, bounds.height);
		// fit-the-frame takes priority over the usability minimum on a tiny frame
		w = Math.max(w, Math.min(MIN_WIDTH, bounds.width));
		h = Math.max(h, Math.min(MIN_HEIGHT, bounds.height));
		dialog.setSize(new Dimension(w, h));
		dialog.setLocationRelativeTo(owner);
		java.awt.Point loc = dialog.getLocation();
		int x = Math.min(Math.max(loc.x, bounds.x), bounds.x + bounds.width - w);
		int y = Math.min(Math.max(loc.y, bounds.y), bounds.y + bounds.height - h);
		dialog.setLocation(x, y);
		dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		dialog.setVisible(true);
	}

	/**
	 * Minimal {@link Transferable} for copying an image to the system clipboard.
	 * Also implements {@link ClipboardOwner} (a no-op) because
	 * {@code UIUtil.setClipboardContent} uses the transferable as its own owner.
	 */
	private static final class ImageTransferable implements Transferable, ClipboardOwner {
		private final Image image;

		ImageTransferable(Image image) {
			this.image = image;
		}

		@Override
		public void lostOwnership(Clipboard clipboard, Transferable contents) {
			// no-op
		}

		@Override
		public DataFlavor[] getTransferDataFlavors() {
			return new DataFlavor[] { DataFlavor.imageFlavor };
		}

		@Override
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			return DataFlavor.imageFlavor.equals(flavor);
		}

		@Override
		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
			if (!DataFlavor.imageFlavor.equals(flavor)) {
				throw new UnsupportedFlavorException(flavor);
			}
			return image;
		}
	}
}
