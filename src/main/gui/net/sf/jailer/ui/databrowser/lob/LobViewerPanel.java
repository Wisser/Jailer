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
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;

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

	/** Minimum viewer-dialog size, so tiny content still gets a usable window. */
	private static final int MIN_WIDTH = 420;
	private static final int MIN_HEIGHT = 300;

	private LobContent content;
	private RSyntaxTextAreaWithSQLSyntaxStyle textArea;
	private BufferedImage image;
	private String textForCopy;
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
	}

	private void rebuild() {
		removeAll();
		dispose();
		image = null;
		textForCopy = null;
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
				return messagePanel("PDF document (" + humanSize(content.getLength(), false)
						+ ").\nInline preview is not supported – use „Save to file…“ to open it externally.");
			case ARCHIVE:
				if (content.getType() == LobContentType.ZIP) {
					return buildZip();
				}
				return messagePanel(content.getType().displayName + " (" + humanSize(content.getLength(), false)
						+ ").\nUse „Save to file…“ to extract it.");
			case BINARY:
			default:
				return buildHex();
			}
		} catch (Throwable t) {
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
		textArea = new RSyntaxTextAreaWithSQLSyntaxStyle(false, false) {
			private static final long serialVersionUID = 1L;
			@Override
			protected boolean withModifingMenuItems() {
				return false;
			}
		};
		String style = content.getType().syntaxStyle;
		textArea.setSyntaxEditingStyle(style != null ? style : SyntaxConstants.SYNTAX_STYLE_NONE);
		textForCopy = content.getTextForDisplay(TEXT_DISPLAY_CAP);
		textArea.setText(textForCopy);
		textArea.setEditable(false);
		textArea.setCaretPosition(0);
		if (textForCopy.length() < TEXT_DISPLAY_CAP) {
			return scrollPane(textArea);
		}
		// the on-screen text was capped - tell the user (the full content is still exported)
		JPanel panel = new JPanel(new BorderLayout());
		panel.add(scrollPane(textArea), BorderLayout.CENTER);
		JLabel notice = new JLabel("Showing the first " + String.format("%,d", TEXT_DISPLAY_CAP)
				+ " characters — use „Save to file…“ for the full content.");
		notice.setBorder(BorderFactory.createEmptyBorder(4, 10, 4, 10));
		notice.setForeground(Colors.Color_128_128_128);
		panel.add(notice, BorderLayout.SOUTH);
		return panel;
	}

	private JComponent buildHex() throws IOException {
		byte[] bytes = content.readAllBytes(HEX_MAX);
		return new HexViewPanel(bytes, content.getLength());
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
		return scrollPane(table);
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
	private static final ImageIcon COPY_ICON = loadButtonIcon("/copy.png");
	private static final ImageIcon CLOSE_ICON = loadButtonIcon("/Cancel.png");

	private static ImageIcon loadButtonIcon(String resource) {
		try {
			return UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage(resource, false));
		} catch (Throwable t) {
			return null;
		}
	}

	private JComponent buildToolBar() {
		JPanel bar = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 4));
		JButton save = new JButton("Save to file…");
		save.setIcon(SAVE_ICON);
		save.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				saveToFile();
			}
		});
		bar.add(save);

		if (content.getType().category == LobContentType.Category.TEXT
				|| content.getType().category == LobContentType.Category.IMAGE) {
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
		return bar;
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

	private void copyToClipboard() {
		if (textForCopy != null) {
			UIUtil.setClipboardContent(new StringSelection(textForCopy));
		} else if (image != null) {
			UIUtil.setClipboardContent(new ImageTransferable(image));
		}
	}

	private static String humanSize(long length, boolean chars) {
		if (length < 0) {
			return "unknown size";
		}
		if (chars) {
			return length + (length == 1 ? " char" : " chars");
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
		// natural dimensions, a short text's extent), then clamp to sensible
		// minimum/maximum bounds so tiny content still gets a usable window and
		// huge content does not exceed the owner/screen.
		dialog.pack();
		Dimension pref = dialog.getSize();
		// clamp to the screen (not the owner window - a small owner e.g. a details-view popup must not shrink the viewer)
		java.awt.Rectangle screen;
		if (owner != null && owner.getGraphicsConfiguration() != null) {
			screen = owner.getGraphicsConfiguration().getBounds();
		} else {
			screen = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
		}
		int maxW = Math.round(screen.width * 0.9f);
		int maxH = Math.round(screen.height * 0.9f);
		maxW = Math.max(maxW, MIN_WIDTH);
		maxH = Math.max(maxH, MIN_HEIGHT);
		int w = Math.max(MIN_WIDTH, Math.min(pref.width + 8, maxW));
		int h = Math.max(MIN_HEIGHT, Math.min(pref.height + 8, maxH));
		dialog.setSize(new Dimension(w, h));
		dialog.setLocationRelativeTo(owner);
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
