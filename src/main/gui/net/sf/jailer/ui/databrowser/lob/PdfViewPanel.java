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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;

import net.sf.jailer.ui.Colors;
import net.sf.jailer.util.LogUtil;

/**
 * Renders a PDF document page by page using Apache PDFBox. Offers page
 * navigation (first/previous/next/last) and zoom. Pages are rendered off the
 * event dispatch thread; a render token discards results that are stale because
 * the user has since paged or zoomed again.
 *
 * @author Ralf Wisser
 */
public class PdfViewPanel extends JPanel {

	private static final long serialVersionUID = 1L;

	/** Rendering resolution bounds and default (dots per inch). */
	private static final float MIN_DPI = 24f;
	private static final float MAX_DPI = 300f;
	private static final float DEFAULT_DPI = 96f;
	private static final float ZOOM_FACTOR = 1.25f;

	/** Wheel/track scroll increment. */
	private static final int SCROLL_UNIT = 16;

	private PDDocument document;
	private PDFRenderer renderer;
	private final int pageCount;
	private int pageIndex; // 0-based
	private float dpi = DEFAULT_DPI;

	private final JLabel pageView;
	private final JLabel statusLabel;
	private final JButton firstButton;
	private final JButton prevButton;
	private final JButton nextButton;
	private final JButton lastButton;
	private final JButton zoomOutButton;
	private final JButton zoomInButton;

	private final ExecutorService renderExec;
	/** Incremented (on the EDT) whenever a new render is requested; a worker only applies its result if it still matches. */
	private volatile long renderToken;

	/**
	 * @param pdfBytes the complete PDF content
	 * @throws IOException if the document cannot be parsed (e.g. it is encrypted)
	 */
	public PdfViewPanel(byte[] pdfBytes) throws IOException {
		super(new BorderLayout());
		document = Loader.loadPDF(pdfBytes);
		renderer = new PDFRenderer(document);
		pageCount = document.getNumberOfPages();

		renderExec = Executors.newSingleThreadExecutor(r -> {
			Thread t = new Thread(r, "pdf-render");
			t.setDaemon(true);
			return t;
		});

		pageView = new JLabel();
		pageView.setHorizontalAlignment(JLabel.CENTER);
		pageView.setVerticalAlignment(JLabel.TOP);
		// Give the panel a content-based preferred size before the first (async)
		// render completes, so the hosting dialog packs to the page size rather
		// than to the empty "Rendering…" placeholder. Cleared once a page image
		// is applied, so later zooming scrolls against the image's natural size.
		if (pageCount > 0) {
			pageView.setPreferredSize(pageSizePx(0, DEFAULT_DPI));
		}
		JScrollPane scrollPane = new JScrollPane(pageView);
		scrollPane.getVerticalScrollBar().setUnitIncrement(SCROLL_UNIT);
		scrollPane.getHorizontalScrollBar().setUnitIncrement(SCROLL_UNIT);
		add(scrollPane, BorderLayout.CENTER);

		JPanel bar = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 4));
		firstButton = navButton("|◀", "First page", e -> goTo(0));
		prevButton = navButton("◀", "Previous page", e -> goTo(pageIndex - 1));
		nextButton = navButton("▶", "Next page", e -> goTo(pageIndex + 1));
		lastButton = navButton("▶|", "Last page", e -> goTo(pageCount - 1));
		zoomOutButton = navButton("−", "Zoom out", e -> zoom(1f / ZOOM_FACTOR));
		zoomInButton = navButton("+", "Zoom in", e -> zoom(ZOOM_FACTOR));
		statusLabel = new JLabel();
		statusLabel.setBorder(BorderFactory.createEmptyBorder(0, 8, 0, 8));
		bar.add(firstButton);
		bar.add(prevButton);
		bar.add(nextButton);
		bar.add(lastButton);
		bar.add(statusLabel);
		bar.add(zoomOutButton);
		bar.add(zoomInButton);
		add(bar, BorderLayout.NORTH);

		updateControls();
		renderPage();
	}

	private static JButton navButton(String text, String tooltip, java.awt.event.ActionListener action) {
		JButton b = new JButton(text);
		b.setToolTipText(tooltip);
		b.setMargin(new java.awt.Insets(2, 8, 2, 8));
		b.addActionListener(action);
		return b;
	}

	private void goTo(int index) {
		int clamped = Math.max(0, Math.min(index, pageCount - 1));
		if (clamped != pageIndex) {
			pageIndex = clamped;
			updateControls();
			renderPage();
		}
	}

	private void zoom(float factor) {
		float next = Math.max(MIN_DPI, Math.min(dpi * factor, MAX_DPI));
		if (next != dpi) {
			dpi = next;
			updateControls();
			renderPage();
		}
	}

	private void updateControls() {
		firstButton.setEnabled(pageIndex > 0);
		prevButton.setEnabled(pageIndex > 0);
		nextButton.setEnabled(pageIndex < pageCount - 1);
		lastButton.setEnabled(pageIndex < pageCount - 1);
		zoomOutButton.setEnabled(dpi > MIN_DPI);
		zoomInButton.setEnabled(dpi < MAX_DPI);
		statusLabel.setText("Page " + (pageIndex + 1) + " / " + pageCount
				+ "   ·   " + Math.round(dpi / DEFAULT_DPI * 100) + "%");
	}

	/**
	 * Renders the current page at the current zoom off the EDT and applies the
	 * result only if no newer render has been requested in the meantime.
	 */
	private void renderPage() {
		final long token = ++renderToken;
		final int idx = pageIndex;
		final float d = dpi;
		pageView.setText("Rendering…");
		pageView.setIcon(null);
		final PDFRenderer r = renderer;
		if (r == null) {
			return;
		}
		renderExec.submit(() -> {
			try {
				final BufferedImage image = r.renderImageWithDPI(idx, d, ImageType.RGB);
				if (token != renderToken) {
					return;
				}
				SwingUtilities.invokeLater(() -> {
					if (token != renderToken) {
						return;
					}
					// release the initial content-based size; from now on the icon's
					// natural size drives layout and scrolling (e.g. when zoomed in).
					pageView.setPreferredSize(null);
					pageView.setText(null);
					pageView.setIcon(new ImageIcon(image));
					pageView.revalidate();
					pageView.repaint();
				});
			} catch (Throwable t) {
				if (token != renderToken) {
					return;
				}
				SwingUtilities.invokeLater(() -> {
					if (token != renderToken) {
						return;
					}
					pageView.setPreferredSize(null);
					pageView.setIcon(null);
					pageView.setText("Could not render this page: " + t.getMessage());
					pageView.setForeground(Colors.Color_128_128_128);
				});
			}
		});
	}

	/**
	 * Returns the pixel size of the given page rendered at the given resolution,
	 * derived from the page's crop box (points) and honouring page rotation.
	 */
	private Dimension pageSizePx(int index, float dpi) {
		PDPage page = document.getPage(index);
		PDRectangle box = page.getCropBox();
		float wPt = box.getWidth();
		float hPt = box.getHeight();
		int rotation = page.getRotation();
		if (rotation == 90 || rotation == 270) {
			float t = wPt;
			wPt = hPt;
			hPt = t;
		}
		int w = Math.max(1, Math.round(wPt * dpi / 72f));
		int h = Math.max(1, Math.round(hPt * dpi / 72f));
		return new Dimension(w, h);
	}

	/**
	 * Releases the PDF document and the render thread. Call when the hosting
	 * window closes or the viewer is rebuilt.
	 */
	public void dispose() {
		renderToken++;
		renderExec.shutdownNow();
		renderer = null;
		if (document != null) {
			try {
				document.close();
			} catch (IOException e) {
				// ignore - we are tearing down
			}
			document = null;
		}
		pageView.setIcon(null);
	}
}
