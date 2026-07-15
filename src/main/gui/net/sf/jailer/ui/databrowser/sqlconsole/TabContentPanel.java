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
package net.sf.jailer.ui.databrowser.sqlconsole;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Types;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RowSorter;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.fife.ui.rsyntaxtextarea.RSyntaxDocument;
import org.fife.ui.rsyntaxtextarea.RSyntaxTextArea;
import org.fife.ui.rsyntaxtextarea.SyntaxConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.formdev.flatlaf.FlatClientProperties;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Column;
import net.sf.jailer.modelbuilder.JDBCMetaDataBasedModelElementFinder;
import net.sf.jailer.modelbuilder.MemorizedResultSet;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;
import net.sf.jailer.ui.databrowser.Row;
import net.sf.jailer.ui.databrowser.geo.Geometry;
import net.sf.jailer.ui.databrowser.geo.GeometryPreviewPanel;
import net.sf.jailer.ui.databrowser.geo.SpatialCellSupport;
import net.sf.jailer.ui.databrowser.metadata.MetaDataDetailsPanel;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithSQLSyntaxStyle;
import net.sf.jailer.ui.syntaxtextarea.RSyntaxTextAreaWithTheme;
import net.sf.jailer.util.Pair;

public class TabContentPanel extends javax.swing.JPanel {

	private final String SEPARATOR_TAB = "Tabulator";
	private final String SEPARATOR_SPACE = "Space";

	/**
	 * The logger.
	 */
	private static final Logger logger = LoggerFactory.getLogger(MetaDataDetailsPanel.class);
	
	private final boolean silent;

	private static final int MAX_TOOLTIP_COLUMNS = 20;
	private static final int MAX_TOOLTIP_VALUE_LENGTH = 150;

	/** Result of scanning a query result's rows for spatial (geometry) columns, see {@link #collectSpatialGeometries}. */
	private static final class RowScanResult {
		final Map<Geometry, Row> rowByGeometry;
		final Set<Integer> spatialColumnIndices;

		RowScanResult(Map<Geometry, Row> rowByGeometry, Set<Integer> spatialColumnIndices) {
			this.rowByGeometry = rowByGeometry;
			this.spatialColumnIndices = spatialColumnIndices;
		}
	}

	/**
	 * Scans every cell of every row for a decodable WGS84 geometry value (see
	 * {@link SpatialCellSupport#parse(Object)} - already returns <code>null</code> for
	 * anything that isn't a map-previewable spatial value), so the map overlay can be shown
	 * only when the result set actually has something to display on one. Keeps the originating
	 * {@link Row} for each found geometry (identity-keyed - {@link Geometry} has neither equals
	 * nor hashCode, so each instance is unique per row), and also tracks which column indices ever
	 * held a spatial value, so the legend-column combobox can exclude them ("keine Geoobjektspalte
	 * anbieten") without a second scan.
	 */
	private static RowScanResult collectSpatialGeometries(List<Row> rows) {
		Map<Geometry, Row> rowByGeometry = new LinkedHashMap<Geometry, Row>();
		Set<Integer> spatialColumnIndices = new LinkedHashSet<Integer>();
		if (rows != null) {
			for (Row row : rows) {
				if (row.values == null) {
					continue;
				}
				for (int i = 0; i < row.values.length; i++) {
					Geometry geometry = SpatialCellSupport.parse(row.values[i]);
					if (geometry != null) {
						spatialColumnIndices.add(i);
						rowByGeometry.put(geometry, row);
					}
				}
			}
		}
		return new RowScanResult(rowByGeometry, spatialColumnIndices);
	}

	/**
	 * Renders a compact HTML table of {@code row}'s column/value pairs for the map overlay's
	 * hover tooltip - every spatial column is skipped (no geo-objects in the tooltip, matching
	 * what {@link #collectSpatialGeometries} already excludes from the map itself), capped at
	 * {@link #MAX_TOOLTIP_COLUMNS} columns and {@link #MAX_TOOLTIP_VALUE_LENGTH} characters per
	 * value. If {@code legendColumnIndex >= 0} (the user picked a legend/label column), that
	 * column's row is shown first, then skipped in the general loop to avoid a duplicate.
	 */
	private static String buildRowTooltipHtml(Row row, TableModel model, int legendColumnIndex, Color legendColor) {
		StringBuilder html = new StringBuilder("<html><table cellpadding=\"2\">");
		int shown = 0;
		if (legendColumnIndex >= 0 && legendColumnIndex < row.values.length) {
			appendTooltipRow(html, row, model, legendColumnIndex, legendColor);
			shown++;
		}
		for (int i = 0; i < row.values.length && shown < MAX_TOOLTIP_COLUMNS; i++) {
			if (i == legendColumnIndex) {
				continue;
			}
			Object value = row.values[i];
			if (SpatialCellSupport.parse(value) != null) {
				continue;
			}
			appendTooltipRow(html, row, model, i, null);
			shown++;
		}
		html.append("</table></html>");
		return html.toString();
	}

	/**
	 * @param swatchColor when non-null (only for the legend column's row), a small colored square
	 *        is rendered before the value - "nutze in tooltip für Legendenspalte die Legendenfarbe" -
	 *        so the tooltip visually ties back to the same row's marker/legend color on the map.
	 */
	private static void appendTooltipRow(StringBuilder html, Row row, TableModel model, int i, Color swatchColor) {
		Object value = row.values[i];
		String columnName = model != null && i < model.getColumnCount() ? stripHtml(model.getColumnName(i)) : "#" + i;
		String text = value == null ? "" : String.valueOf(value);
		if (text.length() > MAX_TOOLTIP_VALUE_LENGTH) {
			text = text.substring(0, MAX_TOOLTIP_VALUE_LENGTH) + "...";
		}
		html.append("<tr><td><b>").append(UIUtil.toHTMLFragment(columnName, 0, true, false)).append("</b></td><td>");
		if (swatchColor != null) {
			html.append("<font color=\"").append(toHexColor(swatchColor)).append("\">■</font>&nbsp;");
		}
		html.append(UIUtil.toHTMLFragment(text, 0, true, false)).append("</td></tr>");
	}

	private static String toHexColor(Color c) {
		return String.format("#%02x%02x%02x", c.getRed(), c.getGreen(), c.getBlue());
	}

	private static int findColumnIndexByName(TableModel model, String name) {
		if (model == null || name == null) {
			return -1;
		}
		for (int i = 0; i < model.getColumnCount(); i++) {
			if (name.equals(stripHtml(model.getColumnName(i)))) {
				return i;
			}
		}
		return -1;
	}

	private static final Pattern TOOLTIP_COLUMN_NAME_BOLD_PATTERN = Pattern.compile("<b>(.*?)</b>");

	/**
	 * {@code TableModel.getColumnName(...)} for the console's result table can itself be an HTML
	 * string (e.g. wrapped in {@code <html><b>...</b></html>} - see the same pattern already used
	 * in {@code SQLConsoleChartPanel.stripHtml}/{@code ColumnsTable.java}) - re-escaping that as
	 * plain text (as {@link UIUtil#toHTMLFragment} does) would show the raw tags literally in the
	 * tooltip instead of just the column name, so strip it down to plain text first.
	 */
	private static String stripHtml(String html) {
		if (html == null) {
			return "";
		}
		if (html.startsWith("<html>")) {
			Matcher m = TOOLTIP_COLUMN_NAME_BOLD_PATTERN.matcher(html);
			if (m.find()) {
				return m.group(1);
			}
			return html.replaceAll("<[^>]*>", " ").replaceAll("\\s+", " ").trim();
		}
		return html;
	}

	private GeometryPreviewPanel mapOverlayPanel;
	private boolean mapOverlayExpanded;
	// Set when the user dismisses the map via the context menu's "Close" item; keeps it hidden across
	// tab switches (updateMapOverlayVisibility honors it) until the next data load re-shows it.
	private boolean mapOverlayUserHidden;
	private GridBagConstraints mapOverlayCornerConstraints;
	private GridBagConstraints mapOverlayExpandedConstraints;

	private static final String NO_LEGEND_COLUMN = "(none)";
	private javax.swing.JComboBox<String> legendColumnCombo;
	private javax.swing.JComboBox<String> legendPaletteCombo;
	private JSpinner legendFontSizeSpinner;
	// Remembered across TabContentPanel instances (one per query/tab) - same pattern as
	// lastColumnSeparator/lastHeaderCheckBoxIsSelected above.
	private static String lastLegendColumnName;
	private static String lastLegendPaletteName;
	// Session-remembered legend text-size delta (see GeometryPreviewPanel.setLegendFontSizeDelta),
	// carried across result tabs like the palette choice above.
	private static int lastLegendFontSizeDelta;

	private Map<Geometry, Row> rowByGeometry = java.util.Collections.emptyMap();
	// Guards legendColumnCombo's item-list rebuild in refreshLegendColumnComboItems() against
	// firing its own ActionListener (which would otherwise treat the rebuild as a user selection).
	private boolean suppressLegendComboEvents;

	/**
	 * Toggles the floating map overlay between its small bottom-left "corner" presentation and
	 * covering the entire {@code TabContentPanel} (all tabs), by swapping its {@link GridBagConstraints}
	 * within the shared {@code jLayeredPane1} cell - the same mechanism {@code loadingPanel} and
	 * {@code tabbedPane} already use to overlay each other there.
	 */
	private void toggleMapOverlayExpanded() {
		mapOverlayExpanded = !mapOverlayExpanded;
		((GridBagLayout) jLayeredPane1.getLayout()).setConstraints(
				mapOverlayPanel, mapOverlayExpanded ? mapOverlayExpandedConstraints : mapOverlayCornerConstraints);
		jLayeredPane1.revalidate();
		jLayeredPane1.repaint();
	}

	/** Popup offering the supported bitmap formats plus copy-to-clipboard for the current map view. */
	private void showMapExportMenu(Component anchor) {
		JPopupMenu menu = new JPopupMenu();
		for (String fmt : new String[] {"png", "jpg", "bmp", "gif"}) {
			JMenuItem item = new JMenuItem("Save as " + fmt.toUpperCase() + "...");
			item.addActionListener(e -> saveMapAs(fmt));
			menu.add(item);
		}
		menu.addSeparator();
		JMenuItem clip = new JMenuItem("Copy to Clipboard");
		clip.addActionListener(e -> copyMapToClipboard());
		menu.add(clip);
		menu.show(anchor, 0, anchor.getHeight());
	}

	private void saveMapAs(String format) {
		JFileChooser chooser = new JFileChooser();
		chooser.setSelectedFile(new File("map." + format));
		chooser.setFileFilter(new FileNameExtensionFilter(format.toUpperCase() + " Image (*." + format + ")", format));
		if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION) {
			return;
		}
		try {
			BufferedImage image = mapOverlayPanel.renderToImage();
			// PNG/GIF keep the alpha channel; JPEG/BMP can't, so flatten onto white to avoid corrupted output.
			if (!"png".equals(format)) {
				image = toOpaque(image);
			}
			ImageIO.write(image, format, chooser.getSelectedFile());
		} catch (IOException ex) {
			UIUtil.showException(this, "Export Error", ex);
		}
	}

	private void copyMapToClipboard() {
		final BufferedImage image = mapOverlayPanel.renderToImage();
		Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new Transferable() {
			@Override public DataFlavor[] getTransferDataFlavors() { return new DataFlavor[] {DataFlavor.imageFlavor}; }
			@Override public boolean isDataFlavorSupported(DataFlavor f) { return DataFlavor.imageFlavor.equals(f); }
			@Override public Object getTransferData(DataFlavor f) throws UnsupportedFlavorException {
				if (!isDataFlavorSupported(f)) throw new UnsupportedFlavorException(f);
				return image;
			}
		}, null);
	}

	/** Flattens a (possibly translucent) image onto an opaque white RGB image for alpha-less formats. */
	private static BufferedImage toOpaque(BufferedImage src) {
		BufferedImage rgb = new BufferedImage(src.getWidth(), src.getHeight(), BufferedImage.TYPE_INT_RGB);
		Graphics2D g = rgb.createGraphics();
		try {
			g.setColor(Color.WHITE);
			g.fillRect(0, 0, src.getWidth(), src.getHeight());
			g.drawImage(src, 0, 0, null);
		} finally {
			g.dispose();
		}
		return rgb;
	}

	/**
	 * Re-scans {@link #rowBrowser}'s rows for spatial data and updates the map overlay accordingly.
	 * Rows are loaded asynchronously (see {@link BrowserContentPane.LoadJob}), so at construction
	 * time {@link #rowBrowser}'s rows are typically still empty - this must be called again once
	 * they have actually arrived (i.e. from {@code afterReload()}).
	 */
	public void refreshMapOverlay() {
		// A fresh data load clears a prior user "Close" dismissal - the map is eligible to show again.
		mapOverlayUserHidden = false;
		RowScanResult scan = collectSpatialGeometries(rowBrowser == null ? null : rowBrowser.rows);
		rowByGeometry = scan.rowByGeometry;
		List<Geometry> spatialGeometries = new ArrayList<Geometry>(rowByGeometry.keySet());
		boolean hasSpatialData = !spatialGeometries.isEmpty();
		Geometry combined = hasSpatialData
				? (spatialGeometries.size() == 1
						? spatialGeometries.get(0)
						: new Geometry.GeometryCollection(spatialGeometries.get(0).getSrid(), spatialGeometries.toArray(new Geometry[0])))
				: null;
		mapOverlayPanel.setGeometry(combined);
		refreshLegendColumnComboItems(scan.spatialColumnIndices);
		updateTooltipsAndLegend();
		updateMapOverlayVisibility();
	}

	/**
	 * The geo-map overlay is shown only when it has spatial data AND the "Rows" or "Columns" tab is
	 * selected - it must not float over the Chart/Text/Meta tabs. GridBagLayout skips invisible
	 * components entirely when computing bounds, so it never gave mapOverlayPanel real bounds while
	 * hidden - becoming visible alone doesn't trigger a new layout pass, it has to be requested here.
	 */
	private void updateMapOverlayVisibility() {
		Component selected = tabbedPane.getSelectedComponent();
		boolean onMapTab = selected == contentPanel || selected == columnsPanel;
		boolean visible = onMapTab && mapOverlayPanel.getGeometry() != null && !mapOverlayUserHidden;
		if (mapOverlayPanel.isVisible() != visible) {
			mapOverlayPanel.setVisible(visible);
			jLayeredPane1.revalidate();
			jLayeredPane1.repaint();
		}
	}

	/**
	 * Rebuilds {@link #legendColumnCombo}'s items from the current result set's columns, excluding
	 * every column that ever held a spatial value ({@code spatialColumnIndices}, from the same scan
	 * that collected the geometries - "keine Geoobjektspalte anbieten"). Preserves the current
	 * selection by name if this instance already had one selected; on the very first populate (a
	 * brand new tab/query), falls back to the remembered {@link #lastLegendColumnName} instead.
	 */
	private void refreshLegendColumnComboItems(Set<Integer> spatialColumnIndices) {
		if (theRowsTable == null) {
			return;
		}
		TableModel model = theRowsTable.getModel();
		boolean firstPopulate = legendColumnCombo.getItemCount() == 0;
		String previousSelection = (String) legendColumnCombo.getSelectedItem();
		suppressLegendComboEvents = true;
		try {
			legendColumnCombo.removeAllItems();
			legendColumnCombo.addItem(NO_LEGEND_COLUMN);
			for (int i = 0; i < model.getColumnCount(); i++) {
				if (spatialColumnIndices.contains(i)) {
					continue;
				}
				legendColumnCombo.addItem(stripHtml(model.getColumnName(i)));
			}
			String toSelect = firstPopulate ? lastLegendColumnName : previousSelection;
			boolean found = false;
			if (toSelect != null) {
				for (int i = 0; i < legendColumnCombo.getItemCount(); i++) {
					if (toSelect.equals(legendColumnCombo.getItemAt(i))) {
						legendColumnCombo.setSelectedItem(toSelect);
						found = true;
						break;
					}
				}
			}
			if (!found) {
				legendColumnCombo.setSelectedItem(NO_LEGEND_COLUMN);
			}
		} finally {
			suppressLegendComboEvents = false;
		}
		boolean legendSelected = !NO_LEGEND_COLUMN.equals(legendColumnCombo.getSelectedItem());
		legendPaletteCombo.setEnabled(legendSelected);
		legendFontSizeSpinner.setEnabled(legendSelected);
	}

	/**
	 * Rebuilds the map overlay's tooltips (legend column first, if selected - "im Tooltip soll die
	 * Bezeichnungsspalte ganz oben angezeigt werden") and, if a legend column is selected, the
	 * per-row color assignment/legend list (from {@link #legendPaletteCombo}, reusing the exact
	 * same palettes as the "Chart" tab via {@link ChartColorPalettes}) - called after every data
	 * reload and whenever either combobox selection changes.
	 */
	private void updateTooltipsAndLegend() {
		if (theRowsTable == null) {
			return;
		}
		TableModel model = theRowsTable.getModel();
		String legendColumnName = (String) legendColumnCombo.getSelectedItem();
		boolean hasLegend = legendColumnName != null && !NO_LEGEND_COLUMN.equals(legendColumnName);
		int legendColumnIndex = hasLegend ? findColumnIndexByName(model, legendColumnName) : -1;

		Color[] palette = hasLegend ? ChartColorPalettes.getPalette((String) legendPaletteCombo.getSelectedItem()) : null;
		boolean effectiveLegend = hasLegend && palette != null && palette.length > 0;

		// One legend entry/color per DISTINCT label value - rows sharing the same value share the
		// same color ("fasse alle Geoobjekte einer Spalte in einem Legendeneintrag zusammen"). Only
		// rows that actually have a geo-object are ever considered here, since rowByGeometry (by
		// construction, see collectSpatialGeometries) never holds a row without one ("lass rows weg,
		// die keine Geoobjekte beinhalten").
		Map<String, Color> colorByValue = null;
		List<GeometryPreviewPanel.LegendEntry> legendEntries = null;
		if (effectiveLegend) {
			colorByValue = new LinkedHashMap<String, Color>();
			for (Row row : rowByGeometry.values()) {
				String label = legendLabelFor(row, legendColumnIndex);
				if (!colorByValue.containsKey(label)) {
					colorByValue.put(label, palette[colorByValue.size() % palette.length]);
				}
			}
			legendEntries = new ArrayList<GeometryPreviewPanel.LegendEntry>();
			for (Map.Entry<String, Color> e : colorByValue.entrySet()) {
				legendEntries.add(new GeometryPreviewPanel.LegendEntry(e.getKey(), e.getValue()));
			}
		}

		Map<Geometry, String> tooltipsByGeometry = new LinkedHashMap<Geometry, String>();
		Map<Geometry, Color> colorByGeometry = effectiveLegend ? new java.util.IdentityHashMap<Geometry, Color>() : null;
		for (Map.Entry<Geometry, Row> e : rowByGeometry.entrySet()) {
			Row row = e.getValue();
			Color rowColor = effectiveLegend ? colorByValue.get(legendLabelFor(row, legendColumnIndex)) : null;
			if (effectiveLegend) {
				colorByGeometry.put(e.getKey(), rowColor);
			}
			// The legend column's own row gets its swatch color rendered in the tooltip too
			// ("nutze in tooltip für Legendenspalte die Legendenfarbe") - null (no swatch) when
			// there's no effective legend, same tooltip as before this feature existed.
			tooltipsByGeometry.put(e.getKey(), buildRowTooltipHtml(row, model, legendColumnIndex, rowColor));
		}
		mapOverlayPanel.setTooltipProvider(tooltipsByGeometry::get);
		mapOverlayPanel.setLegend(legendEntries, effectiveLegend ? colorByGeometry::get : null);
	}

	private static String legendLabelFor(Row row, int legendColumnIndex) {
		if (legendColumnIndex < 0 || legendColumnIndex >= row.values.length) {
			return "";
		}
		Object rawLabel = row.values[legendColumnIndex];
		return rawLabel == null ? "" : String.valueOf(rawLabel);
	}

	/**
     * Creates new form TabContentPanel.
     *
     * @param rowBrowser the browser content pane providing the row data
     * @param rowsCount label to display the row count
     * @param theRowsTable the rows table to display
     * @param metaDataDetails the meta data details component
     * @param type the result type identifier
     * @param explain <code>true</code> if this panel shows an explain plan
     * @param shimPanel optional shim panel, or <code>null</code>
     * @param caretDotMark caret position marker as a pair of line and column, or <code>null</code>
     * @param rowColumnTypes the SQL types of the row columns
     * @param onlySelectedCells <code>true</code> to show only selected cells
     * @param silent <code>true</code> to suppress user interaction
     */
    public TabContentPanel(BrowserContentPane rowBrowser, JLabel rowsCount, JTable theRowsTable, JComponent metaDataDetails, String type, boolean explain, javax.swing.JPanel shimPanel, Pair<Integer, Integer> caretDotMark, List<Integer> rowColumnTypes, boolean onlySelectedCells, boolean silent) {
    	this.rowBrowser = rowBrowser;
    	this.shimPanel = shimPanel == null? new javax.swing.JPanel(new GridBagLayout()) : shimPanel;
    	this.caretDotMark = caretDotMark;
        this.rowColumnTypes = rowColumnTypes;
        this.onlySelectedCells = onlySelectedCells;
        this.theRowsTable = theRowsTable;
        this.silent = silent;
        
        initComponents(); UIUtil.initComponents(this);
        controlsContainer.setFloatable(false);
        if (UIUtil.plaf.isFlat) {
			tabbedPane.putClientProperty(FlatClientProperties.TABBED_PANE_TAB_HEIGHT, 16);
		}
        loadingPanel.setVisible(false);

        textArea = new RSyntaxTextAreaWithSQLSyntaxStyle(
				false, false) {
			protected boolean withModifingMenuItems() {
				return false;
			}
		};
		textArea.setSyntaxEditingStyle(null);
		textArea.setEditable(false);
		
        textViewScrollPane.setViewportView(textArea);
        
        cancelLoadButton.setIcon(UIUtil.scaleIcon(cancelLoadButton, cancelIcon));
        copyCBButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/copy.png")));
		
        statementLabel = new JLabel();
        statementLabel.setForeground(Colors.Color_100_110_210);
        GridBagConstraints gridBagConstraints;
        
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 10;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.anchor = explain? java.awt.GridBagConstraints.WEST : java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new Insets(0, explain? 8 : 16, 0, 2);
		//        panel.add(statementLabel, gridBagConstraints);
        panel.add(new JPanel() {
	        	{
	        		setLayout(null);
	        		setOpaque(false);
	        		statementLabel.addPropertyChangeListener("text", e -> resize());
	        		addComponentListener(new ComponentListener() {
						@Override
						public void componentShown(ComponentEvent e) {
						}
						@Override
						public void componentResized(ComponentEvent e) {
							resize();
						}
						@Override
						public void componentMoved(ComponentEvent e) {
						}
						@Override
						public void componentHidden(ComponentEvent e) {
						}
					});
	        		statementLabel.setName("statementLabel");
		        	add(statementLabel);
		    	}
	        	
				private void resize() {
					Rectangle visibleRect = getBounds();
					int x = Math.max(32, visibleRect.width - statementLabel.getPreferredSize().width);
					statementLabel.setBounds(x, 0, visibleRect.width - x, visibleRect.height);
				}
        	}
        	, gridBagConstraints);
//        gridBagConstraints = new java.awt.GridBagConstraints();
//        gridBagConstraints.gridx = 20;
//        gridBagConstraints.gridy = 1;
//        gridBagConstraints.insets = new Insets(0, 10, 0, 10);
//        gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
//        panel.add(rowsCount, gridBagConstraints);
        
        ComboBoxModel<String> sepModel = new DefaultComboBoxModel<String>(
        		new String[] {
        				"",
        				";",
        				",",
        				"|",
        				"!",
        				SEPARATOR_TAB,
        				SEPARATOR_SPACE
        		});
		columnSeparatorComboBox.setModel(sepModel);
		if (!silent && !explain && lastColumnSeparator != null && !"".equals(lastColumnSeparator)) {
			columnSeparatorComboBox.setSelectedItem(lastColumnSeparator);
		}
	    Component ec = columnSeparatorComboBox.getEditor().getEditorComponent();
		if (ec instanceof JTextField) {
			((JTextField) ec).getDocument().addDocumentListener(new DocumentListener() {
				@Override
				public void removeUpdate(DocumentEvent e) {
					update();
				}
				@Override
				public void insertUpdate(DocumentEvent e) {
					update();
				}
				@Override
				public void changedUpdate(DocumentEvent e) {
					update();
				}
				private void update() {
					if (TabContentPanel.this.theRowsTable != null) {
						updateTextView(TabContentPanel.this.theRowsTable);
					}
				}
			});
		}
		
		if (!silent && !explain && lastHeaderCheckBoxIsSelected != null) {
			headerCheckBox.setSelected(lastHeaderCheckBoxIsSelected);
		}
		
		JScrollPane scrollPane = new JScrollPane();
		RSyntaxTextArea area = new RSyntaxTextAreaWithTheme();
		area.setBracketMatchingEnabled(false);

		area.setEditable(false);
		scrollPane.setViewportView(area);
		area.setSyntaxEditingStyle(SyntaxConstants.SYNTAX_STYLE_SQL);
		area.setText(type);
		area.setCaretPosition(0);
		typePanel.add(scrollPane); 
		
		if (explain) {
			tabbedPane.remove(contentPanel);
			tabbedPane.remove(columnsPanel);
			tabbedPane.remove(metaTabPanel);
			columnSeparatorLabel.setVisible(false);
			headerCheckBox.setVisible(false);
			copyCBButton.setVisible(false);
			rotateCheckBox.setVisible(false);
			columnSeparatorComboBox.setVisible(false);
			// controlsContainer.setVisible(false);
			rowsCount.setVisible(false);
		} else {
			if (metaDataDetails != null) {
				metaPanel.add(metaDataDetails);
			}
			chartPanel = new SQLConsoleChartPanel(rowColumnTypes);
			tabbedPane.insertTab("Chart", null, chartPanel, null, tabbedPane.indexOfComponent(metaTabPanel));
			tabbedPane.addChangeListener(e -> {
				if (tabbedPane.getSelectedComponent() == chartPanel && theRowsTable != null) {
					chartPanel.setTable(theRowsTable);
				}
			});
		}

		mapOverlayPanel = new GeometryPreviewPanel();
		mapOverlayPanel.setPreferredSize(new java.awt.Dimension(328, 218));
		// Without an explicit minimum size, GridBagLayout shrinks this zero-weight, fill=NONE
		// component down toward (0,0) whenever tabbedPane's own real content (e.g. the "Chart" tab)
		// reports a preferred width larger than the container - pinning minimum = preferred gives
		// GridBagLayout a floor it won't shrink below, regardless of tabbedPane's content demands.
		mapOverlayPanel.setMinimumSize(new java.awt.Dimension(328, 218));
		mapOverlayPanel.setVisible(false);
		mapOverlayPanel.setClickAction(this::toggleMapOverlayExpanded);
		mapOverlayPanel.setCloseAction(() -> {
			mapOverlayUserHidden = true;
			updateMapOverlayVisibility();
		});
		mapOverlayPanel.setZoomAtCursor(true); // console map zooms about the mouse cursor
		mapOverlayPanel.setSettingsPanelEnabled(true);
		// Hide/show the map overlay when switching tabs - only "Rows"/"Columns" may show it.
		tabbedPane.addChangeListener(e -> updateMapOverlayVisibility());

		legendColumnCombo = new javax.swing.JComboBox<String>();
		legendColumnCombo.addActionListener(e -> {
			if (suppressLegendComboEvents) {
				return;
			}
			boolean hasLegend = !NO_LEGEND_COLUMN.equals(legendColumnCombo.getSelectedItem());
			legendPaletteCombo.setEnabled(hasLegend);
			legendFontSizeSpinner.setEnabled(hasLegend);
			lastLegendColumnName = hasLegend ? (String) legendColumnCombo.getSelectedItem() : null;
			updateTooltipsAndLegend();
		});

		legendPaletteCombo = new javax.swing.JComboBox<String>(ChartColorPalettes.GEO_PALETTE_NAMES);
		legendPaletteCombo.setEnabled(false);
		if (lastLegendPaletteName != null) {
			legendPaletteCombo.setSelectedItem(lastLegendPaletteName);
		}
		legendPaletteCombo.addActionListener(e -> {
			if (suppressLegendComboEvents) {
				return;
			}
			lastLegendPaletteName = (String) legendPaletteCombo.getSelectedItem();
			updateTooltipsAndLegend();
		});

		JPanel legendSettingsContent = new JPanel(new GridBagLayout());
		GridBagConstraints lgc = new GridBagConstraints();
		lgc.gridx = 0; lgc.gridy = 0; lgc.anchor = GridBagConstraints.WEST; lgc.insets = new Insets(2, 4, 2, 4);
		legendSettingsContent.add(new JLabel("Legend:"), lgc);
		lgc = new GridBagConstraints();
		lgc.gridx = 1; lgc.gridy = 0; lgc.fill = GridBagConstraints.HORIZONTAL; lgc.weightx = 1; lgc.insets = new Insets(2, 4, 2, 4);
		legendSettingsContent.add(legendColumnCombo, lgc);
		lgc = new GridBagConstraints();
		lgc.gridx = 0; lgc.gridy = 1; lgc.anchor = GridBagConstraints.WEST; lgc.insets = new Insets(2, 4, 2, 4);
		legendSettingsContent.add(new JLabel("Palette:"), lgc);
		lgc = new GridBagConstraints();
		lgc.gridx = 1; lgc.gridy = 1; lgc.fill = GridBagConstraints.HORIZONTAL; lgc.weightx = 1; lgc.insets = new Insets(2, 4, 2, 4);
		legendSettingsContent.add(legendPaletteCombo, lgc);

		legendFontSizeSpinner = new JSpinner(new SpinnerNumberModel(lastLegendFontSizeDelta, -3, 16, 1));
		legendFontSizeSpinner.setToolTipText("Legend text size relative to the default (-1, 0, +1, +2, ...)");
		legendFontSizeSpinner.setEnabled(false); // enabled only while a legend is shown (see below)
		legendFontSizeSpinner.addChangeListener(e -> {
			lastLegendFontSizeDelta = (Integer) legendFontSizeSpinner.getValue();
			mapOverlayPanel.setLegendFontSizeDelta(lastLegendFontSizeDelta);
		});
		mapOverlayPanel.setLegendFontSizeDelta(lastLegendFontSizeDelta);
		lgc = new GridBagConstraints();
		lgc.gridx = 0; lgc.gridy = 2; lgc.anchor = GridBagConstraints.WEST; lgc.insets = new Insets(2, 4, 2, 4);
		legendSettingsContent.add(new JLabel("Text size:"), lgc);
		lgc = new GridBagConstraints();
		lgc.gridx = 1; lgc.gridy = 2; lgc.fill = GridBagConstraints.HORIZONTAL; lgc.weightx = 1; lgc.insets = new Insets(2, 4, 2, 4);
		legendSettingsContent.add(legendFontSizeSpinner, lgc);

		JButton exportButton = new JButton("Save as image...", UIUtil.scaleIcon(UIUtil.readImage("/export.png"), 20, 18));
		exportButton.setToolTipText("Export map as image file or copy to clipboard");
		exportButton.addActionListener(e -> showMapExportMenu(exportButton));
		lgc = new GridBagConstraints();
		lgc.gridx = 0; lgc.gridy = 3; lgc.gridwidth = 2; lgc.fill = GridBagConstraints.HORIZONTAL; lgc.insets = new Insets(4, 4, 2, 4);
		legendSettingsContent.add(exportButton, lgc);
		mapOverlayPanel.setSettingsPanelContent(legendSettingsContent);

		mapOverlayCornerConstraints = new java.awt.GridBagConstraints();
		mapOverlayCornerConstraints.gridx = 1;
		mapOverlayCornerConstraints.gridy = 1;
		mapOverlayCornerConstraints.anchor = java.awt.GridBagConstraints.SOUTHEAST;
		mapOverlayCornerConstraints.insets = new Insets(0, 0, 6, 6);

		mapOverlayExpandedConstraints = new java.awt.GridBagConstraints();
		mapOverlayExpandedConstraints.gridx = 1;
		mapOverlayExpandedConstraints.gridy = 1;
		mapOverlayExpandedConstraints.fill = java.awt.GridBagConstraints.BOTH;
		mapOverlayExpandedConstraints.weightx = 1;
		mapOverlayExpandedConstraints.weighty = 1;

		// Above tabbedPane (default layer 0), below loadingPanel (PALETTE_LAYER=100) - so it floats
		// over whichever tab is selected, but the loading overlay still takes precedence while busy.
		jLayeredPane1.setLayer(mapOverlayPanel, 50);
		jLayeredPane1.add(mapOverlayPanel, mapOverlayCornerConstraints);

		refreshMapOverlay();

		this.shimPanel.removeAll();
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1;
        gridBagConstraints.weighty = 1;
        gridBagConstraints.fill = GridBagConstraints.BOTH;
        this.shimPanel.add(this, gridBagConstraints);
    }

    public static String toType(ResultSetMetaData metaData, Session session, ExecutionContext executionContext) throws Exception {
        StringBuilder columnTypes = new StringBuilder("(" + UIUtil.LINE_SEPARATOR);
    	String nullableContraint = session.dbms.getNullableContraint();
        for (int i = 0; i < metaData.getColumnCount(); ++i) {
        	if (i > 0) {
        		columnTypes.append(","+ UIUtil.LINE_SEPARATOR);
        	}
        	Column column = JDBCMetaDataBasedModelElementFinder.toColumn(metaData, i + 1, session);
        	String constraint;
        	if (nullableContraint != null) {
        		constraint = column.isNullable? " " + nullableContraint :  " NOT NULL";
            } else {
            	constraint = column.isNullable? "" : " NOT NULL";
            }
			columnTypes.append("     "
					+ (column.name == null ? ""
							: column.name.replaceAll("\\s+", "").replaceFirst("^(.{64})....*", "$1..."))
					+ " " 
					+ (column.name == null ? ""
							: column.toSQL("").substring(column.name.length()).trim()) + constraint);
        }
        columnTypes.append(UIUtil.LINE_SEPARATOR + ")");
        return columnTypes.toString();
    }

    public static ResultSet toMetaDataResultSet(ResultSetMetaData metaData, Session session, ExecutionContext executionContext) throws Exception {
        List<Object[]> rowList = new ArrayList<Object[]>();
        
        String[] names = new String[] {
        		"getColumnLabel",
        		"getColumnTypeName",
        		"getPrecision",
        		"getScale",
        		"getColumnType",
        		"isAutoIncrement",
        		"isCaseSensitive",
        		"isNullable",
        		"isReadOnly",
        		"isSigned"
	    };

        String[] titel = new String[names.length];
        int[] types = new int[names.length];
    	int i = 0;
        for (String name: names) {
    		titel[i] = name.startsWith("get")? name.substring(3) : name;
    		types[i] = Types.VARCHAR;
    		++i;
        }
        for (int col = 0; col < metaData.getColumnCount(); ++col) {
        	Object[] row = new Object[names.length];
        	i = 0;
        	try {
        		row[i] = metaData.getColumnLabel(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getColumnTypeName(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getPrecision(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getScale(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.getColumnType(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isAutoIncrement(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		// not necessary, MySQL driver throws exception here, row[i] = metaData.isCaseSensitive(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isNullable(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isReadOnly(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
        	try {
        		row[i] = metaData.isSigned(col + 1);
        	} catch (Throwable t) {
        		logger.info("error", t);
        	}
        	++i;
	        rowList.add(row);
        }

        return new MemorizedResultSet(rowList, names.length, titel, types);
    }

    private JTable theRowsTable = null;
    protected boolean forDetailsView = false;
    private boolean columnNamesInFirstRow = false;
    
    public void setColumnNamesInFirstRow(boolean columnNamesInFirstRow) {
		this.columnNamesInFirstRow = columnNamesInFirstRow;
	}

	private static Boolean lastHeaderCheckBoxIsSelected;
	private static Object lastColumnSeparator;
    private final boolean onlySelectedCells;
    
    private int rowAndColumnsLimit = Integer.MAX_VALUE;
    
    public void setRowAndColumnsLimit(int rowAndColumnsLimit) {
		this.rowAndColumnsLimit = rowAndColumnsLimit;
	}

	public void updateTextView(JTable rowsTable) {
    	theRowsTable = rowsTable;
		if (chartPanel != null && tabbedPane.getSelectedComponent() == chartPanel) {
			chartPanel.setTable(rowsTable);
		}
		Object sep = getSeparatorFromCombobox();
    	StringBuilder sb = createContent(rowsTable, sep, false, false, null, rowAndColumnsLimit, rowAndColumnsLimit, new boolean[2]);

		Point vPos = textViewScrollPane.getViewport().getViewPosition();
		textArea.setText(sb.toString());
		textArea.setCaretPosition(0);
		textArea.setEditable(false);
		textArea.discardAllEdits();
		if (UIUtil.plaf.isFlat) {
			textArea.setBackground(Colors.Color_255_255_255);
		}
		UIUtil.invokeLater(() -> textViewScrollPane.getViewport().setViewPosition(vPos));
	}

	/**
	 * @return
	 */
	private Object getSeparatorFromCombobox() {
		Object sep = columnSeparatorComboBox.getEditor().getItem();
		if (!silent && columnSeparatorComboBox.isVisible()) {
			lastColumnSeparator = sep;
		}
		if ("".equals(sep)) {
			sep = null;
		} else if (SEPARATOR_SPACE.equals(sep)) {
			sep = " ";
		} else if (SEPARATOR_TAB.equals(sep)) {
			sep = "\t";
		}
		return sep;
	}

	private StringBuilder createContent(JTable rowsTable, Object sep, boolean aligned, boolean colored, Integer cellpadding, int maxColumns, int maxRows, boolean[] stopped) {
		boolean html = false;
    	if ("HTML".equals(sep)) {
    		sep = "";
    		html = true;
    	}
    	TableColumnModel cm = rowsTable.getColumnModel();
    	TableModel rDm = rowsTable.getModel();
    	RowSorter<? extends TableModel> sorter = rowsTable.getRowSorter();
		int startI = 0;
		boolean incHeader = headerCheckBox.isSelected();
		boolean rotate = rotateCheckBox.isSelected();
    	if (!silent && headerCheckBox.isVisible()) {
			lastHeaderCheckBoxIsSelected = incHeader;
		}
    	if (!silent && rotateCheckBox.isVisible()) {
//    		lastRotated = rotateCheckBox.isVisible();
    	}
		if (forDetailsView) {
			if (!incHeader) {
				startI = cm.getColumnCount() - 1;
			} else if (rotate) {
				incHeader = false;
			}
		}
		String[][] cell = columnNamesInFirstRow? (rotate? new String[rDm.getColumnCount()][] : new String[sorter.getViewRowCount()][]) : rotate? new String[rDm.getColumnCount()][] : new String[sorter.getViewRowCount() + 1][];
    	int[] maxLength = columnNamesInFirstRow? (rotate? new int[sorter.getViewRowCount()] : new int[rDm.getColumnCount()]) : rotate? new int[sorter.getViewRowCount() + 1] : new int[rDm.getColumnCount()];
    	for (int y = 0; y < cell.length; ++y) {
			cell[y] = columnNamesInFirstRow? (rotate? new String[sorter.getViewRowCount()] : new String[rDm.getColumnCount()]) : rotate? new String[sorter.getViewRowCount() + 1] : new String[rDm.getColumnCount()];
    	}
    	int pSHX = -1;
    	int yCount = 0;
    	for (int y = 0; y < cell.length; ++y) {
    		if (yCount > maxRows) {
    			cell[y][0] = html? null : "Preview stops here...";
    			stopped[1] = true;
    			if (pSHX > 0) {
    				for (int x = 1; x < pSHX; ++x) {
    					cell[y][x] = "";
    				}
    				cell[y][pSHX] = cell[y][0];
    			}
    			break;
    		}
    		int xCount = 0;
        	for (int x = 0; x < cell[y].length; ++x) {
				int mx;
				int my;
				int vx;
				int vy;
				String colName;
				boolean isColName;
				if (columnNamesInFirstRow) {
					if (!rotate) {
						isColName = x == 0;
						vx = x - 1;
						vy = y;
						mx = x == 0? 0 : cm.getColumn(x - 1).getModelIndex();
						my = sorter.convertRowIndexToModel(y);
						colName = String.valueOf(rDm.getValueAt(my, 0));
					} else {
						isColName = y == 0;
						vx = y - 1;
						vy = x;
						mx = y == 0? 0 : cm.getColumn(y - 1).getModelIndex();
						my = x == 0? 0 : sorter.convertRowIndexToModel(x);
						colName = String.valueOf(rDm.getValueAt(my, 0));
					}
				} else {
					if (rotate) {
						isColName = x == 0;
						vx = y;
						vy = x - 1;
						mx = cm.getColumn(y).getModelIndex();
						my = x == 0? 0 : sorter.convertRowIndexToModel(x - 1);
						colName = rDm.getColumnName(mx);
					} else {
						isColName = y == 0;
						vx = x;
						vy = y - 1;
						mx = cm.getColumn(x).getModelIndex();
						my = y == 0? 0 : sorter.convertRowIndexToModel(y - 1);
						colName = rDm.getColumnName(mx);
					}
				}
				Object value;
				if (isColName) {
					if (onlySelectedCells) {
						if (columnNamesInFirstRow && !rowsTable.isRowSelected(vy)
								||
							!columnNamesInFirstRow && !rowsTable.isColumnSelected(vx)) {
							continue;
						}
					}
					value = colName;
					if (value != null && value.toString().startsWith("<html>")) {
						String[] ntPair = value.toString().replaceAll("<br>", "\t").replaceAll("<[^>]*>", "").split("\t");
						if (ntPair.length == 2) {
							value = ntPair[0];
						}
						if (ntPair.length == 3) {
							value = ntPair[1];
						}
					}
				} else {
					if (onlySelectedCells && !rowsTable.isCellSelected(vy, vx)) {
						continue;
					}
					value = rDm.getValueAt(my, mx);
				}
				if (value instanceof TableModelItem) {
					Object v = ((TableModelItem) value).value;
					if (sep != null || (v == UIUtil.NULL || v == null)) {
						value = v;
					} else {
						value = value.toString().replace((char) 182, '\n');
						value = value.toString().replace(UIUtil.spaceIndicator, ' ');
						value = value.toString().replace(UIUtil.tabIndicator, '\t');
						value = rtrimRegExpr.matcher(value.toString()).replaceAll("");
					}
				}
				String cellContent = value == UIUtil.NULL || value == null? "" : value.toString();
				if (sep != null && !html) {
					if (cellContent.contains(sep.toString()) || cellContent.indexOf("\n") >= 0 || cellContent.indexOf("\r") >= 0 || cellContent.indexOf("\"") >= 0) {
						cellContent = "\"" + (cellContent.replaceAll("\"", "\"\"")) + "\"";
					}
				}
				 ++xCount;
				boolean stop = xCount > maxColumns;
				if (stop) {
					cellContent = "";
	    			stopped[0] = true;
	    			if (yCount == 1 && !html) {
						cellContent = "Preview stops here...";
					} else {
						cellContent = "";
					}
				}
				cell[y][x] = cellContent;
				maxLength[x] = Math.max(maxLineLength(cellContent), maxLength[x]);
				pSHX = Math.max(pSHX, x);
				if (stop) {
					break;
				}
			}
        	if (xCount > 0) {
        		++yCount;
        	}
		}
		StringBuilder sb = new StringBuilder();
		int cellsRendered = 0;
		synchronized (rowColumnTypes) {
			if (html) {
				sb.append("<html><body><table" + (cellpadding != null? " cellpadding=\"" + cellpadding + "\"" : "") + ">");
			}
			int rowNum = 0;
			if (!columnNamesInFirstRow) {
				if (incHeader && !rotate) {
					++rowNum;
				}
			}
			for (int y = !rotate? 0 : startI; y < cell.length; ++y) {
				int sbLength = sb.length();
				for (int x = !rotate? startI : 0; x < cell[y].length; ++x) {
					if (cell[y][x] == null) {
						continue;
					}
					if (columnNamesInFirstRow && (!rotate && x == 0 || rotate && y == 0) && !incHeader) {
						continue;
					}
					if (!columnNamesInFirstRow && (rotate && x == 0 || !rotate && y == 0) && !incHeader) {
						continue;
					}
					boolean rightAlign = false;
					if (!rotate && !columnNamesInFirstRow) {
						int mx = cm.getColumn(x).getModelIndex();
						if (rowColumnTypes.size() > mx) {
							switch (rowColumnTypes.get(mx)) {
							case Types.BIGINT:
							case Types.DECIMAL:
							case Types.DOUBLE:
							case Types.FLOAT:
							case Types.INTEGER:
							case Types.NUMERIC:
							case Types.REAL:
							case Types.SMALLINT:
								rightAlign = true;
							}
						}
					} else if (rotate && columnNamesInFirstRow) {
						int mx = sorter.convertRowIndexToModel(x);
						if (rowColumnTypes.size() > mx) {
							switch (rowColumnTypes.get(mx)) {
							case Types.BIGINT:
							case Types.DECIMAL:
							case Types.DOUBLE:
							case Types.FLOAT:
							case Types.INTEGER:
							case Types.NUMERIC:
							case Types.REAL:
							case Types.SMALLINT:
								rightAlign = true;
							}
						}
					}
					if (sep != null) {
						if (html) {
							if (sb.length() > sbLength) {
								sb.append("</td><td");
								if (columnNamesInFirstRow) {
									++rowNum;
								}
							} else {
								sb.append("<tr><td");
								if (columnNamesInFirstRow) {
									rowNum = 0;
									if (!incHeader || rotate) {
										++rowNum;
									}
								} else {
									++rowNum;
								}
							}
							if (incHeader && ((rotate ^ columnNamesInFirstRow)? (x == 0) : (y == 0))) {
								if (colored) {
									sb.append(" bgcolor=\"#eeeeff\"");
								}
							} else {
								if (colored) {
									sb.append(rowNum % 2 == 0? "" : " bgcolor=\"#eeffee\"");
								}
							}
							if (aligned) {
								sb.append(rightAlign? " align=\"right\"" : "");
							}
							sb.append(">" + UIUtil.toHTMLFragment(cell[y][x], 0, true, false));
						} else {
							if (sb.length() > sbLength) {
								sb.append(sep);
							}
							sb.append(cell[y][x]);
						}
					} else {
						if (x < cell[y].length - 1) {
							if (sb.length() > sbLength) {
								sb.append(" ");
							}
							if (rightAlign) {
								for (int i = maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
									sb.append(" ");
								}
								sb.append(cell[y][x]);
							} else {
								sb.append(cell[y][x]);
								for (int i = maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
									sb.append(" ");
								}
							}
							sb.append(" ");
						} else {
							if (cell[y].length != 1) {
								sb.append(" ");
							}
							if (rightAlign) {
								for (int i = maxLength[x] - lastLineLength(cell[y][x]); i > 0; --i) {
									sb.append(" ");
								}
							}
							sb.append(cell[y][x]);
						}
						++cellsRendered;
					}
				}
				if (sb.length() > sbLength) {
					if (html) {
						sb.append("</td></tr>");
					}
					sb.append(UIUtil.LINE_SEPARATOR);
				}
				if (y == 0 && sep == null && incHeader && !(rotate ^ columnNamesInFirstRow)) {
					int o = 2;
					for (int x = 0; x < cell[y].length; ++x) {
						if (maxLength[x] > 0) {
							for (int i = o; i < 2 + maxLength[x]; ++i) {
								sb.append(i < 2? " " : "-");
							}
							o = 0;
						}
					}
					sb.append(UIUtil.LINE_SEPARATOR);
				}
			}
		}
		if (html) {
			sb.append("</table></body></html>");
		} else if (cellsRendered == 1) {
			sb = new StringBuilder(sb.toString().trim());
		}
		return sb;
	}

    private int lastLineLength(String cellContent) {
		int i = cellContent.lastIndexOf('\n');
		int l = cellContent.length();
		if (i >= 0) {
			return l - i - 1;
		}
		return l;
	}

	private int maxLineLength(String cellContent) {
    	int l = cellContent.length();
    	int currentLength = 0;
    	int maxLength = 0;
		for (int i = 0; i < l; ++i) {
			char c = cellContent.charAt(i);
			if (c == '\n') {
				currentLength = 0;
			} else {
				++currentLength;
			}
			if (maxLength < currentLength) {
				maxLength = currentLength;
			}
		}
		return maxLength;
	}

	public String getHTMLContent(JTable rowsTable, boolean aligned, boolean colored, Integer cellpadding, int maxColumns, int maxRows, boolean[] stopped) {
		return createContent(rowsTable, "HTML", aligned, colored, cellpadding, maxColumns, maxRows, stopped).toString();
	}
	
	public String getPlainContent(JTable rowsTable, boolean aligned, boolean colored, int maxColumns, int maxRows, boolean[] stopped) {
		return createContent(rowsTable, getSeparatorFromCombobox(), aligned, colored, null, maxColumns, maxRows, stopped).toString();
	}

	/**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        panel = new javax.swing.JPanel();
        controlsContainer = new javax.swing.JToolBar();
        jPanel1 = new javax.swing.JPanel();
        jLayeredPane1 = new javax.swing.JLayeredPane();
        loadingPanel = new javax.swing.JPanel();
        cancelLoadButton = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jPanel13 = new javax.swing.JPanel();
        loadingCauseLabel = new javax.swing.JLabel();
        loadingLabel = new javax.swing.JLabel();
        tabbedPane = new javax.swing.JTabbedPane();
        contentPanel = new javax.swing.JPanel();
        columnsPanel = new javax.swing.JPanel();
        columnsScrollPane = new javax.swing.JScrollPane();
        columnsSortedStateLabel = new javax.swing.JLabel();
        jPanel2 = new javax.swing.JPanel();
        fullTSearchPanel = new javax.swing.JPanel();
        columnsTabRowCountPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        textTabPanel = new javax.swing.JPanel();
        textViewScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        columnSeparatorComboBox = new javax.swing.JComboBox<>();
        copyCBButton = new javax.swing.JButton();
        columnSeparatorLabel = new javax.swing.JLabel();
        headerCheckBox = new javax.swing.JCheckBox();
        textSortedStateLabel = new javax.swing.JLabel();
        rotateCheckBox = new javax.swing.JCheckBox();
        textTabRowCountPanel = new javax.swing.JPanel();
        metaTabPanel = new javax.swing.JPanel();
        metaPanel = new javax.swing.JPanel();
        typePanel = new javax.swing.JPanel();

        setLayout(new java.awt.GridBagLayout());

        panel.setLayout(new java.awt.GridBagLayout());

        controlsContainer.setRollover(true);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        panel.add(controlsContainer, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        add(panel, gridBagConstraints);

        jPanel1.setLayout(new java.awt.GridBagLayout());

        jLayeredPane1.setLayout(new java.awt.GridBagLayout());

        loadingPanel.setBackground(/* Renaming also in *.form! */ Colors.Color_255_255_255_150);
        loadingPanel.setLayout(new java.awt.GridBagLayout());

        cancelLoadButton.setText("Cancel");
        cancelLoadButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                cancelLoadButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        loadingPanel.add(cancelLoadButton, gridBagConstraints);

        jLabel2.setText(" ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        loadingPanel.add(jLabel2, gridBagConstraints);

        jPanel13.setOpaque(false);
        jPanel13.setLayout(new java.awt.GridBagLayout());

        loadingCauseLabel.setFont(loadingCauseLabel.getFont().deriveFont(loadingCauseLabel.getFont().getSize()+3f));
        loadingCauseLabel.setForeground(/* Renaming also in *.form! */ Colors.Color_141_16_16);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel13.add(loadingCauseLabel, gridBagConstraints);

        loadingLabel.setFont(loadingLabel.getFont().deriveFont(loadingLabel.getFont().getStyle() | java.awt.Font.BOLD, loadingLabel.getFont().getSize()+3));
        loadingLabel.setForeground(/* Renaming also in *.form! */ Colors.Color_141_16_16);
        loadingLabel.setText("loading...     ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel13.add(loadingLabel, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(8, 8, 8, 8);
        loadingPanel.add(jPanel13, gridBagConstraints);

        jLayeredPane1.setLayer(loadingPanel, javax.swing.JLayeredPane.PALETTE_LAYER);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(4, 4, 2, 4);
        jLayeredPane1.add(loadingPanel, gridBagConstraints);

        tabbedPane.setTabPlacement(javax.swing.JTabbedPane.BOTTOM);

        contentPanel.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
        contentPanel.setLayout(new java.awt.BorderLayout());
        tabbedPane.addTab("Rows", contentPanel);

        columnsPanel.setLayout(new java.awt.BorderLayout());
        columnsPanel.add(columnsScrollPane, java.awt.BorderLayout.CENTER);

        columnsSortedStateLabel.setForeground(/* Renaming also in *.form! */ Colors.Color_0_0_255);
        columnsSortedStateLabel.setText(" ");
        columnsPanel.add(columnsSortedStateLabel, java.awt.BorderLayout.NORTH);

        jPanel2.setLayout(new java.awt.GridBagLayout());

        fullTSearchPanel.setLayout(new javax.swing.BoxLayout(fullTSearchPanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        jPanel2.add(fullTSearchPanel, gridBagConstraints);

        columnsTabRowCountPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        jPanel2.add(columnsTabRowCountPanel, gridBagConstraints);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.weightx = 1.0;
        jPanel2.add(jLabel1, gridBagConstraints);

        columnsPanel.add(jPanel2, java.awt.BorderLayout.PAGE_END);

        tabbedPane.addTab("Columns", columnsPanel);

        textTabPanel.setLayout(new java.awt.GridBagLayout());

        textViewScrollPane.setEnabled(false);
        textViewScrollPane.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N

        jTextArea.setColumns(20);
        jTextArea.setFont(new java.awt.Font("Monospaced", 0, 12)); // NOI18N
        jTextArea.setRows(5);
        textViewScrollPane.setViewportView(jTextArea);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 6;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 2, 0);
        textTabPanel.add(textViewScrollPane, gridBagConstraints);

        columnSeparatorComboBox.setEditable(true);
        columnSeparatorComboBox.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 4);
        textTabPanel.add(columnSeparatorComboBox, gridBagConstraints);

        copyCBButton.setText("Copy to Clipboard");
        copyCBButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                copyCBButtonActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 5;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 2);
        textTabPanel.add(copyCBButton, gridBagConstraints);

        columnSeparatorLabel.setText("Separator ");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.EAST;
        gridBagConstraints.insets = new java.awt.Insets(0, 4, 3, 0);
        textTabPanel.add(columnSeparatorLabel, gridBagConstraints);

        headerCheckBox.setSelected(true);
        headerCheckBox.setText("With Headings");
        headerCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                headerCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 3;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        textTabPanel.add(headerCheckBox, gridBagConstraints);

        textSortedStateLabel.setForeground(/* Renaming also in *.form! */ Colors.Color_0_0_255);
        textSortedStateLabel.setText("jLabel2");
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 0;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        textTabPanel.add(textSortedStateLabel, gridBagConstraints);

        rotateCheckBox.setText("Rotated");
        rotateCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rotateCheckBoxActionPerformed(evt);
            }
        });
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 4;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.anchor = java.awt.GridBagConstraints.WEST;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        textTabPanel.add(rotateCheckBox, gridBagConstraints);

        textTabRowCountPanel.setLayout(new java.awt.GridBagLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 2;
        gridBagConstraints.insets = new java.awt.Insets(0, 0, 3, 0);
        textTabPanel.add(textTabRowCountPanel, gridBagConstraints);

        tabbedPane.addTab("Text", textTabPanel);

        metaTabPanel.setLayout(new java.awt.GridBagLayout());

        metaPanel.setLayout(new java.awt.BorderLayout());
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        metaTabPanel.add(metaPanel, gridBagConstraints);

        typePanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Type"));
        typePanel.setLayout(new javax.swing.BoxLayout(typePanel, javax.swing.BoxLayout.LINE_AXIS));
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 2;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        metaTabPanel.add(typePanel, gridBagConstraints);

        tabbedPane.addTab("Meta", metaTabPanel);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jLayeredPane1.add(tabbedPane, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        jPanel1.add(jLayeredPane1, gridBagConstraints);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 1;
        gridBagConstraints.gridy = 1;
        gridBagConstraints.gridwidth = 3;
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 1.0;
        gridBagConstraints.weighty = 1.0;
        add(jPanel1, gridBagConstraints);
    }// </editor-fold>//GEN-END:initComponents

    private void copyCBButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_copyCBButtonActionPerformed
    	StringSelection selection = new StringSelection(textArea.getText());
	    UIUtil.setClipboardContent(selection);
    }//GEN-LAST:event_copyCBButtonActionPerformed

    private void headerCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_headerCheckBoxActionPerformed
    	if (theRowsTable != null) {
			updateTextView(theRowsTable);
		}
    }//GEN-LAST:event_headerCheckBoxActionPerformed

    private void cancelLoadButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cancelLoadButtonActionPerformed
    }//GEN-LAST:event_cancelLoadButtonActionPerformed

    private void rotateCheckBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_rotateCheckBoxActionPerformed
    	if (theRowsTable != null) {
			updateTextView(theRowsTable);
		}
    }//GEN-LAST:event_rotateCheckBoxActionPerformed

	public void repaintShowingAnimatedTables() {
		if (isShowing()) {
			columnsScrollPane.repaint();
			if (theRowsTable != null) {
				theRowsTable.repaint();
			}
		}
	}
	
	public void destroy() {
    	if (theRowsTable != null) {
    		theRowsTable.setModel(new DefaultTableModel());
    	}
    	
    	textArea.discardAllEdits();
    	textArea.setDocument(new RSyntaxDocument(null, SyntaxConstants.SYNTAX_STYLE_NONE)); // prevent memory leak
    }
    
    SQLConsoleChartPanel chartPanel;
    private final RSyntaxTextAreaWithSQLSyntaxStyle textArea;
    final Pair<Integer, Integer> caretDotMark;
    private final List<Integer> rowColumnTypes;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    javax.swing.JButton cancelLoadButton;
    public javax.swing.JComboBox<String> columnSeparatorComboBox;
    public javax.swing.JLabel columnSeparatorLabel;
    public javax.swing.JPanel columnsPanel;
    public javax.swing.JScrollPane columnsScrollPane;
    public javax.swing.JLabel columnsSortedStateLabel;
    public javax.swing.JPanel columnsTabRowCountPanel;
    public javax.swing.JPanel contentPanel;
    javax.swing.JToolBar controlsContainer;
    public javax.swing.JButton copyCBButton;
    public javax.swing.JPanel fullTSearchPanel;
    public javax.swing.JCheckBox headerCheckBox;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLayeredPane jLayeredPane1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel13;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JTextArea jTextArea;
    private javax.swing.JLabel loadingCauseLabel;
    private javax.swing.JLabel loadingLabel;
    javax.swing.JPanel loadingPanel;
    private javax.swing.JPanel metaPanel;
    private javax.swing.JPanel metaTabPanel;
    public javax.swing.JPanel panel;
    public javax.swing.JCheckBox rotateCheckBox;
    public javax.swing.JTabbedPane tabbedPane;
    public javax.swing.JLabel textSortedStateLabel;
    public javax.swing.JPanel textTabPanel;
    public javax.swing.JPanel textTabRowCountPanel;
    public javax.swing.JScrollPane textViewScrollPane;
    private javax.swing.JPanel typePanel;
    // End of variables declaration//GEN-END:variables
    public final javax.swing.JLabel statementLabel;
	public final javax.swing.JPanel shimPanel;
	public final BrowserContentPane rowBrowser;
	javax.swing.JButton loadButton;
	protected boolean doSync;
	
	private Pattern rtrimRegExpr = Pattern.compile("^ |\\s+$");
	
	private static ImageIcon cancelIcon;
	
	static {
        // load images
        cancelIcon = UIUtil.readImage("/buttoncancel.png");
	}

}
