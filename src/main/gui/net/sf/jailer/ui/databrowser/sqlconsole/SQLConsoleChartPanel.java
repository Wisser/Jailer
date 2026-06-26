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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.RowSorter;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartUtils;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.axis.LogAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.labels.StandardCategoryItemLabelGenerator;
import org.jfree.chart.labels.StandardPieSectionLabelGenerator;
import org.jfree.chart.labels.StandardXYItemLabelGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.category.CategoryItemRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.Range;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.DefaultPieDataset;
import org.jfree.data.statistics.HistogramDataset;
import org.jfree.data.statistics.HistogramType;
import org.jfree.data.xy.DefaultXYZDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.svg.SVGGraphics2D;

import com.orsoncharts.Chart3D;
import com.orsoncharts.Chart3DFactory;
import com.orsoncharts.Chart3DPanel;
import com.orsoncharts.data.StandardPieDataset3D;
import com.orsoncharts.data.category.StandardCategoryDataset3D;
import com.orsoncharts.graphics3d.ViewPoint3D;
import com.orsoncharts.plot.CategoryPlot3D;
import com.orsoncharts.label.StandardPieLabelGenerator;
import com.orsoncharts.plot.PiePlot3D;
import com.orsoncharts.plot.StandardColorSource;
import com.orsoncharts.plot.StandardFontSource;
import com.orsoncharts.renderer.category.AbstractCategoryRenderer3D;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;

/**
 * Chart panel for the SQL Console result view.
 * Renders JFreeChart diagrams from query result data.
 */
public class SQLConsoleChartPanel extends JPanel {

    private enum ChartType {
        BAR         ("Bar"),
        BAR_3D      ("Bar 3D"),
        LINE        ("Line"),
        AREA        ("Area"),
        STACKED_AREA("Stacked Area"),
        PIE         ("Pie"),
        PIE_3D      ("Pie 3D"),
        RING        ("Ring"),
        XY          ("XY (Scatter)"),
        BUBBLE      ("Bubble"),
        HISTOGRAM   ("Histogram");

        final String label;
        ChartType(String label) { this.label = label; }

        @Override public String toString() { return label; }
    }

    // --- Main controls ---
    private final JComboBox<ChartType> chartTypeCombo = new JComboBox<>(ChartType.values());
    private final JComboBox<String> xColumnCombo   = new JComboBox<>();
    private final JButton           yButton        = new JButton("(none)");
    private final JPopupMenu        yPopup         = new JPopupMenu();
    private final JPanel            yCheckPanel    = new JPanel();
    private final List<JCheckBox>   yCheckBoxes    = new ArrayList<>();
    private final JButton           exportButton   = new JButton("Export");

    // --- Color palettes ---
    private static final Color[] PALETTE_PASTEL      = { new Color(174,199,232), new Color(255,187,120), new Color(152,223,138), new Color(255,152,150), new Color(197,176,213), new Color(196,156,148), new Color(255,247,153), new Color(162,228,226), new Color(250,210,210), new Color(235,210,165) };
    private static final Color[] PALETTE_EARTH       = { new Color(140, 86, 75), new Color(214,139, 74), new Color(188,189, 34), new Color( 23,190,207), new Color( 44,160, 44), new Color(127,127,127), new Color(222,184,135), new Color(143,161,114), new Color( 99, 67, 41), new Color(178,130, 55) };
    private static final Color[] PALETTE_COLORBLIND  = { new Color(  0,114,178), new Color(230,159,  0), new Color(  0,158,115), new Color(213, 94,  0), new Color( 86,180,233), new Color(204,121,167), new Color(0,0,0), new Color(240,228,66), new Color(238,136,102), new Color( 51, 34,136), new Color(136,204,170), new Color(136, 34, 85) };
    private static final Color[] PALETTE_MONOCHROME  = { new Color( 30, 30, 30), new Color( 80, 80, 80), new Color(130,130,130), new Color(180,180,180), new Color( 50, 50, 50), new Color(210,210,210), new Color(  5,  5,  5), new Color(105,105,105), new Color(158,158,158), new Color(235,235,235) };
    private static final Color[] PALETTE_OCEAN       = { new Color(  4, 90,141), new Color(  5,142,217), new Color(  2,194,197), new Color( 95,218,214), new Color(  0, 77, 64), new Color( 38,166,154), new Color(  7, 27, 76), new Color( 29,105,150), new Color(127,205,187), new Color(175,240,232) };
    private static final Color[] PALETTE_SUNSET      = { new Color(255, 87, 34), new Color(255,152,  0), new Color(255,193,  7), new Color(233, 30, 99), new Color(156, 39,176), new Color( 63, 81,181), new Color(255,111, 97), new Color(183,  0, 56), new Color(255,214, 90), new Color(106,  0,244) };
    private static final Color[] PALETTE_VIBRANT     = { new Color(220, 20, 60), new Color(  0,128,255), new Color(  0,200, 81), new Color(255,140,  0), new Color(148,  0,211), new Color(  0,210,210), new Color(255,220,  0), new Color(255,  0,153), new Color(100,255,  0), new Color( 65,  0,255) };
    private static final Color[] PALETTE_FOREST      = { new Color( 27, 94, 32), new Color( 85,139, 47), new Color(156,204,101), new Color(  0, 77, 64), new Color( 38,166,154), new Color(121, 85, 72), new Color( 72, 40, 20), new Color(163, 90, 33), new Color(200,160, 50), new Color(145,120,100) };
    private static final Color[] PALETTE_RETRO       = { new Color(188, 80, 80), new Color(210,140, 70), new Color(220,200,100), new Color(100,155,105), new Color( 80,110,160), new Color(140, 90,150), new Color( 80,140,140), new Color(155,130, 80), new Color( 70, 80,120), new Color(120,130, 70) };
    private static final Color[] PALETTE_NEON        = { new Color( 57,255, 20), new Color(  0,245,255), new Color(255,  7, 58), new Color(255,234,  0), new Color(185,  0,255), new Color(255,128,  0), new Color(255,  0,200), new Color(180,255,  0), new Color(  0, 80,255), new Color(  0,255,160) };
    private static final Color[] PALETTE_CORPORATE   = { new Color( 31, 73,125), new Color(112,173, 71), new Color(255,192,  0), new Color( 68,114,196), new Color(237,125, 49), new Color(112, 48,160), new Color(192, 80, 77), new Color( 75,172,198), new Color( 17, 46, 81), new Color(166,166,166) };
    private static final Color[] PALETTE_SPRING      = { new Color(255, 80,120), new Color( 60,180, 60), new Color(240,200,  0), new Color( 60,150,220), new Color(255, 90, 60), new Color(160, 60,200), new Color( 60,210,170), new Color(190,140,220), new Color(255,180,120), new Color(  0,190,230) };

    // --- General settings ---
    private final JTextField  titleField        = new JTextField(12);
    private final JCheckBox   legendCheckBox    = new JCheckBox("Legend", true);
    private final JCheckBox   dataLabelsCheckBox = new JCheckBox("Data Labels");
    private final JCheckBox   gridCheckBox           = new JCheckBox("Grid", false);
    private final JCheckBox   chartTranspCheckBox    = new JCheckBox("Transp. Chart", true);
    private final JCheckBox   plotTranspCheckBox     = new JCheckBox("Transp. Plot", true);
    private final JCheckBox   sortXCheckBox     = new JCheckBox("Sort X");
    private final JComboBox<String> aggregateCombo = new JComboBox<>(new String[]{"None", "Sum", "Average", "Count"});
    private final JComboBox<String> rowLimitCombo  = new JComboBox<>(new String[]{"All", "10", "50", "100", "500", "1000", "5000"});
    private final JComboBox<String> colorSchemeCombo = new JComboBox<>(new String[]{"Corporate", "Pastel", "Earth", "Colorblind", "Monochrome", "Darkness", "Ocean", "Sunset", "Vibrant", "Forest", "Retro", "Neon", "Spring"});
    private final JLabel colorSchemeLabel = new JLabel("Colors:");

    // --- Chart-specific settings ---
    private final JComboBox<String> orientationCombo = new JComboBox<>(new String[]{"Vertical", "Horizontal"});
    private final JCheckBox   stackedCheckBox   = new JCheckBox("Stacked");
    private final JComboBox<String> xRotateCombo = new JComboBox<>(new String[]{"0°", "45°", "90°"});
    private final JTextField  yMinField         = new JTextField(5);
    private final JTextField  yMaxField         = new JTextField(5);
    private final JCheckBox   logScaleCheckBox  = new JCheckBox("Log");
    private final JComboBox<String> binsCombo       = new JComboBox<>(new String[]{"10", "20", "50", "100"});
    private final JLabel            binsLabel       = new JLabel("Bins:");
    private final JComboBox<String> pieLabelCombo   = new JComboBox<>(new String[]{"Key + Value + %", "Key + %", "Key", "Key + Value", "None"});
    private final JLabel            pieLabelLabel   = new JLabel("Labels:");
    private final JTextField        projField       = new JTextField("1500", 5);
    private final JLabel            projLabel       = new JLabel("Proj:");
    private final JSpinner          fontSizeSpinner   = new JSpinner(new SpinnerNumberModel(12, 6, 36, 1));
    private final JLabel            fontSizeLabel     = new JLabel("Font:");
    private final JSpinner          lineWidthSpinner  = new JSpinner(new SpinnerNumberModel(2.0, 0.5, 10.0, 0.5));
    private final JLabel            lineWidthLabel    = new JLabel("Line:");
    private final JButton           textColorButton   = new JButton();
    private Color                   textColor         = Color.BLACK;
    private final JButton           resetViewButton = new JButton("Reset View");

    // --- Labels to enable/disable with controls ---
    private final JLabel orientationLabel = new JLabel("Orientation:");
    private final JLabel xRotateLabel     = new JLabel("X-Rotate:");
    private final JLabel yMinLabel        = new JLabel("Y-Min:");
    private final JLabel yMaxLabel        = new JLabel("Y-Max:");
    private final JLabel aggregateLabel   = new JLabel("Aggregate:");
    private final JLabel rowLimitLabel    = new JLabel("Rows:");

    private final JPanel chartContainer = new JPanel(new BorderLayout());

    private String       pendingXColumn  = null;
    private List<String> pendingYColumns = null;
    private final Map<ChartType, String>       userXByChartType = new HashMap<>();
    private final Map<ChartType, List<String>> userYByChartType = new HashMap<>();

    private JTable currentTable;
    private JFreeChart   currentChart;
    private Chart3D      currentChart3D;
    private ViewPoint3D  initialViewPoint3D;
    private ChartType    currentChartType = null;
    private Range        pendingDomainRange = null;
    private Range        pendingRangeRange  = null;
    private ViewPoint3D  pendingViewPoint3D = null;
    private final List<Integer> columnTypes;
    private boolean suppressUpdate = false;

    private static final Pattern BOLD_PATTERN = Pattern.compile("<b>(.*?)</b>");

    public SQLConsoleChartPanel(List<Integer> columnTypes) {
        this.columnTypes = columnTypes;
        setLayout(new BorderLayout());

        // Y popup
        yCheckPanel.setLayout(new BoxLayout(yCheckPanel, BoxLayout.Y_AXIS));
        JScrollPane yPopupScroll = new JScrollPane(yCheckPanel);
        yPopupScroll.setBorder(null);
        yPopupScroll.setPreferredSize(new Dimension(200, 300));
        yPopup.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        yPopup.add(yPopupScroll);

        JPanel northPanel = new JPanel();
        northPanel.setLayout(new BoxLayout(northPanel, BoxLayout.Y_AXIS));
        northPanel.add(buildMainToolbar());
        northPanel.add(buildSettingsRow1());
        northPanel.add(buildSettingsRow2());

        add(northPanel, BorderLayout.NORTH);
        add(chartContainer, BorderLayout.CENTER);
        chartContainer.add(new JLabel("Select columns above to create a chart.", SwingConstants.CENTER));

        // Listeners
        ActionListener redraw = e -> { if (!suppressUpdate) updateChart(); };
        chartTypeCombo.addActionListener(e -> { if (!suppressUpdate) { updateSettingsForChartType(); updateChart(); } });
        xColumnCombo.addActionListener(e -> {
            if (!suppressUpdate) {
                userXByChartType.put(currentChartType(), (String) xColumnCombo.getSelectedItem());
                updateChart();
            }
        });
        yCheckPanel.putClientProperty("redraw", redraw); // stored for use in buildCheckBox

        legendCheckBox.addActionListener(redraw);
        dataLabelsCheckBox.addActionListener(redraw);
        gridCheckBox.addActionListener(redraw);
        chartTranspCheckBox.addActionListener(redraw);
        plotTranspCheckBox.addActionListener(redraw);
        sortXCheckBox.addActionListener(redraw);
        aggregateCombo.addActionListener(redraw);
        rowLimitCombo.addActionListener(redraw);
        orientationCombo.addActionListener(redraw);
        stackedCheckBox.addActionListener(redraw);
        xRotateCombo.addActionListener(redraw);
        logScaleCheckBox.addActionListener(redraw);
        colorSchemeCombo.addActionListener(redraw);
        binsCombo.setSelectedItem("20");
        binsCombo.addActionListener(redraw);
        fontSizeSpinner.addChangeListener(e -> { if (!suppressUpdate) updateChart(); });
        lineWidthSpinner.setToolTipText("Line thickness (px) for Line charts");
        lineWidthSpinner.setPreferredSize(new Dimension(55, fontSizeSpinner.getPreferredSize().height));
        lineWidthSpinner.addChangeListener(e -> { if (!suppressUpdate) updateChart(); });
        textColorButton.setToolTipText("Text color for title, axes and labels");
        textColorButton.setPreferredSize(new Dimension(32, fontSizeSpinner.getPreferredSize().height));
        updateTextColorIcon();
        textColorButton.addActionListener(e -> {
            Color chosen = JColorChooser.showDialog(this, "Text Color", textColor);
            if (chosen != null) {
                textColor = chosen;
                updateTextColorIcon();
                updateChart();
            }
        });
        pieLabelCombo.addActionListener(redraw);

        FocusAdapter projListener = new FocusAdapter() {
            @Override public void focusLost(FocusEvent e) { applyProjection(); }
        };
        projField.addFocusListener(projListener);
        projField.addActionListener(e -> applyProjection());

        resetViewButton.addActionListener(e -> {
            if (currentChart3D != null && initialViewPoint3D != null) {
                currentChart3D.setViewPoint(new ViewPoint3D(initialViewPoint3D));
                currentChart3D.setProjDistance(parseProjDistance());
            }
        });

        FocusAdapter boundsListener = new FocusAdapter() {
            @Override public void focusLost(FocusEvent e) { if (!suppressUpdate) updateChart(); }
        };
        yMinField.addFocusListener(boundsListener);
        yMaxField.addFocusListener(boundsListener);
        yMinField.addActionListener(redraw);
        yMaxField.addActionListener(redraw);

        titleField.getDocument().addDocumentListener(new DocumentListener() {
            private void onChange() { if (!suppressUpdate) SwingUtilities.invokeLater(SQLConsoleChartPanel.this::updateChart); }
            @Override public void insertUpdate(DocumentEvent e) { onChange(); }
            @Override public void removeUpdate(DocumentEvent e) { onChange(); }
            @Override public void changedUpdate(DocumentEvent e) { onChange(); }
        });

        updateSettingsForChartType();
    }

    private JPanel buildMainToolbar() {
        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 2));

        JLabel chartTypeLabel = new JLabel("Chart type:");
        chartTypeLabel.setToolTipText("Select the type of chart to display");
        chartTypeCombo.setToolTipText("Select the type of chart to display");
        toolbar.add(Box.createHorizontalStrut(4));
        toolbar.add(chartTypeLabel);
        toolbar.add(Box.createHorizontalStrut(4));
        toolbar.add(chartTypeCombo);
        toolbar.add(Box.createHorizontalStrut(12));

        JLabel xLabel = new JLabel("X:");
        xLabel.setToolTipText("Column used as the X axis (category or horizontal value)");
        xColumnCombo.setToolTipText("Column used as the X axis (category or horizontal value)");
        xColumnCombo.setPreferredSize(new Dimension(150, xColumnCombo.getPreferredSize().height));
        toolbar.add(xLabel);
        toolbar.add(Box.createHorizontalStrut(4));
        toolbar.add(xColumnCombo);
        toolbar.add(Box.createHorizontalStrut(12));

        JLabel yLabel = new JLabel("Y:");
        yLabel.setToolTipText("Columns used as Y values — click to select");
        yButton.setToolTipText("Columns used as Y values — click to select");
        yButton.setPreferredSize(new Dimension(200, yButton.getPreferredSize().height));
        yButton.setHorizontalAlignment(SwingConstants.LEFT);
        yButton.addActionListener(e -> yPopup.show(yButton, 0, yButton.getHeight()));
        toolbar.add(yLabel);
        toolbar.add(Box.createHorizontalStrut(4));
        toolbar.add(yButton);
        toolbar.add(Box.createHorizontalStrut(12));

        exportButton.setIcon(UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage("/run.png")));
        exportButton.setToolTipText("Export chart as image file or copy to clipboard");
        exportButton.setEnabled(false);
        exportButton.addActionListener(e -> showExportMenu());
        toolbar.add(exportButton);
        toolbar.add(Box.createHorizontalStrut(12));

        JLabel tipLabel = new JLabel("Tip");
        tipLabel.setToolTipText("<html><b>Mouse interactions:</b><br>"
                + "&#x2022; <b>Wheel</b>: zoom in/out<br>"
                + "&#x2022; <b>Left drag</b>: zoom to selection<br>"
                + "&#x2022; <b>Right-click</b>: zoom / reset options<br>"
                + "&#x2022; <b>Double-click</b>: reset zoom</html>");
        tipLabel.setForeground(tipLabel.getForeground().brighter());
        toolbar.add(tipLabel);
        toolbar.add(Box.createHorizontalStrut(4));

        return toolbar;
    }

    private JPanel buildSettingsRow1() {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 1));

        JLabel titleLabel = new JLabel("Title:");
        titleLabel.setToolTipText("Optional chart title");
        titleField.setToolTipText("Optional chart title");
        row.add(Box.createHorizontalStrut(4));
        row.add(titleLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(titleField);
        row.add(Box.createHorizontalStrut(8));

        fontSizeLabel.setToolTipText("Base font size for all chart text");
        fontSizeSpinner.setToolTipText("Base font size for all chart text");
        fontSizeSpinner.setPreferredSize(new Dimension(55, fontSizeSpinner.getPreferredSize().height));
        row.add(fontSizeLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(fontSizeSpinner);
        row.add(Box.createHorizontalStrut(4));
        row.add(new JLabel("Text:"));
        row.add(Box.createHorizontalStrut(4));
        row.add(textColorButton);
        row.add(Box.createHorizontalStrut(8));
        row.add(lineWidthLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(lineWidthSpinner);
        row.add(Box.createHorizontalStrut(12));

        legendCheckBox.setToolTipText("Show or hide the chart legend");
        row.add(legendCheckBox);
        row.add(Box.createHorizontalStrut(8));

        dataLabelsCheckBox.setToolTipText("Show data values directly on the chart");
        row.add(dataLabelsCheckBox);
        row.add(Box.createHorizontalStrut(8));

        gridCheckBox.setToolTipText("Show or hide the grid lines");
        row.add(gridCheckBox);
        row.add(Box.createHorizontalStrut(8));

        chartTranspCheckBox.setToolTipText("Transparent chart background (outer area)");
        row.add(chartTranspCheckBox);
        row.add(Box.createHorizontalStrut(8));

        plotTranspCheckBox.setToolTipText("Transparent plot background (inner area)");
        row.add(plotTranspCheckBox);
        row.add(Box.createHorizontalStrut(8));

        sortXCheckBox.setToolTipText("Sort categories alphabetically by X value");
        row.add(sortXCheckBox);
        row.add(Box.createHorizontalStrut(12));

        aggregateLabel.setToolTipText("Aggregate multiple rows with the same X value");
        aggregateCombo.setToolTipText("Aggregate multiple rows with the same X value");
        row.add(aggregateLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(aggregateCombo);
        row.add(Box.createHorizontalStrut(12));

        rowLimitLabel.setToolTipText("Maximum number of rows to include in the chart");
        rowLimitCombo.setToolTipText("Maximum number of rows to include in the chart");
        row.add(rowLimitLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(rowLimitCombo);
        row.add(Box.createHorizontalStrut(12));

        colorSchemeLabel.setToolTipText("Color palette for the chart series");
        colorSchemeCombo.setToolTipText("Color palette for the chart series");
        row.add(colorSchemeLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(colorSchemeCombo);
        row.add(Box.createHorizontalStrut(4));

        return row;
    }

    private JPanel buildSettingsRow2() {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 1));

        orientationLabel.setToolTipText("Bar chart orientation");
        orientationCombo.setToolTipText("Bar chart orientation");
        row.add(Box.createHorizontalStrut(4));
        row.add(orientationLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(orientationCombo);
        row.add(Box.createHorizontalStrut(8));

        stackedCheckBox.setToolTipText("Stack multiple series on top of each other");
        row.add(stackedCheckBox);
        row.add(Box.createHorizontalStrut(12));

        xRotateLabel.setToolTipText("Rotation angle for X axis labels");
        xRotateCombo.setToolTipText("Rotation angle for X axis labels");
        row.add(xRotateLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(xRotateCombo);
        row.add(Box.createHorizontalStrut(12));

        yMinLabel.setToolTipText("Fixed lower bound for the Y axis (leave empty for auto)");
        yMinField.setToolTipText("Fixed lower bound for the Y axis (leave empty for auto)");
        row.add(yMinLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(yMinField);
        row.add(Box.createHorizontalStrut(8));

        yMaxLabel.setToolTipText("Fixed upper bound for the Y axis (leave empty for auto)");
        yMaxField.setToolTipText("Fixed upper bound for the Y axis (leave empty for auto)");
        row.add(yMaxLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(yMaxField);
        row.add(Box.createHorizontalStrut(12));

        logScaleCheckBox.setToolTipText("Use a logarithmic scale for the Y axis");
        row.add(logScaleCheckBox);
        row.add(Box.createHorizontalStrut(12));

        pieLabelLabel.setToolTipText("What to show as pie/ring slice labels");
        pieLabelCombo.setToolTipText("What to show as pie/ring slice labels");
        row.add(pieLabelLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(pieLabelCombo);
        row.add(Box.createHorizontalStrut(12));

        binsLabel.setToolTipText("Number of histogram bins");
        binsCombo.setToolTipText("Number of histogram bins");
        row.add(binsLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(binsCombo);
        row.add(Box.createHorizontalStrut(12));

        projLabel.setToolTipText("Projection distance — lower = wider angle, higher = more telephoto");
        projField.setToolTipText("Projection distance — lower = wider angle, higher = more telephoto");
        row.add(projLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(projField);
        row.add(Box.createHorizontalStrut(8));

        resetViewButton.setToolTipText("Reset 3D rotation and projection to default");
        row.add(resetViewButton);
        row.add(Box.createHorizontalStrut(4));

        return row;
    }

    private void updateSettingsForChartType() {
        ChartType type = (ChartType) chartTypeCombo.getSelectedItem();
        boolean is3D      = type == ChartType.BAR_3D || type == ChartType.PIE_3D;
        boolean isBar     = type == ChartType.BAR;
        boolean isCat     = isBar || type == ChartType.LINE || type == ChartType.AREA || type == ChartType.STACKED_AREA;
        boolean isPie     = type == ChartType.PIE || type == ChartType.RING;
        boolean isXYLike  = type == ChartType.XY || type == ChartType.BUBBLE || type == ChartType.HISTOGRAM;
        boolean isHist    = type == ChartType.HISTOGRAM;
        boolean hasStack  = isBar || type == ChartType.LINE || type == ChartType.AREA;

        setEnabled2(orientationLabel, orientationCombo, !is3D && (isCat || isHist));
        setEnabled2(null, stackedCheckBox, !is3D && hasStack);
        setEnabled2(xRotateLabel, xRotateCombo, !is3D && isCat);
        setEnabled2(yMinLabel, yMinField, !is3D && !isPie);
        setEnabled2(yMaxLabel, yMaxField, !is3D && !isPie);
        setEnabled2(null, logScaleCheckBox, !is3D && !isPie);
        setEnabled2(null, gridCheckBox, !is3D && !isPie);
        setEnabled2(null, dataLabelsCheckBox, !is3D && !isPie);
        setEnabled2(pieLabelLabel, pieLabelCombo, isPie || type == ChartType.PIE_3D);
        setEnabled2(aggregateLabel, aggregateCombo, !is3D && (isCat || type == ChartType.BAR_3D));
        setEnabled2(null, sortXCheckBox, !is3D && isCat);
        setEnabled2(binsLabel, binsCombo, isHist);
        setEnabled2(lineWidthLabel, lineWidthSpinner, type == ChartType.LINE);
        setEnabled2(projLabel, projField, is3D);
        setEnabled2(null, resetViewButton, is3D);
    }

    private void setEnabled2(JLabel label, java.awt.Component control, boolean enabled) {
        if (label != null) label.setEnabled(enabled);
        control.setEnabled(enabled);
    }

    // -------------------------------------------------------------------------

    public void setTable(JTable table) {
        this.currentTable = table;
        userXByChartType.clear();
        userYByChartType.clear();
        refreshColumnCombos();
        updateChart();
    }

    private void refreshColumnCombos() {
        if (currentTable == null) return;
        TableModel model = currentTable.getModel();
        TableColumnModel cm = currentTable.getColumnModel();
        int colCount = cm.getColumnCount();

        String xSel = pendingXColumn != null ? pendingXColumn : (String) xColumnCombo.getSelectedItem();
        List<String> ySel = new ArrayList<>();
        if (pendingYColumns != null) {
            ySel.addAll(pendingYColumns);
        } else {
            for (JCheckBox cb : yCheckBoxes) {
                if (cb.isSelected()) ySel.add(cb.getText());
            }
        }
        pendingXColumn = null;
        pendingYColumns = null;

        suppressUpdate = true;
        try {
            xColumnCombo.removeAllItems();
            yCheckBoxes.clear();
            yCheckPanel.removeAll();

            for (int i = 0; i < colCount; i++) {
                int modelIdx = cm.getColumn(i).getModelIndex();
                String name = stripHtml(model.getColumnName(modelIdx));
                xColumnCombo.addItem(name);
                JCheckBox cb = new JCheckBox(name);
                cb.setSelected(ySel.contains(name));
                cb.addActionListener(e -> {
                    updateYButtonLabel();
                    if (!suppressUpdate) {
                        userYByChartType.put(currentChartType(), selectedYNames());
                        updateChart();
                    }
                });
                yCheckBoxes.add(cb);
                yCheckPanel.add(cb);
            }

            if (xSel != null) xColumnCombo.setSelectedItem(xSel);
            if (xColumnCombo.getSelectedIndex() < 0 && colCount > 0) {
                String overX = userXByChartType.get(currentChartType());
                if (overX != null) xColumnCombo.setSelectedItem(overX);
                if (xColumnCombo.getSelectedIndex() < 0) xColumnCombo.setSelectedIndex(0);
            }

            if (ySel.isEmpty() || yCheckBoxes.stream().noneMatch(JCheckBox::isSelected)) {
                List<String> overY = userYByChartType.get(currentChartType());
                if (overY != null) {
                    yCheckBoxes.forEach(cb -> cb.setSelected(overY.contains(cb.getText())));
                } else {
                    int defY = findFirstNumericViewCol();
                    int idx = defY >= 0 ? defY : Math.min(1, colCount - 1);
                    if (idx >= 0 && idx < yCheckBoxes.size()) yCheckBoxes.get(idx).setSelected(true);
                }
            }
            updateYButtonLabel();
        } finally {
            suppressUpdate = false;
        }
    }

    private void updateYButtonLabel() {
        List<String> selected = new ArrayList<>();
        for (JCheckBox cb : yCheckBoxes) {
            if (cb.isSelected()) selected.add(cb.getText());
        }
        if (selected.isEmpty()) {
            yButton.setText("(none)");
        } else if (selected.size() == 1) {
            yButton.setText(selected.get(0));
        } else {
            yButton.setText(selected.get(0) + " +" + (selected.size() - 1) + " more");
        }
    }

    private int[] getSelectedYIndices() {
        List<Integer> indices = new ArrayList<>();
        for (int i = 0; i < yCheckBoxes.size(); i++) {
            if (yCheckBoxes.get(i).isSelected()) indices.add(i);
        }
        int[] arr = new int[indices.size()];
        for (int i = 0; i < arr.length; i++) arr[i] = indices.get(i);
        return arr;
    }

    private int findFirstNumericViewCol() {
        if (currentTable == null) return -1;
        TableColumnModel cm = currentTable.getColumnModel();
        for (int i = 0; i < cm.getColumnCount(); i++) {
            if (isNumeric(cm.getColumn(i).getModelIndex())) return i;
        }
        return -1;
    }

    private boolean isNumeric(int modelCol) {
        if (columnTypes != null && modelCol < columnTypes.size()) {
            switch (columnTypes.get(modelCol)) {
                case Types.BIGINT: case Types.DECIMAL: case Types.DOUBLE:
                case Types.FLOAT:  case Types.INTEGER: case Types.NUMERIC:
                case Types.REAL:   case Types.SMALLINT: case Types.TINYINT:
                    return true;
                default: break;
            }
        }
        return false;
    }

    private ChartType currentChartType() {
        ChartType ct = (ChartType) chartTypeCombo.getSelectedItem();
        return ct != null ? ct : ChartType.BAR;
    }

    private List<String> selectedYNames() {
        List<String> names = new ArrayList<>();
        for (JCheckBox cb : yCheckBoxes) if (cb.isSelected()) names.add(cb.getText());
        return names;
    }

    // -------------------------------------------------------------------------

    private void updateChart() {
        if (currentTable == null) return;
        int xView = xColumnCombo.getSelectedIndex();
        int[] yViews = getSelectedYIndices();
        if (xView < 0 || yViews.length == 0) return;

        TableColumnModel cm = currentTable.getColumnModel();
        RowSorter<? extends TableModel> sorter = currentTable.getRowSorter();
        TableModel model = currentTable.getModel();
        if (cm.getColumnCount() == 0) return;

        int xModelCol = cm.getColumn(xView).getModelIndex();
        String xLabel = stripHtml(model.getColumnName(xModelCol));
        int totalRows = sorter != null ? sorter.getViewRowCount() : model.getRowCount();
        int rowCount = getEffectiveRowCount(totalRows);

        int[] yModelCols = new int[yViews.length];
        String[] yLabels = new String[yViews.length];
        for (int i = 0; i < yViews.length; i++) {
            yModelCols[i] = cm.getColumn(yViews[i]).getModelIndex();
            yLabels[i] = stripHtml(model.getColumnName(yModelCols[i]));
        }

        ChartType type = (ChartType) chartTypeCombo.getSelectedItem();

        // Save current view state (only restore when chart type is unchanged)
        Range savedDomainRange = null;
        Range savedRangeRange = null;
        ViewPoint3D savedViewPoint3D = null;
        if (type == currentChartType) {
            if (currentChart != null) {
                if (currentChart.getPlot() instanceof XYPlot) {
                    XYPlot xyPlot = (XYPlot) currentChart.getPlot();
                    if (!xyPlot.getDomainAxis().isAutoRange()) savedDomainRange = xyPlot.getDomainAxis().getRange();
                    if (!xyPlot.getRangeAxis().isAutoRange()) savedRangeRange = xyPlot.getRangeAxis().getRange();
                } else if (currentChart.getPlot() instanceof CategoryPlot) {
                    CategoryPlot catPlot = (CategoryPlot) currentChart.getPlot();
                    if (!catPlot.getRangeAxis().isAutoRange()) savedRangeRange = catPlot.getRangeAxis().getRange();
                }
            }
            if (currentChart3D != null) {
                savedViewPoint3D = new ViewPoint3D(currentChart3D.getViewPoint());
            }
        }
        currentChartType = type;

        if (type == ChartType.BAR_3D || type == ChartType.PIE_3D) {
            try {
                Chart3D chart3d = type == ChartType.BAR_3D
                    ? buildBar3DChart(model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels)
                    : buildPie3DChart(model, sorter, rowCount, xModelCol, yModelCols[0]);
                applyFonts3D(chart3d, type);
                ViewPoint3D vp3d = pendingViewPoint3D != null ? pendingViewPoint3D : savedViewPoint3D;
                pendingViewPoint3D = null;
                pendingDomainRange = null;
                pendingRangeRange  = null;
                if (vp3d != null) {
                    chart3d.setViewPoint(vp3d);
                }
                currentChart = null;
                currentChart3D = chart3d;
                exportButton.setEnabled(true);
                chartContainer.removeAll();
                chartContainer.add(new Chart3DPanel(chart3d), BorderLayout.CENTER);
                chartContainer.revalidate();
                chartContainer.repaint();
            } catch (Exception ex) {
                showError(ex.getMessage());
            }
            return;
        }

        JFreeChart chart;
        try {
            if (type == ChartType.PIE || type == ChartType.RING) {
                chart = buildPieChart(type, model, sorter, rowCount, xModelCol, yModelCols[0]);
            } else if (type == ChartType.XY) {
                chart = buildXYChart(model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels);
            } else if (type == ChartType.BUBBLE) {
                chart = buildBubbleChart(model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels);
            } else if (type == ChartType.HISTOGRAM) {
                chart = buildHistogramChart(model, sorter, rowCount, yModelCols[0], yLabels[0]);
            } else {
                chart = buildCategoryChart(type, model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels);
            }
        } catch (Exception ex) {
            showError(ex.getMessage());
            return;
        }

        applySettings(chart, type);

        // Pending values (from copySettingsFrom) take priority over same-panel saved state
        Range domainRange = pendingDomainRange != null ? pendingDomainRange : savedDomainRange;
        Range rangeRange  = pendingRangeRange  != null ? pendingRangeRange  : savedRangeRange;
        pendingDomainRange = null;
        pendingRangeRange  = null;
        pendingViewPoint3D = null;
        if (chart.getPlot() instanceof XYPlot) {
            XYPlot xyPlot = (XYPlot) chart.getPlot();
            if (domainRange != null) xyPlot.getDomainAxis().setRange(domainRange);
            if (rangeRange  != null) xyPlot.getRangeAxis().setRange(rangeRange);
        } else if (chart.getPlot() instanceof CategoryPlot) {
            if (rangeRange != null) ((CategoryPlot) chart.getPlot()).getRangeAxis().setRange(rangeRange);
        }

        currentChart = chart;
        currentChart3D = null;
        exportButton.setEnabled(true);
        ChartPanel cp = new ChartPanel(chart);
        cp.setMouseWheelEnabled(true);
        chartContainer.removeAll();
        chartContainer.add(cp, BorderLayout.CENTER);
        chartContainer.revalidate();
        chartContainer.repaint();
    }

    // -------------------------------------------------------------------------

    private JFreeChart buildPieChart(ChartType type, TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol) {
        Map<String, Double> data = aggregateAndSort(model, sorter, rowCount, xCol, yCol);
        DefaultPieDataset dataset = new DefaultPieDataset();
        for (Map.Entry<String, Double> e : data.entrySet()) dataset.setValue(e.getKey(), e.getValue());
        boolean legend = legendCheckBox.isSelected();
        if (type == ChartType.RING) {
            return ChartFactory.createRingChart(null, dataset, legend, true, false);
        }
        return ChartFactory.createPieChart(null, dataset, legend, true, false);
    }

    private JFreeChart buildXYChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int[] yCols, String xLabel, String[] yLabels) {
        XYSeriesCollection dataset = new XYSeriesCollection();
        for (int si = 0; si < yCols.length; si++) {
            XYSeries series = new XYSeries(yLabels[si]);
            for (int i = 0; i < rowCount; i++) {
                int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
                double x = cellDouble(model.getValueAt(row, xCol));
                double y = cellDouble(model.getValueAt(row, yCols[si]));
                if (!Double.isNaN(x) && !Double.isNaN(y)) series.add(x, y);
            }
            dataset.addSeries(series);
        }
        String yAxisLabel = yCols.length == 1 ? yLabels[0] : "Value";
        return ChartFactory.createScatterPlot(null, xLabel, yAxisLabel, dataset,
                PlotOrientation.VERTICAL, legendCheckBox.isSelected(), true, false);
    }

    private JFreeChart buildCategoryChart(ChartType type, TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int[] yCols, String xLabel, String[] yLabels) {
        boolean stacked = stackedCheckBox.isSelected();
        boolean horizontal = "Horizontal".equals(orientationCombo.getSelectedItem());
        PlotOrientation orientation = horizontal ? PlotOrientation.HORIZONTAL : PlotOrientation.VERTICAL;
        boolean legend = legendCheckBox.isSelected();
        String yAxisLabel = yCols.length == 1 ? yLabels[0] : "Value";

        DefaultCategoryDataset dataset = new DefaultCategoryDataset();
        for (int si = 0; si < yCols.length; si++) {
            Map<String, Double> data = aggregateAndSort(model, sorter, rowCount, xCol, yCols[si]);
            for (Map.Entry<String, Double> e : data.entrySet()) dataset.addValue(e.getValue(), yLabels[si], e.getKey());
        }

        if (type == ChartType.BAR) {
            return stacked
                    ? ChartFactory.createStackedBarChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false)
                    : ChartFactory.createBarChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false);
        }
        if (type == ChartType.AREA) {
            return stacked
                    ? ChartFactory.createStackedAreaChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false)
                    : ChartFactory.createAreaChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false);
        }
        if (type == ChartType.STACKED_AREA) {
            return ChartFactory.createStackedAreaChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false);
        }
        // Line
        return stacked
                ? ChartFactory.createStackedAreaChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false)
                : ChartFactory.createLineChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false);
    }

    private JFreeChart buildBubbleChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int[] yCols, String xLabel, String[] yLabels) {
        DefaultXYZDataset dataset = new DefaultXYZDataset();
        int zCol = yCols.length > 1 ? yCols[1] : -1;
        double[] xData = new double[rowCount];
        double[] yData = new double[rowCount];
        double[] zData = new double[rowCount];
        int count = 0;
        for (int i = 0; i < rowCount; i++) {
            int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
            double x = cellDouble(model.getValueAt(row, xCol));
            double y = cellDouble(model.getValueAt(row, yCols[0]));
            double z = zCol >= 0 ? cellDouble(model.getValueAt(row, zCol)) : 1.0;
            if (!Double.isNaN(x) && !Double.isNaN(y)) {
                xData[count] = x;
                yData[count] = y;
                zData[count] = Double.isNaN(z) ? 1.0 : z;
                count++;
            }
        }
        double[][] data = { Arrays.copyOf(xData, count), Arrays.copyOf(yData, count), Arrays.copyOf(zData, count) };
        dataset.addSeries(yLabels[0], data);
        return ChartFactory.createBubbleChart(null, xLabel, yLabels[0], dataset,
                PlotOrientation.VERTICAL, legendCheckBox.isSelected(), true, false);
    }

    private JFreeChart buildHistogramChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int yCol, String yLabel) {
        List<Double> values = new ArrayList<>();
        for (int i = 0; i < rowCount; i++) {
            int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
            double v = cellDouble(model.getValueAt(row, yCol));
            if (!Double.isNaN(v)) values.add(v);
        }
        if (values.isEmpty()) throw new IllegalArgumentException("No numeric data in selected column");
        double[] data = new double[values.size()];
        for (int i = 0; i < data.length; i++) data[i] = values.get(i);
        int bins = 20;
        try { bins = Integer.parseInt((String) binsCombo.getSelectedItem()); } catch (NumberFormatException e) { /* ignore */ }
        HistogramDataset dataset = new HistogramDataset();
        dataset.setType(HistogramType.FREQUENCY);
        dataset.addSeries(yLabel, data, bins);
        boolean horizontal = "Horizontal".equals(orientationCombo.getSelectedItem());
        PlotOrientation orientation = horizontal ? PlotOrientation.HORIZONTAL : PlotOrientation.VERTICAL;
        return ChartFactory.createHistogram(null, yLabel, "Frequency", dataset, orientation, legendCheckBox.isSelected(), true, false);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Chart3D buildBar3DChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int[] yCols, String xLabel, String[] yLabels) {
        StandardCategoryDataset3D dataset = new StandardCategoryDataset3D();
        for (int si = 0; si < yCols.length; si++) {
            Map<String, Double> data = aggregateAndSort(model, sorter, rowCount, xCol, yCols[si]);
            for (Map.Entry<String, Double> e : data.entrySet()) {
                dataset.addValue(e.getValue(), yLabels[si], yLabels[si], e.getKey());
            }
        }
        String yAxisLabel = yCols.length == 1 ? yLabels[0] : "Value";
        Chart3D chart = Chart3DFactory.createBarChart(null, null, dataset, null, xLabel, yAxisLabel);
        String title = titleField.getText().trim();
        if (!title.isEmpty()) chart.setTitle(title);
        if (!legendCheckBox.isSelected()) chart.setLegendBuilder(null);
        Color[] palette = getPalette((String) colorSchemeCombo.getSelectedItem());
        if (palette != null) ((AbstractCategoryRenderer3D) ((CategoryPlot3D) chart.getPlot()).getRenderer()).setColors(palette);
        chart.setAntiAlias(true);
        chart.setProjDistance(parseProjDistance());
        initialViewPoint3D = new ViewPoint3D(chart.getViewPoint());
        return chart;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private Chart3D buildPie3DChart(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol) {
        Map<String, Double> data = aggregateAndSort(model, sorter, rowCount, xCol, yCol);
        StandardPieDataset3D dataset = new StandardPieDataset3D();
        for (Map.Entry<String, Double> e : data.entrySet()) dataset.add(e.getKey(), e.getValue());
        Chart3D chart = Chart3DFactory.createPieChart(null, null, dataset);
        String title = titleField.getText().trim();
        if (!title.isEmpty()) chart.setTitle(title);
        if (!legendCheckBox.isSelected()) chart.setLegendBuilder(null);
        Color[] palette = getPalette((String) colorSchemeCombo.getSelectedItem());
        PiePlot3D piePlot3D = (PiePlot3D) chart.getPlot();
        if (palette != null) piePlot3D.setSectionColors(palette);
        piePlot3D.setSectionLabelGenerator(buildPieLabelGenerator3D());
        chart.setAntiAlias(true);
        chart.setProjDistance(parseProjDistance());
        initialViewPoint3D = new ViewPoint3D(chart.getViewPoint());
        return chart;
    }

    // -------------------------------------------------------------------------

    private Map<String, Double> aggregateAndSort(TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol) {
        String aggMode = (String) aggregateCombo.getSelectedItem();
        Map<String, List<Double>> raw = new LinkedHashMap<>();
        for (int i = 0; i < rowCount; i++) {
            int row = sorter != null ? sorter.convertRowIndexToModel(i) : i;
            String key = cellString(model.getValueAt(row, xCol));
            double val = cellDouble(model.getValueAt(row, yCol));
            if (!Double.isNaN(val)) raw.computeIfAbsent(key, k -> new ArrayList<>()).add(val);
        }
        Map<String, Double> result = new LinkedHashMap<>();
        for (Map.Entry<String, List<Double>> e : raw.entrySet()) result.put(e.getKey(), computeAggregate(e.getValue(), aggMode));
        if (sortXCheckBox.isEnabled() && sortXCheckBox.isSelected()) {
            List<Map.Entry<String, Double>> entries = new ArrayList<>(result.entrySet());
            entries.sort(Map.Entry.comparingByKey());
            result = new LinkedHashMap<>();
            for (Map.Entry<String, Double> e : entries) result.put(e.getKey(), e.getValue());
        }
        return result;
    }

    private double computeAggregate(List<Double> values, String mode) {
        switch (mode != null ? mode : "None") {
            case "Sum":     return values.stream().mapToDouble(Double::doubleValue).sum();
            case "Average": return values.stream().mapToDouble(Double::doubleValue).average().orElse(Double.NaN);
            case "Count":   return values.size();
            default:        return values.isEmpty() ? Double.NaN : values.get(0);
        }
    }

    private int getEffectiveRowCount(int total) {
        String sel = (String) rowLimitCombo.getSelectedItem();
        if ("All".equals(sel)) return total;
        try { return Math.min(total, Integer.parseInt(sel)); } catch (NumberFormatException e) { return total; }
    }

    // -------------------------------------------------------------------------

    private void applySettings(JFreeChart chart, ChartType type) {
        String scheme = (String) colorSchemeCombo.getSelectedItem();
        if ("Darkness".equals(scheme)) StandardChartTheme.createDarknessTheme().apply(chart);

        chart.setBackgroundPaint(chartTranspCheckBox.isSelected() ? null : Color.WHITE);
        if (chart.getLegend() != null) chart.getLegend().setBackgroundPaint(chartTranspCheckBox.isSelected() ? null : Color.WHITE);
        chart.getPlot().setBackgroundPaint(plotTranspCheckBox.isSelected() ? null : Color.WHITE);

        String title = titleField.getText().trim();
        chart.setTitle(title.isEmpty() ? null : title);
        if (chart.getLegend() != null) chart.getLegend().setVisible(legendCheckBox.isSelected());

        boolean isPieLike = type == ChartType.PIE || type == ChartType.RING;
        boolean isXYLike  = type == ChartType.XY || type == ChartType.BUBBLE || type == ChartType.HISTOGRAM;
        if (isPieLike) {
            applyPieSettings(chart);
        } else if (isXYLike) {
            applyXYSettings(chart);
        } else {
            applyCategorySettings(chart);
        }

        applyFonts(chart, type);
        applyColorScheme(chart, type, scheme);
    }

    private void applyFonts(JFreeChart chart, ChartType type) {
        int base = (int) fontSizeSpinner.getValue();
        java.awt.Font labelFont = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, base);
        java.awt.Font tickFont  = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, Math.max(6, base - 2));
        java.awt.Font titleFont = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.BOLD,  base + 2);

        if (chart.getTitle()  != null) { chart.getTitle().setFont(titleFont); chart.getTitle().setPaint(textColor); }
        if (chart.getLegend() != null) chart.getLegend().setItemFont(labelFont);

        boolean isPieLike = type == ChartType.PIE || type == ChartType.RING;
        boolean isXYLike  = type == ChartType.XY || type == ChartType.BUBBLE || type == ChartType.HISTOGRAM;

        if (isPieLike) {
            PiePlot plot = (PiePlot) chart.getPlot();
            if (plot.getLabelGenerator() != null) { plot.setLabelFont(labelFont); plot.setLabelPaint(textColor); }
        } else if (isXYLike) {
            XYPlot plot = (XYPlot) chart.getPlot();
            plot.getDomainAxis().setLabelFont(labelFont);   plot.getDomainAxis().setLabelPaint(textColor);
            plot.getDomainAxis().setTickLabelFont(tickFont); plot.getDomainAxis().setTickLabelPaint(textColor);
            plot.getRangeAxis().setLabelFont(labelFont);    plot.getRangeAxis().setLabelPaint(textColor);
            plot.getRangeAxis().setTickLabelFont(tickFont);  plot.getRangeAxis().setTickLabelPaint(textColor);
            if (dataLabelsCheckBox.isSelected()) { plot.getRenderer().setDefaultItemLabelFont(labelFont); plot.getRenderer().setDefaultItemLabelPaint(textColor); }
        } else {
            CategoryPlot plot = (CategoryPlot) chart.getPlot();
            plot.getDomainAxis().setLabelFont(labelFont);   plot.getDomainAxis().setLabelPaint(textColor);
            plot.getDomainAxis().setTickLabelFont(tickFont); plot.getDomainAxis().setTickLabelPaint(textColor);
            plot.getRangeAxis().setLabelFont(labelFont);    plot.getRangeAxis().setLabelPaint(textColor);
            plot.getRangeAxis().setTickLabelFont(tickFont);  plot.getRangeAxis().setTickLabelPaint(textColor);
            if (dataLabelsCheckBox.isSelected()) { plot.getRenderer().setDefaultItemLabelFont(labelFont); plot.getRenderer().setDefaultItemLabelPaint(textColor); }
            if (type == ChartType.LINE && plot.getRenderer() instanceof org.jfree.chart.renderer.category.LineAndShapeRenderer) {
                float w = ((Number) lineWidthSpinner.getValue()).floatValue();
                org.jfree.chart.renderer.category.LineAndShapeRenderer lr =
                        (org.jfree.chart.renderer.category.LineAndShapeRenderer) plot.getRenderer();
                java.awt.BasicStroke stroke = new java.awt.BasicStroke(w, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND);
                lr.setAutoPopulateSeriesStroke(false);
                lr.setDefaultStroke(stroke);
            }
        }
    }

    private com.orsoncharts.label.PieLabelGenerator buildPieLabelGenerator3D() {
        switch ((String) pieLabelCombo.getSelectedItem()) {
            case "Key":             return new StandardPieLabelGenerator("%s");
            case "Key + Value":     return new StandardPieLabelGenerator("%s (%2$,.0f)");
            case "Key + %":         return new StandardPieLabelGenerator("%s (%3$,.0f%%)");
            case "Key + Value + %": return new StandardPieLabelGenerator("%s: %2$,.0f (%3$,.0f%%)");
            default:                return null;
        }
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void applyFonts3D(Chart3D chart, ChartType type) {
        int base = (int) fontSizeSpinner.getValue();
        java.awt.Font labelFont = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, base);
        java.awt.Font tickFont  = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, Math.max(6, base - 2));
        java.awt.Font titleFont = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.BOLD,  base + 2);

        String title = titleField.getText().trim();
        if (!title.isEmpty()) chart.setTitle(title, titleFont, textColor);

        if (type == ChartType.BAR_3D) {
            CategoryPlot3D plot = (CategoryPlot3D) chart.getPlot();
            plot.getColumnAxis().setLabelFont(labelFont);   plot.getColumnAxis().setLabelColor(textColor);
            plot.getColumnAxis().setTickLabelFont(tickFont); plot.getColumnAxis().setTickLabelColor(textColor);
            plot.getValueAxis().setLabelFont(labelFont);    plot.getValueAxis().setLabelColor(textColor);
            plot.getValueAxis().setTickLabelFont(tickFont);  plot.getValueAxis().setTickLabelColor(textColor);
        } else if (type == ChartType.PIE_3D) {
            PiePlot3D plot = (PiePlot3D) chart.getPlot();
            plot.setSectionLabelFontSource(new StandardFontSource<>(labelFont));
            plot.setSectionLabelColorSource(new StandardColorSource<>(textColor));
        }
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void applyColorScheme(JFreeChart chart, ChartType type, String scheme) {
        Color[] palette = getPalette(scheme);
        if (palette == null) return;
        boolean isPieLike = type == ChartType.PIE || type == ChartType.RING;
        boolean isXYLike  = type == ChartType.XY || type == ChartType.BUBBLE || type == ChartType.HISTOGRAM;
        if (isPieLike) {
            PiePlot plot = (PiePlot) chart.getPlot();
            List keys = plot.getDataset().getKeys();
            for (int i = 0; i < keys.size(); i++) {
                plot.setSectionPaint((Comparable) keys.get(i), palette[i % palette.length]);
            }
        } else if (isXYLike) {
            XYPlot plot = (XYPlot) chart.getPlot();
            XYItemRenderer renderer = plot.getRenderer();
            for (int i = 0; i < plot.getDataset().getSeriesCount(); i++) {
                renderer.setSeriesPaint(i, palette[i % palette.length]);
            }
        } else {
            CategoryPlot plot = (CategoryPlot) chart.getPlot();
            CategoryItemRenderer renderer = plot.getRenderer();
            for (int i = 0; i < plot.getDataset().getRowCount(); i++) {
                renderer.setSeriesPaint(i, palette[i % palette.length]);
            }
        }
    }

    private Color[] getPalette(String scheme) {
        switch (scheme != null ? scheme : "Spring") {
            case "Pastel":     return PALETTE_PASTEL;
            case "Earth":      return PALETTE_EARTH;
            case "Colorblind": return PALETTE_COLORBLIND;
            case "Monochrome": return PALETTE_MONOCHROME;
            case "Ocean":      return PALETTE_OCEAN;
            case "Sunset":     return PALETTE_SUNSET;
            case "Vibrant":    return PALETTE_VIBRANT;
            case "Forest":     return PALETTE_FOREST;
            case "Retro":      return PALETTE_RETRO;
            case "Neon":       return PALETTE_NEON;
            case "Spring":     return PALETTE_SPRING;
            case "Darkness":   return null;
            default:           return PALETTE_CORPORATE;
        }
    }

    private double parseProjDistance() {
        try { return Double.parseDouble(projField.getText().trim()); } catch (NumberFormatException e) { return 1500.0; }
    }

    private void applyProjection() {
        if (currentChart3D != null) currentChart3D.setProjDistance(parseProjDistance());
    }

    private void applyPieSettings(JFreeChart chart) {
        PiePlot plot = (PiePlot) chart.getPlot();
        switch ((String) pieLabelCombo.getSelectedItem()) {
            case "Key":             plot.setLabelGenerator(new StandardPieSectionLabelGenerator("{0}")); break;
            case "Key + Value":     plot.setLabelGenerator(new StandardPieSectionLabelGenerator("{0}: {1}")); break;
            case "Key + %":         plot.setLabelGenerator(new StandardPieSectionLabelGenerator("{0}: {2}")); break;
            case "Key + Value + %": plot.setLabelGenerator(new StandardPieSectionLabelGenerator("{0}: {1} ({2})")); break;
            default:                plot.setLabelGenerator(null); break;
        }
    }

    private void applyXYSettings(JFreeChart chart) {
        XYPlot plot = (XYPlot) chart.getPlot();
        plot.setDomainGridlinesVisible(gridCheckBox.isSelected());
        plot.setRangeGridlinesVisible(gridCheckBox.isSelected());
        if (dataLabelsCheckBox.isSelected()) {
            XYItemRenderer renderer = plot.getRenderer();
            renderer.setDefaultItemLabelGenerator(new StandardXYItemLabelGenerator());
            renderer.setDefaultItemLabelsVisible(true);
        }
        ValueAxis rangeAxis = plot.getRangeAxis();
        if (logScaleCheckBox.isSelected()) {
            LogAxis logAxis = new LogAxis(rangeAxis.getLabel());
            plot.setRangeAxis(logAxis);
            rangeAxis = logAxis;
        }
        applyYBounds(rangeAxis);
    }

    private void applyCategorySettings(JFreeChart chart) {
        CategoryPlot plot = (CategoryPlot) chart.getPlot();
        plot.setDomainGridlinesVisible(gridCheckBox.isSelected());
        plot.setRangeGridlinesVisible(gridCheckBox.isSelected());
        if (dataLabelsCheckBox.isSelected()) {
            CategoryItemRenderer renderer = plot.getRenderer();
            renderer.setDefaultItemLabelGenerator(new StandardCategoryItemLabelGenerator());
            renderer.setDefaultItemLabelsVisible(true);
        }
        CategoryAxis domainAxis = plot.getDomainAxis();
        switch ((String) xRotateCombo.getSelectedItem()) {
            case "45°": domainAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_45); break;
            case "90°": domainAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_90); break;
            default: break;
        }
        ValueAxis rangeAxis = plot.getRangeAxis();
        if (logScaleCheckBox.isSelected()) {
            LogAxis logAxis = new LogAxis(rangeAxis.getLabel());
            plot.setRangeAxis(logAxis);
            rangeAxis = logAxis;
        }
        applyYBounds(rangeAxis);
    }

    private void applyYBounds(ValueAxis axis) {
        String minText = yMinField.getText().trim();
        String maxText = yMaxField.getText().trim();
        try { if (!minText.isEmpty()) axis.setLowerBound(Double.parseDouble(minText)); } catch (NumberFormatException e) { /* ignore */ }
        try { if (!maxText.isEmpty()) axis.setUpperBound(Double.parseDouble(maxText)); } catch (NumberFormatException e) { /* ignore */ }
    }

    // -------------------------------------------------------------------------

    private void showExportMenu() {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem pngItem = new JMenuItem("Save as PNG...");
        pngItem.addActionListener(e -> saveChartAs("png"));
        menu.add(pngItem);
        JMenuItem jpgItem = new JMenuItem("Save as JPEG...");
        jpgItem.addActionListener(e -> saveChartAs("jpg"));
        menu.add(jpgItem);
        menu.addSeparator();
        JMenuItem svgItem = new JMenuItem("Save as SVG...");
        svgItem.addActionListener(e -> saveChartAsSVG());
        menu.add(svgItem);
        menu.addSeparator();
        JMenuItem clipItem = new JMenuItem("Copy to Clipboard");
        clipItem.addActionListener(e -> copyChartToClipboard());
        menu.add(clipItem);
        menu.show(exportButton, 0, exportButton.getHeight());
    }

    private void saveChartAs(String format) {
        JFileChooser chooser = new JFileChooser();
        chooser.setSelectedFile(new File("chart." + format));
        boolean isPng = "png".equals(format);
        chooser.setFileFilter(isPng
                ? new FileNameExtensionFilter("PNG Image (*.png)", "png")
                : new FileNameExtensionFilter("JPEG Image (*.jpg, *.jpeg)", "jpg", "jpeg"));
        if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION) return;
        int w = chartContainer.getWidth(), h = chartContainer.getHeight();
        try {
            if (currentChart != null) {
                if (isPng) ChartUtils.saveChartAsPNG(chooser.getSelectedFile(), currentChart, w, h);
                else       ChartUtils.saveChartAsJPEG(chooser.getSelectedFile(), currentChart, w, h);
            } else if (currentChart3D != null) {
                BufferedImage img = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
                currentChart3D.draw(img.createGraphics(), new java.awt.Rectangle(w, h));
                javax.imageio.ImageIO.write(img, isPng ? "png" : "jpeg", chooser.getSelectedFile());
            }
        } catch (IOException ex) {
            UIUtil.showException(this, "Export Error", ex);
        }
    }

    private void saveChartAsSVG() {
        JFileChooser chooser = new JFileChooser();
        chooser.setSelectedFile(new File("chart.svg"));
        chooser.setFileFilter(new FileNameExtensionFilter("SVG Image (*.svg)", "svg"));
        if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION) return;
        int w = chartContainer.getWidth(), h = chartContainer.getHeight();
        SVGGraphics2D g2 = new SVGGraphics2D(w, h);
        java.awt.geom.Rectangle2D bounds = new java.awt.Rectangle(w, h);
        if (currentChart != null)       currentChart.draw(g2, bounds);
        else if (currentChart3D != null) currentChart3D.draw(g2, bounds);
        else return;
        try (java.io.OutputStreamWriter fw = new java.io.OutputStreamWriter(new java.io.FileOutputStream(chooser.getSelectedFile()), StandardCharsets.UTF_8)) {
            fw.write(g2.getSVGElement());
        } catch (IOException ex) {
            UIUtil.showException(this, "Export Error", ex);
        }
    }

    private void copyChartToClipboard() {
        int w = chartContainer.getWidth(), h = chartContainer.getHeight();
        BufferedImage image;
        if (currentChart != null) {
            image = currentChart.createBufferedImage(w, h);
        } else if (currentChart3D != null) {
            image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
            currentChart3D.draw(image.createGraphics(), new java.awt.Rectangle(w, h));
        } else {
            return;
        }
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new Transferable() {
            @Override public DataFlavor[] getTransferDataFlavors() { return new DataFlavor[]{DataFlavor.imageFlavor}; }
            @Override public boolean isDataFlavorSupported(DataFlavor f) { return DataFlavor.imageFlavor.equals(f); }
            @Override public Object getTransferData(DataFlavor f) throws UnsupportedFlavorException {
                if (!isDataFlavorSupported(f)) throw new UnsupportedFlavorException(f);
                return image;
            }
        }, null);
    }

    // -------------------------------------------------------------------------

    private void showError(String msg) {
        chartContainer.removeAll();
        chartContainer.add(new JLabel("<html><center>Error: " + UIUtil.toHTMLFragment(msg != null ? msg : "unknown error", 80) + "</center></html>", SwingConstants.CENTER));
        chartContainer.revalidate();
        chartContainer.repaint();
    }

    private String cellString(Object value) {
        if (value instanceof TableModelItem) value = ((TableModelItem) value).value;
        if (value == null || value == UIUtil.NULL) return "(null)";
        return value.toString();
    }

    private double cellDouble(Object value) {
        if (value instanceof TableModelItem) value = ((TableModelItem) value).value;
        if (value == null || value == UIUtil.NULL) return Double.NaN;
        if (value instanceof Number) return ((Number) value).doubleValue();
        try { return Double.parseDouble(value.toString().trim().replace(",", ".")); }
        catch (NumberFormatException e) { return Double.NaN; }
    }

    private String stripHtml(String html) {
        if (html == null) return "";
        if (html.startsWith("<html>")) {
            Matcher m = BOLD_PATTERN.matcher(html);
            if (m.find()) return m.group(1);
            return html.replaceAll("<[^>]*>", " ").replaceAll("\\s+", " ").trim();
        }
        return html;
    }

    public void copySettingsFrom(SQLConsoleChartPanel other) {
        suppressUpdate = true;
        try {
            pendingXColumn = (String) other.xColumnCombo.getSelectedItem();
            pendingYColumns = new ArrayList<>();
            for (JCheckBox cb : other.yCheckBoxes) {
                if (cb.isSelected()) pendingYColumns.add(cb.getText());
            }
            chartTypeCombo.setSelectedItem(other.chartTypeCombo.getSelectedItem());
            titleField.setText(other.titleField.getText());
            legendCheckBox.setSelected(other.legendCheckBox.isSelected());
            dataLabelsCheckBox.setSelected(other.dataLabelsCheckBox.isSelected());
            gridCheckBox.setSelected(other.gridCheckBox.isSelected());
            chartTranspCheckBox.setSelected(other.chartTranspCheckBox.isSelected());
            plotTranspCheckBox.setSelected(other.plotTranspCheckBox.isSelected());
            sortXCheckBox.setSelected(other.sortXCheckBox.isSelected());
            aggregateCombo.setSelectedItem(other.aggregateCombo.getSelectedItem());
            rowLimitCombo.setSelectedItem(other.rowLimitCombo.getSelectedItem());
            colorSchemeCombo.setSelectedItem(other.colorSchemeCombo.getSelectedItem());
            orientationCombo.setSelectedItem(other.orientationCombo.getSelectedItem());
            stackedCheckBox.setSelected(other.stackedCheckBox.isSelected());
            xRotateCombo.setSelectedItem(other.xRotateCombo.getSelectedItem());
            yMinField.setText(other.yMinField.getText());
            yMaxField.setText(other.yMaxField.getText());
            logScaleCheckBox.setSelected(other.logScaleCheckBox.isSelected());
            binsCombo.setSelectedItem(other.binsCombo.getSelectedItem());
            projField.setText(other.projField.getText());
            fontSizeSpinner.setValue(other.fontSizeSpinner.getValue());
            lineWidthSpinner.setValue(other.lineWidthSpinner.getValue());
            textColor = other.textColor;
            updateTextColorIcon();
            pieLabelCombo.setSelectedItem(other.pieLabelCombo.getSelectedItem());
            updateSettingsForChartType();
        } finally {
            suppressUpdate = false;
        }
        // Copy view state (axis zoom / 3D viewpoint) from source panel
        pendingDomainRange = null;
        pendingRangeRange  = null;
        pendingViewPoint3D = null;
        if (other.currentChart != null) {
            if (other.currentChart.getPlot() instanceof XYPlot) {
                XYPlot xyPlot = (XYPlot) other.currentChart.getPlot();
                if (!xyPlot.getDomainAxis().isAutoRange()) pendingDomainRange = xyPlot.getDomainAxis().getRange();
                if (!xyPlot.getRangeAxis().isAutoRange())  pendingRangeRange  = xyPlot.getRangeAxis().getRange();
            } else if (other.currentChart.getPlot() instanceof CategoryPlot) {
                CategoryPlot catPlot = (CategoryPlot) other.currentChart.getPlot();
                if (!catPlot.getRangeAxis().isAutoRange()) pendingRangeRange = catPlot.getRangeAxis().getRange();
            }
        }
        if (other.currentChart3D != null) {
            pendingViewPoint3D = new ViewPoint3D(other.currentChart3D.getViewPoint());
        }
        // Don't call refreshColumnCombos()/updateChart() here:
        // rb.rowsTable has no columns yet at this point (updateTableModel() runs later).
        // setTable() will be triggered by updateColumnsAndTextView() once real data arrives.
    }

    private void updateTextColorIcon() {
        BufferedImage img = new BufferedImage(16, 16, BufferedImage.TYPE_INT_RGB);
        java.awt.Graphics2D g = img.createGraphics();
        g.setColor(textColor);
        g.fillRect(0, 0, 16, 16);
        g.setColor(Color.DARK_GRAY);
        g.drawRect(0, 0, 15, 15);
        g.dispose();
        textColorButton.setIcon(new ImageIcon(img));
    }
}
