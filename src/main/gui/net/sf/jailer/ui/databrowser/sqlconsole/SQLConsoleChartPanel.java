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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
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
import org.jfree.chart.labels.StandardXYItemLabelGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.category.CategoryItemRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
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
import com.orsoncharts.plot.CategoryPlot3D;
import com.orsoncharts.plot.PiePlot3D;
import com.orsoncharts.renderer.category.AbstractCategoryRenderer3D;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.databrowser.BrowserContentPane.TableModelItem;

/**
 * Chart panel for the SQL Console result view.
 * Renders JFreeChart diagrams from query result data.
 */
public class SQLConsoleChartPanel extends JPanel {

    private static final String CHART_BAR          = "Bar";
    private static final String CHART_LINE         = "Line";
    private static final String CHART_AREA         = "Area";
    private static final String CHART_STACKED_AREA = "Stacked Area";
    private static final String CHART_PIE          = "Pie";
    private static final String CHART_RING         = "Ring";
    private static final String CHART_XY           = "XY (Scatter)";
    private static final String CHART_BUBBLE       = "Bubble";
    private static final String CHART_HISTOGRAM    = "Histogram";
    private static final String CHART_BAR_3D       = "Bar 3D";
    private static final String CHART_PIE_3D       = "Pie 3D";

    // --- Main controls ---
    private final JComboBox<String> chartTypeCombo = new JComboBox<>(new String[]{
        CHART_BAR, CHART_LINE, CHART_AREA, CHART_STACKED_AREA,
        CHART_PIE, CHART_RING, CHART_XY, CHART_BUBBLE, CHART_HISTOGRAM,
        CHART_BAR_3D, CHART_PIE_3D
    });
    private final JComboBox<String> xColumnCombo   = new JComboBox<>();
    private final JButton           yButton        = new JButton("(none)");
    private final JPopupMenu        yPopup         = new JPopupMenu();
    private final JPanel            yCheckPanel    = new JPanel();
    private final List<JCheckBox>   yCheckBoxes    = new ArrayList<>();
    private final JButton           exportButton   = new JButton("Export");

    // --- Color palettes ---
    private static final Color[] PALETTE_PASTEL      = { new Color(174,199,232), new Color(255,187,120), new Color(152,223,138), new Color(255,152,150), new Color(197,176,213), new Color(196,156,148) };
    private static final Color[] PALETTE_EARTH       = { new Color(140, 86, 75), new Color(214,139, 74), new Color(188,189, 34), new Color( 23,190,207), new Color( 44,160, 44), new Color(127,127,127) };
    private static final Color[] PALETTE_COLORBLIND  = { new Color(  0,114,178), new Color(230,159,  0), new Color(  0,158,115), new Color(213, 94,  0), new Color( 86,180,233), new Color(204,121,167), new Color(0,0,0), new Color(240,228,66) };
    private static final Color[] PALETTE_MONOCHROME  = { new Color( 30, 30, 30), new Color( 80, 80, 80), new Color(130,130,130), new Color(180,180,180), new Color( 50, 50, 50), new Color(210,210,210) };

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
    private final JComboBox<String> colorSchemeCombo = new JComboBox<>(new String[]{"Default", "Pastel", "Earth", "Colorblind", "Monochrome", "Darkness"});
    private final JLabel colorSchemeLabel = new JLabel("Colors:");

    // --- Chart-specific settings ---
    private final JComboBox<String> orientationCombo = new JComboBox<>(new String[]{"Vertical", "Horizontal"});
    private final JCheckBox   stackedCheckBox   = new JCheckBox("Stacked");
    private final JComboBox<String> xRotateCombo = new JComboBox<>(new String[]{"0°", "45°", "90°"});
    private final JTextField  yMinField         = new JTextField(5);
    private final JTextField  yMaxField         = new JTextField(5);
    private final JCheckBox   logScaleCheckBox  = new JCheckBox("Log");
    private final JComboBox<String> binsCombo   = new JComboBox<>(new String[]{"10", "20", "50", "100"});
    private final JLabel            binsLabel   = new JLabel("Bins:");

    // --- Labels to enable/disable with controls ---
    private final JLabel orientationLabel = new JLabel("Orientation:");
    private final JLabel xRotateLabel     = new JLabel("X-Rotate:");
    private final JLabel yMinLabel        = new JLabel("Y-Min:");
    private final JLabel yMaxLabel        = new JLabel("Y-Max:");
    private final JLabel aggregateLabel   = new JLabel("Aggregate:");
    private final JLabel rowLimitLabel    = new JLabel("Rows:");

    private final JPanel chartContainer = new JPanel(new BorderLayout());

    private JTable currentTable;
    private JFreeChart currentChart;
    private Chart3D    currentChart3D;
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
        yPopupScroll.setPreferredSize(new Dimension(200, 150));
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
        xColumnCombo.addActionListener(redraw);
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

        binsLabel.setToolTipText("Number of histogram bins");
        binsCombo.setToolTipText("Number of histogram bins");
        row.add(binsLabel);
        row.add(Box.createHorizontalStrut(4));
        row.add(binsCombo);
        row.add(Box.createHorizontalStrut(4));

        return row;
    }

    private void updateSettingsForChartType() {
        String type = (String) chartTypeCombo.getSelectedItem();
        boolean is3D      = CHART_BAR_3D.equals(type) || CHART_PIE_3D.equals(type);
        boolean isBar     = CHART_BAR.equals(type);
        boolean isCat     = isBar || CHART_LINE.equals(type) || CHART_AREA.equals(type) || CHART_STACKED_AREA.equals(type);
        boolean isPie     = CHART_PIE.equals(type) || CHART_RING.equals(type);
        boolean isXYLike  = CHART_XY.equals(type) || CHART_BUBBLE.equals(type) || CHART_HISTOGRAM.equals(type);
        boolean isHist    = CHART_HISTOGRAM.equals(type);
        boolean hasStack  = isBar || CHART_LINE.equals(type) || CHART_AREA.equals(type);

        setEnabled2(orientationLabel, orientationCombo, !is3D && (isCat || isHist));
        setEnabled2(null, stackedCheckBox, !is3D && hasStack);
        setEnabled2(xRotateLabel, xRotateCombo, !is3D && isCat);
        setEnabled2(yMinLabel, yMinField, !is3D && !isPie);
        setEnabled2(yMaxLabel, yMaxField, !is3D && !isPie);
        setEnabled2(null, logScaleCheckBox, !is3D && !isPie);
        setEnabled2(null, gridCheckBox, !is3D && !isPie);
        setEnabled2(aggregateLabel, aggregateCombo, !is3D && (isCat || CHART_BAR_3D.equals(type)));
        setEnabled2(null, sortXCheckBox, !is3D && isCat);
        setEnabled2(binsLabel, binsCombo, isHist);
    }

    private void setEnabled2(JLabel label, java.awt.Component control, boolean enabled) {
        if (label != null) label.setEnabled(enabled);
        control.setEnabled(enabled);
    }

    // -------------------------------------------------------------------------

    public void setTable(JTable table) {
        this.currentTable = table;
        refreshColumnCombos();
        updateChart();
    }

    private void refreshColumnCombos() {
        if (currentTable == null) return;
        TableModel model = currentTable.getModel();
        TableColumnModel cm = currentTable.getColumnModel();
        int colCount = cm.getColumnCount();

        String xSel = (String) xColumnCombo.getSelectedItem();
        List<String> ySel = new ArrayList<>();
        for (JCheckBox cb : yCheckBoxes) {
            if (cb.isSelected()) ySel.add(cb.getText());
        }

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
                cb.addActionListener(e -> { updateYButtonLabel(); if (!suppressUpdate) updateChart(); });
                yCheckBoxes.add(cb);
                yCheckPanel.add(cb);
            }

            if (xSel != null) xColumnCombo.setSelectedItem(xSel);
            if (xColumnCombo.getSelectedIndex() < 0 && colCount > 0) xColumnCombo.setSelectedIndex(0);

            if (ySel.isEmpty() || yCheckBoxes.stream().noneMatch(JCheckBox::isSelected)) {
                int defY = findFirstNumericViewCol();
                int idx = defY >= 0 ? defY : Math.min(1, colCount - 1);
                if (idx < yCheckBoxes.size()) yCheckBoxes.get(idx).setSelected(true);
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

        String type = (String) chartTypeCombo.getSelectedItem();

        if (CHART_BAR_3D.equals(type) || CHART_PIE_3D.equals(type)) {
            try {
                Chart3D chart3d = CHART_BAR_3D.equals(type)
                    ? buildBar3DChart(model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels)
                    : buildPie3DChart(model, sorter, rowCount, xModelCol, yModelCols[0]);
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
            if (CHART_PIE.equals(type) || CHART_RING.equals(type)) {
                chart = buildPieChart(type, model, sorter, rowCount, xModelCol, yModelCols[0]);
            } else if (CHART_XY.equals(type)) {
                chart = buildXYChart(model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels);
            } else if (CHART_BUBBLE.equals(type)) {
                chart = buildBubbleChart(model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels);
            } else if (CHART_HISTOGRAM.equals(type)) {
                chart = buildHistogramChart(model, sorter, rowCount, yModelCols[0], yLabels[0]);
            } else {
                chart = buildCategoryChart(type, model, sorter, rowCount, xModelCol, yModelCols, xLabel, yLabels);
            }
        } catch (Exception ex) {
            showError(ex.getMessage());
            return;
        }

        applySettings(chart, type);

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

    private JFreeChart buildPieChart(String type, TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int yCol) {
        Map<String, Double> data = aggregateAndSort(model, sorter, rowCount, xCol, yCol);
        DefaultPieDataset dataset = new DefaultPieDataset();
        for (Map.Entry<String, Double> e : data.entrySet()) dataset.setValue(e.getKey(), e.getValue());
        boolean legend = legendCheckBox.isSelected();
        if (CHART_RING.equals(type)) {
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

    private JFreeChart buildCategoryChart(String type, TableModel model, RowSorter<? extends TableModel> sorter, int rowCount, int xCol, int[] yCols, String xLabel, String[] yLabels) {
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

        if (CHART_BAR.equals(type)) {
            return stacked
                    ? ChartFactory.createStackedBarChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false)
                    : ChartFactory.createBarChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false);
        }
        if (CHART_AREA.equals(type)) {
            return stacked
                    ? ChartFactory.createStackedAreaChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false)
                    : ChartFactory.createAreaChart(null, xLabel, yAxisLabel, dataset, orientation, legend, true, false);
        }
        if (CHART_STACKED_AREA.equals(type)) {
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
        if (palette != null) ((PiePlot3D) chart.getPlot()).setSectionColors(palette);
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

    private void applySettings(JFreeChart chart, String type) {
        String scheme = (String) colorSchemeCombo.getSelectedItem();
        if ("Darkness".equals(scheme)) StandardChartTheme.createDarknessTheme().apply(chart);

        chart.setBackgroundPaint(chartTranspCheckBox.isSelected() ? null : Color.WHITE);
        if (chart.getLegend() != null) chart.getLegend().setBackgroundPaint(chartTranspCheckBox.isSelected() ? null : Color.WHITE);
        chart.getPlot().setBackgroundPaint(plotTranspCheckBox.isSelected() ? null : Color.WHITE);

        String title = titleField.getText().trim();
        chart.setTitle(title.isEmpty() ? null : title);
        if (chart.getLegend() != null) chart.getLegend().setVisible(legendCheckBox.isSelected());

        boolean isPieLike = CHART_PIE.equals(type) || CHART_RING.equals(type);
        boolean isXYLike  = CHART_XY.equals(type) || CHART_BUBBLE.equals(type) || CHART_HISTOGRAM.equals(type);
        if (isPieLike) {
            applyPieSettings(chart);
        } else if (isXYLike) {
            applyXYSettings(chart);
        } else {
            applyCategorySettings(chart);
        }

        applyColorScheme(chart, type, scheme);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void applyColorScheme(JFreeChart chart, String type, String scheme) {
        Color[] palette = getPalette(scheme);
        if (palette == null) return;
        boolean isPieLike = CHART_PIE.equals(type) || CHART_RING.equals(type);
        boolean isXYLike  = CHART_XY.equals(type) || CHART_BUBBLE.equals(type) || CHART_HISTOGRAM.equals(type);
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
        switch (scheme != null ? scheme : "Default") {
            case "Pastel":     return PALETTE_PASTEL;
            case "Earth":      return PALETTE_EARTH;
            case "Colorblind": return PALETTE_COLORBLIND;
            case "Monochrome": return PALETTE_MONOCHROME;
            default:           return null;
        }
    }

    private void applyPieSettings(JFreeChart chart) {
        PiePlot plot = (PiePlot) chart.getPlot();
        if (!dataLabelsCheckBox.isSelected()) plot.setLabelGenerator(null);
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
        try (FileWriter fw = new FileWriter(chooser.getSelectedFile(), StandardCharsets.UTF_8)) {
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
}
