package net.sf.jailer.ui.graphical_view;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.HashSet;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;

import prefuse.Display;
import prefuse.Visualization;
import prefuse.util.GraphicsLib;
import prefuse.util.io.IOLib;
import prefuse.util.io.SimpleFileFilter;

/**
 * This class exports a prefuse.Display to a graphics file. The scalefactor will be 1.
 *
 *
 * @version    1.0.0, 07.11.2008
 * @author     Marcus St&auml;nder (<a href="mailto:webmaster@msdevelopment.org">webmaster@msdevelopment.org</a>)    
 */
public class DisplayExporter {

    /** The FileChooser to select the file to save to */
    protected JFileChooser  chooser = null;

    //~--- Constructors -------------------------------------------------------

    public DisplayExporter() {
    }

    //~--- Methods ------------------------------------------------------------

    /**
     * This method initiates the chooser components, detecting available image formats
     *
     */
    protected void init() {

        // Initialize the chooser
        chooser = new JFileChooser();
        chooser.setDialogType(JFileChooser.SAVE_DIALOG);
        chooser.setDialogTitle("Export graph as image");
        chooser.setAcceptAllFileFilterUsed(false);

        HashSet<String> availableFormats = new HashSet<String>();
        String[]        fmts             = ImageIO.getWriterFormatNames();

        for (int i = 0; i < fmts.length; i++) {
            String s = fmts[i].toLowerCase();
            if ((s.length() == 3) &&!availableFormats.contains(s)) {
                availableFormats.add(s);
                chooser.setFileFilter(new SimpleFileFilter(s, s.toUpperCase() + " Image (*." + s + ")"));
            }
        }

        availableFormats.clear();
        availableFormats = null;

    }

    /**
     * This method lets the user select the target file and exports the <code>Display</code>
     * 
     * @paran display the <code>Display</code> to export
     *
     */
    public void export(Display display) throws Exception {

        // Initialize if needed
        if (chooser == null) {
            init();
        }

        // open image save dialog
        File f         = null;
        int  returnVal = chooser.showSaveDialog(display);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            f = chooser.getSelectedFile();
        } else {
            return;
        }

        String format = ((SimpleFileFilter) chooser.getFileFilter()).getExtension();
        String ext    = IOLib.getExtension(f);

        if (!format.equals(ext)) {
            f = new File(f.toString() + "." + format);
        }

        // Now save the image
        OutputStream out = new BufferedOutputStream(new FileOutputStream(f));

        exportImage(display, out, format);
        out.flush();
        out.close();
    }

    
    private boolean exportImage(Display display, OutputStream output, String format) throws Exception {

        String m_group = Visualization.ALL_ITEMS;

        try {
        	display.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        	
        	// Now comes the nice part

            // Get the bounding box
            Rectangle2D bounds = display.getVisualization().getBounds(m_group);
            
            // Some little extra spacing
            GraphicsLib.expand(bounds, 10 + (int) (1 /* / display.getScale() */));
            
            // Get a buffered image to draw into
            BufferedImage img = getNewOffscreenBuffer(display, (int) (bounds.getWidth() * display.getScale()), (int) (bounds.getHeight() * display.getScale()));
            Graphics2D g = (Graphics2D)img.getGraphics();

            /*
             * Set up the display, render, then revert to normal settings
             */
  
            // The zoom point, zooming should not change anything else than the scale
//          Point2D zoomPoint = new Point2D.Double(0, 0);

            // Get and remember the current scaling
            Double scale = display.getScale();

            // Change scale to normal (1)
//            display.zoom(zoomPoint, 1/scale);

            boolean isHighQuality = display.isHighQuality();
            display.setHighQuality(true);

            // Remember the current point
            Point2D currentPoint = new Point2D.Double(display.getDisplayX(), display.getDisplayY());

            // Now pan so the most left element is at the left side of the display and 
            // the highest element is at the top.
            display.panToAbs(new Point2D.Double(bounds.getMinX() + display.getWidth()/scale/2,
                    bounds.getMinY() + display.getHeight()/scale/2));

            // Now lets prefuse to the actual painting
            display.paintDisplay(g, new Dimension((int) (bounds.getWidth() * scale), (int) (bounds.getHeight() * scale)));

            // Undo the panning, zooming and reset the quality mode
            display.panToAbs(new Point2D.Double((currentPoint.getX() + display.getWidth()/2)/scale,
                    (currentPoint.getY() + display.getHeight()/2)/scale));
            display.setHighQuality(isHighQuality);
//            display.zoom(zoomPoint, scale);    // also takes care of damage report

            // Save the image and return
            ImageIO.write(img, format, output);

            return true;
        } finally {
        	display.setCursor(Cursor.getDefaultCursor());
        }
    }

    //~--- Get methods --------------------------------------------------------

    // From Display
    private BufferedImage getNewOffscreenBuffer(Display display, int width, int height) {
        BufferedImage img = null;

        if (!GraphicsEnvironment.isHeadless()) {
            try {
                img = (BufferedImage) display.createImage(width, height);
            } catch (Exception e) {
                img = null;
            }
        }

        if (img == null) {
            return new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        }

        return img;
    }
}
