package net.sf.jailer.ui.graphical_view;

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
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.render.HtmlDataModelRenderer;
import net.sf.jailer.ui.UIUtil;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.util.GraphicsLib;
import prefuse.util.io.IOLib;
import prefuse.util.io.SimpleFileFilter;
import prefuse.visual.tuple.TableVisualItem;

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
			String s = fmts[i].toLowerCase(Locale.ENGLISH);
			if ((s.length() == 3) &&!availableFormats.contains(s)) {
				availableFormats.add(s);
				chooser.setFileFilter(new SimpleFileFilter(s, s.toUpperCase(Locale.ENGLISH) + " Image (*." + s + ")"));
			}
		}

		availableFormats.clear();
		availableFormats = null;

	}

	/**
	 * This method lets the user select the target file and exports the <code>Display</code>
	 * @param model 
	 * 
	 * @paran display the <code>Display</code> to export
	 *
	 */
	public void export(Display display, File img, File mapHtmlFile, DataModel model) throws Exception {

		// Initialize if needed
		if (chooser == null && img == null) {
			init();
		}

		// open image save dialog
		File f         = img;
		String format;
		if (f == null) {
			int  returnVal = chooser.showSaveDialog(display);
	
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				f = chooser.getSelectedFile();
			} else {
				return;
			}
			format = ((SimpleFileFilter) chooser.getFileFilter()).getExtension();
			String ext    = IOLib.getExtension(f);

			if (!format.equals(ext)) {
				f = new File(f.toString() + "." + format);
			}
		} else {
			format = IOLib.getExtension(f);
		}
		
		// Now save the image
		f.getParentFile().mkdirs();
		OutputStream out = new BufferedOutputStream(new FileOutputStream(f));

		exportImage(display, out, format, img, mapHtmlFile, model);
		out.flush();
		out.close();
	}

	private boolean exportImage(Display display, OutputStream output, String format, File imgFile, File mapHtmlFile, DataModel model) throws Exception {

		String m_group = Visualization.ALL_ITEMS;

		try {
			UIUtil.setWaitCursor(display);
			
			// Now comes the nice part

			// Get the bounding box
			Rectangle2D bounds = display.getVisualization().getBounds(m_group);
			
			// Some little extra spacing
			GraphicsLib.expand(bounds, 10 + (1 /* / display.getScale() */));
			
			// Get a buffered image to draw into
			BufferedImage img = getNewOffscreenBuffer(display, Math.max(1, (int) (bounds.getWidth() * display.getScale())), Math.max(1, (int) (bounds.getHeight() * display.getScale())));
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

			if (mapHtmlFile != null) {
				PrintWriter out = new PrintWriter(mapHtmlFile);
//				out.println("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
//				out.println("<html>");
//				out.println("<head>");
//				out.println(" <meta content=\"text/html; charset=ISO-8859-1\"");
//				out.println(" http-equiv=\"content-type\">");
//				out.println(" <title>JailerModel</title>");
//				out.println(" <link href=\"styles.css\" rel=\"stylesheet\" type=\"text/css\">");
//				out.println("</head>");
//				out.println("<body>");
//				out.println("<h1 style=\"font-style: italic;\"><small>JailerModel<small><small> " + SimpleDateFormat.getInstance().format(new Date()) + "</small></small></small></h1>");
				out.println("<img src=\"" + imgFile.getParentFile().getName() + "/" + imgFile.getName() + "\" usemap=\"#TableMap\" />");
				out.println("<map name=\"TableMap\">");
				
				@SuppressWarnings("rawtypes")
				Iterator ii = display.getVisualization().items();
				while (ii.hasNext()) {
					Object o = ii.next();
					
					if (o instanceof TableVisualItem) {
						TableVisualItem vi = (TableVisualItem) o;
						String tableName = null;
						Table table = null;
						if (vi.canGetString("label")) {
							tableName = vi.getString("label");
							table = model.getTable(tableName);
							if (table != null) {
								tableName = model.getDisplayName(table);
							}
													}
						if (table != null && tableName != null && vi.isVisible() && !ZoomBoxControl.BOX_ITEM_LABEL.equals(tableName)) {
							Rectangle2D viBounds = vi.getBounds();
							
							int x = (int) ((viBounds.getX() - bounds.getX()) * scale);
							int y = (int) ((viBounds.getY() - bounds.getY()) * scale);
							int w = (int) (viBounds.getWidth() * scale);
							int h = (int) (viBounds.getHeight() * scale);
							
							out.println("  <area shape=\"rect\" coords=\"" + x + "," + y + "," + (x + w) + "," + (y + h) + "\"");
							out.println("    href=\"" + HtmlDataModelRenderer.CONTENT_FOLDER_NAME + "/" + HtmlDataModelRenderer.toFileName(table) + "\""); 
							out.println("    title=\"" + HtmlDataModelRenderer.escapeHtmlEntities(tableName) + "\" />");
						}
					}
				}
				out.println("</map>");
//				out.println("</body>");
//				out.println("</html>");
				out.close();
			}
			
			return true;
		} finally {
			UIUtil.resetWaitCursor(display);
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
