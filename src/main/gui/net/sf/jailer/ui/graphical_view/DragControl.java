package net.sf.jailer.ui.graphical_view;

import java.awt.Cursor;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;

import javax.swing.SwingUtilities;

import prefuse.Display;
import prefuse.controls.ControlAdapter;
import prefuse.data.Table;
import prefuse.data.event.EventConstants;
import prefuse.data.event.TableListener;
import prefuse.visual.VisualItem;


/**
 * Changes a node's location when dragged on screen. Other effects
 * include fixing a node's position when the mouse if over it, and
 * changing the mouse cursor to a hand when the mouse passes over an
 * item.
 *
 * @author <a href="http://jheer.org">jeffrey heer</a>
 */
public class DragControl extends ControlAdapter implements TableListener {

    private VisualItem activeItem;
    protected String action;
    protected Point2D down = new Point2D.Double();
    protected Point2D temp = new Point2D.Double();
    protected boolean dragged, wasFixed, resetItem;
	private boolean m_DownValid;
    private boolean fixOnMouseOver = true;
    protected boolean repaint = true;
    
    /**
     * Creates a new drag control that issues repaint requests as an item
     * is dragged.
     */
    public DragControl() {
    }
    
    /**
     * Creates a new drag control that optionally issues repaint requests
     * as an item is dragged.
     * @param repaint indicates whether or not repaint requests are issued
     *  as drag events occur. This can be set to false if other activities
     *  (for example, a continuously running force simulation) are already
     *  issuing repaint events.
     */
    public DragControl(boolean repaint) {
        this.repaint = repaint;
    }
    
    /**
     * Creates a new drag control that optionally issues repaint requests
     * as an item is dragged.
     * @param repaint indicates whether or not repaint requests are issued
     *  as drag events occur. This can be set to false if other activities
     *  (for example, a continuously running force simulation) are already
     *  issuing repaint events.
     * @param fixOnMouseOver indicates if object positions should become
     * fixed (made stationary) when the mouse pointer is over an item.
     */
    public DragControl(boolean repaint, boolean fixOnMouseOver) {
        this.repaint = repaint;
        this.fixOnMouseOver = fixOnMouseOver;
    }
    
    /**
     * Creates a new drag control that invokes an action upon drag events.
     * @param action the action to run when drag events occur.
     */
    public DragControl(String action) {
        this.repaint = false;
        this.action = action;
    }
    
    /**
     * Creates a new drag control that invokes an action upon drag events.
     * @param action the action to run when drag events occur
     * @param fixOnMouseOver indicates if object positions should become
     * fixed (made stationary) when the mouse pointer is over an item.
     */
    public DragControl(String action, boolean fixOnMouseOver) {
        this.repaint = false;
        this.fixOnMouseOver = fixOnMouseOver;
        this.action = action;
    }
    
    /**
     * Determines whether or not an item should have it's position fixed
     * when the mouse moves over it.
     * @param s whether or not item position should become fixed upon
     *  mouse over.
     */
    public void setFixPositionOnMouseOver(boolean s) {
        fixOnMouseOver = s;
    }
    
    /**
     * @see prefuse.controls.Control#itemEntered(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemEntered(VisualItem item, MouseEvent e) {
        Display d = (Display)e.getSource();
        d.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        activeItem = item;
        if ( fixOnMouseOver ) {
            wasFixed = item.isFixed();
            resetItem = true;
            item.setFixed(true);
            item.getTable().addTableListener(this);
        }
    }
    
    /**
     * @see prefuse.controls.Control#itemExited(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemExited(VisualItem item, MouseEvent e) {
        if ( activeItem == item ) {
            activeItem = null;
            item.getTable().removeTableListener(this);
            if ( resetItem ) item.setFixed(wasFixed);
        }
        Display d = (Display)e.getSource();
        d.setCursor(null); // d.setCursor(Cursor.getDefaultCursor());
    } //
    
    /**
     * @see prefuse.controls.Control#itemPressed(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemPressed(VisualItem item, MouseEvent e) {
        if (!SwingUtilities.isLeftMouseButton(e)) return;
        if ( !fixOnMouseOver ) {
            wasFixed = item.isFixed();
            resetItem = true;
            item.setFixed(true);
            item.getTable().addTableListener(this);
        }
        dragged = false;
        Display d = (Display)e.getComponent();
        d.getAbsoluteCoordinate(e.getPoint(), down);
        m_DownValid = true;
    }
    
    /**
     * @see prefuse.controls.Control#itemReleased(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemReleased(VisualItem item, MouseEvent e) {
        if (!SwingUtilities.isLeftMouseButton(e)) return;
        if ( dragged ) {
            activeItem = null;
            item.getTable().removeTableListener(this);
            if ( resetItem ) item.setFixed(wasFixed);
            dragged = false;
        }
        m_DownValid = false;
    }
    
    /**
     * @see prefuse.controls.Control#itemDragged(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemDragged(VisualItem item, MouseEvent e) {
        if (!SwingUtilities.isLeftMouseButton(e)) return;
        dragged = true;
        Display d = (Display)e.getComponent();
        d.getAbsoluteCoordinate(e.getPoint(), temp);
        double dx = temp.getX()-down.getX();
        double dy = temp.getY()-down.getY();
        double x = item.getX();
        double y = item.getY();

        if (m_DownValid) {
	        item.setStartX(x);  item.setStartY(y);
	        item.setX(x+dx);    item.setY(y+dy);
	        item.setEndX(x+dx); item.setEndY(y+dy);
        }

        if ( repaint )
            item.getVisualization().repaint();
        
        down.setLocation(temp);
        m_DownValid = true;
        if ( action != null )
            d.getVisualization().run(action);
    }

    /**
     * @see prefuse.data.event.TableListener#tableChanged(prefuse.data.Table, int, int, int, int)
     */
    public void tableChanged(Table t, int start, int end, int col, int type) {
        if ( activeItem == null || type != EventConstants.UPDATE 
                || col != t.getColumnNumber(VisualItem.FIXED) )
            return;
        int row = activeItem.getRow();
        if ( row >= start && row <= end )
            resetItem = false;
    }

} // end of class DragControl
