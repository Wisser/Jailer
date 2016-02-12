package net.sf.jailer.ui.graphical_view;

import java.awt.Cursor;
import java.awt.event.MouseEvent;

import prefuse.Display;
import prefuse.controls.ControlAdapter;
import prefuse.util.ui.UILib;
import prefuse.visual.VisualItem;


/**
 * Pans the display, changing the viewable region of the visualization.
 * By default, panning is accomplished by clicking on the background of a
 * visualization with the left mouse button and then dragging.
 *
 * @author <a href="http://jheer.org">jeffrey heer</a>
 */
public class PanControl extends ControlAdapter {

    private boolean m_panOverItem;
    private boolean m_DownValid;
    private int m_xDown, m_yDown;
    private int m_button;
    
    /**
     * Create a new PanControl.
     */
    public PanControl() {
        this(LEFT_MOUSE_BUTTON, false);
    }
    
    /**
     * Create a new PanControl.
     * @param panOverItem if true, the panning control will work even while
     * the mouse is over a visual item.
     */
    public PanControl(boolean panOverItem) {
        this(LEFT_MOUSE_BUTTON, panOverItem);
    }
    
    /**
     * Create a new PanControl.
     * @param mouseButton the mouse button that should initiate a pan. One of
     * {@link Control#LEFT_MOUSE_BUTTON}, {@link Control#MIDDLE_MOUSE_BUTTON},
     * or {@link Control#RIGHT_MOUSE_BUTTON}.
     */
    public PanControl(int mouseButton) {
        this(mouseButton, false);
    }
    
    /**
     * Create a new PanControl
     * @param mouseButton the mouse button that should initiate a pan. One of
     * {@link Control#LEFT_MOUSE_BUTTON}, {@link Control#MIDDLE_MOUSE_BUTTON},
     * or {@link Control#RIGHT_MOUSE_BUTTON}.
     * @param panOverItem if true, the panning control will work even while
     * the mouse is over a visual item.
     */
    public PanControl(int mouseButton, boolean panOverItem) {
        m_button = mouseButton;
        m_panOverItem = panOverItem;
    }
    
    // ------------------------------------------------------------------------
    
    /**
     * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(MouseEvent e) {
        if ( UILib.isButtonPressed(e, m_button) ) {
            e.getComponent().setCursor(
                Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
            m_xDown = e.getX();
            m_yDown = e.getY();
            m_DownValid = true;
        }
    }
    
    /**
     * @see java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.MouseEvent)
     */
    public void mouseDragged(MouseEvent e) {
        if ( UILib.isButtonPressed(e, m_button) ) {
            Display display = (Display)e.getComponent();
            int x = e.getX(),   y = e.getY();
            int dx = x-m_xDown, dy = y-m_yDown;
            if (m_DownValid) {
            	display.pan(dx,dy);
            }
            m_xDown = x;
            m_yDown = y;
            m_DownValid = true;
            display.repaint();
        }
    }
    
    /**
     * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(MouseEvent e) {
        if ( UILib.isButtonPressed(e, m_button) ) {
            e.getComponent().setCursor(Cursor.getDefaultCursor());
            m_xDown = -1;
            m_yDown = -1;
            m_DownValid = false;
        }
    }
    
    /**
     * @see prefuse.controls.Control#itemPressed(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemPressed(VisualItem item, MouseEvent e) {
        if ( m_panOverItem )
            mousePressed(e);
    }

    /**
     * @see prefuse.controls.Control#itemDragged(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemDragged(VisualItem item, MouseEvent e) {
        if ( m_panOverItem )
            mouseDragged(e);
    }
    
    /**
     * @see prefuse.controls.Control#itemReleased(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemReleased(VisualItem item, MouseEvent e) {
        if ( m_panOverItem )
            mouseReleased(e);
    }
    
} // end of class PanControl
