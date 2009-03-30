/*
 * Copyright 2007 the original author or authors.
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
package net.sf.jailer.ui.graphical_view;

import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import prefuse.Display;
import prefuse.controls.AbstractZoomControl;
import prefuse.util.ColorLib;
import prefuse.util.display.DisplayLib;
import prefuse.util.ui.UILib;
import prefuse.visual.VisualItem;


/**
 * Zoom control for model view.
 * 
 * @author Ralf Wisser
 */
public class ZoomBoxControl extends AbstractZoomControl {
    
    /**
     * Label of Zoom-box item.
     */
    public static final String BOX_ITEM_LABEL = "$ZOOMBOX";

    /**
     * The renderer for the zoom box.
     */
    private ZoomBoxRenderer zoomBoxRenderer;
 
    /**
     * Zoom drag box as visual item.
     */
    private VisualItem boxItem;
    
    /**
     * Point at which dragging was started.
     */
    private Point2D down = null;
    
    /**
     * Current mouse position.
     */
    private Point2D current = null;
    
    /**
     * Mouse button to be used for zooming.
     */
    private int button = RIGHT_MOUSE_BUTTON;
    
    /**
     * Constructor.
     * 
     * @param visualGraph the visual model graph
     */
    public ZoomBoxControl() {
    	zoomBoxRenderer = new ZoomBoxRenderer();
    }
    
	/**
     * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(MouseEvent e) {
        if ( UILib.isButtonPressed(e, button) ) {
            Display display = (Display)e.getComponent();
            if (display.isTranformInProgress()) {
                return;
            }
            down = new Point2D.Float();
            current = null;
            display.getAbsoluteCoordinate(e.getPoint(), down);
        }
    }
    
    /**
     * Overridden to hide zoom box if no mouse button is pressed.
     */
    @Override
	public void mouseMoved(MouseEvent e) {
    	if (!UILib.isButtonPressed(e, button) ) {
    		hideZoomBox();
        }
	}

    /**
     * @see java.awt.event.MouseMotionListener#mouseDragged(java.awt.event.MouseEvent)
     */
    public void mouseDragged(MouseEvent e) {
        if (UILib.isButtonPressed(e, button) ) {
            Display display = (Display)e.getComponent();
            if (display.isTranformInProgress() || down == null) {
                hideZoomBox();
                return;
            }
            current = new Point2D.Float();
            display.getAbsoluteCoordinate(e.getPoint(), current);
       
            if (boxItem != null) {
            	boxItem.setVisible(true);
	            boxItem.setValidated(false);
	            boxItem.getVisualization().repaint();
            }
        }
    }

    /**
     * Hides zoom box.
     */
	private void hideZoomBox() {
		if (down != null || current != null) {
			down = null;
			current = null;
			if (boxItem != null) {
				boxItem.setValidated(false);
				boxItem.getVisualization().repaint();
			}
		}
	}

    /**
     * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(MouseEvent e) {
        if ( UILib.isButtonPressed(e, button) ) {
        	if (down != null && current != null) {
        		Display display = (Display)e.getComponent();
                DisplayLib.fitViewToBounds(display, getZoomBoxBounds(200), 500);
        	}
        	hideZoomBox();
        }
    }
    
    /**
     * @see prefuse.controls.Control#itemPressed(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemPressed(VisualItem item, MouseEvent e) {
        if ( m_zoomOverItem )
            mousePressed(e);
    }

    /**
     * @see prefuse.controls.Control#itemDragged(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemDragged(VisualItem item, MouseEvent e) {
        if ( m_zoomOverItem )
            mouseDragged(e);
    }
    
    /**
     * @see prefuse.controls.Control#itemReleased(prefuse.visual.VisualItem, java.awt.event.MouseEvent)
     */
    public void itemReleased(VisualItem item, MouseEvent e) {
        if ( m_zoomOverItem )
            mouseReleased(e);
    }

    /**
     * Gets bounding rectangle of zoom box.
     * 
     * @return bounding rectangle of zoom box
     */
	private Rectangle2D getZoomBoxBounds(double minWidth) {
		Rectangle2D bounds = new Rectangle2D.Double();
		
		if (down == null || current == null) {
			bounds.setRect(0, 0, 0, 0);
		} else {
			bounds.setRect(Math.min(down.getX(), current.getX()), Math.min(down.getY(), current.getY()), Math.max(minWidth, Math.abs(down.getX() - current.getX())), Math.max(minWidth, Math.abs(down.getY() - current.getY())));
		}
		
		return bounds;
	}
	
    /**
     * Gets the renderer for the zoom box.
     * 
     * @return the renderer for the zoom box
     */
	public ZoomBoxRenderer getRenderer() {
		return zoomBoxRenderer;
	}

	/**
	 * Renders the zoom box.
	 */
	public class ZoomBoxRenderer extends prefuse.render.ShapeRenderer {

		public boolean locatePoint(Point2D p, VisualItem item) {
			return false;
		}

		protected Shape getRawShape(VisualItem item) {
			Rectangle2D bounds = getZoomBoxBounds(1);
			return rectangle(bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight());
		}

		public void setBounds(VisualItem item) {
			Rectangle2D bounds = getZoomBoxBounds(1);
			item.setBounds(bounds.getX(), bounds.getY(), bounds.getWidth(), bounds.getHeight());
		}
		
		public boolean isBoxItem(VisualItem item) {
			if (BOX_ITEM_LABEL.equals(item.getString("label"))) {
				boxItem = item;
				item.setFillColor(ColorLib.rgba(150, 150, 255, 100));
				return true;
			}
			return false;
		}
		
	}
	
}
