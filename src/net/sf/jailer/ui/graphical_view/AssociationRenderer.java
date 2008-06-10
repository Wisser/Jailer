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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;

import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import prefuse.Constants;
import prefuse.render.EdgeRenderer;
import prefuse.util.GraphicsLib;
import prefuse.visual.EdgeItem;
import prefuse.visual.VisualItem;

/**
 * Renderer for {@link Association}s.
 * 
 * @author Ralf Wisser
 */
public class AssociationRenderer extends EdgeRenderer {

	// color setting
	public static Color COLOR_IGNORED;
	public static Color COLOR_ASSOCIATION;
	public static Color COLOR_DEPENDENCY;
	public static Color COLOR_REVERSE_DEPENDENCY;
	
	/**
	 * <code>true</code> for reversed rendering.
	 */
	boolean reversed;

	/**
	 * <code>true</code> for full rendering (for setting bounds).
	 */
	boolean full = false;

	/**
	 * Constructor.
	 * 
	 * @param reversed <code>true</code> for reversed rendering
	 */
	public AssociationRenderer(boolean reversed) {
		super(Constants.EDGE_TYPE_LINE, reversed? Constants.EDGE_ARROW_REVERSE : Constants.EDGE_ARROW_FORWARD);
		this.reversed = reversed;
	}

	/**
	 * Constructor.
	 */
	public AssociationRenderer() {
		full = true;
	}
    
	/**
     * Return a non-transformed shape for the visual representation of the
     * {@link Association}.
     * 
     * @param item the VisualItem being drawn
     * @return the "raw", untransformed shape
     */
	@Override
	protected Shape getRawShape(VisualItem item) {
		EdgeItem   edge = (EdgeItem)item;
		VisualItem item1 = edge.getSourceItem();
		VisualItem item2 = edge.getTargetItem();
		
		int type = m_edgeType;
		boolean reversedCurve = false;
		Association association = (Association) item.get("association");
		if (association != null && association.source == association.destination) {
			type = Constants.EDGE_TYPE_CURVE;
			reversedCurve = association.reversed;
		}
		
		getAlignedPoint(m_tmpPoints[0], item1.getBounds(),
		                m_xAlign1, m_yAlign1);
		getAlignedPoint(m_tmpPoints[1], item2.getBounds(),
		                m_xAlign2, m_yAlign2);
		m_curWidth = (float)(m_width * getLineWidth(item));
		
		double midX = (m_tmpPoints[0].getX() + m_tmpPoints[1].getX()) / 2;
		double midY = (m_tmpPoints[0].getY() + m_tmpPoints[1].getY()) / 2;
		
		if (!full) {
			m_tmpPoints[reversed? 1 : 0].setLocation(midX, midY);
		}
		
		// create the arrow head, if needed
		EdgeItem e = (EdgeItem)item;
		if ( e.isDirected() && m_edgeArrow != Constants.EDGE_ARROW_NONE) {
		    // get starting and ending edge endpoints
		    boolean forward = (m_edgeArrow == Constants.EDGE_ARROW_FORWARD);
		    Point2D start = null, end = null;
		    start = m_tmpPoints[forward?0:1];
		    end   = m_tmpPoints[forward?1:0];
		    
		    if (type == Constants.EDGE_TYPE_CURVE) {
			    AffineTransform t = new AffineTransform();
		    	t.setToRotation(Math.PI/4 * (reversedCurve? 1 : -1));
		    	Point2D p = new Point2D.Double(), shift = new Point2D.Double();
		    	double d = start.distance(end) / 5.0;
		    	p.setLocation((end.getX() - start.getX()) / d, (end.getY() - start.getY()) / d);
		    	t.transform(p, shift);
		    	start.setLocation(start.getX() + shift.getX(), start.getY() + shift.getY());
		    	end.setLocation(end.getX() + shift.getX(), end.getY() + shift.getY());
		    }
	    	
		    // compute the intersection with the target bounding box
		    VisualItem dest = forward ? e.getTargetItem() : e.getSourceItem();
		    int i = GraphicsLib.intersectLineRectangle(start, end,
		            dest.getBounds(), m_isctPoints);
		    if ( i > 0 ) end = m_isctPoints[0];
		    
		    // create the arrow head shape
		    AffineTransform at = getArrowTrans(start, end, m_curWidth);
		    m_curArrow = at.createTransformedShape(m_arrowHead);
		    
		    // update the endpoints for the edge shape
		    // need to bias this by arrow head size
		    if (type == Constants.EDGE_TYPE_CURVE) {
		    	if (!"XML".equals(association.getDataModel().getExportModus()) || !isAggregation(association)) {
		    		m_curArrow = null;
		    	}
		    }
	    	Point2D lineEnd = m_tmpPoints[forward?1:0]; 
	    	lineEnd.setLocation(0, type == Constants.EDGE_TYPE_CURVE? 0 : -m_arrowHeight);
	    	at.transform(lineEnd, lineEnd);
		} else {
		    m_curArrow = null;
		}
		
		// create the edge shape
		Shape shape = null;
		double n1x = m_tmpPoints[0].getX();
		double n1y = m_tmpPoints[0].getY();
		double n2x = m_tmpPoints[1].getX();
		double n2y = m_tmpPoints[1].getY();
        m_line.setLine(n1x, n1y, n2x, n2y);
        shape = m_line;
        
		return shape;
	}

	/**
     * Returns an affine transformation that maps the arrowhead shape
     * to the position and orientation specified by the provided
     * line segment end points.
     */
    protected AffineTransform getArrowTrans(Point2D p1, Point2D p2, 
                                            double width)
    {
        m_arrowTrans.setToTranslation(p2.getX(), p2.getY());
        m_arrowTrans.rotate(-HALF_PI + 
            Math.atan2(p2.getY()-p1.getY(), p2.getX()-p1.getX()));
        if ( width > 1 ) {
            double scalar = width/2;
            m_arrowTrans.scale(scalar, scalar);
        }
        return m_arrowTrans;
    }

    /**
     * Renders an {@link Association}.
     * 
     * @param g the 2D graphics
     * @param item visual item for the association
     * @param isSelected <code>true</code> for selected association
     */
    public void render(Graphics2D g, VisualItem item, boolean isSelected) {
		Association association = (Association) item.get("association");
		item.setSize(isSelected? 3 : 1);
		int color;
		if (!Boolean.TRUE.equals(item.get("full"))) {
			if (!full) {
				return;
			}
			color = associationColor(association);
		} else {
			if (full) {
				return;
			}
			color = reversed? associationColor(association.reversalAssociation) : associationColor(association);
		}
		item.setFillColor(color);
		item.setStrokeColor(color);
		BasicStroke stroke = item.getStroke();
		if (stroke != null) {
			if (reversed) {
				if (association != null) {
					association = association.reversalAssociation;
				}
			}
			if (association != null && association.isRestricted() && !association.isIgnored()) {
				item.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit(), 
					new float[] { 8f, 6f }, 1.0f));
			} else {
				item.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit()));
			}
		}
		if ("XML".equals(association.getDataModel().getExportModus())) {
			m_arrowHead = updateArrowHead(m_arrowWidth, m_arrowHeight, association, isSelected);
			arrowIsPotAggregation = true;
		} else {
			if (arrowIsPotAggregation) {
				m_arrowHead = updateArrowHead(m_arrowWidth, m_arrowHeight);
			}
			arrowIsPotAggregation = false;
		}
		render(g, item);
	}

    private boolean arrowIsPotAggregation = false;
    
    /**
     * Gets color for association.
     * 
     * @param association the association
     * @return the color for the association
     */
	private int associationColor(Association association) {
		if (association.isIgnored()) {
			return COLOR_IGNORED.getRGB();
		}
		if (association.isInsertDestinationBeforeSource()) {
			return COLOR_DEPENDENCY.getRGB();
		}
		if (association.isInsertSourceBeforeDestination()) {
			return COLOR_REVERSE_DEPENDENCY.getRGB();
		}
		return COLOR_ASSOCIATION.getRGB();
	}

	 /**
     * Returns true if the Point is located inside the extents of the item.
     * This calculation matches against the exact item shape, and so is more
     * sensitive than just checking within a bounding box.
     * 
     * @param p the point to test for containment
     * @param item the item to test containment against
     * @return true if the point is contained within the the item, else false
     */
	@Override
	public boolean locatePoint(Point2D p, VisualItem item) {
		Shape s = getShape(item);
		if ( s == null ) {
		    return false;
		} else {
		    double width = Math.max(12, getLineWidth(item));
		    double halfWidth = width/2.0;
		    return s.intersects(p.getX()-halfWidth,
		                        p.getY()-halfWidth,
		                        width,width);
		}
	}

	/**
	 * Render aggregation symbols.
	 */
	protected Polygon updateArrowHead(int w, int h, Association association, boolean isSelected) {
		if (isAggregation(association)) {
			if ( m_arrowHead == null ) {
			    m_arrowHead = new Polygon();
			} else {
			    m_arrowHead.reset();
			}
			double ws = 0.9;
			double hs = 2.0/3.0;
			if (isSelected) {
				ws /= 1.3;
				hs /= 1.3;
			}
			m_arrowHead.addPoint(0, 0);
			m_arrowHead.addPoint((int) (ws*-w), (int) (hs*(-h)));
			m_arrowHead.addPoint( 0, (int) (hs*(-2*h)));
			m_arrowHead.addPoint((int) (ws*w), (int) (hs*(-h)));
			m_arrowHead.addPoint(0, 0);
			return m_arrowHead;
		} else {
			return updateArrowHead(w, h);
		}
	}

	/**
	 * Checks whether association must be rendered as aggregation.
	 * 
	 * @param association the association to check
	 * @return <code>true</code> if association must be rendered as aggregation
	 */
	private boolean isAggregation(Association association) {
		return association.reversalAssociation.getAggregationSchema() != AggregationSchema.NONE;
	}
	
}
