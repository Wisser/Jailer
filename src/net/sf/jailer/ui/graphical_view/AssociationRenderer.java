package net.sf.jailer.ui.graphical_view;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;

import net.sf.jailer.datamodel.Association;
import prefuse.Constants;
import prefuse.render.EdgeRenderer;
import prefuse.util.ColorLib;
import prefuse.util.GraphicsLib;
import prefuse.visual.EdgeItem;
import prefuse.visual.VisualItem;


/**
 * Renderer for {@link Association}s.
 * 
 * @author Wisser
 */
public class AssociationRenderer extends EdgeRenderer {

	public static Color COLOR_IGNORED;
	public static Color COLOR_ASSOCIATION;
	public static Color COLOR_DEPENDENCY;
	public static Color COLOR_REVERSE_DEPENDENCY;
	
	/**
	 * <code>true</code> for revered rendering.
	 */
	boolean reversed;

	/**
	 * <code>true</code> for full rendering (for setting bounds).
	 */
	boolean full = false;
	
	public AssociationRenderer(boolean reversed) {
		super(Constants.EDGE_TYPE_LINE, reversed? Constants.EDGE_ARROW_REVERSE : Constants.EDGE_ARROW_FORWARD);
		this.reversed = reversed;
	}

	public AssociationRenderer() {
		full = true;
	}

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
		    	m_curArrow = null;
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
		render(g, item);
	}

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
	
}
