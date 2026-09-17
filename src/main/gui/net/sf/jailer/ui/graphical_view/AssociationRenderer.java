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
package net.sf.jailer.ui.graphical_view;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.QuadCurve2D;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.Map;

import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.subsetting.ScriptFormat;
import net.sf.jailer.ui.Colors;
import net.sf.jailer.ui.UIUtil;
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

	private final DataModel dataModel;

	/**
	 * Constructor.
	 *
	 * @param dataModel the data model
	 * @param reversed <code>true</code> for reversed rendering
	 */
	public AssociationRenderer(DataModel dataModel, boolean reversed) {
		super(Constants.EDGE_TYPE_LINE, reversed? Constants.EDGE_ARROW_REVERSE : Constants.EDGE_ARROW_FORWARD);
		this.dataModel = dataModel;
		this.reversed = reversed;
	}

	/**
	 * Constructor for full rendering (used for setting bounds).
	 *
	 * @param dataModel the data model
	 */
	public AssociationRenderer(DataModel dataModel) {
		this.dataModel = dataModel;
		full = true;
	}

	/**
	 * Temporary used in getRawShape.
	 */
	private Point2D m_isctPoints2[] = new Point2D[2];
	private Path2D.Double crowsFoot = null;
	private Point2D midPosition = null;
	private Point2D pendingPosition = null;

	/**
	 * Direction of the edge at the end at which the arrow head is drawn. For a reflexive
	 * association this is the tangent of the arc, not the direction of the chord.
	 */
	private double markerTheta;

	/**
	 * Geometry of the crow's foot which marks the "many" end of an association.
	 */
	private static final double CROWS_FOOT_LENGTH = 11.0;
	private static final double CROWS_FOOT_SPREAD = 5.5;
	private static final double CROWS_FOOT_GAP = 2.0;
	private static final float CROWS_FOOT_WIDTH = 1.3f;

	/**
	 * Arc shape for the two edges of a reflexive association. Both edges connect the same
	 * pair of nodes, so they are bowed to opposite sides to keep them visible and selectable.
	 */
	private final QuadCurve2D.Double m_reflexiveCurve = new QuadCurve2D.Double();
	private static final double REFLEXIVE_BOW_FACTOR = 0.11;
	private static final double REFLEXIVE_MIN_BOW = 8;
	private static final double REFLEXIVE_MAX_BOW = 18;

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

		Association association = (Association) item.get("association");
		boolean reflexive = association != null && association.source == association.destination;

		getAlignedPoint(m_tmpPoints[0], item1.getBounds(),
						m_xAlign1, m_yAlign1);
		getAlignedPoint(m_tmpPoints[1], item2.getBounds(),
						m_xAlign2, m_yAlign2);
		m_curWidth = (float)(m_width * getLineWidth(item));
		EdgeItem e = (EdgeItem)item;

		boolean forward = (m_edgeArrow == Constants.EDGE_ARROW_FORWARD);

		// get starting and ending edge endpoints
		Point2D start = null, end = null;
		start = m_tmpPoints[forward?0:1];
		end   = m_tmpPoints[forward?1:0];

		if (!full) {
			double midX;
			double midY;
			Point2D sp = start, ep = end;

			VisualItem dest = forward ? e.getTargetItem() : e.getSourceItem();
			int i = GraphicsLib.intersectLineRectangle(start, end,
					dest.getBounds(), m_isctPoints);
			if ( i > 0 ) ep = m_isctPoints[0];

			VisualItem src = !forward ? e.getTargetItem() : e.getSourceItem();
			i = GraphicsLib.intersectLineRectangle(start, end,
					src.getBounds(), m_isctPoints2);
			if ( i > 0 ) sp = m_isctPoints2[0];

			midX = (sp.getX() + ep.getX()) / 2;
			midY = (sp.getY() + ep.getY()) / 2;
			m_tmpPoints[reversed? 1 : 0].setLocation(midX, midY);
		}

		// control point of the arc of a reflexive association
		if (reflexive) {
			double dx = end.getX() - start.getX();
			double dy = end.getY() - start.getY();
			double len = Math.sqrt(dx * dx + dy * dy);
			if (len > 1) {
				double bow = Math.max(REFLEXIVE_MIN_BOW, Math.min(REFLEXIVE_BOW_FACTOR * len, REFLEXIVE_MAX_BOW));
				// both edges of the pair connect the same two nodes, so they bow to opposite sides
				double offset = 2 * bow * (association.reversed? 1 : -1);
				m_ctrlPoints[0].setLocation(
						(start.getX() + end.getX()) / 2 - dy / len * offset,
						(start.getY() + end.getY()) / 2 + dx / len * offset);
			} else {
				reflexive = false;
			}
		}

		// create the arrow head, if needed
		if ( e.isDirected() && m_edgeArrow != Constants.EDGE_ARROW_NONE) {
			// the arc arrives from its control point, a straight edge from its start point
			Point2D arrowFrom = reflexive? m_ctrlPoints[0] : start;

			// compute the intersection with the target bounding box
			VisualItem dest = forward ? e.getTargetItem() : e.getSourceItem();
			int i = GraphicsLib.intersectLineRectangle(arrowFrom, end,
					dest.getBounds(), m_isctPoints);
			if ( i > 0 ) end = m_isctPoints[0];
			markerTheta = Math.atan2(end.getY() - arrowFrom.getY(), end.getX() - arrowFrom.getX());

			// create the arrow head shape
			AffineTransform at = getArrowTrans(arrowFrom, end, m_curWidth);
			m_curArrow = at.createTransformedShape(m_arrowHead);

			// update the endpoints for the edge shape
			// need to bias this by arrow head size
			Point2D lineEnd = m_tmpPoints[forward?1:0];
			lineEnd.setLocation(0, -m_arrowHeight);
			at.transform(lineEnd, lineEnd);
		} else {
			m_curArrow = null;
			markerTheta = Math.atan2(end.getY() - start.getY(), end.getX() - start.getX());
		}

		// create the edge shape
		Shape shape = null;
		double n1x = m_tmpPoints[0].getX();
		double n1y = m_tmpPoints[0].getY();
		double n2x = m_tmpPoints[1].getX();
		double n2y = m_tmpPoints[1].getY();
		if (reflexive) {
			m_reflexiveCurve.setCurve(n1x, n1y, m_ctrlPoints[0].getX(), m_ctrlPoints[0].getY(), n2x, n2y);
			shape = m_reflexiveCurve;
		} else {
			m_line.setLine(n1x, n1y, n2x, n2y);
			shape = m_line;
		}

		if (association == null) {
			return shape;
		}

		crowsFootBounds = null;
		crowsFoot = null;
		if (reflexive) {
			// apex of the arc, so that the marker sits on the visible curve
			midPosition = new Point2D.Double(
					0.25 * n1x + 0.5 * m_ctrlPoints[0].getX() + 0.25 * n2x,
					0.25 * n1y + 0.5 * m_ctrlPoints[0].getY() + 0.25 * n2y);
		} else {
			midPosition = new Point2D.Double((n1x + n2x) / 2, (n1y + n2y) / 2);
		}

		if (!forward && (Cardinality.MANY_TO_MANY.equals(association.getCardinality()) || Cardinality.MANY_TO_ONE.equals(association.getCardinality()))
		||   forward && (Cardinality.MANY_TO_MANY.equals(association.getCardinality()) || Cardinality.ONE_TO_MANY.equals(association.getCardinality()))) {
			// the foot sits on the line right behind the arrow head and opens towards the table
			Point2D tip = m_tmpPoints[forward? 1:0];
			double ux = Math.cos(markerTheta), uy = Math.sin(markerTheta);
			double nx = -uy, ny = ux;
			double scale = Math.max(1.0, m_curWidth / 2); // as in getArrowTrans
			double gap = CROWS_FOOT_GAP * scale;
			double spread = CROWS_FOOT_SPREAD * scale;
			// on a short edge the line ends at the middle of the edge, so don't reach beyond it
			double len = Math.min(CROWS_FOOT_LENGTH * scale, 0.45 * m_tmpPoints[0].distance(m_tmpPoints[1]));
			if (len > 1) {
				double tx = tip.getX() - ux * gap, ty = tip.getY() - uy * gap;
				double ax = tip.getX() - ux * (gap + len), ay = tip.getY() - uy * (gap + len);
				crowsFoot = new Path2D.Double();
				crowsFoot.moveTo(ax, ay);
				crowsFoot.lineTo(tx, ty);
				crowsFoot.moveTo(ax, ay);
				crowsFoot.lineTo(tx + nx * spread, ty + ny * spread);
				crowsFoot.moveTo(ax, ay);
				crowsFoot.lineTo(tx - nx * spread, ty - ny * spread);
				crowsFootBounds = crowsFoot.getBounds2D();
				double pad = CROWS_FOOT_WIDTH * Math.max(1, m_curWidth);
				crowsFootBounds.setRect(crowsFootBounds.getX() - pad, crowsFootBounds.getY() - pad,
						crowsFootBounds.getWidth() + 2 * pad, crowsFootBounds.getHeight() + 2 * pad);
			}
		}

		pendingBounds = null;
		pendingPosition = null;

		if (!forward && association.getDataModel().decisionPending.contains(association.getName())
		||   forward && association.getDataModel().decisionPending.contains(association.reversalAssociation.getName())) {
			pendingPosition = new Point2D.Double(m_tmpPoints[forward? 1:0].getX(), m_tmpPoints[forward? 1:0].getY());
			start = pendingPosition;
			end = m_tmpPoints[forward? 0:1];
			Point2D p = new Point2D.Double(), shift = new Point2D.Double();
			double d = 1.11;
			p.setLocation((end.getX() - start.getX()) / d, (end.getY() - start.getY()) / d);
			shift = p;
			pendingPosition.setLocation(pendingPosition.getX() + shift.getX(), pendingPosition.getY() + shift.getY());
			pendingBounds = new Rectangle2D.Double(pendingPosition.getX() - PENDING_SIZE * (pendingWidth / 2), pendingPosition.getY() - PENDING_SIZE * (pendingHeight / 2), pendingWidth * PENDING_SIZE, pendingHeight * PENDING_SIZE);
		}

		return shape;
	}

	/**
	 * Returns an affine transformation that maps the arrowhead shape
	 * to the position and orientation specified by the provided
	 * line segment end points.
	 */
	@Override
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
		int color = 0;
		if (!Boolean.TRUE.equals(item.get("full"))) {
			if (!full) {
				return;
			}
			if (association != null) {
				color = associationColor(association);
			}
		} else {
			if (full) {
				return;
			}
			if (association != null) {
				color = reversed? associationColor(association.reversalAssociation) : associationColor(association);
			}
		}
		boolean restricted = false;
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
				restricted = true;
			} else {
				item.setStroke(new BasicStroke(stroke.getLineWidth(), stroke.getEndCap(), stroke.getLineJoin(), stroke.getMiterLimit()));
			}
		}
		if (isSelected) {
			item.setStrokeColor(Colors.Color_0_0_0.getRGB());
			stroke = item.getStroke();
			if (stroke != null) {
				BasicStroke itemStroke;
				long animationstep = System.currentTimeMillis();
				if (restricted) {
					int length = 20 * 100;
					itemStroke = new BasicStroke(stroke.getLineWidth(), BasicStroke.CAP_ROUND, stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 7f, 6f, 1f, 6f },
							(reversed? animationstep % length : length - animationstep % length) / 100.0f);
				} else {
					int length = 12 * 100;
					itemStroke = new BasicStroke(stroke.getLineWidth(), BasicStroke.CAP_ROUND, stroke.getLineJoin(), stroke.getMiterLimit(), new float[] { 7f, 5f },
						(reversed? animationstep % length : length - animationstep % length) / 100.0f);
				}
				item.setStroke(itemStroke);
			}
		}
		item.setFillColor(color);
		item.setStrokeColor(color);
		if (association != null && isObjectNatationFormat(association)) {
			m_arrowHead = updateArrowHead(m_arrowWidth, m_arrowHeight, association, isSelected);
			arrowIsPotAggregation = true;
		} else {
			if (arrowIsPotAggregation) {
				m_arrowHead = updateArrowHead(m_arrowWidth, m_arrowHeight);
			}
			arrowIsPotAggregation = false;
		}
		crowsFoot = null;
		pendingPosition = null;
		midPosition = null;
		render(g, item);
		if (crowsFoot != null) {
			// solid, no matter whether the edge is dashed (restricted) or animated (selected)
			Stroke oldStroke = g.getStroke();
			g.setColor(new Color(color));
			g.setStroke(new BasicStroke(CROWS_FOOT_WIDTH * Math.max(1, m_curWidth), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
			g.draw(crowsFoot);
			g.setStroke(oldStroke);
			crowsFoot = null;
		}
		if (pendingPosition != null && pendingImage != null) {
			double size = PENDING_SIZE;
			transform.setTransform(size, 0, 0, size, pendingPosition.getX() - size * (pendingWidth / 2), pendingPosition.getY() - size * (pendingHeight / 2));
			g.drawImage(pendingImage, transform, null);
			pendingPosition = null;
		}
		if (midPosition != null) {
			if (dataModel.version != lastDataModelVersion) {
				withNullFK.clear();
				lastDataModelVersion = dataModel.version;
			}
			Boolean isFKNull = withNullFK.get(association);
			if (isFKNull == null) {
				isFKNull = association.isRestrictedDependencyWithNulledFK() && !association.fkHasExcludeFilter();
				withNullFK.put(association, isFKNull);
			}
			if (isFKNull) {
				int r = 5;
				g.setStroke(new BasicStroke(1.5f));
				g.setColor(new Color(color));
				g.drawOval((int) midPosition.getX() - r, (int) midPosition.getY() - r, 2 * r, 2 * r);
			}
		}
	}

	/**
	 * @see prefuse.render.Renderer#setBounds(prefuse.visual.VisualItem)
	 */
	@Override
	public void setBounds(VisualItem item) {
		super.setBounds(item);
		if (crowsFootBounds != null ) {
			Rectangle2D bbox = (Rectangle2D)item.get(VisualItem.BOUNDS);
			if (bbox != null) {
				Rectangle2D.union(bbox, crowsFootBounds, bbox);
			}
		}
		if (pendingBounds != null ) {
			Rectangle2D bbox = (Rectangle2D)item.get(VisualItem.BOUNDS);
			if (bbox != null) {
				Rectangle2D.union(bbox, pendingBounds, bbox);
			}
		}
	}

	private boolean arrowIsPotAggregation = false;
	private AffineTransform transform = new AffineTransform();
	private Rectangle2D crowsFootBounds = null;
	private Rectangle2D pendingBounds = null;
	private long lastDataModelVersion = -1;
	private Map<Association, Boolean> withNullFK = new HashMap<Association, Boolean>();

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
			double width = Math.max(14, getLineWidth(item));
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

	private boolean isObjectNatationFormat(Association association) {
		try {
			return ScriptFormat.valueOf(association.getDataModel().getExportModus()).isObjectNotation();
		} catch (Exception e) {
			return false;
		}
	}
	
	private Image pendingImage = null;
	private double pendingWidth = 0;
	private double pendingHeight = 0;
	private final double PENDING_SIZE = 0.32;
	{
		// load image
		try {
			pendingImage = UIUtil.readImage("/wanr.png").getImage();
			pendingWidth = pendingImage.getWidth(null);
			pendingHeight = pendingImage.getHeight(null);
		} catch (Throwable t) {
			// ignore
		}
	}

}
