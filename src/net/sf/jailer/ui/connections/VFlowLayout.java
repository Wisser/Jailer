package net.sf.jailer.ui.connections;

import java.awt.*;
import java.io.Serializable;

/**
 * A Simple layout manager that will stack components vertically as they are added. When
 * there is excess space, the components are not assigned it so they all remain to the
 * top of the container. The components stretch horizontally to width of the container
 */
public class VFlowLayout implements LayoutManager2, Serializable {

	Dimension layoutSize=null;

	public Dimension preferredLayoutSize(Container parent) {
		return getLayoutSize(parent);
	}

	public Dimension minimumLayoutSize(Container parent) {
		return getLayoutSize(parent);
	}

	public Dimension maximumLayoutSize(Container parent) {
		return parent.getSize();
	}

	/**
	 * Lays out the container by stacking the components in one vertical column.
	 * Stretches each component to the width of the container, does not stretch
	 * or shrink the components vertically.
	 */
	public void layoutContainer(Container parent) {
		Component components[] = parent.getComponents();
		Component comp;
		Insets insets = parent.getInsets();
		Rectangle r = new Rectangle();
		Dimension size=parent.getSize();
		r.width=size.width-insets.left-insets.right;
		r.x=insets.left;
		int compCount=components.length;
		for (int i=0; i < compCount; i++) {
			comp = components[i];
			r.height=comp.getPreferredSize().height;
			r.y+=insets.top;
			comp.setBounds(r);
			// Prepare the tally for the next component
			r.y+=r.height;
			r.y+=insets.bottom;
		}
	}

	/**
	 * Total up the height of each component in the container plus the insets of
	 * the container itself. Width is width of the container
	 *
	 * @param parent The parent component.
	 * @return The calculated target size of the parent component.
	 */
	protected Dimension calculateLayoutSize(Container parent) {
		Component components[] = parent.getComponents();
		Component comp;
		Insets insets = parent.getInsets();
		int width = 0;
		int height = 0;
		int compCount=components.length;
		for (int i = 0; i < compCount; i++) {
			comp = components[i];
			height += comp.getPreferredSize().height + insets.top + insets.bottom;
			width = Math.max(width, comp.getPreferredSize().width + insets.left + insets.right);
		}
		return new Dimension (width, height);
	}

	/**
	 * The layout size is not changed by this layout manager so we can cache the
	 * the dimensions. This method only recalculates them if the cache is null which
	 * may happen by the invalidateLayout method.
	 *
	 * @param parent The parent component.
	 * @return The calculated target size of the parent component.
	 */
	protected Dimension getLayoutSize(Container parent) {
		if (layoutSize == null) {
			layoutSize = calculateLayoutSize(parent);
		}
		return layoutSize;
	}

	/** Not used by this class */
	public void addLayoutComponent(String name, Component comp) {}

	/** Not used by this class */
	public void removeLayoutComponent(Component comp) {}

	public synchronized float getLayoutAlignmentY(Container target) {
		return 0.0f;
	}

	public synchronized float getLayoutAlignmentX(Container target) {
		return 0.0f;
	}

	public synchronized void invalidateLayout(Container target) {
		layoutSize=null;
	}

	/**
	*   Not currently used but likely to be overridden in subclasses
	*/
	public void addLayoutComponent(Component comp, Object constraints) {}
}
