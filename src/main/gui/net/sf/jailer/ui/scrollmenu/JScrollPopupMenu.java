package net.sf.jailer.ui.scrollmenu;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;

/**
 * http://stackoverflow.com/questions/9288350/adding-vertical-scroll-to-a-jpopupmenu
 */
public class JScrollPopupMenu extends JPopupMenu {
	protected int maximumVisibleRows = 40;

	public JScrollPopupMenu() {
		this(null);
	}

	public JScrollPopupMenu(String label) {
		super(label);
		setLayout(new ScrollPopupMenuLayout());

		super.add(getScrollBar());
		addMouseWheelListener(new MouseWheelListener() {
			@Override
			public void mouseWheelMoved(MouseWheelEvent event) {
				JScrollBar scrollBar = getScrollBar();
				int amount = (event.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) ? event
						.getUnitsToScroll() * scrollBar.getUnitIncrement()
						: (event.getWheelRotation() < 0 ? -1 : 1)
								* scrollBar.getBlockIncrement();

				scrollBar.setValue(scrollBar.getValue() + amount);
				event.consume();
			}
		});
	}

	private JScrollBar popupScrollBar;

	protected JScrollBar getScrollBar() {
		if (popupScrollBar == null) {
			popupScrollBar = new JScrollBar(JScrollBar.VERTICAL);
			popupScrollBar.addAdjustmentListener(new AdjustmentListener() {
				@Override
				public void adjustmentValueChanged(AdjustmentEvent e) {
					doLayout();
					repaint();
				}
			});

			popupScrollBar.setVisible(false);
		}

		return popupScrollBar;
	}

	public int getMaximumVisibleRows() {
		return maximumVisibleRows;
	}

	public void setMaximumVisibleRows(int maximumVisibleRows) {
		this.maximumVisibleRows = maximumVisibleRows;
	}

	public void paintChildren(Graphics g) {
		Insets insets = getInsets();
		g.clipRect(insets.left, insets.top, getWidth(), getHeight()
				- insets.top - insets.bottom);
		super.paintChildren(g);
	}

	protected void addImpl(Component comp, Object constraints, int index) {
		super.addImpl(comp, constraints, index);

		if (maximumVisibleRows < getComponentCount() - 1) {
			getScrollBar().setVisible(true);
		}
	}

	public void remove(int index) {
		// can't remove the scrollbar
		++index;

		super.remove(index);

		if (maximumVisibleRows >= getComponentCount() - 1) {
			getScrollBar().setVisible(false);
		}
	}

	public void show(Component invoker, int x, int y) {
		JScrollBar scrollBar = getScrollBar();
		if (scrollBar.isVisible()) {
			int extent = 0;
			int max = 0;
			int i = 0;
			int unit = -1;
			int width = 0;
			for (Component comp : getComponents()) {
				if (!(comp instanceof JScrollBar)) {
					Dimension preferredSize = comp.getPreferredSize();
					width = Math.max(width, preferredSize.width);
					if (unit < 0) {
						unit = preferredSize.height;
					}
					if (i++ < maximumVisibleRows) {
						extent += preferredSize.height;
					}
					max += preferredSize.height;
				}
			}

			Insets insets = getInsets();
			int widthMargin = insets.left + insets.right;
			int heightMargin = insets.top + insets.bottom;
			scrollBar.setUnitIncrement(unit);
			scrollBar.setBlockIncrement(extent);
			scrollBar
					.setValues(0, heightMargin + extent, 0, heightMargin + max);

			width += scrollBar.getPreferredSize().width + widthMargin;
			int height = heightMargin + extent;

			setPopupSize(new Dimension(width, height));
		}

		super.show(invoker, x, y);
	}

	protected static class ScrollPopupMenuLayout implements LayoutManager {
		@Override
		public void addLayoutComponent(String name, Component comp) {
		}

		@Override
		public void removeLayoutComponent(Component comp) {
		}

		@Override
		public Dimension preferredLayoutSize(Container parent) {
			int visibleAmount = Integer.MAX_VALUE;
			Dimension dim = new Dimension();
			for (Component comp : parent.getComponents()) {
				if (comp.isVisible()) {
					if (comp instanceof JScrollBar) {
						JScrollBar scrollBar = (JScrollBar) comp;
						visibleAmount = scrollBar.getVisibleAmount();
					} else {
						Dimension pref = comp.getPreferredSize();
						dim.width = Math.max(dim.width, pref.width);
						dim.height += pref.height;
					}
				}
			}

			Insets insets = parent.getInsets();
			dim.height = Math.min(dim.height + insets.top + insets.bottom,
					visibleAmount);

			return dim;
		}

		@Override
		public Dimension minimumLayoutSize(Container parent) {
			int visibleAmount = Integer.MAX_VALUE;
			Dimension dim = new Dimension();
			for (Component comp : parent.getComponents()) {
				if (comp.isVisible()) {
					if (comp instanceof JScrollBar) {
						JScrollBar scrollBar = (JScrollBar) comp;
						visibleAmount = scrollBar.getVisibleAmount();
					} else {
						Dimension min = comp.getMinimumSize();
						dim.width = Math.max(dim.width, min.width);
						dim.height += min.height;
					}
				}
			}

			Insets insets = parent.getInsets();
			dim.height = Math.min(dim.height + insets.top + insets.bottom,
					visibleAmount);

			return dim;
		}

		@Override
		public void layoutContainer(Container parent) {
			Insets insets = parent.getInsets();

			int width = parent.getWidth() - insets.left - insets.right;
			int height = parent.getHeight() - insets.top - insets.bottom;

			int x = insets.left;
			int y = insets.top;
			int position = 0;

			for (Component comp : parent.getComponents()) {
				if ((comp instanceof JScrollBar) && comp.isVisible()) {
					JScrollBar scrollBar = (JScrollBar) comp;
					Dimension dim = scrollBar.getPreferredSize();
					scrollBar.setBounds(x + width - dim.width, y, dim.width,
							height);
					width -= dim.width;
					position = scrollBar.getValue();
				}
			}

			y -= position;
			for (Component comp : parent.getComponents()) {
				if (!(comp instanceof JScrollBar) && comp.isVisible()) {
					Dimension pref = comp.getPreferredSize();
					comp.setBounds(x, y, width, pref.height);
					y += pref.height;
				}
			}
		}
	}
}