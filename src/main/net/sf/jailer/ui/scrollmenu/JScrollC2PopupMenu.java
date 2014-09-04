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
public class JScrollC2PopupMenu extends JPopupMenu {
	protected int maximumVisibleRows = 40;
	private static int C2WIDTH = 50;
	
	public JScrollC2PopupMenu() {
		this(null);
	}

	public JScrollC2PopupMenu(String label) {
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

		if (maximumVisibleRows < (getComponentCount() - 1) / 2) {
			getScrollBar().setVisible(true);
		}
	}

	public void remove(int index) {
		// can't remove the scrollbar
		++index;

		super.remove(index);

		if (maximumVisibleRows >= (getComponentCount() - 1) / 2) {
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
			int l = 0;
			for (Component comp : getComponents()) {
				++l;
				if (!(comp instanceof JScrollBar)) {
					Dimension preferredSize = comp.getPreferredSize();
					if (l % 2 == 0) {
						width = Math.max(width, preferredSize.width + C2WIDTH);
						if (unit < 0) {
							unit = preferredSize.height;
						}
						if (i++ < maximumVisibleRows) {
							extent += preferredSize.height;
						}
						max += preferredSize.height;
					}
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
			int y = 0;
			int sbw = 0;
			for (Component comp : parent.getComponents()) {
				++y;
				if (comp.isVisible()) {
					if (comp instanceof JScrollBar) {
						JScrollBar scrollBar = (JScrollBar) comp;
						visibleAmount = scrollBar.getVisibleAmount();
						sbw = comp.getPreferredSize().width;
					} else if (y % 2 == 0) {
						Dimension pref = comp.getPreferredSize();
						dim.width = Math.max(dim.width, pref.width + C2WIDTH);
						dim.height += pref.height;
					}
				}
			}

			dim.width += sbw;
			
			Insets insets = parent.getInsets();
			dim.height = Math.min(dim.height + insets.top + insets.bottom,
					visibleAmount);

			return dim;
		}

		@Override
		public Dimension minimumLayoutSize(Container parent) {
			int visibleAmount = Integer.MAX_VALUE;
			Dimension dim = new Dimension();
			Component pre = null;
			int y = 0;
			for (Component comp : parent.getComponents()) {
				++y;
				if (comp.isVisible()) {
					if (comp instanceof JScrollBar) {
						JScrollBar scrollBar = (JScrollBar) comp;
						visibleAmount = scrollBar.getVisibleAmount();
					} else {
						if (y % 2 == 0 && pre != null) {
							Dimension min = comp.getMinimumSize();
							dim.width = Math.max(dim.width, min.width + C2WIDTH);
							dim.height += min.height;
						}
					}
				}
				pre = comp;
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
					scrollBar.setBounds(x + width - dim.width - 4, y, dim.width,
							height);
					width -= dim.width;
					position = scrollBar.getValue();
				}
			}

			y -= position;
			Component pre = null;
			int l = 0;
			for (Component comp : parent.getComponents()) {
				++l;
				if (!(comp instanceof JScrollBar) && comp.isVisible()) {
					Dimension pref = comp.getPreferredSize();
					if (l % 2 == 0 && pre != null) {
						comp.setBounds(x, y, width - C2WIDTH, pref.height);
					} else {
						comp.setBounds(x + width - pref.width - 12, y, pref.width, pref.height);
						y += pref.height + 2;
					}
				}
				pre = comp;
			}
		}
	}
}