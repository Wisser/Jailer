package net.sf.jailer.ui.scrollmenu;

import java.awt.Adjustable;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import net.sf.jailer.ui.UIUtil;
import net.sf.jailer.ui.UIUtil.PLAF;

/**
 * http://stackoverflow.com/questions/9288350/adding-vertical-scroll-to-a-jpopupmenu
 */
public class JScrollPopupMenu extends JPopupMenu {
	private static final long serialVersionUID = 8754906500805992108L;
	
	private int maximumVisibleRows = 40;
	private final boolean noSearchFieldNorScrollbar;
	private static final String PLACEHOLDERTEXT = "Type partial value to search";
	
	public JScrollPopupMenu() {
		this(null, false);
	}

	public JScrollPopupMenu(boolean noSearchField) {
		this(null, noSearchField);
	}

	public JScrollPopupMenu(String label, boolean noSearchField) {
		super(label);
		this.noSearchFieldNorScrollbar = noSearchField;
		setLayout(new ScrollPopupMenuLayout());

		if (!noSearchFieldNorScrollbar) {
			super.add(getScrollBar());
		}
		
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
		
		searchField = new JTextField();
		searchField.setVisible(true);
		searchField.setToolTipText("<html>Search criteria.<br><br>\nSearch for items that contain the search criteria as:<br>\n<table>\n<tr><td><b>Prefix</b></td><td>if it starts with a space</td></tr>\n<tr><td><b>Suffix</b></td><td>if it ends with a space</td></tr>\n<tr><td><b>Substring</b></td><td>else</td></tr>\n</table>\n<br>\n(<b>*</b> = any string, <b>?</b> = any character)\n</html>");
		searchFieldPreferredSize = searchField.getPreferredSize();
		
		searchField.getDocument().addDocumentListener(new DocumentListener() {
			@Override
			public void removeUpdate(DocumentEvent e) {
				update();
			}
			@Override
			public void insertUpdate(DocumentEvent e) {
				update();
			}
			@Override
			public void changedUpdate(DocumentEvent e) {
				update();
			}
			private void update() {
				search(searchField.getText());
			}
		});
		searchField.addKeyListener(new KeyListener() {
			@Override
			public void keyTyped(KeyEvent e) {
				if (e.getKeyChar() == '\n') {
					MenuSelectionManager msm = MenuSelectionManager.defaultManager();
					msm.processKeyEvent(e);
					MenuElement[] path = msm.getSelectedPath();
					if (path != null && path.length > 0) {
						for (Component comp : getComponents()) {
							if (comp instanceof JMenuItem && comp == path[path.length - 1]) {
								((JMenuItem) comp).doClick();
								for (int i = path.length - 1; i >= 0; --i) {
									path[i].getComponent().setVisible(false);
								}
								break;
							}
						}
					}
				}
			}
			@Override
			public void keyReleased(KeyEvent e) {
			}
			@Override
			public void keyPressed(KeyEvent e) {
			}
		});
		
		if (UIUtil.plaf.isFlat) {
			searchField.putClientProperty("JTextField.placeholderText", PLACEHOLDERTEXT);
		}
//		searchField.setVisible(false);
		separator = new JSeparator();
		if (!noSearchField) {
			super.add(searchField);
			super.add(separator);
		}
	}

	private Map<JMenuItem, String> compText = null;
	private JSeparator separator;
	private final static String HL_COLOR = "#0000D0";

	protected void search(String searchText) {
		if (compText == null) {
			compText = new HashMap<>();
			for (Component comp : getComponents()) {
				if (comp instanceof JMenuItem) {
					compText.put((JMenuItem) comp, ((JMenuItem) comp).getText());
				}
			}
		}
		List<JMenuItem> mismatches = new ArrayList<>();
		for (Component comp : getComponents()) {
			if (comp instanceof JMenuItem) {
				String v = compText.get(comp);
				if (v == null) {
					continue;
				}
				if (searchText.trim().isEmpty()) {
					((JMenuItem) comp).setText(v);
					((JMenuItem) comp).setVisible(true);
					continue;
				}
				String searchTextUC = extendedSearchText(searchText, v.toString().trim()).toUpperCase(Locale.ENGLISH);
				if (!v.toUpperCase(Locale.ENGLISH).contains(searchTextUC)) {
					mismatches.add((JMenuItem) comp);
					continue;
				}
			
				String value = v;
				if (value != null && value.toUpperCase(Locale.ENGLISH).contains(searchTextUC)) {
					String markedValue = null;
					int i;
					int offset = 0;
					while (offset < value.length() && value.charAt(offset) == ' ') {
						offset += 1;
					}
					String core = value.trim();
					if (searchText.startsWith(" ") && !core.toUpperCase(Locale.ENGLISH).startsWith(searchTextUC)) {
						mismatches.add((JMenuItem) comp);
						continue;
					}
					if (searchText.endsWith(" ") && !core.toUpperCase(Locale.ENGLISH).endsWith(searchTextUC)) {
						mismatches.add((JMenuItem) comp);
						continue;
					}
					if (searchText.startsWith(" ") && searchText.endsWith(" ") && !core.toUpperCase(Locale.ENGLISH).equals(searchTextUC)) {
						mismatches.add((JMenuItem) comp);
						continue;
					}
					i = searchText.endsWith(" ")? core.toUpperCase(Locale.ENGLISH).lastIndexOf(searchTextUC) : core.toUpperCase(Locale.ENGLISH).indexOf(searchTextUC);
					if (i < 0) {
						mismatches.add((JMenuItem) comp);
						continue;
					}
					i += offset;
					i = Math.min(i, value.length());
					if (i + searchTextUC.length() <= value.length()) {
						markedValue = UIUtil.toHTMLFragment(value.substring(0, i), 0, false) + "<b><u><font color=\"" + HL_COLOR + "\">" + UIUtil.toHTMLFragment(value.substring(i, i + searchTextUC.length()), 0, false) + "</font></u></b>" + UIUtil.toHTMLFragment(value.substring(i + searchTextUC.length()), 0, false);
					}
					if (markedValue == null) {
						markedValue = "<b><u><font color=\"" + HL_COLOR + "\">" + UIUtil.toHTMLFragment(value, 0, false) + "</font></u></b>";
					}
					markedValue = "<html>" + markedValue + "</html>";
					((JMenuItem) comp).setText(markedValue);
					comp.setVisible(true);
					getScrollBar().setValue(0);
				}
			}
		}
		boolean nothingFound = mismatches.size() == compText.size() && !searchText.trim().isEmpty();
		if (nothingFound && !prevNothingFound) {
			Toolkit.getDefaultToolkit().beep();
		}
		prevNothingFound = nothingFound;
		mismatches.forEach(item -> {
			item.setText(compText.get(item));
			item.setVisible(false);
		});
		int extent = 0;
		int max = 0;
		int i = 0;
		int unit = -1;
		for (Component comp : getComponents()) {
			if (!(comp instanceof JScrollBar) && comp.isVisible()) {
				Dimension preferredSize = comp.getPreferredSize();
				if (unit < 0 && comp instanceof JMenuItem) {
					unit = preferredSize.height;
				}
				if (i++ < maximumVisibleRows) {
					extent += preferredSize.height;
				}
				max += preferredSize.height;
			}
		}

		JScrollBar scrollBar = getScrollBar();
		Insets insets = getInsets();
		int heightMargin = insets.top + insets.bottom;
		scrollBar.setUnitIncrement(unit);
		scrollBar.setBlockIncrement(extent);
		scrollBar.setValues(0, heightMargin + extent, 0, heightMargin + max);
	}
	
	private boolean prevNothingFound = false;
	private Pattern extSTPattern = null;
	private String extSTText = null;
	
	private String extendedSearchText(String text, String item) {
		text = text.replaceAll("\\s|\\h", " ");
		String searchText = text.toUpperCase(Locale.ENGLISH);
		if (!searchText.contains("*") && !searchText.contains("?")) {
			return searchText.trim();
		}
		
		if (!text.equals(extSTText)) {
			boolean withPrefix = !text.startsWith(" ");
			boolean withSuffix = !text.endsWith(" ");
			String reg = (withPrefix? ".*?" : "") + "(\\Q" +
						text.trim().replace("?", "\\E.\\Q").replace("*", "\\E.*\\Q") +
						"\\E)" +
						(withSuffix? ".*?" : "");
			
			extSTText = reg;
			extSTPattern = Pattern.compile(reg, Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
		}
		
		Matcher matcher = extSTPattern.matcher(item);
		
		if (matcher.matches()) {
			return matcher.group(1);
		} else {
			return searchText.trim();
		}
	}
	
	private JScrollBar popupScrollBar;
	private JTextField searchField;
	private Dimension searchFieldPreferredSize;

	protected JScrollBar getScrollBar() {
		if (popupScrollBar == null) {
			popupScrollBar = new JScrollBar(Adjustable.VERTICAL);
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

	@Override
	public void paintChildren(Graphics g) {
		Insets insets = getInsets();
		g.clipRect(insets.left, insets.top, getWidth(), getHeight()
				- insets.top - insets.bottom);
		super.paintChildren(g);
	}

	@Override
	protected void addImpl(Component comp, Object constraints, int index) {
		super.addImpl(comp, constraints, index);

		if (maximumVisibleRows < getComponentCount() - 1) {
			getScrollBar().setVisible(true);
		}
	}

	@Override
	public void remove(int index) {
		super.remove(index);
		if (maximumVisibleRows >= getComponentCount() - 1) {
			getScrollBar().setVisible(false);
		}
	}

	@Override
	public void show(Component invoker, int x, int y) {
		if (getComponentCount() < 14 + 2 || noSearchFieldNorScrollbar) {
			searchField.setVisible(false);
			separator.setVisible(false);
		}
		JScrollBar scrollBar = getScrollBar();
//		if (scrollBar.isVisible()) {
			int extent = 0;
			int max = 0;
			int i = 0;
			int unit = -1;
			int width = Math.max(new JLabel(PLACEHOLDERTEXT).getPreferredSize().width + getScrollBar().getPreferredSize().width, titleWidth()) + 32;
			for (Component comp : getComponents()) {
				if (!(comp instanceof JScrollBar) && comp.isVisible()) {
					Dimension preferredSize = comp == searchField? searchFieldPreferredSize : comp.getPreferredSize();
					width = Math.max(width, preferredSize.width);
					if (unit < 0 && comp instanceof JMenuItem) {
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
			scrollBar.setValues(0, heightMargin + extent, 0, heightMargin + max);

			int height = heightMargin + extent;

			Rectangle bounds = null;
			Component tp = invoker;
			while (tp != null) {
				if (tp instanceof Window) {
					if (bounds == null) {
						bounds = tp.getBounds();
					} else {
						bounds.add(tp.getBounds());
					}
				}
				tp = tp.getParent();
			}
			
			if (bounds != null) {
				Point p = new Point(x, y);
				Point p2 = new Point(x, y);
				SwingUtilities.convertPointToScreen(p, invoker);
				// y
				double d = p.y + height - (bounds.getY() + bounds.getHeight() - 8);
				if (d > 0) {
					p.y -= d;
					d = p.y - bounds.getY();
					if (d < 0) {
						p.y -= d;
						height += d;
					}
					SwingUtilities.convertPointFromScreen(p, invoker);
					y = p.y;
				}
				
				// x
				p = p2;
				SwingUtilities.convertPointToScreen(p, invoker);
				d = p.x + width - (bounds.getX() + bounds.getWidth() - 8);
				if (d > 0) {
					p.x -= d;
					d = p.x - bounds.getX();
					if (d < 0) {
						p.x -= d;
						width += d;
					}
					SwingUtilities.convertPointFromScreen(p, invoker);
					x = p.x;
				}
			}
			
			setPopupSize(new Dimension(width, height));
			
//		}

		UIUtil.invokeLater(() -> {
			searchField.grabFocus();
			searchField.setBorder(null);
		});
		super.show(invoker, x, y);
	}

	private int titleWidth() {
		Border border = getBorder();
		while (border instanceof CompoundBorder) {
			border = ((CompoundBorder) border).getInsideBorder();
		}
		if (border instanceof TitledBorder) {
			return (int) new JLabel(((TitledBorder) border).getTitle()).getPreferredSize().getWidth();
		}
		return 0;
	}

	protected class ScrollPopupMenuLayout implements LayoutManager {
		@Override
		public void addLayoutComponent(String name, Component comp) {
		}

		@Override
		public void removeLayoutComponent(Component comp) {
		}

		@Override
		public Dimension preferredLayoutSize(Container parent) {
			int visibleAmount = Integer.MAX_VALUE;
			Dimension dim = new Dimension(Math.max(new JLabel(PLACEHOLDERTEXT).getPreferredSize().width + getScrollBar().getPreferredSize().width, titleWidth()) + 32, 0);
			for (Component comp : parent.getComponents()) {
				if (comp.isVisible()) {
					if (comp instanceof JScrollBar) {
						JScrollBar scrollBar = (JScrollBar) comp;
						visibleAmount = scrollBar.getVisibleAmount();
					} else {
						Dimension pref = comp == searchField? searchFieldPreferredSize : comp.getPreferredSize();
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
					Dimension pref = comp == searchField? searchFieldPreferredSize : comp.getPreferredSize();
					int o = (comp instanceof JTextField)? 4 : 0;
					comp.setBounds(x + o, y, width - o, pref.height);
					y += pref.height;
				}
			}
		}
	}
}
