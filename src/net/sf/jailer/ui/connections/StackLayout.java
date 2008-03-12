package net.sf.jailer.ui.connections;

/*****************************************************************************
 gui.StackLayout

 Simple layout manager for filling a panel horizontally or vertically.

 bruce.miller@nist.gov
 Contribution of the National Institute of Standards and Technology,
 not subject to copyright.
 *****************************************************************************/

import java.awt.*;
import java.util.Hashtable;

/** **************************************************************************
 StackLayout is a LayoutManager that arranges components in a vertical
 (or horizontal) strip aligning them at right, left or centered, and/or
 filling them to take up any extra vertical or horizontal space.
 Arrangement tags are provided by using the add(tag,component) form to
 add components to the container.

 The tag consists of one or more of the following, with the two forms applying
 to horizontal or vertical dimension.
<pre>
 Positioning:
	"Center"								: centered horizontally & vertically (the default)
	"Left"		or "Top"			: pushed at the left|top edge.
	"Right"	 or "Bottom"	 : pushed against the right|top edge
 Sizing:
	"Wide"		or "Tall"		 : filled to use available space.
	"Wide*#"	or "Tall*#"	 : filled but weighted by the number #.
	"Fill" (or "Fill*#")		: filled in both directions.
	"Width=#" or "Height=#" : given explicit width|height
 Margins:
	"Flush"								 : margins are not added around this component.
</pre>
	By default, a component is centered in both directions.
	The available space along the orientation is divided between the filled
	components.	A common idiom is to build a complicated panel out of, say, a
	vertical stack of horizontal stacks (both using StackLayout).	In that
	case, it would usually be good to add the horizontal panels using the
	tag "Wide Flush", so that spacing comes out evenly.
<p>

 Much of what can be done with GridBagLayout can be achieved by combining
 a set of subpanels using StackLayout, but typically more concisely.
 On the other hand, with StackLayout there is less compile time checking
 of the layout.

@author Bruce R. Miller (bruce.miller@nist.gov)
@author Contribution of the National Institute of Standards and Technology,
@author not subject to copyright.
*/

public class StackLayout implements LayoutManager {
	/** The orientation constant for horizontal layouts. */
	public static final int HORIZONTAL = 0;
	/** The orientation constant for vertical layouts. */
	public static final int VERTICAL = 1;

	int orientation = HORIZONTAL;
	int margin = 2;
	Hashtable codeTable = new Hashtable();

	/* Layout codes */
	static final int CENTER = 0x00;
	static final int FRONT	= 0x01;
	static final int BACK	 = 0x02;
	static final int FILL	 = 0x04;
	static final int ABS		= 0x08;
	static final int FLUSH	= 0x10;

	static final int POSMASK=0x03;
	static final int SIZEMASK=0x0C;

	/* *********************************************************************
		Creators. */

	/** Create a horizontal StackLayout. */
	public StackLayout() {}

	/** Create a StackLayout with the given orientation. */
	public StackLayout(int orientation) {
		this.orientation = orientation; }

	/** Create a StackLayout with the given orientation and space
	between components. */
	public StackLayout(int orientation, int margin) {
		this.orientation = orientation;
		this.margin = margin; }

	/* *********************************************************************
		Note any named components . */

	/** Add the specified component to the layout, parsing the layout tag. */

	public void addLayoutComponent(String tag, Component comp) {
		tag = tag.toUpperCase().trim();
		int hcode=CENTER,vcode=CENTER, harg=0, varg=0;
		int i,l = tag.length(),n;
		for(i=0; i<l; ) {
			if (tag.startsWith("CENTER",i)) {i += 6; }
			else if (tag.startsWith("LEFT",i))	 {i += 4; hcode |= FRONT;}
			else if (tag.startsWith("TOP",i))		{i += 3; vcode |= FRONT;}
			else if (tag.startsWith("RIGHT",i))	{i += 5; hcode |= BACK; }
			else if (tag.startsWith("BOTTOM",i)) {i += 6; vcode |= BACK;}
			else if (tag.startsWith("WIDE",i)) {
	i += 4;	hcode |= FILL;
	if (tag.startsWith("*",i)) {
		i++;	n = countDigits(tag,i); harg = parseArg(tag,i,n); i += n; }
	else harg = 1; }
			else if (tag.startsWith("TALL",i)) {
	i += 4;	vcode |= FILL;
	if (tag.startsWith("*",i)) {
		i++;	n = countDigits(tag,i); varg = parseArg(tag,i,n); i += n; }
	else varg = 1; }
			else if (tag.startsWith("FILL",i))	 {
	i += 4;	hcode |= FILL; vcode |= FILL;
	if (tag.startsWith("*",i)) {
		i++;	n = countDigits(tag,i);
		harg = varg = parseArg(tag,i,n); i += n; }
	else harg = varg = 1; }
			else if (tag.startsWith("WIDTH",i)) {
	i += 5; hcode |= ABS;
	if (tag.startsWith("=",i)) {
		i++;	n = countDigits(tag,i); harg = parseArg(tag,i,n); i += n; }
	else {
		harg = -1; break; }}
			else if (tag.startsWith("HEIGHT",i)) {
	i += 6; vcode |= ABS;
	if (tag.startsWith("=",i)) {
		i++;	n = countDigits(tag,i); varg = parseArg(tag,i,n); i += n; }
	else {
		varg = -1; break; }}
			else if (tag.startsWith("FLUSH",i)) {
	i += 5; hcode |= FLUSH; vcode |= FLUSH; }
			else {
	harg = -1; break; }
			for(; (i < l) && Character.isSpace(tag.charAt(i)); i++); // skip whitesp.
		}
		if ((harg == -1) || (varg == -1))
			System.out.println("StackLayout: can't understand \"" + tag + "\"");
		else {
			int codes[] = {hcode,vcode,harg,varg};
			codeTable.put(comp,codes); }}

	int countDigits(String tag, int i) {
		int j, l = tag.length();
		for(j = i; (j < l) && Character.isDigit(tag.charAt(j)); j++);
		return j-i; }

	int parseArg(String tag, int i, int n) {
		int num = -1;
		try {
			num = Integer.parseInt(tag.substring(i,i+n)); }
		catch(Exception e) {}
		return num; }

	/** Remove the specified component from the layout. */
	public void removeLayoutComponent(Component comp) {
		codeTable.remove(comp); }

	int defaultCode[] = {CENTER, CENTER, 0, 0};

	int[] getCode(Component comp) {
		int code[] = (int[]) codeTable.get(comp);
		return (code == null ? defaultCode : code); }

	boolean stretches(Component comp) {
		int c[] = getCode(comp);
		return c[orientation] == FILL; }

	Dimension computeLayoutSize(Container parent, boolean preferred) {
		Insets in = parent.insets();
		int inW = in.left+in.right, inH = in.top+in.bottom;
		int n = parent.countComponents();
		if (orientation == HORIZONTAL) {
			int maxH=0, totW=0,m;
			for(int i=0; i<n; i++) {
	Component comp=parent.getComponent(i);
	if (comp.isVisible()) {
		int code = getCode(comp)[orientation];
		m = ((code & FLUSH) == 0 ? margin : 0);
		Dimension d = (preferred && ((code & SIZEMASK) == FILL) ?
			 comp.preferredSize(): comp.minimumSize());
		maxH = Math.max(maxH,d.height+2*m);
		totW += d.width + 2*m;	}}
			return new Dimension(totW + inW, maxH + inH); }
		else {
			int maxW=0, totH=0, m;
			for(int i=0; i<n; i++) {
	Component comp=parent.getComponent(i);
	if (comp.isVisible()) {
		int code = getCode(comp)[orientation];
		m = ((code & FLUSH) == 0 ? margin : 0);
		Dimension d = (preferred && ((code & SIZEMASK) == FILL) ?
			 comp.preferredSize(): comp.minimumSize());
		maxW = Math.max(maxW,d.width+2*m);
		totH += d.height + 2*m; }}
			return new Dimension(maxW + inW, totH + inH); }}

	/** Calculate the minimum size dimensions for the specififed container.*/
	public Dimension minimumLayoutSize(Container parent) {
		return computeLayoutSize(parent, false); }

	/** Calculate the preferred size dimensions for the specififed container.*/
	public Dimension preferredLayoutSize(Container parent) {
		return computeLayoutSize(parent, true); }

	/** Lays out the specified container. */
	public void layoutContainer(Container parent) {
		int along = orientation, across = (orientation+1)%2;
		int n = parent.countComponents();
		Insets in = parent.insets();
		Dimension sz = parent.size();
		int W=sz.width-in.left-in.right,
				H=sz.height-in.top-in.bottom;
		int L = (orientation == HORIZONTAL ? W : H), // total running Length
				D = (orientation == HORIZONTAL ? H : W); // sideways Depth.

		// First pass: find visible components, record min. sizes,
		// find out how much leftover space there is.
		int nFills=0, nRubber = 0;
		int sum=0, prev;
		prev = FRONT;
		int codes[][] = new int[n][];
		int sizes[][] = new int[n][2];
		for(int i=0; i<n; i++) { // determine # of fills & remaining space.
			Component comp = parent.getComponent(i);
			if (comp.isVisible()) {
	Dimension d = comp.minimumSize();
	int code[] = getCode(comp);
	int size[] = sizes[i];
	codes[i] = code;
	size[0] = d.width;
	size[1] = d.height;
	int l = size[along],
			c = code[along];
	switch (c & SIZEMASK) {
			case FILL	 : nFills += code[along + 2]; break;
			case ABS		: sum += code[along + 2]; break;
			default		 : sum += l; break; }
	switch (c & POSMASK) {
		case CENTER : nRubber++; break;
					case BACK	 : if(prev != BACK) nRubber++; break; }
	if ((c & FLUSH) == 0)
		sum += 2*margin;
	prev = (c & POSMASK); }}
		if (prev == CENTER) nRubber++;
		// Divide up the leftover space among filled components (if any)
		// else as filler between centered or justified components.
		int rubber = ((nFills != 0) || (nRubber == 0) ? 0 :
			Math.max(0,(L - sum)/nRubber)),
				fill = (nFills == 0 ? 0 : Math.max(0,(L - sum)/nFills));

		// Second pass: layout the components.
		int r = (orientation == HORIZONTAL ? in.left : in.top), // running pos.
				s0= (orientation == HORIZONTAL ? in.top : in.left), // side pos.
				s,l,d,m;
		prev = FRONT;
		for(int i=0; i<n; i++) {
			int code[] = codes[i];
			int size[] = sizes[i];
			if (code != null) {
	int c = code[along], ca = code[across];
	m = ((c & FLUSH) == 0 ? margin : 0);
	r += m; s = s0 + m; l = size[along]; d = size[across];
	switch (c & SIZEMASK) {
					case FILL	 : if (fill > 0) l = fill*code[along+2]; break;
					case ABS		: l = code[along+2]; break; }
	switch (c & POSMASK) {
				case CENTER : r += rubber; break;
	 		case BACK	 : if(prev != BACK) r += rubber; break; }
	prev = (c & POSMASK);
	switch (ca & SIZEMASK) {
		case FILL	 : d = D-2*m; break;
		case ABS		: d = code[across+2]; break; }
	switch (ca & POSMASK) {
		case BACK	 : s += D-d; break;
		case CENTER : s += (D-d)/2; break; }
	Component comp = parent.getComponent(i);
	if (orientation == HORIZONTAL)
		comp.reshape(r,s,l,d);
	else
		comp.reshape(s,r,d,l);
	r += l + m;	}}
	}

}
