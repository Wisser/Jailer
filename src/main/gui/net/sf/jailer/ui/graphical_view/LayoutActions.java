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

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.swing.AbstractAction;
import javax.swing.Action;

import net.sf.jailer.ui.graphical_view.LayoutIcon.Kind;

/**
 * The actions arranging the tables of a {@link GraphicalDataModelView}.
 * Used by the "View" menu and the layout tool panel.
 *
 * @author Ralf Wisser
 */
public class LayoutActions {

	/**
	 * Key of the action value that is {@link Boolean#TRUE} for the actions arranging the tables anew
	 * ("Arrange ..."), as opposed to those adjusting the current arrangement.
	 */
	public static final String ARRANGEMENT = "LayoutActions.arrangement";

	/**
	 * Creates the layout actions.
	 *
	 * @param view gets the view to arrange, when the action is performed. May return <code>null</code>.
	 * @return the actions, in menu order
	 */
	public static List<Action> create(Supplier<GraphicalDataModelView> view) {
		List<Action> actions = new ArrayList<Action>();
		actions.add(create(view, "Arrange as tree", Kind.TREE,
				"Arrange the tables as a tree, starting at the subject table, and fix their positions.",
				GraphicalDataModelView::arrangeLayout));
		actions.add(create(view, "Arrange as tree (top-down)", Kind.TREE_TOP_DOWN,
				"Arrange the tables as a tree from top to bottom, starting at the subject table, and fix their positions.",
				GraphicalDataModelView::arrangeLayoutTopDown));
		actions.add(create(view, "Arrange radially", Kind.RADIAL,
				"Arrange the tables in rings around the subject table, one ring per level, and fix their positions.",
				GraphicalDataModelView::arrangeRadial));
		actions.add(create(view, "Arrange in a circle", Kind.CIRCLE,
				"Arrange the tables in a circle, associated tables next to each other, and fix their positions.",
				GraphicalDataModelView::arrangeCircle));
		actions.add(create(view, "Arrange in layers", Kind.LAYERS,
				"Arrange the tables in layers, parent tables left, child tables right, and fix their positions.",
				GraphicalDataModelView::arrangeLayered));
		actions.add(create(view, "Separate overlapping tables", Kind.SEPARATE,
				"Push overlapping tables apart, keeping the arrangement otherwise, and fix their positions.",
				GraphicalDataModelView::removeTableOverlaps));
		actions.add(create(view, "Compact", Kind.COMPACT,
				"Move the tables closer together, keeping the arrangement, and fix their positions.",
				GraphicalDataModelView::compact));
		actions.add(create(view, "Spread", Kind.SPREAD,
				"Move the tables further apart, keeping the arrangement, and fix their positions.",
				GraphicalDataModelView::spread));
		return actions;
	}

	private static Action create(final Supplier<GraphicalDataModelView> view, String name, Kind icon, String toolTip, final Consumer<GraphicalDataModelView> arrange) {
		Action action = new AbstractAction(name, new LayoutIcon(icon)) {
			@Override
			public void actionPerformed(ActionEvent e) {
				GraphicalDataModelView v = view.get();
				if (v != null) {
					arrange.accept(v);
				}
			}
		};
		action.putValue(Action.SHORT_DESCRIPTION, toolTip);
		action.putValue(ARRANGEMENT, Boolean.valueOf(icon != Kind.SEPARATE && icon != Kind.COMPACT && icon != Kind.SPREAD));
		return action;
	}

}
