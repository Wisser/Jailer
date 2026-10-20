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
import javax.swing.JLabel;

import net.sf.jailer.ui.UIUtil;

/**
 * The actions arranging the tables of a {@link GraphicalDataModelView}.
 * Used by the "View" menu and the layout tool panel.
 *
 * @author Ralf Wisser
 */
public class LayoutActions {

	/**
	 * Creates the layout actions.
	 *
	 * @param view gets the view to arrange, when the action is performed. May return <code>null</code>.
	 * @return the actions, in menu order
	 */
	public static List<Action> create(Supplier<GraphicalDataModelView> view) {
		List<Action> actions = new ArrayList<Action>();
		actions.add(create(view, "Arrange as tree", "/navigation.png",
				"Arrange the tables as a tree, starting at the subject table, and fix their positions.",
				GraphicalDataModelView::arrangeLayout));
		actions.add(create(view, "Arrange as tree (top-down)", "/navigation.png",
				"Arrange the tables as a tree from top to bottom, starting at the subject table, and fix their positions.",
				GraphicalDataModelView::arrangeLayoutTopDown));
		actions.add(create(view, "Arrange radially", "/anchor.png",
				"Arrange the tables in rings around the subject table, one ring per level, and fix their positions.",
				GraphicalDataModelView::arrangeRadial));
		actions.add(create(view, "Arrange in a circle", "/sync.png",
				"Arrange the tables in a circle, associated tables next to each other, and fix their positions.",
				GraphicalDataModelView::arrangeCircle));
		actions.add(create(view, "Arrange in layers", "/tables.png",
				"Arrange the tables in layers, parent tables left, child tables right, and fix their positions.",
				GraphicalDataModelView::arrangeLayered));
		actions.add(create(view, "Separate overlapping tables", "/expandvisible.png",
				"Push overlapping tables apart, keeping the arrangement otherwise, and fix their positions.",
				GraphicalDataModelView::removeTableOverlaps));
		return actions;
	}

	private static Action create(final Supplier<GraphicalDataModelView> view, String name, String icon, String toolTip, final Consumer<GraphicalDataModelView> arrange) {
		Action action = new AbstractAction(name, UIUtil.scaleIcon(new JLabel(""), UIUtil.readImage(icon))) {
			@Override
			public void actionPerformed(ActionEvent e) {
				GraphicalDataModelView v = view.get();
				if (v != null) {
					arrange.accept(v);
				}
			}
		};
		action.putValue(Action.SHORT_DESCRIPTION, toolTip);
		return action;
	}

}
