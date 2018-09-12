/*
 * Copyright 2007 - 2018 the original author or authors.
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
package net.sf.jailer.ui.pathfinder;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Toolkit;
import java.util.List;
import java.util.Set;

import org.fife.rsta.ui.EscapableDialog;

import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.UIUtil;

/**
 * Finds path between two tables.
 * 
 * @author Ralf Wisser
 */
public class PathFinder {

	private List<Table> path;
	private boolean expand;
	
	public class Result {
		public List<Table> path;
		public Set<Table> excludedTables;
		public boolean expand;
	}
	
	public Result find(Table source, Table destination, DataModel dataModel, boolean withOpenTablesButton, Frame owner) {
		final EscapableDialog dialog = new EscapableDialog(owner, "PathFinder", true) {
			private static final long serialVersionUID = 1L;
		};

		PathFinderView view = new PathFinderView(dataModel, source, destination, withOpenTablesButton) {
			@Override
			protected void applyPath(List<Table> path, boolean expand) {
				PathFinder.this.path = path;
				PathFinder.this.expand = expand;
				dialog.setVisible(false);
			}
		};

		view.showGraph(true);
		
		dialog.getContentPane().add(view);
		dialog.pack();
		int minWidth = 1200;
		dialog.setSize(minWidth, Math.min(Math.max(dialog.getHeight() + 20, 440), 600));
		Point ownerLoc;
		Dimension ownerSize;
		if (owner != null) {
			ownerLoc = owner.getLocation();
			ownerSize = owner.getSize();
		} else {
			ownerLoc = new Point();
			ownerSize = Toolkit.getDefaultToolkit().getScreenSize();
		}
		int x = (int) (ownerLoc.getX() + ownerSize.getWidth() / 2 - dialog.getWidth() / 2);
		int y = (int) (ownerLoc.getY() + ownerSize.getHeight() / 2 - dialog.getHeight() / 2);
		dialog.setLocation(x, y);
		int h = dialog.getHeight();
		UIUtil.fit(dialog);
		if (h > dialog.getHeight()) {
			dialog.setLocation(x, y - (h - dialog.getHeight()));
			dialog.setSize(Math.max(minWidth, dialog.getWidth()), Math.min(Math.max(dialog.getHeight() + 20, 400), 600));
			UIUtil.fit(dialog);
		}
		
		dialog.setVisible(true);
		if (path == null) {
			return null;
		}
		Result result = new Result();
		result.path = path;
		result.excludedTables = view.getExcludedTables();
		result.expand = expand;
		return result;
	}

}
