/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

import java.io.Serializable;
import java.util.List;
import java.util.Set;

/**
 * Item in the path-history of {@link PathFinderView}.
 * 
 * @author Ralf Wisser
 */
public class HistoryItem implements Serializable {
	private static final long serialVersionUID = -2228012653415732355L;

	public String source;
	public String destination;
	public boolean implicit = false;
	
	public List<String> pathStations;
	public List<String> path;
	public Set<String> excludedTables;

	public boolean isValid() {
		return path != null && pathStations != null && excludedTables != null && source != null && destination != null;
	}
	
}
