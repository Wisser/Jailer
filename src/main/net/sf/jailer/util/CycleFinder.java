/*
 * Copyright 2007 - 2015 the original author or authors.
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
package net.sf.jailer.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.DataModel;
import net.sf.jailer.datamodel.Table;

/**
 * Finds dependency cycles.
 * 
 * @author Ralf Wisser
 */
public class CycleFinder {

	/** 
	 * Path from n table <code>from</code> to another table <code>to</code>.
	 * Concatenation of two paths left/right or an edge, if left and right is null.
	 */
	public static class Path {
		final Table from;
		final Table to;
		final Path left;
		final Path right;
		final int birthday;
		final int length;
		
		Path(Table from, Table to, Path left, Path right, int birthday) {
			this.from = from;
			this.to = to;
			this.left = left;
			this.right = right;
			this.birthday = birthday;
			this.length = left == null? 1 : (left.length + right.length);
		}
		
		public boolean equals(Object other) {
			if (!(other instanceof Path)) {
				return false;
			}
			Path op = (Path) other;
			if (from != op.from || to != op.to || length != op.length) {
				return false;
			}
			Set<Table> p1 = new TreeSet<Table>();
			Set<Table> p2 = new TreeSet<Table>();
			fillSet(p1);
			op.fillSet(p2);
			return p1.equals(p2);
		}
		
		public void fillPath(List<Table> path) {
			if (left == null) {
				path.add(from);
				path.add(to);
			} else {
				left.fillPath(path);
				path.remove(path.size() - 1);
				right.fillPath(path);
			}
		}

		private void fillSet(Set<Table> set) {
			if (left == null) {
				set.add(from);
				if (to != from) {
					set.add(to);
				}
			} else {
				left.fillSet(set);
				right.fillSet(set);
			}
		}

		public String toString() {
			return "{" + (left == null? from.getName() + "->" + to.getName() : (left + "-->" + right)) + " " + birthday + "/" + length + "}";
		}
	}
	
	/**
	 * Finds all dependency cycles in a data model.
	 * 
	 * @param dataModel the data model
	 * @return all cycles in the data model
	 */
	public static Collection<Path> findCycle(DataModel dataModel, Collection<Table> tables) {
		Collection<Path> allCycles = new ArrayList<Path>();
		Set<Pair<Table, Table>> tabu = new HashSet<Pair<Table,Table>>();
		
		for (;;) {
			tables = getCycle(tables, tabu);
			Map<Table, List<Path>> fromToPaths = new TreeMap<Table, List<Path>>();
			for (Table table: tables) {
				List<Path> pathList = new ArrayList<Path>(10);
				fromToPaths.put(table, pathList);
			}
			for (Table table: tables) {
				for (Association association: table.associations) {
					// if (!association.isIgnored()) {
						if (association.isInsertDestinationBeforeSource()) {
							if (tables.contains(association.destination)) {
								Path path = new Path(association.source, association.destination, null, null, 0);
								fromToPaths.get(path.from).add(path);
							}
						}
	//				}
				}
			}
			List<Path> newPaths = new ArrayList<Path>();
			Set<Table> tSet = new TreeSet<Table>();
			for (int today = 1; ; ++today) {
				newPaths.clear();
				int yesterday = today - 1;
				
				for (Map.Entry<Table, List<Path>> e: fromToPaths.entrySet()) {
					for (Path path: e.getValue()) {
						CancellationHandler.checkForCancellation(null);
						if (path.from != path.to) {
							List<Path> list = fromToPaths.get(path.to);
							if (list != null) {
								for (Path toAppend: list) {
									if (toAppend.from != toAppend.to) {
										if (toAppend.birthday == yesterday) {
											Path newPath = new Path(path.from, toAppend.to, path, toAppend, today);
											if (!fromToPaths.get(newPath.from).contains(newPath)) {
												int aSize;
												if (newPath.from == newPath.to) {
													aSize = newPath.length;
												} else {
													aSize = newPath.length + 1;
												}
												tSet.clear();
												newPath.fillSet(tSet);
												if (tSet.size() == aSize) {
													newPaths.add(newPath);
												}
											}
										}
									}
								}
							}
						}
					}
				}
				if (newPaths.isEmpty()) {
					break;
				} else {
					for (Path path: newPaths) {
						fromToPaths.get(path.from).add(path);
					}
				}
				boolean cycFound = false;
				for (List<Path> pList: fromToPaths.values()) {
					for (Path path: pList) {
						if (path.from == path.to) {
							cycFound = true;
							break;
						}
					}
					if (cycFound) {
						break;
					}
				}
				if (cycFound) {
					break;
				}
			}
			
			Map<Set<Table>, Path> cycles = new HashMap<Set<Table>, Path>();
			long nPath = 0;
			for (List<Path> pList: fromToPaths.values()) {
				CancellationHandler.checkForCancellation(null);
				nPath += pList.size();
				for (Path path: pList) {
					if (path.from == path.to) {
						List<Table> pl = new ArrayList<Table>();
						path.fillPath(pl);
						for (int i = 0; i < pl.size(); ++i) {
							tabu.add(new Pair<Table, Table>(pl.get(i), pl.get((i + 1) % pl.size())));
						}
						Set<Table> taSet = new TreeSet<Table>();
						path.fillSet(taSet);
						cycles.put(taSet, path);
					}
				}
			}
			allCycles.addAll(cycles.values());
			if (cycles.isEmpty()) {
				break;
			}
		}
		return allCycles;
	}
	
	/**
     * Gets set of all tables involved in a cycle.
     * 
     * @param tables all tables
     * @return subset of <code>tables</code> involved in a cycle
     */
    public static Set<Table> getCycle(Collection<Table> tables) {
    	return getCycle(tables, new HashSet<Pair<Table,Table>>());
	}

	/**
     * Gets set of all tables involved in a cycle.
     * 
     * @param tables all tables
     * @return subset of <code>tables</code> involved in a cycle
     */
	private static Set<Table> getCycle(Collection<Table> tables, Set<Pair<Table, Table>> tabu) {
		Set<Table> cycle = new TreeSet<Table>(tables);
		for (;;) {
			Set<Table> notInCycle = new TreeSet<Table>();
			for (Table table: cycle) {
				boolean hasIn = false;
				boolean hasOut = false;
				for (Association association: table.associations) {
					if (cycle.contains(association.destination)) {
						if (association.isInsertSourceBeforeDestination()) {
							if (!tabu.contains(new Pair<Table, Table>(association.source, association.destination))) {
								hasOut = true;
							}
						}
						if (association.isInsertDestinationBeforeSource()) {
							if (!tabu.contains(new Pair<Table, Table>(association.destination, association.source))) {
								hasIn = true;
							}
						}
					}
				}
				if (!(hasIn && hasOut)) {
					notInCycle.add(table);
				}
			}
			if (notInCycle.isEmpty()) {
				break;
			} else {
				cycle.removeAll(notInCycle);
			}
		}
		return cycle;
	}

}
