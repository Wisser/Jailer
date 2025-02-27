/*
 * Copyright 2007 - 2025 Ralf Wisser.
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
package net.sf.jailer.ui.databrowser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;
import java.util.function.Consumer;

/**
 * Data browser table windows layout optimizer.
 * 
 * @author Ralf Wisser
 */
public class TreeLayoutOptimizer<T> {

	public static class Node<T> {
		private final T userObject;
		private final List<Node<T>> children = new ArrayList<Node<T>>();
		private final boolean isAnchor;
		int level = 0;
		Node<T> parent = null;
		double position = 0;
		
		public T getUserObject() {
			return userObject;
		}
		
		public Node(T userObject, boolean isAnchor) {
			this.userObject = userObject;
			this.isAnchor = isAnchor;
		}
		
		public void addChild(Node<T> child) {
			children.add(child);
			child.parent = this;
			child.level = level + 1;
		}
		
		public void visit(Consumer<Node<T>> visitor) {
			visitor.accept(this);
			children.forEach(child -> child.visit(visitor));
		}
		
		public double getPosition() {
			return position;
		}

		public int getLevel() {
			return level;
		}

		public List<Node<T>> getChildren() {
			return children;
		}
		
		public int getDepth() {
			int maxChildDepth = 0;
			for (Node<T> child: children) {
				maxChildDepth = Math.max(maxChildDepth, child.getDepth());
			}
			return 1 + maxChildDepth;
		}

		public double getMinPosition() {
			double minPos = position;
			if (getUserObject() == null) {
				if (!children.isEmpty()) {
					minPos = children.get(0).position;
				}
			}
			for (Node<T> child: children) {
				minPos = Math.min(minPos, child.getMinPosition());
			}
			return minPos;
		}

		public void adjustPosition(double delta) {
			position -= delta;
			for (Node<T> child: children) {
				child.adjustPosition(delta);
			}
		}

		public void adjustPosition(double delta, double thres) {
			if (position >= thres) {
				position -= delta;
			}
			for (Node<T> child: children) {
				child.adjustPosition(delta, thres);
			}
		}

		public int getNodesCount() {
			int count = 1;
			for (Node<T> child: children) {
				count += child.getNodesCount();
			}
			return count;
		}

		public double getCompactness() {
			double compactness = 0;
			if (children.size() > 0) {
				compactness = children.get(children.size() - 1).position - children.get(0).position;
				compactness *= compactness;
				Node<T> pre = null;
				for (Node<T> child: children) {
					compactness += child.getCompactness();
					if (pre != null) {
						if (pre.position + 1 > child.position) {
							double w = pre.position + 1 - child.position;
							compactness += w * w * 1000;
						}
					}
					pre = child;
				}
			}
			return compactness;
		}

		public double getAnchorQuality() {
			double quality = 0;
			for (Node<T> child: children) {
				quality += child.getAnchorQuality();
			}

			if (isAnchor && parent != null) {
				double d = parent.position - position;
				quality += d * d;
			}
			return quality;
		}
	}

	public static <T> void optimizeTreeLayout(Node<T> root) {
		int numNodes = root.getNodesCount();
		optimizeChildrenOrder(root, System.currentTimeMillis(), MAX_OPTIM_TIME_MS, numNodes);
		layoutTree(root, numNodes);
		optimizeLeafs(root);
		resolveOverlaps(root, root, new HashMap<Integer, Node<T>>());
		root.adjustPosition(root.getMinPosition());
		SortedSet<Double> ys = new TreeSet<Double>();
		root.children.forEach(child -> child.visit(node -> ys.add(node.position)));
		double yo = 0;
		double shift = 0;
		for (double y: ys) {
			if (y > yo + 1) {
				double d = y - (yo + 1);
				double fShift = shift;
				root.children.forEach(child -> child.visit(
						node -> {
							if (node.position >= y - fShift) {
								node.position -= d;
							}
						}));
						
				shift += d;
			}
			yo = y;
		}
	}

	private static <T> void resolveOverlaps(Node<T> node, Node<T> root, HashMap<Integer, Node<T>> pred) {
		for (Node<T> child: node.children) {
			if (pred.containsKey(child.level)) {
				double delta = (pred.get(child.level).position + 1) - child.position;
				if (delta > 0) {
					root.adjustPosition(-delta, child.position);
				}
			}
			pred.put(child.level, child);
			resolveOverlaps(child, root, pred);
		}
	}

	private static long MAX_OPTIM_TIME_MS = 1000;
	
	private static <T> double layoutTree(Node<T> root, int numNodes) {
		double[] maxPositionPerLevel = new double[root.getDepth()];
		for (int i = 0; i < maxPositionPerLevel.length; ++i) {
			maxPositionPerLevel[i] = -1;
		}
		root.position = 0.0;
		layoutNode(root, maxPositionPerLevel);

		double sumMaxPositionSqr = 0;
		for (int i = 0; i < maxPositionPerLevel.length; ++i) {
			if (maxPositionPerLevel[i] > 0) {
				sumMaxPositionSqr += maxPositionPerLevel[i] * maxPositionPerLevel[i];
			}
		}
		return sumMaxPositionSqr / numNodes + root.getCompactness() + root.getAnchorQuality();
	}
	
	private static <T> void layoutNode(Node<T> node, double[] maxPositionPerLevel) {
		for (Node<T> child: node.children) {
			child.position = Math.max(node.position - (node.children.size() - 1) / 2.0, maxPositionPerLevel[child.level] + 1);
			maxPositionPerLevel[child.level] = child.position;
			layoutNode(child, maxPositionPerLevel);
		}
		if (!node.children.isEmpty()) {
			adjustParentPosition(node, maxPositionPerLevel);
		}
	}
	
	private static <T> void adjustParentPosition(Node<T> node, double[] maxPositionPerLevel) {
		while (node != null) {
			Node<T> anchorChild = null;
			if (node.isAnchor) {
				for (Node<T> child: node.children) {
					if (child.isAnchor) {
						anchorChild = child;
						break;
					}
				}
			}
			if (anchorChild != null) {
				node.position = anchorChild.position;
			} else {
				double parentPos = node.children.get(0).position + (maxPositionPerLevel[node.level + 1] - node.children.get(0).position) / 2.0;
				if (node.position < parentPos) {
					node.position = parentPos;
					// node.position = (node.position + parentPos) / 2.0;
					maxPositionPerLevel[node.level] = parentPos;
				} else {
					break;
				}
			}
			node = node.parent;
		}
	}

	private static <T> void optimizeLeafs(Node<T> node) {
		if (node.children.isEmpty() && node.parent != null && node.position < node.parent.position) {
			int i = node.parent.children.indexOf(node);
			if (i >= 0 && i < node.parent.children.size() - 1) {
				Node<T> brother = node.parent.children.get(i + 1);
				node.position = Math.min(node.parent.position, brother.position - 1);
			}
		}
		for (int i = node.children.size() - 1; i >= 0; --i) {
			optimizeLeafs(node.children.get(i));
		}
	}
	
	private static abstract class Modification {
		public abstract void doIt();
		public abstract void undoIt();
	}
	
	private static <T> void optimizeChildrenOrder(Node<T> root, long startTime, long maxTime, int numNodes) {
		Stack<Node<T>> toVisit = new Stack<TreeLayoutOptimizer.Node<T>>();
		toVisit.push(root);
		List<Modification> modifications = new ArrayList<TreeLayoutOptimizer.Modification>();
		
		while (!toVisit.isEmpty()) {
			final Node<T> node = toVisit.pop();
			for (Node<T> child: node.children) {
				toVisit.push(child);
			}
			for (int i = 0; i < node.children.size(); ++i) {
				for (int j = i + 1; j < node.children.size(); ++j) {
					final int a = i;
					final int b = j;
					modifications.add(new Modification() {
						@Override
						public void doIt() {
							Node<T> h = node.children.get(a);
							node.children.set(a, node.children.get(b));
							node.children.set(b, h);
						}
						@Override
						public void undoIt() {
							doIt();
						}
					});
				}
				
			}
		}
		
		// add modification pairs
		List<Modification> modificationPairs = new ArrayList<TreeLayoutOptimizer.Modification>();
		int l = modifications.size();
		int countDown = Math.max(100, 10 * l);
		for (int i = 0; i < l; ++i) {
			for (int j = i + 1; j < l; ++j) {
				final Modification a = modifications.get(i);
				final Modification b = modifications.get(j);
				modificationPairs.add(new Modification() {
					@Override
					public void doIt() {
						a.doIt();
						b.doIt();
					}
					@Override
					public void undoIt() {
						b.undoIt();
						a.undoIt();
					}
				});
				--countDown;
				if (countDown < 0) {
					break;
				}
			}
			if (countDown < 0) {
				break;
			}
		}
		modifications.addAll(modificationPairs);
		
		double currentMaxPosition = layoutTree(root, numNodes);
		while (System.currentTimeMillis() - startTime < maxTime) {
			Modification bestModification = null;
			double bestMaxPosition = 0;
			for (Modification modification: modifications) {
				modification.doIt();
				double maxPosition = layoutTree(root, numNodes);
				if (maxPosition < currentMaxPosition) {
					if (bestModification == null || maxPosition < bestMaxPosition) {
						bestMaxPosition = maxPosition;
						bestModification = modification;
					}
				}
				modification.undoIt();
			}
			if (bestModification != null) {
				bestModification.doIt();
				currentMaxPosition = bestMaxPosition;
			} else {
				break;
			}
		}
	}

}
