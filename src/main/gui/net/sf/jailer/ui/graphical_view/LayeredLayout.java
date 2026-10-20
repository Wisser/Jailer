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

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Layered ("Sugiyama style") graph layout. Parents are placed in the left layers, children
 * in the right ones, and the order within the layers is chosen so that only few edges cross.
 * <ol>
 * <li>cycles are broken by reversing the back edges of a depth-first search</li>
 * <li>each node gets the layer of the longest path from a source to it</li>
 * <li>edges spanning several layers are routed over dummy nodes</li>
 * <li>the order within the layers is improved by barycenter sweeps</li>
 * <li>the vertical positions are pulled towards the neighbors, keeping order and spacing</li>
 * </ol>
 * Pure layout logic, independent of prefuse.
 *
 * @param <T> the node type
 *
 * @author Ralf Wisser
 */
public class LayeredLayout<T> {

	private static final double LAYER_SPACING = 80;
	private static final double NODE_SPACING = 20;
	private static final double DUMMY_SPACING = 10;
	private static final int SWEEPS = 24;
	private static final int SMOOTHING_PASSES = 8;

	private final List<T> nodes;
	private final Map<T, Integer> index = new HashMap<T, Integer>();

	/**
	 * Width and height per node, real nodes first, then the dummy nodes.
	 */
	private final List<double[]> sizes = new ArrayList<double[]>();

	/**
	 * Edges (parent, child) between real nodes, in insertion order.
	 */
	private final Set<List<Integer>> edges = new LinkedHashSet<List<Integer>>();

	/**
	 * Constructor.
	 *
	 * @param nodes the nodes, in a stable order (the result depends on it)
	 * @param sizes the size per node, only width and height are used
	 */
	public LayeredLayout(List<T> nodes, Map<T, Rectangle2D> sizes) {
		this.nodes = new ArrayList<T>(nodes);
		for (T node: this.nodes) {
			index.put(node, this.sizes.size());
			Rectangle2D size = sizes.get(node);
			this.sizes.add(new double[] { size == null? 0 : size.getWidth(), size == null? 0 : size.getHeight() });
		}
	}

	/**
	 * Adds an edge. Self-loops, duplicates and edges with unknown nodes are ignored.
	 *
	 * @param parent the node to place in a layer left of the child
	 * @param child the child
	 */
	public void addEdge(T parent, T child) {
		Integer p = index.get(parent);
		Integer c = index.get(child);
		if (p != null && c != null && !p.equals(c)) {
			edges.add(Arrays.asList(p, c));
		}
	}

	/**
	 * Computes the layout.
	 *
	 * @return the center position per node
	 */
	public Map<T, Point2D> layout() {
		int n = nodes.size();
		Map<T, Point2D> result = new HashMap<T, Point2D>();
		if (n == 0) {
			return result;
		}
		List<int[]> acyclic = breakCycles(n);
		int[] layerOfReal = assignLayers(n, acyclic);

		// proper layered graph: every edge spans exactly one layer
		List<Integer> layerOf = new ArrayList<Integer>();
		for (int l: layerOfReal) {
			layerOf.add(l);
		}
		List<List<Integer>> preds = new ArrayList<List<Integer>>();
		List<List<Integer>> succs = new ArrayList<List<Integer>>();
		for (int i = 0; i < n; ++i) {
			preds.add(new ArrayList<Integer>());
			succs.add(new ArrayList<Integer>());
		}
		for (int[] e: acyclic) {
			int from = e[0];
			for (int l = layerOf.get(e[0]) + 1; l < layerOf.get(e[1]); ++l) {
				int dummy = sizes.size();
				sizes.add(new double[] { 0, 0 });
				layerOf.add(l);
				preds.add(new ArrayList<Integer>());
				succs.add(new ArrayList<Integer>());
				succs.get(from).add(dummy);
				preds.get(dummy).add(from);
				from = dummy;
			}
			succs.get(from).add(e[1]);
			preds.get(e[1]).add(from);
		}
		int total = sizes.size();
		int numLayers = 0;
		for (int l: layerOf) {
			numLayers = Math.max(numLayers, l + 1);
		}

		List<List<Integer>> layers = initialOrder(total, numLayers, layerOf, succs);
		layers = minimizeCrossings(total, layers, preds, succs);

		double[] x = new double[total];
		double[] y = new double[total];
		assignCoordinates(layers, preds, succs, x, y);

		for (int i = 0; i < n; ++i) {
			result.put(nodes.get(i), new Point2D.Double(x[i], y[i]));
		}
		return result;
	}

	/**
	 * Reverses the back edges of a depth-first search.
	 *
	 * @return acyclic edges (parent, child), without duplicates
	 */
	private List<int[]> breakCycles(int n) {
		List<List<Integer>> out = new ArrayList<List<Integer>>();
		for (int i = 0; i < n; ++i) {
			out.add(new ArrayList<Integer>());
		}
		for (List<Integer> e: edges) {
			out.get(e.get(0)).add(e.get(1));
		}
		// 0 = unvisited, 1 = on the stack, 2 = done
		int[] state = new int[n];
		Set<List<Integer>> backEdges = new HashSet<List<Integer>>();
		for (int start = 0; start < n; ++start) {
			if (state[start] != 0) {
				continue;
			}
			// iterative, so that long chains don't overflow the stack
			List<int[]> stack = new ArrayList<int[]>();
			stack.add(new int[] { start, 0 });
			state[start] = 1;
			while (!stack.isEmpty()) {
				int[] top = stack.get(stack.size() - 1);
				List<Integer> next = out.get(top[0]);
				if (top[1] < next.size()) {
					int v = next.get(top[1]++);
					if (state[v] == 0) {
						state[v] = 1;
						stack.add(new int[] { v, 0 });
					} else if (state[v] == 1) {
						backEdges.add(Arrays.asList(top[0], v));
					}
				} else {
					state[top[0]] = 2;
					stack.remove(stack.size() - 1);
				}
			}
		}
		Set<List<Integer>> acyclic = new LinkedHashSet<List<Integer>>();
		for (List<Integer> e: edges) {
			if (backEdges.contains(e)) {
				acyclic.add(Arrays.asList(e.get(1), e.get(0)));
			} else {
				acyclic.add(e);
			}
		}
		List<int[]> result = new ArrayList<int[]>();
		for (List<Integer> e: acyclic) {
			result.add(new int[] { e.get(0), e.get(1) });
		}
		return result;
	}

	/**
	 * Assigns to each node the length of the longest path from a source to it.
	 */
	private int[] assignLayers(int n, List<int[]> acyclic) {
		int[] inDegree = new int[n];
		List<List<Integer>> out = new ArrayList<List<Integer>>();
		for (int i = 0; i < n; ++i) {
			out.add(new ArrayList<Integer>());
		}
		for (int[] e: acyclic) {
			out.get(e[0]).add(e[1]);
			++inDegree[e[1]];
		}
		int[] layer = new int[n];
		List<Integer> queue = new ArrayList<Integer>();
		for (int i = 0; i < n; ++i) {
			if (inDegree[i] == 0) {
				queue.add(i);
			}
		}
		for (int head = 0; head < queue.size(); ++head) {
			int u = queue.get(head);
			for (int v: out.get(u)) {
				layer[v] = Math.max(layer[v], layer[u] + 1);
				if (--inDegree[v] == 0) {
					queue.add(v);
				}
			}
		}
		return layer;
	}

	/**
	 * Initial order within the layers: the order of a depth-first search.
	 */
	private List<List<Integer>> initialOrder(int total, int numLayers, List<Integer> layerOf, List<List<Integer>> succs) {
		List<List<Integer>> layers = new ArrayList<List<Integer>>();
		for (int l = 0; l < numLayers; ++l) {
			layers.add(new ArrayList<Integer>());
		}
		boolean[] visited = new boolean[total];
		for (int start = 0; start < total; ++start) {
			if (visited[start]) {
				continue;
			}
			List<Integer> stack = new ArrayList<Integer>();
			stack.add(start);
			while (!stack.isEmpty()) {
				int u = stack.remove(stack.size() - 1);
				if (visited[u]) {
					continue;
				}
				visited[u] = true;
				layers.get(layerOf.get(u)).add(u);
				List<Integer> next = succs.get(u);
				for (int i = next.size() - 1; i >= 0; --i) {
					if (!visited[next.get(i)]) {
						stack.add(next.get(i));
					}
				}
			}
		}
		return layers;
	}

	/**
	 * Improves the order within the layers by barycenter sweeps, alternately left to right
	 * and right to left. Keeps the best order found.
	 */
	private List<List<Integer>> minimizeCrossings(int total, List<List<Integer>> layers, List<List<Integer>> preds, List<List<Integer>> succs) {
		final double[] pos = new double[total];
		updatePositions(layers, pos);
		List<List<Integer>> best = copy(layers);
		long bestCrossings = countCrossings(layers, succs, pos);
		for (int sweep = 0; sweep < SWEEPS && bestCrossings > 0; ++sweep) {
			boolean forward = sweep % 2 == 0;
			int numLayers = layers.size();
			for (int i = 1; i < numLayers; ++i) {
				int l = forward? i : numLayers - 1 - i;
				List<List<Integer>> neighbors = forward? preds : succs;
				final double[] key = new double[total];
				for (int u: layers.get(l)) {
					List<Integer> nb = neighbors.get(u);
					if (nb.isEmpty()) {
						key[u] = pos[u];
					} else {
						double sum = 0;
						for (int v: nb) {
							sum += pos[v];
						}
						key[u] = sum / nb.size();
					}
				}
				// stable, nodes with equal keys keep their order
				Collections.sort(layers.get(l), new Comparator<Integer>() {
					@Override
					public int compare(Integer a, Integer b) {
						return Double.compare(key[a], key[b]);
					}
				});
				updatePositions(Collections.singletonList(layers.get(l)), pos);
			}
			long crossings = countCrossings(layers, succs, pos);
			if (crossings < bestCrossings) {
				bestCrossings = crossings;
				best = copy(layers);
			}
		}
		return best;
	}

	private static void updatePositions(List<List<Integer>> layers, double[] pos) {
		for (List<Integer> layer: layers) {
			for (int i = 0; i < layer.size(); ++i) {
				pos[layer.get(i)] = i;
			}
		}
	}

	private static List<List<Integer>> copy(List<List<Integer>> layers) {
		List<List<Integer>> copy = new ArrayList<List<Integer>>();
		for (List<Integer> layer: layers) {
			copy.add(new ArrayList<Integer>(layer));
		}
		return copy;
	}

	/**
	 * Counts the crossings between all adjacent layers: the inversions of the child positions
	 * when the edges are sorted by parent position.
	 */
	private static long countCrossings(List<List<Integer>> layers, List<List<Integer>> succs, double[] pos) {
		long crossings = 0;
		for (int l = 0; l + 1 < layers.size(); ++l) {
			List<int[]> layerEdges = new ArrayList<int[]>();
			for (int u: layers.get(l)) {
				for (int v: succs.get(u)) {
					layerEdges.add(new int[] { (int) pos[u], (int) pos[v] });
				}
			}
			Collections.sort(layerEdges, new Comparator<int[]>() {
				@Override
				public int compare(int[] a, int[] b) {
					return a[0] != b[0]? Integer.compare(a[0], b[0]) : Integer.compare(a[1], b[1]);
				}
			});
			// Fenwick tree over the child positions
			int size = layers.get(l + 1).size();
			long[] tree = new long[size + 1];
			long inserted = 0;
			for (int[] e: layerEdges) {
				long notGreater = 0;
				for (int i = e[1] + 1; i > 0; i -= i & -i) {
					notGreater += tree[i];
				}
				crossings += inserted - notGreater;
				for (int i = e[1] + 1; i <= size; i += i & -i) {
					++tree[i];
				}
				++inserted;
			}
		}
		return crossings;
	}

	/**
	 * Places the layers side by side and stacks the nodes within each layer, then pulls the
	 * nodes towards their neighbors without changing their order or their minimum distance.
	 */
	private void assignCoordinates(List<List<Integer>> layers, List<List<Integer>> preds, List<List<Integer>> succs, double[] x, double[] y) {
		double left = 0;
		for (List<Integer> layer: layers) {
			double width = 0;
			for (int u: layer) {
				width = Math.max(width, sizes.get(u)[0]);
			}
			for (int u: layer) {
				x[u] = left + width / 2;
			}
			left += width + LAYER_SPACING;

			double top = 0;
			for (int i = 0; i < layer.size(); ++i) {
				int u = layer.get(i);
				if (i > 0) {
					top += minDistance(layer.get(i - 1), u);
				}
				y[u] = top;
			}
			for (int u: layer) {
				y[u] -= top / 2;
			}
		}

		for (int pass = 0; pass < SMOOTHING_PASSES; ++pass) {
			for (List<Integer> layer: layers) {
				int size = layer.size();
				double[] desired = new double[size];
				for (int i = 0; i < size; ++i) {
					int u = layer.get(i);
					double sum = 0;
					int count = 0;
					for (int v: preds.get(u)) {
						sum += y[v];
						++count;
					}
					for (int v: succs.get(u)) {
						sum += y[v];
						++count;
					}
					desired[i] = count == 0? y[u] : sum / count;
				}
				// two feasible placements, pushed down resp. up; their mean is feasible too
				double[] down = new double[size];
				double[] up = new double[size];
				for (int i = 0; i < size; ++i) {
					down[i] = i == 0? desired[i] : Math.max(desired[i], down[i - 1] + minDistance(layer.get(i - 1), layer.get(i)));
				}
				for (int i = size - 1; i >= 0; --i) {
					up[i] = i == size - 1? desired[i] : Math.min(desired[i], up[i + 1] - minDistance(layer.get(i), layer.get(i + 1)));
				}
				for (int i = 0; i < size; ++i) {
					y[layer.get(i)] = (down[i] + up[i]) / 2;
				}
			}
		}
	}

	private double minDistance(int upper, int lower) {
		boolean dummy = upper >= nodes.size() || lower >= nodes.size();
		return sizes.get(upper)[1] / 2 + sizes.get(lower)[1] / 2 + (dummy? DUMMY_SPACING : NODE_SPACING);
	}

}
