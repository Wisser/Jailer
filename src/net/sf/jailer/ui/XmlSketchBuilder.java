/*
 * Copyright 2007 the original author or authors.
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
package net.sf.jailer.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import net.sf.jailer.datamodel.AggregationSchema;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Cardinality;
import net.sf.jailer.datamodel.Table;

/**
 * Builds XML sketches.
 * 
 * @author Ralf Wisser
 */
public class XmlSketchBuilder {

	/**
	 * Builds XML sketches for given table.
	 * 
	 * @param table the table
	 * @return xml sketch for table 
	 */
	public static String buildSketch(Table table, int depth) {
		StringBuilder sb = new StringBuilder();
		
		if (table != null && depth < 2) {
			List<Association> sortedSourceAssociations = new ArrayList<Association>(table.associations);
			Collections.sort(sortedSourceAssociations, new Comparator<Association>() {
				public int compare(Association o1, Association o2) {
					return o1.destination.getName().compareTo(o2.destination.getName());
				}
			});

			if (depth == 0) {
				sb.append("<" + table.getName().toLowerCase() + ">\n");
				sb.append("    -- elements of " + table.getName() + " --\n");
			}
			String indent = "    ";
			for (Association a: sortedSourceAssociations) {
				if (a.getAggregationSchema() == AggregationSchema.EXPLICIT_LIST) {
					sb.append(indent + "<" + a.getAggregationTagName() + ">\n");
					sb.append(indent + "    <" + a.destination.getName().toLowerCase() + "/>\n");
					if (a.getCardinality() != Cardinality.MANY_TO_ONE && a.getCardinality() != Cardinality.ONE_TO_ONE) {
						sb.append(indent + "    <" + a.destination.getName().toLowerCase() + "/>\n");
						sb.append(indent + "      ...\n");
					}
					sb.append(indent + "</" + a.getAggregationTagName() + ">\n");
				} else if (a.getAggregationSchema() == AggregationSchema.IMPLICIT_LIST) {
					sb.append(indent + "<" + a.getAggregationTagName() + "/>\n");
					if (a.getCardinality() != Cardinality.MANY_TO_ONE && a.getCardinality() != Cardinality.ONE_TO_ONE) {
						sb.append(indent + "<" + a.getAggregationTagName() + "/>\n");
						sb.append(indent + "  ...\n");
					}
				} else if (a.getAggregationSchema() == AggregationSchema.FLAT) {
					sb.append(indent + "-- elements of " + a.destination.getName() + "(flattened " + a.getName() + ") --\n");
					sb.append(buildSketch(a.destination, depth + 1));
					if (a.getCardinality() != Cardinality.MANY_TO_ONE && a.getCardinality() != Cardinality.ONE_TO_ONE) {
						sb.append(indent + "-- elements of " + a.destination.getName() + "(flattened " + a.getName() + ") --\n");
						sb.append(indent + "     ...\n");
					}
				}
			}
			if (depth == 0) {
				sb.append("</" + table.getName().toLowerCase() + ">\n");
			}
		}
		
		return sb.toString();
	}

}
