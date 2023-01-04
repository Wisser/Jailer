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
package net.sf.jailer.modelbuilder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Transforms (sort/select/group) Memorized Result sets.
 */
public class MemorizedResultSetTransformer {

    public interface AggregateFunction {
        Object eval(List<Object[]> group, int columnIndex);
    }

    public static class ListAggregation implements AggregateFunction {
        @Override
		public Object eval(List<Object[]> group, int columnIndex) {
        	StringBuilder sb = new StringBuilder();
        	for (Object[] row: group) {
        		if (sb.length() > 0) {
        			sb.append(", ");
        		}
        		if (row[columnIndex] != null) {
        			sb.append(String.valueOf(row[columnIndex]));
        		}
        	}
        	return sb.toString();
        }
    }

    /**
     * Definition of a column transformation.
     */
    public static class ColumnTransformation {
        final int columnIndex;
        final AggregateFunction aggregateFunction;
        public ColumnTransformation(int columnIndex, AggregateFunction aggregateFunction) {
            this.columnIndex = columnIndex;
            this.aggregateFunction = aggregateFunction;
        }
    }

    /**
     * Transforms (sort/select/group) Memorized Result sets.
     */
    public static MemorizedResultSet transform(MemorizedResultSet resultSet, ColumnTransformation[] columnTransformations) {
        Map<List<Object>, List<Object[]>> groups = new LinkedHashMap<List<Object>, List<Object[]>>();
        Set<Integer> keyIndexes = new HashSet<Integer>();
        for (ColumnTransformation transformation: columnTransformations) {
            if (transformation.aggregateFunction == null) {
                keyIndexes.add(transformation.columnIndex - 1);
            }
        }
        for (Object[] row: resultSet.getRowList()) {
            List<Object> key = new ArrayList<Object>();
            for (int i: keyIndexes) {
                key.add(row[i]);
            }
            List<Object[]> group = groups.get(key);
            if (group == null) {
                group = new ArrayList<Object[]>();
                groups.put(key, group);
            }
            group.add(row);
        }
        List<Object[]> result = new ArrayList<Object[]>();
        for (List<Object[]> group: groups.values()) {
        	Object[] newRow = new Object[columnTransformations.length];
    		for (int i = 0; i < columnTransformations.length; ++i) {
    			ColumnTransformation transformation = columnTransformations[i];
    			if (transformation.aggregateFunction != null) {
    				newRow[i] = transformation.aggregateFunction.eval(group, transformation.columnIndex - 1);
        		} else {
        			newRow[i] = group.get(0)[columnTransformations[i].columnIndex - 1];
        		}
    		}
        	result.add(newRow);
        }
        int[] types = new int[columnTransformations.length];
		String[] names = new String[columnTransformations.length];
		for (int i = 0; i < columnTransformations.length; ++i) {
			names[i] = resultSet.getMetaData().getColumnName(columnTransformations[i].columnIndex);
			types[i] = resultSet.getMetaData().types[columnTransformations[i].columnIndex - 1];
		}
		return new MemorizedResultSet(result, columnTransformations.length, names, types);
    }

}
