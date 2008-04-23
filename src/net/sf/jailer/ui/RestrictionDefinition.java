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

import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.extractionmodel.ExtractionModel;

/**
 * Defines a restriction on an association.
 * 
 * @author Wisser
 */
public class RestrictionDefinition {

	/**
	 * Source table.
	 */
	public final Table from;
	
	/**
	 * Destination table.
	 */
	public final Table to;
	
	/**
	 * Name of restriction or <code>null</code>.
	 */
	public final String name;
	
	/**
	 * Restriction condition.
	 */
	public final String condition;

	/**
	 * Is ignored?
	 */
	public final boolean isIgnored;
	
	/**
	 * Constructor.
	 * 
	 * @param from source table
	 * @param to destination table
	 * @param name name of restriction or <code>null</code>
	 * @param condition restriction condition
	 */
	private RestrictionDefinition(Table from, Table to, String name, String condition, boolean isIgnored) {
		this.from = from;
		this.to = to;
		this.name = name;
		this.condition = condition;
		this.isIgnored = isIgnored;
	}

	/**
	 * Gets list of all restriction definition from {@link ExtractionModel}.
	 * 
	 * @param extractionnModel the extraction model
	 * @return all restriction definition from extractionModel 
	 */
	public static List<RestrictionDefinition> fromRestrictionModel(ExtractionModel extractionModel) {
		List<RestrictionDefinition> list = new ArrayList<RestrictionDefinition>();
		for (Table table: extractionModel.getTasks().get(0).dataModel.getTables()) {
			for (Association association: table.associations) {
				if (association.isRestricted()) {
					list.add(new RestrictionDefinition(association.source, association.destination, association.getName(), association.isIgnored()? "disabled" : association.getRestrictionCondition(), association.isIgnored()));
				}
			}
		}
		Collections.sort(list, new Comparator<RestrictionDefinition>() {
			public int compare(RestrictionDefinition o1, RestrictionDefinition o2) {
				return (o1.from.getName() + " " + o1.to.getName() + " " + o1.name + " " + o1.condition).
						compareTo(o2.from.getName() + " " + o2.to.getName() + " " + o2.name + " " + o2.condition);
			}
		});
		return list;
	}
	
}
