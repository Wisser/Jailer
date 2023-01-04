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
package net.sf.jailer.datamodel;

/**
 * All XML-aggregation schemas.
 * 
 * @author Ralf Wisser
 */
public enum AggregationSchema {

	/**
	 * Aggregation as explicit list.
	 */
	EXPLICIT_LIST("as explicit list"),

	/**
	 * Aggregation as implicit list.
	 */
	IMPLICIT_LIST("as implicit list"),

	/**
	 * Embed attributes directly.
	 */
	FLAT("flat"),

	/**
	 * No aggregation.
	 */
	NONE("none");
	
	/**
	 * Constructor.
	 * 
	 * @param displayName human readable name
	 */
	private AggregationSchema(String displayName) {
		this.displayName = displayName;
	}

	/**
	 * Gets human readable name.
	 */
	@Override
	public String toString() {
		return displayName;
	}
	
	/**
	 * Human readable name.
	 */
	private String displayName;
	
}
