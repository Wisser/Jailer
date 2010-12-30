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
package net.sf.jailer.ui.databrowser;


/**
 * Mutual reference to an object.
 * 
 * @author Ralf Wisser
 *
 * @param <T>
 */
public class Reference<T> {

	private T object;

	/**
	 * Constructor.
	 * 
	 * @param object the object
	 */
	public Reference(T object) {
		this.object = object;
	}

	/**
	 * Gets the object
	 * 
	 * @return the object
	 */
	public T get() {
		return object;
	}

	/**
	 * Sets the object.
	 * 
	 * @param object the object
	 */
	public void set(T object) {
		this.object = object;
	}
	
}
