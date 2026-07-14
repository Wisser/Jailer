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
package net.sf.jailer.ui.databrowser.geo;

/**
 * Thrown when a spatial column value cannot be decoded into a {@link Geometry}.
 * Callers should fall back to the plain text representation of the value.
 *
 * @author Ralf Wisser
 */
public class GeometryParseException extends Exception {

	private static final long serialVersionUID = 1L;

	public GeometryParseException(String message) {
		super(message);
	}

	public GeometryParseException(String message, Throwable cause) {
		super(message, cause);
	}

}
