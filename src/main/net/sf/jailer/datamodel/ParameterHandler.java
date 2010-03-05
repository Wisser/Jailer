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

package net.sf.jailer.datamodel;

import java.util.Map;
import java.util.Set;

/**
 * Handles parameters in subject conditions, association-restrictions and XML
 * templates.
 * 
 * @author Ralf Wisser
 */
public class ParameterHandler {

	/**
	 * Valid parameter identifier characters.
	 */
	public static final String VALID_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789-öäüÖÄÜß?#";

	/**
	 * Collects all parameters in a given string.
	 * 
	 * @param s the string
	 * @param parameters to put parameters into
	 */
	public static void collectParameter(String s, Set<String> parameters) {
		int l = s.length();
		int i = 0;
		int pStart = -1;
		char lastC = ' ';
		while (i < l) {
			char c = s.charAt(i);
			if (pStart < 0) {
				if (lastC == '$' && c == '{') {
					pStart = i + 1;
				}
			} else {
				if (c == '}') {
					if (pStart < i) {
						parameters.add(s.substring(pStart, i));
					}
					pStart = -1;
				} else {
					if (VALID_CHARS.indexOf(c) < 0) {
						pStart = -1;
					}
				}
			}
			++i;
			lastC = c;
		}
	}

	/**
	 * Replaces parameters with parameter values.
	 * 
	 * @param s string with parameters
	 * @param parameters map from parameters to parameter values
	 * @return string with parameter values
	 */
	public static String assignParameterValues(String s, Map<String, String> parameterValues) {
		int l = s.length();
		int i = 0;
		int pStart = -1;
		char lastC = ' ';
		while (i < l) {
			char c = s.charAt(i);
			if (pStart < 0) {
				if (lastC == '$' && c == '{') {
					pStart = i + 1;
				}
			} else {
				if (c == '}') {
					if (pStart < i) {
						String value = parameterValues.get(s.substring(pStart, i));
						if (value != null) {
							s = s.substring(0, pStart - 2) + value + s.substring(i + 1);
							l = s.length();
							i = pStart + value.length() - 3;
						}
					}
					pStart = -1;
				} else {
					if (VALID_CHARS.indexOf(c) < 0) {
						pStart = -1;
					}
				}
			}
			++i;
			lastC = c;
		}
		
		return s;
	}
	
}
