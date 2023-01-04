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
package net.sf.jailer.subsetting;

/**
 * Thrown if the subsetting result is inconsistent due to insufficient transaction isolation.
 * 
 * @author Ralf Wisser
 */
public class InconsistentSubsettingResultException extends RuntimeException {
	private static final long serialVersionUID = 4705779220417580180L;

	public InconsistentSubsettingResultException(String message) {
		super(message);
	}

}
