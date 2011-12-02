/*
 * Copyright 2007 - 2012 the original author or authors.
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
package net.sf.jailer.util;


/**
 * An ordered pair of objects (a, b).
 * 
 * (a1, b1).equals(a2, b2) 
 * iff
 * (a1 == null && a2 == null || a1.equals(a2))
 * &&
 * (b1 == null && b2 == null || b1.equals(b2))
 * 
 * @author Ralf Wisser
 */
public class Pair<A, B> {

	/**
	 * The <b>a</b> object.
	 */
	public final A a;

	/**
	 * The <b>b</b> object.
	 */
	public final B b;
	
	/**
	 * New Pair (a, b).
	 * 
	 * @param a the <b>a</b> object
	 * @param b the <b>b</b> object
	 */
	public Pair(A a, B b) {
		this.a = a;
		this.b = b;
	}
    
	/**
	 * Compares two pairs.
	 */
	@Override
	public boolean equals(Object other) {
		if (other instanceof Pair) {
			Pair<?, ?> otherPair = (Pair<?, ?>) other;
			if (a == null && otherPair.a != null) {
				return false;
			}
			if (a != null) {
				if (otherPair.a == null) {
					return false;
				}
				if (!a.equals(otherPair.a)) {
					return false;
				}
			}
			
			if (b == null && otherPair.b != null) {
				return false;
			}
			if (b != null) {
				if (otherPair.b == null) {
					return false;
				}
				if (!b.equals(otherPair.b)) {
					return false;
				}
			}
			
			return true;
		}
		return false;
	}

	/**
	 * Hashes a pair.
	 */
	@Override
	public int hashCode() {
		return (a == null? 0 : a.hashCode()) + 3 * (b == null? 0 : b.hashCode());
	}

	/**
	 * Gets string representation of an pair.
	 */
	@Override
	public String toString() {
		return "Pair(" + a + ", " + b + ")";
	}

 }
