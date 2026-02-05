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
package net.sf.jailer;

import junit.framework.TestCase;
import net.sf.jailer.modelbuilder.KnownIdentifierMap;

/**
 * Tests {@link KnownIdentifierMap}
 *  
 * @author Ralf Wisser
 */
public class KnownIdentifierMapTest extends TestCase {

	public void testTableNames() throws Exception {
		KnownIdentifierMap map = new KnownIdentifierMap();
		map.putTableName("u1");
		map.putTableName("U2");
		map.putTableName("U2");
		map.putTableName("nu1");
		map.putTableName("\"nu1\"");
		map.putTableName("nu2");
		map.putTableName("'nu2'");
		map.putTableName("nu3");
		map.putTableName("`nu3`");
		
		assertEquals("u1", map.getTableName("u1"));
		assertEquals("u1", map.getTableName("U1"));
		assertEquals("u1", map.getTableName("'u1'"));
		assertEquals("U2", map.getTableName("U2"));
		assertEquals(null, map.getTableName("nu1"));
		assertEquals(null, map.getTableName("'nu1'"));
		assertEquals(null, map.getTableName("Nu1"));
		assertEquals(null, map.getTableName("Nu2"));
		assertEquals(null, map.getTableName("Nu3"));
		assertEquals(null, map.getTableName("nu3"));
		assertEquals(null, map.getTableName("unknown"));
	}


	public void testColumnNames() throws Exception {
		KnownIdentifierMap map = new KnownIdentifierMap();
		map.putColumnName("A", "u1");
		map.putColumnName("B", "u1");
		map.putColumnName("A", "U2");
		map.putColumnName("'B'", "U2");
		map.putColumnName("A", "nu1");
		map.putColumnName("A", "\"nu1\"");
		map.putColumnName("A", "nu2");
		map.putColumnName("'a'", "'nu2'");
		map.putColumnName("A", "nu3");
		map.putColumnName("A", "`nu3`");
		
		assertEquals("u1", map.getColumnName("a", "u1"));
		assertEquals("u1", map.getColumnName("'b'", "U1"));
		assertEquals("u1", map.getColumnName("A", "'u1'"));
		assertEquals("U2", map.getColumnName("A", "U2"));
		assertEquals(null, map.getColumnName("A", "nu1"));
		assertEquals(null, map.getColumnName("'a'", "'nu1'"));
		assertEquals(null, map.getColumnName("A", "Nu1"));
		assertEquals(null, map.getColumnName("A", "Nu2"));
		assertEquals(null, map.getColumnName("A", "Nu3"));
		assertEquals(null, map.getColumnName("A", "nu3"));
		assertEquals(null, map.getColumnName("A", "unknown"));
	}

	public void testCondition() throws Exception {
		KnownIdentifierMap map = new KnownIdentifierMap();
		map.putCondition("A.u1 = B.u1");
		map.putCondition("B.u1 = B.u1");
		map.putCondition("B.u1 = A.u1 and A.u1 = B.u2");
		map.putCondition("A.U2 = b.u2");
		map.putCondition("A.U2 = b.u2 and 1<0");
		map.putCondition("A.nu1 = B.'NU1'");
		map.putCondition("A.nu1 = B.'nu1'");
		map.putCondition("A.nu3 != A.nu3");
		map.putCondition("A.nu3");
		map.putCondition("a.\"1 2 3\" = b.`#` and A.'7 8 ' = B . test");
		
		assertEquals("A.u1 = B.u1", map.getCondition("A.u1 = B.u1"));
		assertEquals("A.u1 = B.u1", map.getCondition("A.u1 = B.\"U1\""));
		assertEquals("B.u1 = B.u1", map.getCondition("B.u1 = B.u1"));
		assertEquals("B.u1 = A.u1 and A.u1 = B.u2", map.getCondition("B.u1 = A.u1 and A.u1 = B.u2"));
		assertEquals("A.U2 = b.u2", map.getCondition("A.U2 = b.u2"));
		assertEquals("A.U2 = b.u2 and 1<0", map.getCondition("A.U2 = b.u2 and 1<0"));
		assertEquals(null, map.getCondition("A.nu1 = B.'NU1'"));
		assertEquals(null, map.getCondition("A.nu1 = B.'nu1'"));
		assertEquals("A.nu3 != A.nu3", map.getCondition("A.nu3 != A.nu3"));
		assertEquals("A.nu3", map.getCondition("A.NU3"));
		assertEquals(null, map.getCondition("unknown"));
	}
	
}
